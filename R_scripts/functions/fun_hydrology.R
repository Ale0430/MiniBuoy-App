#' Load Rda file containing constant values to estimate current velocity
#' for the different Buoy model types

#### Functions to calculate Hydrodynamic indicators ####

#' Function to predict inundation status, current and wave orbital velocities:
# gaps: minimum gap in an inundation event to be closed (where points were misclassified as non inundated)
# full: minimum duration of a fully inundated event (otherwise event is reclassified as partially inundated)
# part: time window to search for partially inundated cases at the start and end of inundation events
# tilt: minimum tilt to classify an event as fully inundated (otherwise event is reclassified as partially inundated)
# chop: use a proportion of the data for abrupt shift detection (1 = all, 0 = none)
get.hydrodynamics = function(data, design, ui.input_settings = NULL) {
  if (is.data.frame(ui.input_settings)){
    gaps = ui.input_settings$gaps
    full = ui.input_settings$full
    part = ui.input_settings$part
    tilt = ui.input_settings$tilt
    chop = ui.input_settings$chop / 100
  } else {
    gaps = 20
    full = 20
    part = 90
    tilt = 75
    chop = 0.5
    if (chop < 1 & nrow(data) > 0){
      showNotification(paste("NOTE: ", chop * 100, 
                             " % of the data are used in searching for partially 
                           inundated cases. Refine proportion in custom settings.",
                             sep = ""),
                       type = "warning")
    }
  }

  # calculate sampling rate (for selecting the correct current and wave orbital velocity calibration later on):
  rate = as.numeric(data$datetime[2] - data$datetime[1])
  
  data.classified = data %>%
    mutate(
      # truncate acceleration values (above 0 g):
      Acceleration = ifelse(Acceleration > 0, 0, Acceleration),
      # truncate acceleration values (beyond detection limit caused by shock):
      Acceleration = if      (design == 'B4' | design == 'B4+') { ifelse(Acceleration < -1.15, NA, Acceleration) } 
                     else if (design == 'Pendant') { ifelse(Acceleration < -1.075, NA, Acceleration) },
      # calculate 1-minute running standard deviation (for wave orbital velocity calculation):
      runSD = runsd(Acceleration, 60 / rate)) %>%
    # aggregate and summarise acceleration data:
    group_by(
      datetime = if      (design == 'B4' | design == 'B4+') { ceiling_date(datetime, unit = 'minute') }
                 else if (design == 'Pendant')              { ceiling_date(datetime, unit = '10 minutes') }) %>%
    summarise(
      Mean  = mean(Acceleration, na.rm = T),
      IQR   = quantile(Acceleration, 0.75, names = F, na.rm = T) - quantile(Acceleration, 0.25, names = F, na.rm = T),
      runSD = mean(runSD, na.rm = T)) %>%
    ungroup() %>%
    mutate(
      # calculate tilt from acceleration data:
      Tilt = ((-180*(asin(ifelse(Mean < -1, -1, Mean))))/pi),
      # calculate non inundation (N) and inundation (F) using trained classifiers:
      Status = predict(
        if      (design == 'B4')      { readRDS('./models/I_B4_NF.rds')      } 
        else if (design == 'B4+')     { readRDS('./models/I_B4+_NF.rds')     } 
        else if (design == 'Pendant') { readRDS('./models/I_Pendant_NF.rds') }, 
        newdata = tibble(Tilt, IQR)),
      # classify the sequence of inundation events:
      Event   = recode(Status, 'N' = 0, 'P' = 1, 'F' = 1),
      Event   = replace(cumsum(!Event), !Event, NA),
      Event   = as.integer(factor((Event))),
      # merge short (and likely misclassified) inundation events:
      Event   = na.approx(Event, maxgap = gaps, na.rm = F),
      # reclassify inundation status using the corrected inundation events sequence:
      Status  = ifelse(is.na(Event), 'N', 'F'),
      # reclassify the sequence of inundation events with corrected inundation status classification:
      Event   = recode(Status, 'N' = 0, 'P' = 1, 'F' = 1),
      Event   = replace(cumsum(!Event), !Event, NA),
      Event   = as.integer(factor((Event)))) %>%
    # convert short inundated events to non inundated:
    group_by(Event) %>%
    mutate(
      Status = ifelse(length(Event[!is.na(Event)]) < full, 'N', 'F')) %>%
    ungroup() %>%
    mutate(
      Event   = recode(Status, 'N' = 0, 'P' = 1, 'F' = 1),
      Event   = replace(cumsum(!Event), !Event, NA),
      Event   = as.integer(factor((Event)))) %>%
    group_by(Event) %>%
    mutate(
      # classify each inundation event as flood and ebb tide: 
      Tide = as.factor(ifelse(Status == 'N', NA, c(rep('Flood', round((n() / 2), 0)), rep('Ebb', n() - round((n() / 2), 0)))))) %>%
    ungroup()
  
  # part must be even, convert if necessary:
  if(part %% 2 == 0 ) { part } else { part = 2 * round(part / 2) }
  
  # find data within the search window at the start and end of each flood and ebb event for detecting abrupt shifts indicative of partially inundated cases:
  x = sapply(cumsum(rle(data.classified$Status)$lengths), function(i, part) { (i-part/2):(i+((part/2)-1)) }, part)
  x = stack(data.frame(x))
  # exclude non-values in case selection goes beyond the bounds of the data:
  x = filter(x, values > 0 & values <= nrow(data.classified)) 

  # subset data around flood and ebb events:
  df.s = data.classified %>%
    slice(x$values) %>%
    bind_cols(Change = as.numeric(x$ind)) %>%
    group_by(Change) %>%
    mutate(
      Tide  = as.character(Tide),
      Tide  = ifelse(is.na(Tide), unique(Tide[!is.na(Tide)]), Tide)) %>%
    # remove events if misclassifed by rle (sometimes the last row is included as a transition, which it is not):
    drop_na(Tide) 
  
  # subset the data (now all = 1) to reduce CPU time if the search window is large:
  shift = df.s %>%
    group_by(Change) %>%
    slice(round(seq(1, n(), length.out = (chop * n())), 0)) %>%
    mutate(
      # detect abrupt shifts (normalise to remove direction of shift):
      Shift = case_when(Tide == 'Flood' ~ as_detect(Tilt),
                        Tide == 'Ebb'   ~ as_detect(Tilt) * -1),
      Shift  = ifelse(Shift > 0, Shift, 0)) %>%
    ungroup()
  
  # remove any duplicated rows:
  shift = distinct(shift, datetime, .keep_all = T)

  # perform a linear interpolation to unify datasets:
  shift = data.frame(approx(shift$datetime, y = shift$Shift, xout = seq(min(shift$datetime), max(shift$datetime), 60)))
  names(shift) = c('datetime', 'Partial')

  # classify partially inundated cases based on the threshold change detection score (t):
  df.s = df.s %>% left_join(., shift, 'datetime') %>%
    mutate(Partial = ifelse(Partial > 0, 'P', Status)) %>%
    select(datetime, Partial, Change)

  # reclassify inundation status using partially inundated cases:
  data.classified = left_join(data.classified, df.s, by = 'datetime') %>%
    mutate(Status = ifelse(Status == 'F' & !is.na(Partial), Partial, Status)) %>%
    mutate_if(is.character, as.factor)

  data.classified = data.classified %>%
    mutate(
      # convert cases when tilt at 90 has been classified partially inundated to fully inundated: 
      Status = case_when(
        Tilt == 90 ~ 'F',
        TRUE ~ as.character(Status)),
      # reclassify the sequence of inundation events with corrected inundation status classification:
      Event = recode(Status, 'N' = 0, 'P' = 0, 'F' = 1),
      Event = replace(cumsum(!Event), !Event, NA),
      Event = as.integer(factor((Event))),
      # merge short (and likely misclassified) inundation events:
      Event  = na.approx(Event, maxgap = gaps, na.rm = F),
      # reclassify inundation status using the corrected inundation events sequence:
      Status = case_when(
        Status == 'P' & !is.na(Event) ~ 'F',
        TRUE ~ as.character(Status)),
      # reclassify the sequence of inundation events with corrected inundation status classification:
      Event = recode(Status, 'N' = 0, 'P' = 0, 'F' = 1),
      Event = replace(cumsum(!Event), !Event, NA),
      Event = as.integer(factor((Event)))) %>%
    group_by(Event) %>%
    mutate(
      # reclassify any full inundation as partial inundation if tilt fails to reach a minimum value:
      dCheck = max(Tilt),
      dCheck = ifelse(is.na(Event), NA, dCheck),
      Status = case_when(
        dCheck < tilt ~ 'P',
        TRUE ~ as.character(Status))) %>%
    ungroup() %>%
    # reclassify the sequence of inundation events with corrected inundation status classification:
    mutate(
      Event = recode(Status, 'N' = 0, 'P' = 1, 'F' = 1), # note: P = 1 to include partial cases in event classification and thus inundation duration
      Event = replace(cumsum(!Event), !Event, NA),
      Event = as.integer(factor((Event)))) %>%
    # remove any duplicates that may have been introduced:
    distinct(datetime, .keep_all = T) %>%  
    mutate(
      # calculate current velocity during full inundation:
      CurrentVelocity = 
        if      (design == 'B4'      & rate == 1)   { ifelse(Status == 'F', 1.327982676 + (-111.968186400 * Tilt) + (24.601180950 * Tilt ^ 2) + (-4.646001843 * Tilt ^ 3), NA) }
        else if (design == 'B4'      & rate == 2)   { ifelse(Status == 'F', 1.287465546 + ( -76.484142300 * Tilt) + (16.562083690 * Tilt ^ 2) + (-2.992576646 * Tilt ^ 3), NA) }
        else if (design == 'B4'      & rate == 3)   { ifelse(Status == 'F', 1.178190788 + ( -56.426567620 * Tilt) + (11.670334830 * Tilt ^ 2) + (-1.858891517 * Tilt ^ 3), NA) }
        else if (design == 'B4'      & rate == 4)   { ifelse(Status == 'F', 0.974750776 + ( -39.621267190 * Tilt) + ( 7.777249437 * Tilt ^ 2) + (-1.221340473 * Tilt ^ 3), NA) }
        else if (design == 'B4'      & rate == 5)   { ifelse(Status == 'F', 0.962632579 + ( -34.919812000 * Tilt) + ( 6.868197485 * Tilt ^ 2) + (-1.045676666 * Tilt ^ 3), NA) }
        else if (design == 'B4'      & rate == 6)   { ifelse(Status == 'F', 1.122024526 + ( -37.746999030 * Tilt) + ( 7.505287857 * Tilt ^ 2) + (-0.983853256 * Tilt ^ 3), NA) }
        else if (design == 'B4'      & rate == 7)   { ifelse(Status == 'F', 0.244845270 + (  -3.983081510 * Tilt) + (-1.711368654 * Tilt ^ 2) + ( 1.116835340 * Tilt ^ 3), NA) }
        else if (design == 'B4'      & rate == 8)   { ifelse(Status == 'F', 1.236332712 + ( -36.694289410 * Tilt) + ( 8.176819558 * Tilt ^ 2) + (-1.847245525 * Tilt ^ 3), NA) }
        else if (design == 'B4'      & rate == 9)   { ifelse(Status == 'F', 1.572679542 + ( -44.781576690 * Tilt) + (10.109522260 * Tilt ^ 2) + (-1.383777215 * Tilt ^ 3), NA) }
        else if (design == 'B4'      & rate == 10)  { ifelse(Status == 'F', 1.041174263 + ( -27.127508680 * Tilt) + ( 5.647560250 * Tilt ^ 2) + (-1.068772529 * Tilt ^ 3), NA) }
        else if (design == 'B4+'     & rate == 1)   { ifelse(Status == 'F', 0.947729965 + (  -0.010331140 * Tilt), NA) }
        else if (design == 'B4+'     & rate == 2)   { ifelse(Status == 'F', 0.931892290 + (  -0.010109484 * Tilt), NA) }
        else if (design == 'B4+'     & rate == 3)   { ifelse(Status == 'F', 0.922630764 + (  -0.009972599 * Tilt), NA) }
        else if (design == 'B4+'     & rate == 4)   { ifelse(Status == 'F', 0.908081623 + (  -0.009762219 * Tilt), NA) }
        else if (design == 'B4+'     & rate == 5)   { ifelse(Status == 'F', 0.900525865 + (  -0.009624605 * Tilt), NA) }
        else if (design == 'B4+'     & rate == 6)   { ifelse(Status == 'F', 0.887583355 + (  -0.009440689 * Tilt), NA) }
        else if (design == 'B4+'     & rate == 7)   { ifelse(Status == 'F', 0.888956883 + (  -0.009450901 * Tilt), NA) }
        else if (design == 'B4+'     & rate == 8)   { ifelse(Status == 'F', 0.852573695 + (  -0.008953314 * Tilt), NA) }
        else if (design == 'B4+'     & rate == 9)   { ifelse(Status == 'F', 0.862714427 + (  -0.009090706 * Tilt), NA) }
        else if (design == 'B4+'     & rate == 10)  { ifelse(Status == 'F', 0.833515346 + (  -0.008640365 * Tilt), NA) }
        else if (design == 'Pendant' & rate == 120) { ifelse(Status == 'F', 0.267382747 + (  -5.696430351 * Tilt) + (2.211267315 * Tilt ^ 2) + (-0.878314377 * Median ^ 3), NA) }
        else if (design == 'Pendant' & rate == 240) { ifelse(Status == 'F', 0.265114822 + (  -2.537513647 * Tilt) + (1.003144976 * Tilt ^ 2) + (-0.445975484 * Median ^ 3), NA) }
        else if (design == 'Pendant' & rate == 360) { ifelse(Status == 'F', 0.265893101 + (  -2.540444228 * Tilt) + (0.987726452 * Tilt ^ 2) + (-0.421841667 * Median ^ 3), NA) }
        else if (design == 'Pendant' & rate == 480) { ifelse(Status == 'F', 0.268366319 + (  -2.573418888 * Tilt) + (1.035693222 * Tilt ^ 2) + (-0.437099348 * Median ^ 3), NA) }
        else if (design == 'Pendant' & rate == 600) { ifelse(Status == 'F', 0.268577406 + (  -2.565829221 * Tilt) + (1.043130739 * Tilt ^ 2) + (-0.390593216 * Median ^ 3), NA) },
      # calculate wave orbital velocity during full inundation:
      WaveOrbitalVelocity =
        if      (design == 'B4+' & rate == 1)  { ifelse(Status == 'F', (runSD * 1.801662524) - 0.005038870, NA) }
        else if (design == 'B4+' & rate == 2)  { ifelse(Status == 'F', (runSD * 1.665072518) - 0.003362936, NA) }
        else if (design == 'B4+' & rate == 3)  { ifelse(Status == 'F', (runSD * 1.687692307) - 0.005717337, NA) } 
        else if (design == 'B4+' & rate == 4)  { ifelse(Status == 'F', (runSD * 1.597467709) - 0.000999000, NA) } 
        else if (design == 'B4+' & rate == 5)  { ifelse(Status == 'F', (runSD * 1.617975298) - 0.002078224, NA) } 
        else if (design == 'B4+' & rate == 6)  { ifelse(Status == 'F', (runSD * 1.655866528) - 0.004507210, NA) } 
        else if (design == 'B4+' & rate == 7)  { ifelse(Status == 'F', (runSD * 1.707258823) - 0.007984811, NA) } 
        else if (design == 'B4+' & rate == 8)  { ifelse(Status == 'F', (runSD * 1.595497726) - 0.001174779, NA) } 
        else if (design == 'B4+' & rate == 9)  { ifelse(Status == 'F', (runSD * 1.513542336) + 0.002575534, NA) } 
        else if (design == 'B4+' & rate == 10) { ifelse(Status == 'F', (runSD * 1.445359176) + 0.004573273, NA) } 
        else { NA })
  
  # check for full days:
  FullCheck = data.classified %>%
    group_by(ceiling_date(datetime, unit = 'days')) %>%
    summarise(Duration = n() * (.$datetime[2] - .$datetime[1])) %>%
    rename(datetime = 1) %>%
    mutate(FullDay = ifelse(Duration == '1440', T, F),
           Day     = as.Date(datetime)) %>%
    select(Day, FullDay)
  
  # add a tag for days that have full data:
  data.classified = data.classified %>%
    mutate(Day = as.Date(datetime)) %>%
    left_join(FullCheck, by = c('Day')) %>%
    dplyr::select(datetime, Tilt, Status, Event, Tide, CurrentVelocity, WaveOrbitalVelocity, FullDay) %>% 
    mutate_if(is.character, as.factor)
  
  return(data.classified)
}

# List of functions for extracting hydrodynamic parameters:
hydro.SurvMins           = function(data) { data %>% summarise(Value = difftime(max(datetime), min(datetime), units = 'mins')[[1]]) }
hydro.SurvDays           = function(data) { data %>% hydro.SurvMins() / 60 / 24 }
hydro.NumEvents          = function(data) { data %>% na.omit() %>% summarise(Value = n_distinct(Event)) }
hydro.DurEvents          = function(data) { data %>% group_by(Event) %>% summarise(Value = difftime(max(datetime), min(datetime), units = 'mins')[[1]]) %>% na.omit() }
hydro.IndDurMins         = function(data) { data %>% hydro.DurEvents() %>% summarise(Value = sum(Value)) }
hydro.NonIndDurMins      = function(data) { data %>% summarise(hydro.SurvMins(.) - hydro.IndDurMins(.)) }
hydro.IndDurPerc         = function(data) { data %>% summarise(hydro.IndDurMins(.) / hydro.SurvMins(.) * 100) }
hydro.NonIndDurPerc      = function(data) { data %>% summarise(hydro.NonIndDurMins(.) / hydro.SurvMins(.) * 100) }
hydro.IndDurDay          = function(data) { data %>% filter(FullDay == T) %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = as.numeric(sum(!is.na(Event)) * (.$datetime[2] - .$datetime[1]))) %>% rename(datetime = 1) }
hydro.NonIndDurDay       = function(data) { data %>% filter(FullDay == T) %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = as.numeric(sum(is.na(Event)) * (.$datetime[2] - .$datetime[1]))) %>% rename(datetime = 1) }
hydro.IndDurPercDay      = function(data) { data %>% hydro.IndDurDay()    %>% mutate(Value = (Value / 1440) * 100) }
hydro.NonIndDurPercDay   = function(data) { data %>% hydro.NonIndDurDay() %>% mutate(Value = (Value / 1440) * 100) }
hydro.IndFreqDay         = function(data) { data %>% filter(FullDay == T) %>% na.omit() %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = n_distinct(Event)) %>% rename(datetime = 1) }
hydro.IndFreqDayMean     = function(data) { data %>% hydro.IndFreqDay()   %>% summarise(Value = mean(Value, na.rm = T)) }
hydro.IndFreqDayMed      = function(data) { data %>% hydro.IndFreqDay()   %>% summarise(Value = median(Value, na.rm = T)) }
# hydro.MaxWoO             = function(data) { data %>% mutate(Value = ifelse(is.na(Event), 0, 1)) %>% summarise(data.frame(unclass(rle(Value))) %>% filter(values == 0)) %>% summarise(Value = as.numeric(max(lengths) * (data$datetime[2] - data$datetime[1]) / 60 / 24)) }
hydro.MaxWoO = function(data) { 
  df = data %>% mutate(Value = ifelse(is.na(Event), 0, 1))
  df = filter(data.frame(unclass(rle(df$Value))), values == 0)
  df = as.numeric(max(df$lengths) * (data$datetime[2] - data$datetime[1]) / 60 / 24)
  df = data.frame(df) %>% rename(Value = 1)
  return(df) }

hydro.PeakCurEventTide   = function(data) { data %>% group_by(Event, Tide) %>% na.omit() %>% summarise(Value = max(CurrentVelocity)) }
hydro.PeakCurEvent       = function(data) { data %>% group_by(Event)       %>% na.omit() %>% summarise(Value = max(CurrentVelocity)) }
hydro.PeakCurDay         = function(data) { data %>% filter(FullDay == T)  %>% na.omit() %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = max(CurrentVelocity)) %>% rename(datetime = 1) }
hydro.UpperQCurEventTide = function(data) { data %>% group_by(Event, Tide) %>% na.omit() %>% summarise(Value = quantile(CurrentVelocity, 0.75, names = F)) }
hydro.UpperQCurEvent     = function(data) { data %>% group_by(Event)       %>% na.omit() %>% summarise(Value = quantile(CurrentVelocity, 0.75, names = F)) }
hydro.UpperQCurDay       = function(data) { data %>% filter(FullDay == T)  %>% na.omit() %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = quantile(CurrentVelocity, 0.75, names = F)) %>% rename(datetime = 1) }
hydro.MeanCurEventTide   = function(data) { data %>% group_by(Event, Tide) %>% na.omit() %>% summarise(Value = mean(CurrentVelocity)) }
hydro.MeanCurEvent       = function(data) { data %>% group_by(Event)       %>% na.omit() %>% summarise(Value = mean(CurrentVelocity)) }
hydro.MeanCurDay         = function(data) { data %>% filter(FullDay == T)  %>% na.omit() %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = mean(CurrentVelocity)) %>% rename(datetime = 1) }
hydro.MedCurEventTide    = function(data) { data %>% group_by(Event, Tide) %>% na.omit() %>% summarise(Value = median(CurrentVelocity)) }
hydro.MedCurEvent        = function(data) { data %>% group_by(Event)       %>% na.omit() %>% summarise(Value = median(CurrentVelocity)) }
hydro.MedCurDay          = function(data) { data %>% filter(FullDay == T)  %>% na.omit() %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = median(CurrentVelocity)) %>% rename(datetime = 1) }
hydro.CurPeak            = function(data) { data %>% na.omit() %>% summarise(Value = max(CurrentVelocity)) }
hydro.CurUpperQ          = function(data) { data %>% na.omit() %>% summarise(Value = quantile(CurrentVelocity, 0.75, names = F)) }
hydro.CurMean            = function(data) { data %>% na.omit() %>% summarise(Value = mean(CurrentVelocity)) }
hydro.CurMed             = function(data) { data %>% na.omit() %>% summarise(Value = median(CurrentVelocity)) }

hydro.PeakWaveEvent      = function(data) { data %>% group_by(Event)      %>% na.omit() %>% summarise(Value = max(WaveOrbitalVelocity)) }
hydro.PeakWaveDay        = function(data) { data %>% filter(FullDay == T) %>% na.omit() %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = max(WaveOrbitalVelocity)) %>% rename(datetime = 1) }
hydro.UpperQWaveEvent    = function(data) { data %>% group_by(Event)      %>% na.omit() %>% summarise(Value = quantile(WaveOrbitalVelocity, 0.75, names = F)) }
hydro.UpperQWaveDay      = function(data) { data %>% filter(FullDay == T) %>% na.omit() %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = quantile(WaveOrbitalVelocity, 0.75, names = F)) %>% rename(datetime = 1) }
hydro.MeanWaveEvent      = function(data) { data %>% group_by(Event)      %>% na.omit() %>% summarise(Value = mean(WaveOrbitalVelocity)) }
hydro.MeanWaveDay        = function(data) { data %>% filter(FullDay == T) %>% na.omit() %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = mean(WaveOrbitalVelocity)) %>% rename(datetime = 1) }
hydro.MedWaveEvent       = function(data) { data %>% group_by(Event)      %>% na.omit() %>% summarise(Value = median(WaveOrbitalVelocity)) }
hydro.MedWaveDay         = function(data) { data %>% filter(FullDay == T) %>% na.omit() %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = median(WaveOrbitalVelocity)) %>% rename(datetime = 1) }
hydro.WavePeak           = function(data) { data %>% na.omit() %>% summarise(Value = max(WaveOrbitalVelocity)) }
hydro.WaveUpperQ         = function(data) { data %>% na.omit() %>% summarise(Value = quantile(WaveOrbitalVelocity, 0.75, names = F)) }
hydro.WaveMean           = function(data) { data %>% na.omit() %>% summarise(Value = mean(WaveOrbitalVelocity)) }
hydro.WaveMed            = function(data) { data %>% na.omit() %>% summarise(Value = median(WaveOrbitalVelocity)) }

hydro.PeakAssymEvent     = function(data) { data %>% hydro.PeakCurEventTide()   %>% group_by(Event) %>% spread(Tide, Value) %>% summarise(Value = Ebb / Flood) }
hydro.UpperQAssymEvent   = function(data) { data %>% hydro.UpperQCurEventTide() %>% group_by(Event) %>% spread(Tide, Value) %>% summarise(Value = Ebb / Flood) }
hydro.PeakAssym          = function(data) { data %>% hydro.PeakAssymEvent()  %>% na.omit()   %>% summarise(Value = median(Value)) }
hydro.UpperQAssym        = function(data) { data %>% hydro.UpperQAssymEvent()   %>% summarise(Value = median(Value)) }

get.summary.statisics = function(data) {
  
  rbind.data.frame(
    cbind(Parameter = 'Survey minutes',                Units = '[min]',   hydro.SurvMins(data)),
    cbind(Parameter = 'Survey days',                   Units = '[day]',   hydro.SurvDays(data)),
    cbind(Parameter = 'Inundation events',             Units = '[n]',     hydro.NumEvents(data)),
    cbind(Parameter = 'Inundation duration',           Units = '[min]',   hydro.IndDurMins(data)),
    cbind(Parameter = 'Emersion duration',             Units = '[min]',   hydro.NonIndDurMins(data)),
    cbind(Parameter = 'Inundation proportion',         Units = '[%]',     hydro.IndDurPerc(data)),
    cbind(Parameter = 'Emersion proportion',           Units = '[%]',     hydro.NonIndDurPerc(data)),
    cbind(Parameter = 'Mean inundation frequency',     Units = '[n/day]', hydro.IndFreqDayMean(data)),
    cbind(Parameter = 'Median inundation frequency',   Units = '[n/day]', hydro.IndFreqDayMed(data)),
    cbind(Parameter = 'Maximum Window of Opportunity', Units = '[day]',   hydro.MaxWoO(data)),
    cbind(Parameter = 'Peak current velocity',         Units = '[m/s]',   hydro.CurPeak(data)),
    cbind(Parameter = 'Upper current velocity',        Units = '[m/s]',   hydro.CurUpperQ(data)),
    cbind(Parameter = 'Mean current velocity',         Units = '[m/s]',   hydro.CurMean(data)),
    cbind(Parameter = 'Median current velocity',       Units = '[m/s]',   hydro.CurMed(data)),
    cbind(Parameter = 'Peak wave orbital velocity',    Units = '[m/s]',   hydro.WavePeak(data)),
    cbind(Parameter = 'Upper wave orbital velocity',   Units = '[m/s]',   hydro.WaveUpperQ(data)),
    cbind(Parameter = 'Mean wave orbital velocity',    Units = '[m/s]',   hydro.WaveMean(data)),
    cbind(Parameter = 'Median wave orbital velocity',  Units = '[m/s]',   hydro.WaveMed(data)),
    cbind(Parameter = 'Peak ebb-flood ratio',          Units = '[-]',     hydro.PeakAssym(data)))

  }

get.daily.statistics = function(data) {
  
  rbind.data.frame(
    cbind(Parameter = 'Inundation duration',          Units = '[min]', hydro.IndDurDay(data)),
    cbind(Parameter = 'Emersion duration',            Units = '[min]', hydro.NonIndDurDay(data)),
    cbind(Parameter = 'Inundation proportion',        Units = '[%]',   hydro.IndDurPercDay(data)),
    cbind(Parameter = 'Emersion proportion',          Units = '[%]',   hydro.NonIndDurPercDay(data)),
    cbind(Parameter = 'Inundation frequency',         Units = '[n]',   hydro.IndFreqDay(data)),
    cbind(Parameter = 'Peak current velocity',        Units = '[m/s]', hydro.PeakCurDay(data)),
    cbind(Parameter = 'Upper current velocity',       Units = '[m/s]', hydro.UpperQCurDay(data)),
    cbind(Parameter = 'Mean current velocity',        Units = '[m/s]', hydro.MeanCurDay(data)),
    cbind(Parameter = 'Median current velocity',      Units = '[m/s]', hydro.MedCurDay(data)),
    cbind(Parameter = 'Peak wave orbital velocity',   Units = '[m/s]', hydro.PeakWaveDay(data)),
    cbind(Parameter = 'Upper wave orbital velocity',  Units = '[m/s]', hydro.UpperQWaveDay(data)),
    cbind(Parameter = 'Mean wave orbital velocity',   Units = '[m/s]', hydro.MeanWaveDay(data)),
    cbind(Parameter = 'Median wave orbital velocity', Units = '[m/s]', hydro.MedWaveDay(data)))
  
}

get.event.statistics = function(data) {

  rbind.data.frame(
    cbind(Parameter = 'Inundation duration',          Units = '[min]', hydro.DurEvents(data)),
    cbind(Parameter = 'Peak current velocity',        Units = '[m/s]', hydro.PeakCurEvent(data)),
    cbind(Parameter = 'Upper current velocity',       Units = '[m/s]', hydro.UpperQCurEvent(data)),
    cbind(Parameter = 'Mean current velocity',        Units = '[m/s]', hydro.MeanCurEvent(data)),
    cbind(Parameter = 'Median current velocity',      Units = '[m/s]', hydro.MedCurEvent(data)),
    cbind(Parameter = 'Peak wave orbital velocity',   Units = '[m/s]', hydro.PeakWaveEvent(data)),
    cbind(Parameter = 'Upper wave orbital velocity',  Units = '[m/s]', hydro.UpperQWaveEvent(data)),
    cbind(Parameter = 'Mean wave orbital velocity',   Units = '[m/s]', hydro.MeanWaveEvent(data)),
    cbind(Parameter = 'Median wave orbital velocity', Units = '[m/s]', hydro.MedWaveEvent(data)),
    cbind(Parameter = 'Peak ebb-flood ratio',         Units = '[-]',   hydro.PeakAssymEvent(data)),
    cbind(Parameter = 'Upper ebb-flood ratio',        Units = '[-]',   hydro.UpperQAssymEvent(data)))
    
}

get.tidal.statistics = function(data) {
  
  rbind.data.frame(
    cbind(Parameter = 'Peak current velocity',   Units = '[m/s]', hydro.PeakCurEventTide(data)),
    cbind(Parameter = 'Upper current velocity',  Units = '[m/s]', hydro.UpperQCurEventTide(data)),
    cbind(Parameter = 'Mean current velocity',   Units = '[m/s]', hydro.MeanCurEventTide(data)),
    cbind(Parameter = 'Median current velocity', Units = '[m/s]', hydro.MedCurEventTide(data)))
  
}


#' Function to generate results text
get.stats.text = function(data){
  
  Statistics = data %>%
    filter(
      Parameter %in% c(
        'Survey days',
        'Median inundation frequency',
        'Peak ebb-flood ratio',
        'Maximum Window of Opportunity',
        'Mean inundation frequency',
        'Upper current velocity',
        'Upper wave orbital velocity'))

  shortnames = data.frame(
    ParameterShort = c('SurveyDays', 'Tide', 'EbbFlood', 'MaxWoO', 'Inundation', 'UpperCurrent', 'UpperWave'),
    Parameter = c(
      'Survey days',
      'Median inundation frequency',
      'Peak ebb-flood ratio',
      'Maximum Window of Opportunity',
      'Mean inundation frequency',
      'Upper current velocity',
      'Upper wave orbital velocity'))
  Statistics = Statistics %>% left_join(., shortnames, by = 'Parameter') 
  
  m = paste(
    'The survey lasted for <b>',
    Statistics %>%
      filter(ParameterShort == 'SurveyDays') %>%
      select(Value) %>% round(., 1),
    ' days</b>, ',
    if (Statistics %>%
        filter(ParameterShort == 'SurveyDays') %>%
        select(Value) < 15)
    {
      'which is too short to provide robust results. Next time, we recommend a longer deployment of at least 15 days to represent a full spring-neap cycle.<br/><br/>'
    } else {
      'which is long enough to provide robust results.<br/><br/>'
    },
    'Inundation at the site appears to be ',
    if (Statistics %>%
        filter(ParameterShort == 'Tide') %>%
        select(Value) == 0) # was: %>% summarise(round(., digits = 0)) == 0)
    {
      '<b>continuous</b>.<br/><br/>'
    } else
      if (Statistics %>%
          filter(ParameterShort == 'Tide') %>%
          select(Value) == 1) # was: %>%  summarise(round(., digits = 0)) == 1)
      {
        '<b>tidal (diurnal)</b>.<br/><br/>'
      } else
        if (Statistics %>%
            filter(ParameterShort == 'Tide') %>%
            select(Value) == 2) # was: %>% summarise(round(., digits = 0))
        {
          '<b>tidal (semi-diurnal)</b>.<br/><br/>'
        } else {
          '<b>mixed (i.e. no clear tidal signal)</b>.<br/><br/><br/>'
        },
    'Tides at the site apper to be ',
    if (Statistics %>%
        filter(ParameterShort == 'EbbFlood') %>%
        select(Value) > 1.1)
    {
      '<b>ebb dominant</b>, implying a seaward net movement of coarse sediment.<br/><br/>'
    } else
      if (Statistics %>%
          filter(ParameterShort == 'EbbFlood') %>%
          select(Value) < 0.9)
      {
        '<b>flood dominant</b>, implying a landward net movement of coarse sediment.<br/><br/>'
      } else {
        '<b>symmetrical</b>, implying no net transport of coarse sediment.<br/><br/><br/>'
      },
    'The longest window of opportunity was <b>',
    Statistics %>%
      filter(ParameterShort == 'MaxWoO') %>%
      select(Value) %>% round(., 1),
    ' days</b>, ',
    if (Statistics %>%
        filter(ParameterShort == 'MaxWoO') %>%
        select(Value) > 4)
    {
      ' which is long enough to expect natural seedling establishment at this site.<br/><br/>'
    } else {
      'which is too short to expect natural seedling establishment at this site.<br/><br/>'
    },
    'Inundation frequency was <b>',
    Statistics %>%
      filter(ParameterShort == 'Inundation') %>%
      select(Value) %>% round(., 2),
    ' per day</b>, ',
    if (Statistics %>%
        filter(ParameterShort == 'Inundation') %>%
        select(Value) <= 2)
    {
      'which provides suitable conditions for plants to survive.<br/><br/>'
    } else {
      'which is too frequent for plants to survive.<br/><br/>'
    },
    'Larger proportion of current velocities are <b>',
    Statistics %>%
      filter(ParameterShort == 'UpperCurrent') %>%
      select(Value) %>%
      summarise(round(., digits = 2)),
    ' m/s</b>, so can be considered',
    if (Statistics %>%
        filter(ParameterShort == 'UpperCurrent') %>%
        select(Value) > 0.15)
    {
      ' high enough to cause scour and dislodge plants.'
    } else {
      ' low enough to allow coastal plants to thrive. <br/><br/>'
    },
    'Larger proportion of wave orbital velocities are <b>',
    Statistics %>%
      filter(ParameterShort == 'UpperWave') %>%
      select(Value) %>%
      summarise(round(., digits = 2)),
    ' m/s</b>, so can be considered',
    if (Statistics %>%
        filter(ParameterShort == 'UpperWave') %>%
        select(Value) > 0.1)
    {
      ' high enough to cause scour and dislodge plants.'
    } else {
      ' low enough to allow coastal plants to thrive.<br/><br/>'
    }
  )
  return(m)
}

#' Function for site comparison
get.comparison = function(stats.t, stats.r){ 
  comparison = stats.t  %>% 
    left_join(., stats.r %>% 
                select(-Units), 'Parameter') %>%
    rename(Target = Value.x, Reference = Value.y) %>%
    mutate(DifferenceAbsolute = Target - Reference, 
           DifferencePercentage = DifferenceAbsolute / Reference * 100,
           TargetIs = paste(round(abs(DifferencePercentage), 1),
                               ifelse(DifferencePercentage > 0, "% higher", "% lower"))) 
  
  return(comparison)
} 

