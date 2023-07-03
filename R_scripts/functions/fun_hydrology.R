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
  } else {
    gaps = 1
    full = 1
    part = 25
    tilt = 75
  }
  
  # calculate sampling rate (for selecting the correct current and wave orbital velocity calibration later on):
  rate = as.numeric(difftime(data$datetime[2], data$datetime[1], units = 'secs'))
  
  data.NF = data %>%
    mutate(
      # truncate acceleration values (above 0 g):
      Acceleration = ifelse(Acceleration > 0, 0, Acceleration),
      # truncate acceleration values (beyond detection limit caused by shock):
      Acceleration = if      (design == 'B4' | design == 'B4+') { ifelse(Acceleration < -1.15, NA, Acceleration) } 
                     else if (design == 'Pendant') { ifelse(Acceleration < -1.075, NA, Acceleration) },
      # calculate 1-minute running standard deviation (for wave orbital velocity calculation):
      runSD = if(design == 'B4+') { runsd(Acceleration, 60 / rate) }) %>%
    # aggregate and summarise acceleration data:
    group_by(
      datetime = if      (design == 'B4' | design == 'B4+') { ceiling_date(datetime, unit = '1 minute') }
                 else if (design == 'Pendant')              { ceiling_date(datetime, unit = '2 minutes') }) %>%
    summarise(
      Mean  = mean(Acceleration, na.rm = T),
      IQR   = quantile(Acceleration, 0.75, names = F, na.rm = T) - quantile(Acceleration, 0.25, names = F, na.rm = T),
      runSD = if(design == 'B4+') { mean(runSD, na.rm = T) }) %>%
    ungroup()
  
  # convert gaps and full arguments depending on Mini Buoy aggregation rate:
  gaps = ifelse(gaps == 0, 
                gaps,
                data.NF %>%
                  group_by(datetime = ceiling_date(datetime, unit = paste(gaps, 'hours'))) %>%
                  summarise(Count = n()) %>%
                  ungroup() %>%
                  summarise(median = median(Count)) %>%
                  as.numeric())
  
  full = ifelse(full == 0,
                full,
                data.NF %>%
                  group_by(datetime = ceiling_date(datetime, unit = paste(full, 'hours'))) %>%
                  summarise(Count = n()) %>%
                  ungroup() %>%
                  summarise(median = median(Count)) %>%
                  as.numeric())
  
  data.NF = data.NF %>%
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
  
  # select a proportion of the start and end of each flood/ebb event (part) to search for partially inundated cases:
  shift.All = data.NF %>%
    group_by(Event) %>%
    summarise(Flood_start = min(datetime) - ((difftime(max(datetime), min(datetime), units = 'mins')) * (part / 100)),
              Ebb_start   = max(datetime) - ((difftime(max(datetime), min(datetime), units = 'mins')) * (part / 100)),
              Flood_end   = min(datetime) + ((difftime(max(datetime), min(datetime), units = 'mins')) * (part / 100)), 
              Ebb_end     = max(datetime) + ((difftime(max(datetime), min(datetime), units = 'mins')) * (part / 100))) %>%
    gather(Name, datetime, Flood_start:Ebb_end) %>%
    separate(Name, c('Tide', 'Name'), sep = '_') %>%
    spread(Name, datetime) %>% 
    arrange(start) %>%
    relocate(end, .after = last_col()) %>%
    na.omit()
  
  # filter the classified data between these dates:
  shift.All = lapply(1:nrow(shift.All), function(i) {
    
    data.NF %>%
      filter(between(datetime, shift.All$start[i], shift.All$end[i])) %>%
      mutate(Change = i)
    
  } )
  
  shift.All = shift.All %>% 
    bind_rows() %>%
    group_by(Change) %>%
    mutate(
      Tide  = as.character(Tide),
      Tide  = ifelse(is.na(Tide), unique(Tide[!is.na(Tide)]), Tide)) %>%
    ungroup()
  
  # subset the data to reduce CPU time if the search window is large (off for Pendant data):
  shift.All = shift.All %>%
    group_by(Change) %>%
    slice(round(seq(1, n(), length.out = ifelse(design == 'Pendant', 1 * n(), 0.25 * n())), 0))
  
  # ignore short changes (< 5) that cause ASD to fail (especially important for Pendant):
  shift.All = shift.All %>%
    group_by(Change) %>%
    mutate(Count = n()) %>%
    filter(Count >= 5)
  # ##### ADD A WARNING IN THE APP FOR WHICH EVENTS WERE MISSED:
  # # 'Events XXX don't have enough data to check for partial inundation status! Defaulting to full inundation.'
  
  # detect abrupt shifts (normalise to remove direction of shift):
  shift.All = shift.All %>%
    mutate(
      Shift = case_when(Tide == 'Flood' ~ as_detect(Tilt),
                        Tide == 'Ebb'   ~ as_detect(Tilt) * -1),
      Shift  = ifelse(Shift > 0, Shift, 0)) %>%
    ungroup()
  
  # get the start and end dates of each shift: 
  shift.All = shift.All %>%
    group_by(Change, Tide) %>%
    mutate(
      Group = ifelse(Shift > 0, 1, 0), 
      Group = replace(cumsum(!Group), !Group, NA),
      Group = as.integer(factor((Group)))) %>%
    group_by(Change, Tide, Group) %>%
    summarise(start = min(datetime),
              end   = max(datetime)) %>%
    na.omit()
  
  shift.All = lapply(1:nrow(shift.All), function(i) {
    
    data.NF %>%
      select(datetime) %>%
      filter(between(datetime, shift.All$start[i], shift.All$end[i])) %>%
      mutate(Partial = 'P')
    
  } ) %>% 
    bind_rows()
  
  # classify partially inundated cases based on the threshold change detection score (t):
  data.NPF = data.NF %>% left_join(., shift.All, 'datetime') %>%
    mutate(Partial = ifelse(Partial > 0, 'P', Status),
           Status  = ifelse(Status == 'F' & !is.na(Partial), Partial, Status)) %>%
    mutate_if(is.character, as.factor)

  data.NPF = data.NPF %>%
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
    # convert short inundated events to partially inundated:
    group_by(Event) %>%
    mutate(
      fCheck = length(Event) < full,
      Status = ifelse(fCheck == T, 'P', Status)) %>%
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
           if (design == 'B4'  & rate == 1)  { ifelse(Status == 'F', 2.699136272 + (-0.085824310 * Tilt) + ( 9.862232219e-04 * Tilt ^ 2) + (-4.005656168e-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & rate == 2)  { ifelse(Status == 'F', 2.607252330 + (-0.081249794 * Tilt) + ( 9.146377720e-04 * Tilt ^ 2) + (-3.649295895E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & rate == 3)  { ifelse(Status == 'F', 2.358971885 + (-0.069348001 * Tilt) + ( 7.348091059e-04 * Tilt ^ 2) + (-2.778955081E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & rate == 4)  { ifelse(Status == 'F', 1.925638544 + (-0.054340391 * Tilt) + ( 5.610330015e-04 * Tilt ^ 2) + (-2.107620237E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & rate == 5)  { ifelse(Status == 'F', 1.899205083 + (-0.053198643 * Tilt) + ( 5.432655714e-04 * Tilt ^ 2) + (-2.012227394E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & rate == 6)  { ifelse(Status == 'F', 2.228612472 + (-0.061708940 * Tilt) + ( 6.025005906e-04 * Tilt ^ 2) + (-2.074055302E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & rate == 7)  { ifelse(Status == 'F', 0.299041746 + ( 0.016861813 * Tilt) + (-4.489322709e-04 * Tilt ^ 2) + ( 2.557681188E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & rate == 8)  { ifelse(Status == 'F', 2.516344340 + (-0.084257520 * Tilt) + ( 1.037862129e-03 * Tilt ^ 2) + (-4.525852876E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & rate == 9)  { ifelse(Status == 'F', 3.204021725 + (-0.097213992 * Tilt) + ( 1.018015552e-03 * Tilt ^ 2) + (-3.640082156E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & rate == 10) { ifelse(Status == 'F', 2.083570157 + (-0.063605569 * Tilt) + ( 7.136056159e-04 * Tilt ^ 2) + (-2.871382531e-06 * Tilt ^ 3), NA) }
      else if (design == 'B4+' & rate == 1)  { ifelse(Status == 'F', 0.947729965 + (-0.010331140 * Tilt), NA) }
      else if (design == 'B4+' & rate == 2)  { ifelse(Status == 'F', 0.931892290 + (-0.010109484 * Tilt), NA) }
      else if (design == 'B4+' & rate == 3)  { ifelse(Status == 'F', 0.922630764 + (-0.009972599 * Tilt), NA) }
      else if (design == 'B4+' & rate == 4)  { ifelse(Status == 'F', 0.908081623 + (-0.009762219 * Tilt), NA) }
      else if (design == 'B4+' & rate == 5)  { ifelse(Status == 'F', 0.900525865 + (-0.009624605 * Tilt), NA) }
      else if (design == 'B4+' & rate == 6)  { ifelse(Status == 'F', 0.887583355 + (-0.009440689 * Tilt), NA) }
      else if (design == 'B4+' & rate == 7)  { ifelse(Status == 'F', 0.888956883 + (-0.009450901 * Tilt), NA) }
      else if (design == 'B4+' & rate == 8)  { ifelse(Status == 'F', 0.852573695 + (-0.008953314 * Tilt), NA) }
      else if (design == 'B4+' & rate == 9)  { ifelse(Status == 'F', 0.862714427 + (-0.009090706 * Tilt), NA) }
      else if (design == 'B4+' & rate == 10) { ifelse(Status == 'F', 0.833515346 + (-0.008640365 * Tilt), NA) }
      else if (design == 'Pendant')          { ifelse(Status == 'F', 0.957899523 + (-0.034008187 * Tilt) + (0.000473524 * Tilt ^ 2) + (-2.309457425e-06 * Tilt ^ 3), NA) } )
  
  # calculate wave orbital velocity during full inundation for B4+ only:
  data.NPF = if (design == 'B4+') { 
    data.NPF %>% 
      mutate(
        WaveOrbitalVelocity = 
          if    (rate == 1)  { ifelse(Status == 'F', (runSD * 1.801662524) - 0.005038870, NA) }
        else if (rate == 2)  { ifelse(Status == 'F', (runSD * 1.665072518) - 0.003362936, NA) }
        else if (rate == 3)  { ifelse(Status == 'F', (runSD * 1.687692307) - 0.005717337, NA) }
        else if (rate == 4)  { ifelse(Status == 'F', (runSD * 1.597467709) - 0.000999000, NA) }
        else if (rate == 5)  { ifelse(Status == 'F', (runSD * 1.617975298) - 0.002078224, NA) }
        else if (rate == 6)  { ifelse(Status == 'F', (runSD * 1.655866528) - 0.004507210, NA) }
        else if (rate == 7)  { ifelse(Status == 'F', (runSD * 1.707258823) - 0.007984811, NA) }
        else if (rate == 8)  { ifelse(Status == 'F', (runSD * 1.595497726) - 0.001174779, NA) }
        else if (rate == 9)  { ifelse(Status == 'F', (runSD * 1.513542336) + 0.002575534, NA) }
        else if (rate == 10) { ifelse(Status == 'F', (runSD * 1.445359176) + 0.004573273, NA) } ) 
  } else { data.NPF }
  
  # check for full days:
  FullCheck = data.NPF %>%
    group_by(floor_date(datetime, unit = 'days')) %>%
    summarise(Duration = n() * (.$datetime[2] - .$datetime[1])) %>%
    rename(datetime = 1) %>%
    mutate(FullDay = ifelse(Duration == '1440', T, F),
           Day     = as.Date(datetime)) %>%
    select(Day, FullDay)
  
  # add a tag for days that have full data:
  data.NPF = data.NPF %>%
    mutate(Day = as.Date(datetime)) %>%
    left_join(FullCheck, by = c('Day')) %>%
    mutate_if(is.character, as.factor)
  
  data.NPF = if (design == 'B4' | design == 'Pendant') { data.NPF %>% select(datetime, Tilt, Status, Event, Tide, CurrentVelocity, FullDay) } else if (design == 'B4+') { data.NPF %>% select(datetime, Tilt, Status, Event, Tide, CurrentVelocity, WaveOrbitalVelocity, FullDay) }
  
  return(data.NPF)
}

# List of functions for extracting hydrodynamic parameters:
hydro.SurvMins           = function(data) { data %>% summarise(Value = difftime(max(datetime), min(datetime), units = 'mins')[[1]]) }
hydro.SurvDays           = function(data) { data %>% hydro.SurvMins() / 60 / 24 }
hydro.NumEvents          = function(data) { data %>% na.omit() %>% summarise(Value = n_distinct(Event)) }
hydro.DurEvents          = function(data) { data %>% group_by(Event) %>% summarise(Value = difftime(max(datetime), min(datetime), units = 'mins')[[1]]) %>% na.omit() }
hydro.IndDurHrsDay       = function(data) { data %>% hydro.IndDurMins() / 60 / data %>% hydro.SurvDays() }
hydro.NonIndDurHrsDay    = function(data) { data %>% hydro.NonIndDurMins() / 60 / data %>% hydro.SurvDays() }
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

get.summary.statistics = function(data, design) {
  
  hydro.Summary = rbind.data.frame(
    cbind(Parameter = 'Survey days',                   Units = '[day]',     hydro.SurvDays(data)),
    cbind(Parameter = 'Inundation events',             Units = '[n]',       hydro.NumEvents(data)),
    cbind(Parameter = 'Inundation duration',           Units = '[hrs/day]', hydro.IndDurHrsDay(data)),
    cbind(Parameter = 'Emersion duration',             Units = '[hrs/day]', hydro.NonIndDurHrsDay(data)),
    cbind(Parameter = 'Inundation proportion',         Units = '[%]',       hydro.IndDurPerc(data)),
    cbind(Parameter = 'Emersion proportion',           Units = '[%]',       hydro.NonIndDurPerc(data)),
    cbind(Parameter = 'Mean inundation frequency',     Units = '[n/day]',   hydro.IndFreqDayMean(data)),
    cbind(Parameter = 'Median inundation frequency',   Units = '[n/day]',   hydro.IndFreqDayMed(data)),
    cbind(Parameter = 'Maximum Window of Opportunity', Units = '[day]',     hydro.MaxWoO(data)),
    cbind(Parameter = 'Peak ebb-flood ratio',          Units = '[-]',       hydro.PeakAssym(data)),
    cbind(Parameter = 'Peak current velocity',         Units = '[m/s]',     hydro.CurPeak(data)),
    cbind(Parameter = 'Upper current velocity',        Units = '[m/s]',     hydro.CurUpperQ(data)),
    cbind(Parameter = 'Mean current velocity',         Units = '[m/s]',     hydro.CurMean(data)),
    cbind(Parameter = 'Median current velocity',       Units = '[m/s]',     hydro.CurMed(data)))
  
  if (design == 'B4+') {
    hydro.Summary = rbind.data.frame(
      hydro.Summary,
      cbind(Parameter = 'Peak wave orbital velocity',    Units = '[m/s]',     hydro.WavePeak(data)),
      cbind(Parameter = 'Upper wave orbital velocity',   Units = '[m/s]',     hydro.WaveUpperQ(data)),
      cbind(Parameter = 'Mean wave orbital velocity',    Units = '[m/s]',     hydro.WaveMean(data)),
      cbind(Parameter = 'Median wave orbital velocity',  Units = '[m/s]',     hydro.WaveMed(data)))
  } else { hydro.Summary }
  return(hydro.Summary)
  }

get.daily.statistics = function(data, design) {
  
  hydro.Daily = rbind.data.frame(
    cbind(Parameter = 'Inundation duration',          Units = '[min]', hydro.IndDurDay(data)),
    cbind(Parameter = 'Emersion duration',            Units = '[min]', hydro.NonIndDurDay(data)),
    cbind(Parameter = 'Inundation proportion',        Units = '[%]',   hydro.IndDurPercDay(data)),
    cbind(Parameter = 'Emersion proportion',          Units = '[%]',   hydro.NonIndDurPercDay(data)),
    cbind(Parameter = 'Inundation frequency',         Units = '[n]',   hydro.IndFreqDay(data)),
    cbind(Parameter = 'Peak current velocity',        Units = '[m/s]', hydro.PeakCurDay(data)),
    cbind(Parameter = 'Upper current velocity',       Units = '[m/s]', hydro.UpperQCurDay(data)),
    cbind(Parameter = 'Mean current velocity',        Units = '[m/s]', hydro.MeanCurDay(data)),
    cbind(Parameter = 'Median current velocity',      Units = '[m/s]', hydro.MedCurDay(data)))
  
  if (design == 'B4+') {
    hydro.Daily = rbind.data.frame(
      hydro.Daily,
      cbind(Parameter = 'Peak wave orbital velocity',   Units = '[m/s]', hydro.PeakWaveDay(data)),
      cbind(Parameter = 'Upper wave orbital velocity',  Units = '[m/s]', hydro.UpperQWaveDay(data)),
      cbind(Parameter = 'Mean wave orbital velocity',   Units = '[m/s]', hydro.MeanWaveDay(data)),
      cbind(Parameter = 'Median wave orbital velocity', Units = '[m/s]', hydro.MedWaveDay(data)))
  } else { hydro.Daily }
  return(hydro.Daily)
}

get.event.statistics = function(data, design) {

  hydro.Event = rbind.data.frame(
    cbind(Parameter = 'Inundation duration',          Units = '[min]', hydro.DurEvents(data)),
    cbind(Parameter = 'Peak current velocity',        Units = '[m/s]', hydro.PeakCurEvent(data)),
    cbind(Parameter = 'Upper current velocity',       Units = '[m/s]', hydro.UpperQCurEvent(data)),
    cbind(Parameter = 'Mean current velocity',        Units = '[m/s]', hydro.MeanCurEvent(data)),
    cbind(Parameter = 'Median current velocity',      Units = '[m/s]', hydro.MedCurEvent(data)),
    cbind(Parameter = 'Peak ebb-flood ratio',         Units = '[-]',   hydro.PeakAssymEvent(data)),
    cbind(Parameter = 'Upper ebb-flood ratio',        Units = '[-]',   hydro.UpperQAssymEvent(data)))
  
  if (design == 'B4+') {
    hydro.Event = rbind.data.frame(
      hydro.Event,
      cbind(Parameter = 'Peak wave orbital velocity',   Units = '[m/s]', hydro.PeakWaveEvent(data)),
      cbind(Parameter = 'Upper wave orbital velocity',  Units = '[m/s]', hydro.UpperQWaveEvent(data)),
      cbind(Parameter = 'Mean wave orbital velocity',   Units = '[m/s]', hydro.MeanWaveEvent(data)),
      cbind(Parameter = 'Median wave orbital velocity', Units = '[m/s]', hydro.MedWaveEvent(data)))
  } else { hydro.Event }
  return(hydro.Event)
}

get.tidal.statistics = function(data) {
  
  rbind.data.frame(
    cbind(Parameter = 'Peak current velocity',   Units = '[m/s]', hydro.PeakCurEventTide(data)),
    cbind(Parameter = 'Upper current velocity',  Units = '[m/s]', hydro.UpperQCurEventTide(data)),
    cbind(Parameter = 'Mean current velocity',   Units = '[m/s]', hydro.MeanCurEventTide(data)),
    cbind(Parameter = 'Median current velocity', Units = '[m/s]', hydro.MedCurEventTide(data)))
  
}


#' Function to generate results text
get.stats.text = function(data, design){
  
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
    if (design == 'B4+')
    {
      paste(
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
        } )
    } else { '' } )
  return(m)
}


#' Function for statistical comparison of each parameter in site comparison
#' hydro.t, hydro.r: TargetHydro, ReferenceHydro
get.comparison = function(hydro.t, hydro.r, stats.t, stats.r, design.t, design.r) { 
  
  # Get summary statistics:
  summary.r = stats.r %>% rename(Reference = Value)
  summary.t = stats.t %>% rename(Target = Value)
  summary.c = left_join(summary.r, summary.t, by = c('Parameter', 'Units')) %>%
    mutate(Target               = round(Target, 2),
           Reference            = round(Reference, 2),
           DifferencePercentage = (Target - Reference) / Target * 100,
           TargetIs             = paste0(round(abs(DifferencePercentage), 1),
                                         ifelse(DifferencePercentage > 0, '% higher', '% lower')))
  
  # Get event statistics:
  event.t = get.event.statistics(hydro.t, design.t) %>% mutate(Site = 'Target')
  event.r = get.event.statistics(hydro.r, design.r) %>% mutate(Site = 'Reference')
  event.c = bind_rows(event.t, event.r) %>%
    spread(Site, Value) %>%
    group_by(Parameter, Units) %>%
    summarise(p.value   = wilcox.test(Target, Reference, exact = F)$p.value) %>%
    mutate(SignificantlyDifferent = ifelse(p.value < 0.05, 'Yes', 'No'))
  
  comparison = left_join(summary.c, event.c, by = c('Parameter', 'Units')) %>%
    filter(Parameter %in% c('Survey days',
                            'Inundation events',
                            'Mean inundation frequency',
                            'Maximum Window of Opportunity',
                            'Inundation proportion',
                            'Mean current velocity', 
                            'Peak current velocity',
                            'Mean wave orbital velocity',
                            'Peak wave orbital velocity',
                            'Peak ebb-flood ratio')) %>%
    select(Parameter, Units, Reference, Target, SignificantlyDifferent, TargetIs)
  
  return(comparison)
  
}

#' Function to generate results text for comparison
get.comparison.text = function(data){
  Statistics = data %>%
    filter(
      Parameter %in% c(
        'Inundation proportion',
        'Mean inundation frequency',
        'Maximum Window of Opportunity',
        'Mean current velocity',
        'Peak current velocity',
        'Mean wave orbital velocity',
        'Peak wave orbital velocity'))
  
  shortnames = data.frame(
    ParameterShort = c('Duration', 'Frequency', 'MaxWoO', 'MeanCurrent', 'PeakCurrent', 'MeanWave', 'PeakWave'),
    Parameter = c(
      'Inundation proportion',
      'Mean inundation frequency',
      'Maximum Window of Opportunity',
      'Mean current velocity',
      'Peak current velocity',
      'Mean wave orbital velocity',
      'Peak wave orbital velocity'))
  Statistics = Statistics %>% left_join(., shortnames, by = 'Parameter') 
  
  Statistics$Positive  = c('lower', 'lower', 'higher', 'lower', 'lower', 'lower', 'lower')
  Statistics$Outcome = ifelse(gsub('[^a-zA-Z]', '', Statistics$TargetIs) == Statistics$Positive, 'good' , 'bad')
  
  good = paste(if(Statistics$Outcome[1] == 'good') { '<li>Shorter inundation duration</li>' },
               if(Statistics$Outcome[2] == 'good') { '<li>Less frequent inundation</li>' },
               if(Statistics$Outcome[3] == 'good') { '<li>Longer Windows of Opportunity (inundation-free days)</li>' },
               if(Statistics$Outcome[4] == 'good' & Statistics$Outcome[5] == 'good' & Statistics$SignificantlyDifferent[4] == 'Yes' & Statistics$SignificantlyDifferent[5] == 'Yes') { '<li>Meaningfully lower average and peak current velocities</li>' 
               } else if(Statistics$Outcome[5] == 'good' & Statistics$SignificantlyDifferent[5] == 'Yes') { '<li>Meaningfully lower average current velocities inundation</li>' 
               } else if(Statistics$Outcome[4] == 'good' & Statistics$SignificantlyDifferent[4] == 'Yes') { '<li>Meaningfully lower peak current velocities</li>' },
               if(Statistics$Outcome[6] == 'good' & Statistics$Outcome[7] == 'good' & Statistics$SignificantlyDifferent[6] == 'Yes' & Statistics$SignificantlyDifferent[7] == 'Yes') { '<li>Meaningfully lower average and peak wave orbital velocities</li>' 
               } else if(Statistics$Outcome[7] == 'good' & Statistics$SignificantlyDifferent[7] == 'Yes') { '<li>Meaningfully lower average wave orbital velocities</li>' 
               } else if(Statistics$Outcome[6] == 'good' & Statistics$SignificantlyDifferent[6] == 'Yes') { '<li>Meaningfully lower peak current velocities</li>' }
  )
  bad  = paste(if(Statistics$Outcome[1] == 'bad') { '<li>Longer inundation duration</li>' },
               if(Statistics$Outcome[2] == 'bad') { '<li>More frequent inundation</li>' },
               if(Statistics$Outcome[3] == 'bad') { '<li>Shorter Windows of Opportunity (inundation-free days)</li>' },
               if(Statistics$Outcome[4] == 'bad' & Statistics$Outcome[5] == 'bad' & Statistics$SignificantlyDifferent[4] == 'Yes' & Statistics$SignificantlyDifferent[5] == 'Yes') { '<li>Meaningfully higher average and peak current velocities</li>' 
               } else if(Statistics$Outcome[5] == 'bad' & Statistics$SignificantlyDifferent[5] == 'Yes') { '<li>Meaningfully higher average current velocities inundation</li>' 
               } else if(Statistics$Outcome[4] == 'bad' & Statistics$SignificantlyDifferent[4] == 'Yes') { '<li>Meaningfully higher peak current velocities</li>' },
               if(Statistics$Outcome[6] == 'bad' & Statistics$Outcome[7] == 'bad' & Statistics$SignificantlyDifferent[6] == 'Yes' & Statistics$SignificantlyDifferent[7] == 'Yes') { '*Meaningfully higher average and peak wave orbital velocities</li>' 
               } else if(Statistics$Outcome[7] == 'bad' & Statistics$SignificantlyDifferent[7] == 'Yes') { '<li>Meaningfully higher average wave orbital velocities</li>' 
               } else if(Statistics$Outcome[6] == 'bad' & Statistics$SignificantlyDifferent[6] == 'Yes') { '<li>Meaningfully higher peak current velocities</li>' }
  )
  conc = paste(
    '<b>Conclusion:</b><br/>',
    if (!'bad'  %in% Statistics$Outcome) { 'Using the reference site as a baseline, natural establishment or managed restoration is <b>likely</b> to succeed at the target site.<br/><br/>' },
    if (!'good' %in% Statistics$Outcome) { 'Using the reference site as a baseline, natural establishment or managed restoration is <b>unlikely</b> to succeed at the target site without intervention aimed at raising tidal flat elevations and attenuating wave and current velocities.<br/><br/>' },
    if ('good'   %in% Statistics$Outcome & 'bad' %in% Statistics$Outcome) { paste(
      'Using the reference site as a baseline, natural establishment or managed restoration will <b>likely be hampered</b>. ', 
      if(Statistics$Outcome[1] == 'bad' & Statistics$Outcome[2] == 'bad') { 'Prolonged duration and excessive frequency of inundation that exceeds the physiolgical tolerance of the local halophytes. Steps could be taken to raise tidal flat elevations and improve the survival rates of natural or managed plant colonisation. ' 
      } else if (Statistics$Outcome[1] == 'bad') { 'Prolonged inundation duration that exceeds the physiolgical tolerance of the local halophytes. Steps could be taken to raise tidal flat elevations and improve the survival rates of natural or managed plant colonisation. '
      } else if (Statistics$Outcome[2] == 'bad') { 'Excessive inundation frequency that exceeds the physiolgical tolerance of the local halophytes. Steps could be taken to raise tidal flat elevations and improve the survival rates of natural or managed plant colonisation. ' },
      if(!'good' %in% Statistics$Outcome[4:7] & !'No' %in% Statistics$SignificantlyDifferent[4:7]) { 'Current and wave orbital velocities are higher, indicating that scour and dislodgement of plants is more likely. ' 
        
      } else if(!'good' %in% Statistics$Outcome[c(4, 6)] & !'bad' %in% Statistics$Outcome[c(5, 7)] & !'No' %in% Statistics$SignificantlyDifferent[c(4, 6)] & !'Yes' %in% Statistics$SignificantlyDifferent[c(5, 7)]) { 'Peak current and wave orbital velocities are higher, indicating that scour and dislodgement of plants from large events, like storms, is more likely. ' 
      } else if(!'good' %in% Statistics$Outcome[c(5, 7)] & !'bad' %in% Statistics$Outcome[c(4, 6)] & !'No' %in% Statistics$SignificantlyDifferent[c(5, 7)] & !'Yes' %in% Statistics$SignificantlyDifferent[c(4, 6)]) { 'Average current and wave orbital velocities are higher, indicating that scour and dislodgement of plants over successive inundation events is more likely. '
      } else if(Statistics$Outcome[4] == 'bad' & Statistics$SignificantlyDifferent[4] == 'Yes') { 'Peak current velocities are higher, indicating that scour and dislodgement of plants from large events, like storms, is more likely. ' 
      } else if(Statistics$Outcome[6] == 'bad' & Statistics$SignificantlyDifferent[6] == 'Yes') { 'Peak wave orbital velocities are higher, indicating that scour and dislodgement of plants from large events, like storms, is more likely. '
      } else if(Statistics$Outcome[5] == 'bad' & Statistics$SignificantlyDifferent[5] == 'Yes') { 'Average current velocities are higher, indicating that scour and dislodgement of plants over successive inundation events is more likely. '
      } else if(Statistics$Outcome[7] == 'bad' & Statistics$SignificantlyDifferent[7] == 'Yes') { 'Average wave orbital velocities are higher, indicating that scour and dislodgement of plants over successive inundation events is more likely. '},
      if(!'good' %in% Statistics$Outcome[4:7] & !'No' %in% Statistics$SignificantlyDifferent[4:7]) {'Steps could be taken to attenuate velocities and improve the survival rates of natural or managed plant colonisation.'}
    ) } 
  )
  note = '<br/><br/><i>We caution against taking management decisions solely on the basis of this interpretation. Factors such as the timing of the survey, inadequate use of a reference site, or fault in the Mini Buoy assembly would invalidate this interpretation.<i>'
  
  m = 
    paste(
      if('good' %in% Statistics$Outcome & 'bad' %in% Statistics$Outcome)
      { paste('<b>The target site benefits from:</b> <br/>', good, '<br/><b>The target site is disadvantaged by:</b> <br/>', bad, '<br/>',conc) 
      } else 
        if (!'bad' %in% Statistics$Outcome)
        { paste('<b>The target site benefits from:</b> <br/>', good, '<br/>', conc) 
        } else 
          if (!'good' %in% Statistics$Outcome)
          { paste('<b>The target site is disadvantaged by:</b> <br/>', bad, '<br/>', conc)
          }, 
      note
    )
  return(m)
}
