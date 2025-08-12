#' Load Rda file containing constant values to estimate current velocity
#' for the different Buoy model types

#### Functions to calculate Hydrodynamic indicators ####

#### Functions to calculate Hydrodynamic indicators ####
#####  Function to reclassify short events (generic, reusable) #####
# event_col: ether Event (col. with inundation events) or N.Event (col. with non-flood events)
# status_value: loop through "F" and convert to "N" if the consecutive values are lower than the defined threshold
reclassify_short_events <- function(data, event_col, status_value, new_status, threshold) {
  data %>%
    group_by(.data[[event_col]]) %>%
    mutate(event_length = if (is.na(first(.data[[event_col]]))) NA_integer_ else n()) %>%
    ungroup() %>%
    mutate(
      Status = case_when(
        !is.na(.data[[event_col]]) & event_length <= threshold ~ new_status,
        TRUE ~ Status)
    )}

##### Count inundation events consecutively: a unique id from 1 to n will be generated for each flood event (Event) and for each non-flood event (N.Event) #####
assign_seq_id <- function(vec, type) {
  is_event <- vec == type
  rle_ids <- rleid(is_event)
  event_runs <- unique(rle_ids[is_event])
  seq_ids <- setNames(seq_along(event_runs), event_runs)
  ifelse(is_event, seq_ids[as.character(rle_ids)], NA_integer_)
}

##### first clean function #####
# Clean, average to 1-min windows, and calculate tilt:
clean.1 = function(data, design) {
  
  # calculate sampling rate (measurements per minute) to be used in rolling sd and median  ,
  # and freq (time between sampling points in seconds), for selecting the correct current and wave orbital velocity calibration later on)
  
  freq = as.numeric(difftime(data$datetime[2], data$datetime[1], units = 'secs'))
  rate = 60 /freq
  
  data %>%
    mutate(
      Acceleration = ifelse(Acceleration > 0, 0, Acceleration),
      # truncate acceleration values (beyond detection limit caused by shock):
      Acceleration = if      (design == 'B4' | design == 'B4+') { ifelse(Acceleration < -1.15, NA, Acceleration) } 
      else if (design == 'Pendant') { ifelse(Acceleration < -1.075, NA, Acceleration) },
      mediany = runmed(Acceleration, rate, endrule = "median"),,
      runSD = runsd(Acceleration, rate),  #align = "center" is default
      medTilt =  ((-180*(asin(ifelse(mediany < -1, -1, mediany))))/pi)
    )%>%
    # remove NAs:
    filter(complete.cases(.))%>%
    group_by(datetime =  ceiling_date(datetime, unit = 'minute'))%>%
    mutate(IQR = quantile(Acceleration, 0.75, names = F, na.rm=T) - quantile(Acceleration, 0.27, names= F, na.rm=T),
           Mean_Acc = mean(Acceleration, na.rm = T),
           Tilt = ((-180*(asin(ifelse(Mean_Acc < -1, -1, Mean_Acc))))/pi)
    )%>%
    ungroup()
}

##### second clean function #####
### Create conditions to define inundated and non-inundated cases using adjusted tilt, standard deviation, and slope

# remove resting tilt angle over entire survey to detect deviations around zero, with NA as an option:
clean.2 = function(data, design, baseline.window = 3*60, variance.window = 3, slope.window = 1*60) { # in minutes
  
  Baseline = baseline.window / (as.numeric(difftime(data$datetime[2], data$datetime[1], units = 'mins'))) 
  baseline <- ifelse(Baseline %% 2 == 0, Baseline + 1, Baseline)
  variance = variance.window / (as.numeric(difftime(data$datetime[2], data$datetime[1], units = 'mins'))) 
  slope    = slope.window    / (as.numeric(difftime(data$datetime[2], data$datetime[1], units = 'mins'))) 
  
  ### where basline is an even number, add 1 else leave baseline as odd  
  data %>%
    mutate(
      Tilt_adj = medTilt - runmed(medTilt, k = baseline),
      SD_adj   = runsd(Tilt_adj, variance), 
      Slope    = abs(stats::filter(medTilt, c(-1, rep(0, slope - 2), 1) / (slope - 1), sides = 2)), # modify sides?
      # Fill NA at edges by carrying nearest non-NA forward and backward:
      Slope    = na.locf(Slope, na.rm = F),
      Slope    = as.numeric(na.locf(Slope, fromLast = T, na.rm = F)))
}
  #' Function to predict inundation status, current and wave orbital velocities:
  # gaps: minimum gap in an inundation event to be closed (where points were missclassified as non inundated)
  # tilt:tilt degree above which all data is 99% likley to be fully flooded
  # limit: Upper tilt threshold to search for non-flooded events. non-flood cases will be then searched bellow the tilt limit. Users can adjust by looking at the raw data window and identify "Partially flooded" cases that show a flat line or ver shalow slope
  # slope: slope value used to identify non-flood events. Typically lower than 0.01, but it can happen that water ebbs very slow and the slope can increase a  bit (no more than 0.02)
  # adj_tilt: use a proportion of the data for abrupt shift detection (1 = all, 0 = none)
  
get.hydrodynamics = function(data, design, ui.input_settings = NULL) {
  if (is.data.frame(ui.input_settings)){
    tilt = ui.input_settings$tilt
    limit = ui.input_settings$limit
    slope = ui.input_settings$slope
    adj_tilt = ui.input_settings$adj_tilt
  } else {
    tilt = 75
    limit = 9
    slope = 0.01
    adj_tilt = -5
  }
  
  # calculate sampling rate (for selecting the correct current and wave orbital velocity calibration later on):
  rate = 60 / as.numeric(data$datetime[2] - data$datetime[1]) # measurments per minute
  freq = as.numeric(data$datetime[2] - data$datetime[1])      # data recording rate (every n seconds)
  
  data.clean1 = clean.1(data, design)
  data.clean2 = clean.2(data.clean1, design)  
  
  data.NF = data.clean2 %>%
    mutate(
      
      # tilt valuable for variance capturing variance around moving window, excluding threshold tilts (i.e. non-inundated and fully inundated look the same)
      # slope valuable for capturing tilt gradients 
      # sd valuable for capturing low inundation cases (edge cases)
      
      Status = 'N',
      Status = ifelse(Status == 'N' & medTilt > limit,                               'F', Status), # "N" class for points bellow the limit value
      Status = ifelse(Status == 'N' & medTilt > tilt | Tilt_adj < adj_tilt | Tilt_adj > 5, 'F', Status), # -1 to 1 typical values TILT_ADJ CAN BE STRICTER IF VARIANCE AROUND NON-INUNDATION IS LOWER
      Status = ifelse(Status == 'N' & SD_adj > 1,                               'F', Status),
      Status = ifelse(Status == 'N' & Slope > slope,                             'F', Status)) # typical slope used 0.01
  
  # classify flood and non-flood events and adjsut classification of short events
  data.NF = data.NF%>%
    mutate(
      Event   = assign_seq_id(Status, "F"),
      N.Event = assign_seq_id(Status, "N")
    )%>%
    reclassify_short_events("Event", "F", "N", 600)%>%
    mutate(
      Event   = assign_seq_id(Status, "F"),
      N.Event = assign_seq_id(Status, "N")
    )%>%
    reclassify_short_events("N.Event", "N", "F", 20)%>%
    mutate(
      Event   = assign_seq_id(Status, "F"),
      N.Event = assign_seq_id(Status, "N")
    )%>%
    group_by(datetime =  ceiling_date(datetime, unit = 'minute'))%>%
    slice(1)%>%
    ungroup()

  #### use time windows on start and en of each Event to classify partial inundations  ####
  
  data.NPF <- data.NF %>%
    #select(datetime, Acceleration, runSD, mediany, Tilt, Event, N.Event, Status)%>%
    group_by(Event) %>%
    mutate(
      # classify each inundation event as flood and ebb tide: 
      Tide = as.factor(ifelse(Status == 'N', NA, c(rep('Flood', round((n() / 2), 0)), rep('Ebb', n() - round((n() / 2), 0)))))) %>%
    ungroup()%>%
    # remove spike data points t
    group_by(Event) %>%
    mutate(
      tilt_before = lag(medTilt),
      tilt_after = lead(medTilt),
      is_spike = Tilt >= 50 & tilt_before < 5 & tilt_after < 5
    ) %>%
    ungroup() %>%
    filter(!is_spike) %>%
    select(-tilt_before, -tilt_after, -is_spike)%>%
    group_by(Event) %>%
    # define time frame for flood and ebb (start and end of the events that will define partial floods)
    mutate(
      start_time = min(datetime, na.rm = TRUE),
      end_time = max(datetime, na.rm = TRUE),
      from_start = as.numeric(difftime(datetime, start_time, units = "mins")),
      from_end = as.numeric(difftime(end_time, datetime, units = "mins")),
      is_start_20 = from_start <= 350, #350
      is_end_20 = from_end <= 350,          #350
      
      # Identify the index of first 90 Tilt in start window
      first_90_idx = if (any(is_start_20 & medTilt >= 72)) which(is_start_20 & medTilt >= 72)[1] else NA_integer_,
      
      # Identify the index of last 90 Tilt that marks the end of the fully flooded period, in end window
      last_90_idx = if (any(is_end_20 & medTilt >= 72)) rev(which(is_end_20 & medTilt >= 72))[1] else NA_integer_,
      
      row_in_group = row_number(),
      
      is_partial_start = is_start_20 & row_in_group < first_90_idx,
      is_partial_end = is_end_20 & row_in_group > last_90_idx
    ) %>%
    ungroup() %>%
    mutate(
      Status = case_when(
        Status == "F" & (is_partial_start | is_partial_end) ~ "P",
        TRUE ~ Status
      )
    ) %>%
    ungroup()

  #### classify as P all events that fail to reach 90 degrees or where total cases reaching 90 degree tilt are fewer that 10
  
  data.NPF = data.NPF %>%
    group_by(Event)%>%
    mutate(
      Status = case_when(!is.na(Event) & all(medTilt < tilt, na.rm=T) ~ "P", # tipically 85
                         # !is.na(Event) & sum(medTilt == 90, na.rm = TRUE) <= 1 ~ "P", # 30 minutes of total F events not enough to rely on velocity estimates
                         
                         TRUE~ Status)
    )%>%
    ungroup()
  ### add buffer "P" points to ensure edges are removed from potential miss interpretation of high current velocities
  ### the intention here is to convert from F to "P" the first and last 5% data points of each "F" data group
  
  data.NPF = data.NPF %>%
    group_by(Event) %>%
    mutate(
      Status = if(!all(is.na(Event))) {
        # For flood events only
        
        # Get indices of "F" within this group
        f_indices <- which(Status == "F")
        n_f <- length(f_indices)
        
        if(n_f > 0) {
          n_p <- ceiling(n_f * 0.1)  # 5% rounded up
          
          new_status <- Status
          
          if(n_p > 0) {
            new_status[f_indices[1:n_p]] <- "P"
            new_status[f_indices[(n_f - n_p + 1):n_f]] <- "P"
          }
          
          new_status
        } else {
          Status  # no "F" in this event, no change
        }
        
      } else {
        Status  # For NA Events (non-flood), keep original status
      }
    ) %>%
    ungroup()%>%
    #mutate(Tilt = medTilt )%>%
    select( datetime, Acceleration, mediany, runSD, IQR, medTilt, Tilt, Status, Event, N.Event, Tide)%>%
    mutate(#CurrentVelocity = ifelse(Status == "F", curvel15(mediany), NA ))
      CurrentVelocity = 
        if (design == 'B4'  & freq <= 1)  { ifelse(Status == 'F', 2.699136272 + (-0.085824310 * Tilt) + ( 9.862232219e-04 * Tilt ^ 2) + (-4.005656168e-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & freq == 2)  { ifelse(Status == 'F', 2.607252330 + (-0.081249794 * Tilt) + ( 9.146377720e-04 * Tilt ^ 2) + (-3.649295895E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & freq == 3)  { ifelse(Status == 'F', 2.358971885 + (-0.069348001 * Tilt) + ( 7.348091059e-04 * Tilt ^ 2) + (-2.778955081E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & freq == 4)  { ifelse(Status == 'F', 1.925638544 + (-0.054340391 * Tilt) + ( 5.610330015e-04 * Tilt ^ 2) + (-2.107620237E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & freq == 5)  { ifelse(Status == 'F', 1.899205083 + (-0.053198643 * Tilt) + ( 5.432655714e-04 * Tilt ^ 2) + (-2.012227394E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & freq == 6)  { ifelse(Status == 'F', 2.228612472 + (-0.061708940 * Tilt) + ( 6.025005906e-04 * Tilt ^ 2) + (-2.074055302E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & freq == 7)  { ifelse(Status == 'F', 0.299041746 + ( 0.016861813 * Tilt) + (-4.489322709e-04 * Tilt ^ 2) + ( 2.557681188E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & freq == 8)  { ifelse(Status == 'F', 2.516344340 + (-0.084257520 * Tilt) + ( 1.037862129e-03 * Tilt ^ 2) + (-4.525852876E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & freq == 9)  { ifelse(Status == 'F', 3.204021725 + (-0.097213992 * Tilt) + ( 1.018015552e-03 * Tilt ^ 2) + (-3.640082156E-06 * Tilt ^ 3), NA) }
      else if (design == 'B4'  & freq == 10) { ifelse(Status == 'F', 2.083570157 + (-0.063605569 * Tilt) + ( 7.136056159e-04 * Tilt ^ 2) + (-2.871382531e-06 * Tilt ^ 3), NA) }
      else if (design == 'B4+' & freq <= 1)  { ifelse(Status == 'F', 0.947729965 + (-0.010331140 * Tilt), NA) }
      else if (design == 'B4+' & freq == 2)  { ifelse(Status == 'F', 0.931892290 + (-0.010109484 * Tilt), NA) }
      else if (design == 'B4+' & freq == 3)  { ifelse(Status == 'F', 0.922630764 + (-0.009972599 * Tilt), NA) }
      else if (design == 'B4+' & freq == 4)  { ifelse(Status == 'F', 0.908081623 + (-0.009762219 * Tilt), NA) }
      else if (design == 'B4+' & freq == 5)  { ifelse(Status == 'F', 0.900525865 + (-0.009624605 * Tilt), NA) }
      else if (design == 'B4+' & freq == 6)  { ifelse(Status == 'F', 0.887583355 + (-0.009440689 * Tilt), NA) }
      else if (design == 'B4+' & freq == 7)  { ifelse(Status == 'F', 0.888956883 + (-0.009450901 * Tilt), NA) }
      else if (design == 'B4+' & freq == 8)  { ifelse(Status == 'F', 0.852573695 + (-0.008953314 * Tilt), NA) }
      else if (design == 'B4+' & freq == 9)  { ifelse(Status == 'F', 0.862714427 + (-0.009090706 * Tilt), NA) }
      else if (design == 'B4+' & freq == 10) { ifelse(Status == 'F', 0.833515346 + (-0.008640365 * Tilt), NA) }
      else if (design == 'Pendant')          { ifelse(Status == 'F', 0.957899523 + (-0.034008187 * Tilt) + (0.000473524 * Tilt ^ 2) + (-2.309457425e-06 * Tilt ^ 3), NA) } )
  
  # calculate wave orbital velocity during full inundation for B4+ only:
  data.NPF = if (design == 'B4+') {
    data.NPF %>%
      # find mean of 1-min moving SD every 10 minutes:
      mutate(
        time_floor = floor_date(datetime, unit = '10 minutes')) %>%
      group_by(time_floor) %>%
      mutate(runSD = ifelse(datetime == min(datetime), mean(runSD, na.rm = T), NA)) %>%
      ungroup() %>%
      select(-time_floor) %>%
      # convert moving SD to wave orbital velocity:
      mutate(
        WaveOrbitalVelocity = 
          if    (freq <= 1)  { ifelse(Status == 'F', (runSD * 1.801662524) - 0.005038870, NA) }
        else if (freq == 2)  { ifelse(Status == 'F', (runSD * 1.665072518) - 0.003362936, NA) }
        else if (freq == 3)  { ifelse(Status == 'F', (runSD * 1.687692307) - 0.005717337, NA) }
        else if (freq == 4)  { ifelse(Status == 'F', (runSD * 1.597467709) - 0.000999000, NA) }
        else if (freq == 5)  { ifelse(Status == 'F', (runSD * 1.617975298) - 0.002078224, NA) }
        else if (freq == 6)  { ifelse(Status == 'F', (runSD * 1.655866528) - 0.004507210, NA) }
        else if (freq == 7)  { ifelse(Status == 'F', (runSD * 1.707258823) - 0.007984811, NA) }
        else if (freq == 8)  { ifelse(Status == 'F', (runSD * 1.595497726) - 0.001174779, NA) }
        else if (freq == 9)  { ifelse(Status == 'F', (runSD * 1.513542336) + 0.002575534, NA) }
        else if (freq == 10) { ifelse(Status == 'F', (runSD * 1.445359176) + 0.004573273, NA) } ) 
  } else { data.NPF }
  
  # check for full days:
  FullCheck = data.NPF %>%
    group_by(floor_date(datetime, unit = 'days')) %>%
    summarise(Duration = n() * (as.numeric(difftime(.$datetime[2], .$datetime[1], units = 'mins')))) %>%
    rename(datetime = 1) %>%
    mutate(FullDay = ifelse(Duration == '1440', T, F),
           Day     = as.Date(datetime)) %>%
    select(Day, FullDay)
  
  # add a tag for days that have full data:
  data.NPF = data.NPF %>%
    mutate(Day = as.Date(datetime)) %>%
    left_join(FullCheck, by = c('Day')) %>%
    mutate_if(is.character, as.factor)
  
  data.NPF = if (design == 'B4' | design == 'Pendant') { data.NPF %>% select(datetime, Tilt, Status, Event, N.Event, Tide, CurrentVelocity, FullDay) } else if (design == 'B4+') { data.NPF %>% select(datetime, Tilt, Status, Event, N.Event, Tide, CurrentVelocity, WaveOrbitalVelocity, FullDay) }
  
  return(data.NPF)
}


#### List of functions for extracting hydrodynamic parameters:####
hydro.SurvMins           = function(data) { data %>% summarise(Value = difftime(max(datetime), min(datetime), units = 'mins')[[1]]) }
hydro.SurvDays           = function(data) { data %>% summarise(Value = difftime(max(datetime), min(datetime), units = 'days')[[1]]) }

hydro.NumEvents          = function(data) { data %>% summarise(Value = n_distinct(Event, na.rm = T)) }
hydro.DurEvents          = function(data) { data %>% group_by(Event) %>% summarise(Value = as.numeric(sum(!is.na(Event)) / (as.numeric(difftime(.$datetime[2], .$datetime[1], units = 'mins'))))) %>% na.omit() }
hydro.DurNEvents          = function(data) { data %>% group_by(N.Event) %>% summarise(Value = as.numeric(sum(!is.na(N.Event)) / (as.numeric(difftime(.$datetime[2], .$datetime[1], units = 'mins'))))) %>% na.omit() }
hydro.DurEventsHrs       = function(data) { data %>% hydro.DurEvents() %>% group_by(Event) %>% summarise(Value = Value / 60) }
hydro.DurNEventsHrs      = function(data) { data %>% hydro.DurNEvents() %>% group_by(N.Event) %>% summarise(Value = Value / 60) }
hydro.IndDurMins         = function(data) { data %>% hydro.DurEvents() %>% summarise(Value = sum(Value)) }
hydro.NonIndDurMins      = function(data) { data %>% hydro.DurNEvents() %>% summarise(Value = sum(Value)) }
hydro.IndDurHrsDay       = function(data) { data %>% hydro.IndDurMins()    / 60 / data %>% hydro.SurvDays() }
hydro.NonIndDurHrsDay    = function(data) { data %>% hydro.NonIndDurMins() / 60 / data %>% hydro.SurvDays() }
hydro.NonIndDurDays      = function(data) { data %>% hydro.DurNEvents() %>% group_by(N.Event) %>% summarise(Value = Value / 60/24) }
hydro.IndDurPerc         = function(data) { data %>% summarise(hydro.IndDurMins(.) / hydro.SurvMins(.) * 100) }
hydro.NonIndDurPerc      = function(data) { data %>% summarise(hydro.NonIndDurMins(.) / hydro.SurvMins(.) * 100) }
hydro.IndDurDay          = function(data) { data %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = as.numeric(sum(!is.na(Event)) / (as.numeric(difftime(.$datetime[2], .$datetime[1], units = 'mins'))))) %>% rename(datetime = 1) %>% na.omit() }
hydro.IndDurDayHrs       = function(data) { data %>% hydro.IndDurDay() %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = Value / 60) %>% rename(datetime = 1) }
hydro.NonIndDurDay       = function(data) { data %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = as.numeric(sum(is.na(Event))  / (as.numeric(difftime(.$datetime[2], .$datetime[1], units = 'mins'))))) %>% rename(datetime = 1) %>% na.omit() }
hydro.NonIndDurDays      = function(data) { data %>% hydro.DurNEvents() %>% group_by(N.Event) %>% summarise(Value = Value / 60/24) }
hydro.NonIndDurDayHrs    = function(data) { data %>% hydro.NonIndDurDay() %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = Value / 60) %>% rename(datetime = 1) }
hydro.IndDurPercDay      = function(data) { data %>% hydro.IndDurDay()                      %>% mutate(Value = (Value / 1440) * 100) }
hydro.NonIndDurPercDay   = function(data) { data %>% hydro.NonIndDurDay()                   %>% mutate(Value = (Value / 1440) * 100) }
hydro.IndFreqDay         = function(data) { data %>% summarise(n_distinct(Event, na.rm = T) / hydro.SurvDays(.)) }
#hydro.IndFreqDay         = function(data) { data %>% group_by(floor_date(datetime, "day"))  %>%  select(-N.Event)  %>%  summarise(Value = n_distinct(Event, na.rm=T)) %>% filter(Value >=1) %>% summarise(Value = median(Value)) }   

##### assess WoO duration and frequency #####
hydro.WoO3 <- function(data, length, max) { data %>% hydro.NonIndDurDays() %>% filter(Value >= length & Value <= max) %>% summarise(Value = n()) %>% return(.)}


hydro.MaxWoO = function (data){
  df = hydro.NonIndDurDays(data) %>% summarise(Value=max(Value))
  return(df)}

##### current velocity metrics #####
hydro.UpperCurEventTide = function(data) { data %>% group_by(Event, Tide)                  %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = quantile(CurrentVelocity, 0.95, names = F, na.rm=T)) }
hydro.UpperCurEvent     = function(data) { data %>% group_by(Event)                        %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = quantile(CurrentVelocity, 0.95, names = F, na.rm=T)) }

hydro.UpperCurDay       = function(data) { data %>% group_by(floor_date(datetime, 'days')) %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = quantile(CurrentVelocity, 0.95, names = F, na.rm=T)) %>% rename(datetime = 1) }
hydro.MeanCurEventTide  = function(data) { data %>% group_by(Event, Tide)                  %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = mean(CurrentVelocity, na.rm=T)) }
hydro.MeanCurEvent      = function(data) { data %>% group_by(Event)                        %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = mean(CurrentVelocity, na.rm=T)) }
hydro.MeanCurDay        = function(data) { data %>% group_by(floor_date(datetime, 'days')) %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = mean(CurrentVelocity, na.rm=T)) %>% rename(datetime = 1) }
hydro.MedCurEventTide   = function(data) { data %>% group_by(Event, Tide)                  %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = median(CurrentVelocity, na.rm=T)) }
hydro.MedCurEvent       = function(data) { data %>% group_by(Event)                        %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = median(CurrentVelocity, na.rm=T)) }
hydro.MedCurDay         = function(data) { data %>% group_by(floor_date(datetime, 'days')) %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = median(CurrentVelocity, na.rm=T)) %>% rename(datetime = 1) }
hydro.CurUpper          = function(data) { data                                            %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = quantile(CurrentVelocity, 0.95, names = F, na.rm =T)) }
hydro.CurMean           = function(data) { data                                            %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = mean(CurrentVelocity, na.rm = T)) }
hydro.CurMed            = function(data) { data                                            %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = median(CurrentVelocity, na.rm = T)) }

hydro.CurMedTide        = function(data) { data %>% group_by(Tide)                         %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = median(CurrentVelocity, na.rm=T))}
hydro.CurUpperTide      = function(data) { data %>% group_by(Tide)                         %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = quantile(CurrentVelocity, 0.95, names=F, na.rm = T))}
hydro.CurUpperEventTide = function(data) { data %>% group_by(Event, Tide)                  %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = quantile(CurrentVelocity, 0.95, names=F, na.rm = T))}
hydro.CurMedEventTide   = function(data) { data %>% group_by(Event, Tide)                  %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = median(CurrentVelocity, na.rm=T))}

hydro.UpperWaveEvent    = function(data) { data %>% group_by(Event)                        %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = quantile(WaveOrbitalVelocity, 0.95, names = F, na.rm = T)) }
hydro.UpperWaveDay      = function(data) { data %>% group_by(floor_date(datetime, 'days')) %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = quantile(WaveOrbitalVelocity, 0.95, names = F, na.rm = T)) %>% rename(datetime = 1) }
hydro.MeanWaveEvent     = function(data) { data %>% group_by(Event)                        %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = mean(WaveOrbitalVelocity, na.rm = T)) }
hydro.MeanWaveDay       = function(data) { data %>% group_by(floor_date(datetime, 'days')) %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = mean(WaveOrbitalVelocity, na.rm = T)) %>% rename(datetime = 1) }
hydro.MedWaveEvent      = function(data) { data %>% group_by(Event)                        %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = median(WaveOrbitalVelocity, na.rm = T)) }
hydro.MedWaveDay        = function(data) { data %>% group_by(floor_date(datetime, 'days')) %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = median(WaveOrbitalVelocity, na.rm = T)) %>% rename(datetime = 1) }
hydro.WaveUpper         = function(data) { data                                            %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = quantile(WaveOrbitalVelocity, 0.95, names = F, na.rm = T)) }
hydro.WaveMean          = function(data) { data                                            %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = mean(WaveOrbitalVelocity, na.rm = T)) }
hydro.WaveMed           = function(data) { data                                            %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(Value = median(WaveOrbitalVelocity, na.rm = T)) }

hydro.UpperAssymEvent   = function(data) { data %>% hydro.UpperCurEventTide() %>% group_by(Event) %>% spread(Tide, Value) %>% summarise(Value = Ebb / Flood) }
hydro.UpperAssym        = function(data) { data %>% hydro.UpperAssymEvent()   %>% summarise(Value = median(Value, na.rm = T)) }

#### count Events that reached full flood, non-flood events and inundation frequency ####
# full days monitored.... this function is reducing days with FULL TRUE when no full flood event is observed (double check)
hydro.FullDays          = function(data) { data %>% group_by(floor_date(datetime, 'days')) %>% summarise(Value = unique(FullDay)) %>% rename(datetime = 1) }

hydro.FullFloodEvent = function(data)    { data %>% group_by(Event)                        %>% select(-N.Event) %>% filter(!is.na(Event)) %>% summarise(has_curr = any(!is.na(CurrentVelocity))) %>% summarise(Value  = sum(has_curr)) }
hydro.NonFullFloodEvent = function(data) { data                                            %>% summarise(hydro.NumEvents(.) - hydro.FullFloodEvent(.))  }

hydro.SurvDaysNoInund   = function(data) { data                                            %>% mutate(day = floor_date(datetime, "day")) %>% group_by(day) %>% summarise(no_inundation = all(is.na(Event)))%>% filter(no_inundation =="TRUE")%>% summarise(Value=n()) }  



get.summary.statistics = function(data, design) {
  
  hydro.Summary = rbind.data.frame(
    cbind(Parameter = 'Survey days',                   Units = '[day]',     hydro.SurvDays(data)),
    cbind(Parameter = 'Inundation events',             Units = '[n]',       hydro.NumEvents(data)),
    cbind(Parameter = 'Inundation events with full flood',             Units= '[n]',       hydro.FullFloodEvent(data)),
    cbind(Parameter = 'Inundation duration',           Units = '[hrs/day]', hydro.IndDurHrsDay(data)),
    cbind(Parameter = 'Emersion duration',             Units = '[hrs/day]', hydro.NonIndDurHrsDay(data)),
    cbind(Parameter = 'Inundation proportion',         Units = '[%]',       hydro.IndDurPerc(data)),
    cbind(Parameter = 'Emersion proportion',           Units = '[%]',       hydro.NonIndDurPerc(data)),
    cbind(Parameter = 'Inundation frequency',          Units = '[n/day]',   hydro.NumEvents(data)/( hydro.FullDays(data)%>%filter(Value)%>%nrow()-(hydro.SurvDaysNoInund(data)%>%pull(Value)))),
    cbind(Parameter = 'Windows of Opportunity of at least 3 days', Units = '[n]',     hydro.WoO3(data, 3, hydro.MaxWoO(data)%>%pull(Value))),
    cbind(Parameter = 'Maximum Window of Opportunity', Units = '[day]',     hydro.MaxWoO(data)),
    cbind(Parameter = 'Upper 95th percentile ebb-flood ratio',         Units = '[-]',       hydro.UpperAssym(data)),
    cbind(Parameter = 'Upper 95th percentile current velocity',        Units = '[m/s]',     hydro.CurUpper(data)),
    cbind(Parameter = 'Mean current velocity',         Units = '[m/s]',     hydro.CurMean(data)),
    cbind(Parameter = 'Median current velocity',       Units = '[m/s]',     hydro.CurMed(data)),
    cbind(Parameter = 'Ebb median Curr. Vel.',            Units = '[m/s]',      hydro.CurMedTide(data)%>% filter(Tide =="Ebb") %>% select(Value)),
    cbind(Parameter = 'Flood median Curr. Vel.',          Units = '[m/s]',     hydro.CurMedTide(data)%>% filter(Tide =="Flood") %>% select(Value)),
    cbind(Parameter = 'Ebb Upper 95th percentile Cur. Vel.',           Units = '[m/s]',     hydro.CurUpperTide(data)%>% filter(Tide =="Ebb")%>% select(Value)),
    cbind(Parameter = 'Flood Upper 95th percentile Cur. Vel.',           Units = '[m/s]',     hydro.CurUpperTide(data)%>% filter(Tide =="Flood")%>% select(Value)))
  
  if (design == 'B4+') {
    hydro.Summary = rbind.data.frame(
      hydro.Summary,
      cbind(Parameter = 'Upper 95th percentile wave orbital velocity',   Units = '[m/s]',     hydro.WaveUpper(data)),
      cbind(Parameter = 'Mean wave orbital velocity',    Units = '[m/s]',     hydro.WaveMean(data)),
      cbind(Parameter = 'Median wave orbital velocity',  Units = '[m/s]',     hydro.WaveMed(data)))
  } else { hydro.Summary }
  return(hydro.Summary)
  }

get.daily.statistics = function(data, design) {
  
  hydro.Daily = rbind.data.frame(
    cbind(Parameter = 'Inundation duration',          Units = '[hrs]', hydro.IndDurDayHrs(data)),
    cbind(Parameter = 'Emersion duration',            Units = '[hrs]', hydro.NonIndDurDayHrs(data)),
    cbind(Parameter = 'Inundation proportion',        Units = '[%]',   hydro.IndDurPercDay(data)),
    cbind(Parameter = 'Emersion proportion',          Units = '[%]',   hydro.NonIndDurPercDay(data)),
    cbind(Parameter = 'Upper 95th percentile current velocity',       Units = '[m/s]', hydro.UpperCurDay(data)),
    cbind(Parameter = 'Mean current velocity',        Units = '[m/s]', hydro.MeanCurDay(data)),
    cbind(Parameter = 'Median current velocity',      Units = '[m/s]', hydro.MedCurDay(data)))

  if (design == 'B4+') {
    hydro.Daily = rbind.data.frame(
      hydro.Daily,
      cbind(Parameter = 'Upper 95th percentile wave orbital velocity',  Units = '[m/s]', hydro.UpperWaveDay(data)),
      cbind(Parameter = 'Mean wave orbital velocity',   Units = '[m/s]', hydro.MeanWaveDay(data)),
      cbind(Parameter = 'Median wave orbital velocity', Units = '[m/s]', hydro.MedWaveDay(data)))
  } else { hydro.Daily }
  return(hydro.Daily)
}

get.event.statistics = function(data, design) {

  hydro.Event = rbind.data.frame(
    cbind(Parameter = 'Inundation duration',          Units = '[hrs]', hydro.DurEventsHrs(data)),
    cbind(Parameter = 'Upper 95th percentile current velocity',       Units = '[m/s]', hydro.UpperCurEvent(data)),
    cbind(Parameter = 'Mean current velocity',        Units = '[m/s]', hydro.MeanCurEvent(data)),
    cbind(Parameter = 'Median current velocity',      Units = '[m/s]', hydro.MedCurEvent(data)),
    cbind(Parameter = 'Upper 95th percentile ebb-flood ratio',        Units = '[-]',   hydro.UpperAssymEvent(data)),
    cbind(Parameter = 'Ebb median Curr. Vel ', Units = '[m/s]', hydro.CurMedEventTide(data)%>% filter(Tide =="Ebb")%>% select(Value)),
    cbind(Parameter = 'Flood median Median flood Curr. Vel ', Units = '[m/s]', hydro.CurMedEventTide(data)%>% filter(Tide =="Flood")%>% select(Value)),
    cbind(Parameter = 'Ebb upper 9th percentil Curr. Vel.', Units = '[m/s]', hydro.CurUpperEventTide(data)%>% filter(Tide =="Ebb")%>% select(Value)),
    cbind(Parameter = 'Flood upper 95th percntil Curr. Vel.', Units = '[m/s]', hydro.CurUpperEventTide(data)%>% filter(Tide =="Flood")%>% select(Value)))
  
  if (design == 'B4+') {
    hydro.Event = rbind.data.frame(
      hydro.Event,
      cbind(Parameter = 'Upper 95th percentile wave orbital velocity',  Units = '[m/s]', hydro.UpperWaveEvent(data)),
      cbind(Parameter = 'Mean wave orbital velocity',   Units = '[m/s]', hydro.MeanWaveEvent(data)),
      cbind(Parameter = 'Median wave orbital velocity', Units = '[m/s]', hydro.MedWaveEvent(data)))
  } else { hydro.Event }
  return(hydro.Event)
}

get.tidal.statistics = function(data) {
  
  rbind.data.frame(
    cbind(Parameter = 'Upper 95th percentile current velocity',  Units = '[m/s]', hydro.UpperCurEventTide(data)),
    cbind(Parameter = 'Mean current velocity',   Units = '[m/s]', hydro.MeanCurEventTide(data)),
    cbind(Parameter = 'Median current velocity', Units = '[m/s]', hydro.MedCurEventTide(data)))
  
}

get.woo.statistics = function(data) {
  
  rbind.data.frame(
    cbind(Parameter = 'Window of Opportunity maximum length',  Units = '[days]', hydro.MaxWoO(data)),
   # cbind(Parameter = 'Frequency of WoOs >= 3 days', Units = '[n]', hydro.WoO3(data, 3,  hydro.WoO3(data, 3, hydro.MaxWoO(data)%>%pull(Value)))),
   cbind(Parameter = 'Frequency of WoOs >= 3 days WoO', Units = '[n]', hydro.WoO3(data, 3, hydro.MaxWoO(data)%>%pull(Value))),
    cbind(Parameter = 'Frequency of 1-day WoO', Units = '[n]', hydro.WoO3(data, 1, 1.9)),
    cbind(Parameter = ' Frequency of 2-day WoO', Units = '[n]', hydro.WoO3(data, 2, 2.9)),
    cbind(Parameter = ' Frequency of 3-day WoO', Units = '[n]', hydro.WoO3(data, 3, 3.9)),
    cbind(Parameter = ' Frequency of 5-day WoO', Units = '[n]', hydro.WoO3(data, 5, 5.9)),
    cbind(Parameter = ' Frequency of 6-day WoO', Units = '[n]', hydro.WoO3(data, 6, 6.9)),
    cbind(Parameter = ' Frequency of 7-day WoO', Units = '[n]', hydro.WoO3(data, 7, 7.9)),
    cbind(Parameter = ' Frequency of 8-day WoO', Units = '[n]', hydro.WoO3(data, 8, 8.5)),
    cbind(Parameter = ' Frequency of 10-day WoO', Units = '[n]', hydro.WoO3(data, 10, 10.2)))
}

get.emersion.statistics = function (data){
  rbind.data.frame(
    cbind(Parameter="emersion duration", Units = '[hrs]', hydro.DurNEventsHrs(data))
  )
}
#' Function to generate results text
get.stats.text = function(data, design){
  
  Statistics = data %>%
    filter(
      Parameter %in% c(
        'Survey days',
        'Inundation frequency',
        'Upper 95th percentile ebb-flood ratio',
        'Maximum Window of Opportunity',
        'Upper 95th percentile current velocity',
        'Upper 95th percentile wave orbital velocity'))

  shortnames = data.frame(
    ParameterShort = c('SurveyDays', 'Tide', 'EbbFlood', 'MaxWoO', 'UpperCurrent', 'UpperWave'),
    Parameter = c(
      'Survey days',
      'Inundation frequency',
      'Upper 95th percentile ebb-flood ratio',
      'Maximum Window of Opportunity',
      'Upper 95th percentile current velocity',
      'Upper 95th percentile wave orbital velocity'))
  Statistics = Statistics %>% left_join(., shortnames, by = 'Parameter') 
  
  m = paste(
    'The survey lasted for <b>',
    Statistics %>%
      filter(ParameterShort == 'SurveyDays') %>%
      select(Value) %>% round(., 1),
    ' day(s)</b>, ',
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
        select(Value) == 0) 
    {
      '<b>continuous</b>.<br/><br/>'
    } else
      if (Statistics %>%
          filter(ParameterShort == 'Tide') %>%
          select(Value) > 0 &
          Statistics %>%
          filter(ParameterShort == 'Tide') %>%
          select(Value) <= 1)
      {
        '<b>tidal (diurnal)</b>.<br/><br/>'
      } else
        if (Statistics %>%
            filter(ParameterShort == 'Tide') %>%
            select(Value) > 1 &
            Statistics %>%
            filter(ParameterShort == 'Tide') %>%
            select(Value) <= 2)
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
        '<b>symmetrical</b>, implying no net transport of coarse sediment.<br/><br/>'
      },
    'The longest window of opportunity was <b>',
    Statistics %>%
      filter(ParameterShort == 'MaxWoO') %>%
      select(Value) %>% round(., 1),
    ' day(s)</b>, ',
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
      filter(ParameterShort == 'Tide') %>%
      select(Value) %>% round(., 2),
    ' per day</b>, ',
    if (Statistics %>%
        filter(ParameterShort == 'Tide') %>%
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
                            'Inundation frequency',
                            'Maximum Window of Opportunity',
                            'Inundation duration',
                            'Mean current velocity', 
                            'Upper 95th percentile current velocity',
                            'Mean wave orbital velocity',
                            'Upper 95th percentile wave orbital velocity',
                            'Upper 95th percentile ebb-flood ratio')) %>%
    select(Parameter, Units, Reference, Target, SignificantlyDifferent, TargetIs)
  
  return(comparison)
  
}

#' Function to generate results text for comparison
get.comparison.text = function(data){
  Statistics = data %>%
    filter(
      Parameter %in% c(
        'Inundation duration',
        'Inundation frequency',
        'Maximum Window of Opportunity',
        'Mean current velocity',
        'Upper 95th percentile current velocity',
        'Mean wave orbital velocity',
        'Upper 95th percentile wave orbital velocity'))
  
  shortnames = data.frame(
    ParameterShort = c('Duration', 'Frequency', 'MaxWoO', 'MeanCurrent', 'UpperCurrent', 'MeanWave', 'UpperWave'),
    Parameter = c(
      'Inundation duration',
      'Inundation frequency',
      'Maximum Window of Opportunity',
      'Mean current velocity',
      'Upper 95th percentile current velocity',
      'Mean wave orbital velocity',
      'Upper 95th percentile wave orbital velocity'))
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


