#' Load Rda file containing constant values to estimate current velocoty
#' for the different Buoy model types

load("./models/Current_Vel_Constant_Values.rda") # Current_Vel_Constant_Values
CVC <- Current_Vel_Constant_Values


#### Functions to calculate Hydrodynamic indicators ####

# Function to predict inundation status (N,F,P) and calculate velocities:
hydrodynamics = function(data, design) {
  
  # Load the linear SVM for classification of F and N cases:
  SVML.NF = 
    if (design == 'B4')   { 
      readRDS('./models/SVML_NF_B4.rds')
    } else if (design == 'B4+')   { 
      readRDS('./models/SVML_NF_B4+.rds') 
    } else if (design == 'Pendant') { 
      readRDS('./models/SVML_NF_Pendant.rds') 
    } else { message('Error: Did you enter the wrong Mini Buoy type?') #@Ale: delete errors? as no other designs can be selected
    }
  
  # New model built on N, P, and F:
  SVML.NPF = 
    if (design == 'B4')   { 
      readRDS('./models/SVML_NPF_B4.rds')
    } else if (design == 'B4+')   { 
      readRDS('./models/SVML_NPF_B4+.rds') 
    } else if (design == 'Pendant') { 
      readRDS('./models/SVML_NPF_Pendant.rds') 
    } else { message('Error: Did you enter the wrong Mini Buoy type?') 
    }
  
  
  data.classified = data %>%
    # Aggregate the data by minutes:
    group_by(datetime =
               if (design == 'Pendant') {ceiling_date(datetime, unit = "10 minutes")}
             else if (design == 'B4' | design == 'B4+') {ceiling_date(datetime, unit = "minute")}
             else message('Error: No applicable method, did you select the right mini buoy type?')) %>%
    summarise(Median = median(Acceleration),
              Quant  = quantile(Acceleration, 0.75, names = F) - quantile(Acceleration, 0.25, names = F))%>%
    mutate(
      # Calculate N and F cases:
      Status = predict(SVML.NF, newdata = tibble(Median, Quant)), 
      # make status numerical
      Event  = recode(Status, 'N' = 0, 'F' = 1),
      # count events consecutively
      Event  = replace(cumsum(!Event), !Event, NA),
      # Change event to factor level (i.e. 1, 2, 3...):
      Event = as.integer(factor((Event)))) %>%
    
    group_by(Event) %>%
    # filter false events (less than 10 min):
    filter(length(Event) > 10) %>%
    mutate(
      # To classify P cases, create a 'Prox2N' parameter:
      Prox2N = ifelse(is.na(Event), 0, n() - abs(1:n() - n():1))) %>%
    
    ungroup() %>%
    mutate(
      # Calculate N, P and F cases:
      Status = predict(SVML.NPF, newdata = tibble(Median, Quant, Prox2N)),
      # Recode events in case some mismatch with the new classification:
      Event  = recode(Status, 'N' = 0, 'P' = 1, 'F' = 1),
      Event  = replace(cumsum(!Event), !Event, NA),
      Event = as.integer(factor((Event))),
      # Separate into flood and ebb tides:
      Tide = as.factor(ifelse(Status == 'N', NA, c(rep('Flood', round((n() / 2), 0)), rep('Ebb', n() - round((n() / 2), 0))))),
      # Calculate current velocity:
      CurrentVelocity = 
        if (design == 'B4') { 
          ifelse(Status == 'F', CVC['B4','a']+ (CVC["B4", "b"] * Median) + (CVC["B4", "c"] * Median ^ 2) + (CVC["B4", "d"] * Median ^ 3), NA)
        } else if (design == 'B4+') {
          ifelse(Status == 'F', CVC['B4+','a']+ (CVC['B4+','b'] * Median) + (CVC['B4+','c'] * Median ^ 2) + (CVC['B4+','d'] * Median ^ 3), NA) 
        } else if (design == 'Pendant') {
          ifelse(Status == 'F', CVC['Pendant','a']+ (CVC['Pendant','b']  * Median) + (CVC['Pendant','c'] * Median ^ 2) + (CVC['Pendant','d']* Median ^ 3), NA) 
        } else { NA },
      # Calculate wave orbital velocity (based on rolling SD values):
      WaveOrbitalVelocity =
        if (design == 'B4+') {
          ifelse(Status == 'F', runsd(Median, 60 * 5) * 1.7058 - 0.0103, NA)
        } else { NA }) %>% 
    dplyr::select(-Prox2N)
  
  return(data.classified)
}




# Function to generate  hydrodynamics data summary statistics:
statistics = function(data) {
  
  # total and mean inundation (min):
  s.events = data %>%
    group_by(Event) %>%
    summarise(MinInundated = difftime(last(datetime), 
                                      first(datetime),
                                      units = 'mins')[[1]]) %>%
    na.omit() %>%
    ungroup() %>%
    summarise(SumMinInundated  = sum(MinInundated),
              `Average flooding duration (min/d)` = mean(MinInundated))
  
  
  # # daily flood frequency:
  s.days = data %>%
    dplyr::select(datetime, Event) %>%
    na.omit() %>%
    group_by(datetime = as_date(datetime))%>%
    summarise(Frequency = mean(length(unique(Event))))%>%
    summarise(`Flooding frequency (f/d)` = mean(Frequency))
  
  # survey days, total length of survey (min), current and wave orbital velocities (median and upper quantile values):
  s.all = data %>%
    summarise(
      `Monitoring period (d)` = difftime(last(datetime), first(datetime), units = 'days')[[1]],
      SurveyMins = difftime(last(datetime), first(datetime), units = 'mins')[[1]],
      `Median Current Vel. (m/s)` = median(CurrentVelocity, na.rm = T),
      `75 percentile Vel (m/s)` = quantile(CurrentVelocity, 0.75, names = F, na.rm = T),
      `Median wave orbital vel. (m/s)` = median(WaveOrbitalVelocity, na.rm = T),
      `75 percentile wave orbital vel. (m/s)` = quantile(WaveOrbitalVelocity, 0.75, names = F, na.rm = T))
  
  # maximum WoO length (days):
  max.WoO = data %>%
    mutate(WoO = recode(Status, 'N' = 1, 'P' = 0 , 'F' = 0),
           WoO = replace(cumsum(!WoO), !WoO, NA),
           WoO = as.integer(factor((WoO)))) %>%
    group_by(WoO) %>%
    summarise(start = first(datetime),
              end   = last(datetime), 
              length = n()) %>%
    na.omit() %>%
    filter(length == max(length)) %>%
    mutate(`Max. WoO duration (d)` = difftime(end, start, units = 'days')[[1]]) %>%
    dplyr::select(`Max. WoO duration (d)`)
  
  # flood-ebb velocity:
  flood.ebb = data %>%
    group_by(Tide) %>%
    summarise(`Flood Ebb Median velocity (m/s)` = median(CurrentVelocity, na.rm = T)) %>%
    na.omit() %>%
    spread(Tide, `Flood Ebb Median velocity (m/s)`) %>%
    summarise(`Flood Ebb Median velocity (m/s)` = Flood - Ebb)
  
  # Merge:
  hydro.tab = bind_cols(s.events, s.days, s.all, max.WoO, flood.ebb) %>%
    mutate(`Time flooded during survey (%)` = SumMinInundated / SurveyMins * 100,
           `Flooding frequency (f/d)`) %>%
    dplyr::select(
      `Monitoring period (d)`,
      `Average flooding duration (min/d)`,
      `Time flooded during survey (%)`,
      `Flooding frequency (f/d)`,
      `Max. WoO duration (d)`,
      `Median Current Vel. (m/s)`,
      `75 percentile Vel (m/s)`,
      `Flood Ebb Median velocity (m/s)`,
      `Median wave orbital vel. (m/s)`,
      `75 percentile wave orbital vel. (m/s)`
    ) %>%
    gather(Parameter,
           Value,
           `Monitoring period (d)`:`75 percentile wave orbital vel. (m/s)`) %>%
    na.omit()
  
  return(hydro.tab)
  
}
