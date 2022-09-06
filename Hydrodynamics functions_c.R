source("Hydrodynamics set-up.R")


#### Load Data ####

# User input 
#@Ale A functioning variation of this is already Already implemented on shinyR - still need warning message "No Reference data has been uploaded"

DESIGN = 'B4+' # Choose 'B4', 'B4+', or 'Pendant'

TARGET    = './TestSet/B4+/Target.csv'
REFERENCE = './TestSet/B4+/Reference.csv'

# Read the data: @already ready to analyze data-set (this is after filtering start and end dates)
Target = read_csv(TARGET, col_types = cols()) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(Date, Acceleration)

Reference = if (exists('REFERENCE')) { 
  read_csv(REFERENCE, col_types = cols()) %>%
    mutate_if(is.character, as.factor) %>%
    dplyr::select(Date, Acceleration) } else { 
      message('No reference site data supplied: only analysing the target site') }


#### Functions to calculate Hydrodynamic indicators:####

# Function to predict inundation status (N,F,P) and calculate velocities:
hydrodynamics = function(DATA, DESIGN) {
  
  # Load the linear SVM for classification of F and N cases:
  SVML.NF = 
    if (DESIGN == 'B4')   { 
      readRDS('./models/SVML_NF_B4.rds')
    } else if (DESIGN == 'B4+')   { 
      readRDS('./models/SVML_NF_B4+.rds') 
    } else if (DESIGN == 'Pendant') { 
      readRDS('./models/SVML_NF_Pendant.rds') 
    } else { message('Error: Did you enter the wrong Mini Buoy type?') 
    }
  
  # New model built on N, P, and F:
  SVML.NPF = 
    if (DESIGN == 'B4')   { 
      readRDS('./models/SVML_NPF_B4.rds')
    } else if (DESIGN == 'B4+')   { 
      readRDS('./models/SVML_NPF_B4+.rds') 
    } else if (DESIGN == 'Pendant') { 
      readRDS('./models/SVML_NPF_Pendant.rds') 
    } else { message('Error: Did you enter the wrong Mini Buoy type?') 
    }
  
  CLASS1 = #DATA %>%
    # Aggregate the data by minutes:
    # summarise_by_time(
    #   Date,
    if (DESIGN == 'B4'| DESIGN == 'B4+') {
      setNames(do.call(data.frame, aggregate(Acceleration ~ as.POSIXct(format(as.POSIXct(Date), "%Y-%m-%d %H:%M"), format = "%Y-%m-%d %H:%M"), 
                                             data=DATA,FUN= function(x) c(Median = median(x), Quant= quantile(x, probs = 0.75) - quantile(x, probs =  0.25)))),
               c("Date","Median","Quant"))
      
    }
  else if  (DESIGN == 'Pendant') {
    
    MinTime<-min(DATA$Date)
    MaxTime<-max(DATA$Date)
    
    minute(MinTime)<-floor(minute(MinTime)/10)*10
    minute(MaxTime)<-ceiling(minute(MaxTime)/10)*10
    second(MinTime)<-0
    second(MaxTime)<-0
    breakpoints <- seq.POSIXt(MinTime, MaxTime, by = 600)
    
    setNames(do.call(data.frame, aggregate(Acceleration ~ cut(Date, breaks = breakpoints), 
                                           data=DATA,FUN= function(x) c(Median = median(x), Quant= quantile(x, probs = 0.75) - quantile(x, probs =  0.25)))),
             c("Date","Median","Quant"))
    
  } else message("Error: No applicable method, did you selected the corret Mini Buoy type?")
  
  CLASS=
    CLASS1 %>%
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
        if (DESIGN == 'B4') { 
          ifelse(Status == 'F', CVC['B4','a']+ (CVC["B4", "b"] * Median) + (CVC["B4", "c"] * Median ^ 2) + (CVC["B4", "d"] * Median ^ 3), NA)
        } else if (DESIGN == 'B4+') {
          ifelse(Status == 'F', CVC['B4+','a']+ (CVC['B4+','b'] * Median) + (CVC['B4+','c'] * Median ^ 2) + (CVC['B4+','d'] * Median ^ 3), NA) 
        } else if (DESIGN == 'Pendant') {
          ifelse(Status == 'F', CVC['Pendant','a']+ (CVC['Pendant','b']  * Median) + (CVC['Pendant','c'] * Median ^ 2) + (CVC['Pendant','d']* Median ^ 3), NA) 
        } else { NA },
      # Calculate wave orbital velocity (based on rolling SD values):
      WaveOrbitalVelocity =
        if (DESIGN == 'B4+') {
          ifelse(Status == 'F', runsd(Median, 60 * 5) * 1.7058 - 0.0103, NA)
        } else { NA }) %>% 
    
    dplyr::select(-Prox2N)
  
  return(CLASS)
}




# Function to generate  hydrodynamics data summary statistics:
statistics = function(DATA) {
  
  # total and mean inundation (min):
  s.events = DATA %>%
    group_by(Event) %>%
    summarise(MinInundated = difftime(last(Date), first(Date), units = 'mins')[[1]]) %>%
    na.omit() %>%
    ungroup() %>%
    summarise(SumMinInundated  = sum(MinInundated),
              `Average flooding duration (min/d)` = mean(MinInundated))
  
  # daily flood frequency:
  s.days = setNames(do.call(data.frame, aggregate(Event ~ as.POSIXct(format(as.POSIXct(Date), "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), 
                                                 data=DATA, FUN= function(x)  length(unique(x)))),
                   c("Date","dailyEvents"))%>%
    summarise(`Flooding frequency (f/d)` = round(mean(dailyEvents), 2))
  #colnames(s.day)<-"`Flooding frequency (f/d)`"

  # survey days, total length of survey (min), current and wave orbital velocities (median and upper quantile values):
  s.all = DATA %>%
    summarise(`Monitoring period (d)`   = difftime(last(Date), first(Date), units = 'days')[[1]],
              SurveyMins   = difftime(last(Date), first(Date), units = 'mins')[[1]],
              `Median Current Vel. (m/s)`= median(CurrentVelocity, na.rm = T),
              `75 percentile Vel (m/s)` = quantile(CurrentVelocity, 0.75, names = F, na.rm = T),
              `Median wave orbital vel. (m/s)`    = median(WaveOrbitalVelocity, na.rm = T),
              `75 percentile wave orbital vel. (m/s)`     = quantile(WaveOrbitalVelocity, 0.75, names = F, na.rm = T))
  
  # maximum WoO length (days):
  max.WoO = DATA %>%
    mutate(WoO = recode(Status, 'N' = 1, 'P' = 0 , 'F' = 0),
           WoO = replace(cumsum(!WoO), !WoO, NA),
           WoO = as.integer(factor((WoO)))) %>%
    group_by(WoO) %>%
    summarise(start = first(Date),
              end   = last(Date), 
              length = n()) %>%
    na.omit() %>%
    filter(length == max(length)) %>%
    mutate(`Max. WoO duration (d)` = difftime(end, start, units = 'days')[[1]]) %>%
    dplyr::select(`Max. WoO duration (d)`)
  
  # flood-ebb velocity:
  flood.ebb = DATA %>%
    group_by(Tide) %>%
    summarise(`Flood Ebb Median velocity (m/s)` = median(CurrentVelocity, na.rm = T)) %>%
    na.omit() %>%
    spread(Tide, `Flood Ebb Median velocity (m/s)`) %>%
    summarise(`Flood Ebb Median velocity (m/s)` = Flood - Ebb)
  
  # Merge:
  HYDRO = bind_cols(s.events, s.days, s.all, max.WoO, flood.ebb) %>%
    mutate(`Time flooded during survey (%)` = SumMinInundated / SurveyMins * 100, `Flooding frequency (f/d)`) %>%
    dplyr::select(`Monitoring period (d)`, `Average flooding duration (min/d)`, `Time flooded during survey (%)`, `Flooding frequency (f/d)`, `Max. WoO duration (d)`, `Median Current Vel. (m/s)`, `75 percentile Vel (m/s)`, `Flood Ebb Median velocity (m/s)`, `Median wave orbital vel. (m/s)`, `75 percentile wave orbital vel. (m/s)`) %>%
    gather(Parameter, Value, `Monitoring period (d)`:`75 percentile wave orbital vel. (m/s)`) %>%
    na.omit()
  
  return(HYDRO)
  
}

#### Get hydrodynamics indicators ####


# Calculate hydrodynamics and store in data frame @Ale: This does not need user inputs
Target.h    = hydrodynamics(Target, DESIGN) %>%
  mutate(Type = 'Target')
Reference.h = if (exists('REFERENCE')) { hydrodynamics(Reference, DESIGN) } %>%
  mutate(Type = 'Reference')

Hydrodynamics_df = if (exists('REFERENCE')) { bind_rows(Target.h, Reference.h) 
} else { Target.h }



# Get summary statistics: @Ale: Create tables that need render to display in the hydrodynamics heading of the Mini Buoy App menu

#### DATA FRAMES TO RENDER TABLES ####
Target.stats    = statistics(Target.h)
Reference.stats = if (exists('REFERENCE')) { statistics(Reference.h) }

# @% time flooded (min flooded/ minutes surveyed) - more informative mean % time flooded per day?





# Statistics table:@ Comparison table, only produced when Reference site is uploaded, and located in the cmparisson section of App's menu
Site_Comparison = if (exists('REFERENCE')) { Target.stats %>% 
    left_join(., Reference.stats, 'Parameter') %>%
    rename(Target = Value.x, Reference = Value.y) %>%
    mutate(Difference = Target - Reference) 
} else { Target.stats }

Site_Comparison #using anyFunction for hydrodynammics-> a bit slow




