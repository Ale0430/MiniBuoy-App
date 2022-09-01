


#### to set-up.R ####
     # The needed libraries (caTools, zoo are already included)- 
     # we might need to remove "kernlab" form the set-up file
# Load libraries:
invisible(
  suppressPackageStartupMessages(
    lapply(c('tidyverse',
             'timetk',
             'lubridate',
             'caTools',
             'scales',
             'zoo'),
           library, character.only = T, quiet = T)))

rm(list = ls(all = T))

### theme_set() also not needed in the Mini Buoy app - already implemented
# Set theme for plots:
theme_set(theme(panel.background  = element_blank(),
                panel.grid        = element_blank(),
                panel.border      = element_rect(fill = NA, colour = 'black', size = 0.5),
                strip.background  = element_blank(),
                strip.text        = element_text(colour = 'black', size = 9),
                axis.title        = element_text(colour = 'black', size = 11),
                axis.text         = element_text(colour = 'black', size = 9),
                axis.ticks.length = unit(-0.1, 'cm'),
                plot.title        = element_text(colour = 'black', size = 11),
                legend.text       = element_text(colour = 'black', size = 9),
                legend.title      = element_text(colour = 'black', size = 9),
                legend.background = element_blank()))

# User input --------------------------------------------------------------
#Loading Target and Reference data files already iplemented in app. 
DESIGN = 'MB2' # Choose 'MB1', 'MB2', or 'MB3' 

TARGET    = './data/TestSet/MB2/Target.csv'
REFERENCE = './data/TestSet/MB2/Reference.csv'


#### Read the data: ####
Target = read_csv(TARGET, col_types = cols()) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(Date, Acceleration)

Reference = if (exists('REFERENCE')) { 
  read_csv(REFERENCE, col_types = cols()) %>%
    mutate_if(is.character, as.factor) %>%
    dplyr::select(Date, Acceleration) } else { 
      message('No reference site data supplied: only analysing the target site') }

# Functions ---------------------------------------------------------------

# Function to predict inundation status and calculate velocities:
hydrodynamics = function(DATA, DESIGN) {
  
  # Load the linear SVM for classification of F and N cases:
  SVML.NF = 
         if (DESIGN == 'MB1')   { 
    readRDS('./models/SVML_NF_MB1.rds')
  } else if (DESIGN == 'MB2')   { 
    readRDS('./models/SVML_NF_MB2.rds') 
  } else if (DESIGN == 'MB3') { 
    readRDS('./models/SVML_NF_MB3.rds') 
  } else { message('Error: Did you enter the wrong Mini Buoy type?') 
  }
  
  # New model built on N, P, and F:
  SVML.NPF = 
         if (DESIGN == 'MB1')   { 
    readRDS('./models/SVML_NPF_MB1.rds')
  } else if (DESIGN == 'MB2')   { 
    readRDS('./models/SVML_NPF_MB2.rds') 
  } else if (DESIGN == 'MB3') { 
    readRDS('./models/SVML_NPF_MB3.rds') 
  } else { message('Error: Did you enter the wrong Mini Buoy type?') 
  }
  
  CLASS = DATA %>%
    # Aggregate the data by minutes:
    summarise_by_time(
      Date,
           if (DESIGN == 'MB3' ) { '10 minutes' }
      else if (DESIGN == 'MB1' | DESIGN == 'MB2') { 'minute' }
      else message('Error: Did you enter the wrong Minio Buoy type?'),
      Median = median(Acceleration),
      Quant  = quantile(Acceleration, 0.75, names = F) - quantile(Acceleration, 0.25, names = F)) %>%
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
               if (DESIGN == 'MB1') { 
          ifelse(Status == 'F', -2.501 + (-10.477 * Median) + (-11.878 * Median ^ 2) + (-3.976 * Median ^ 3), NA)
        } else if (DESIGN == 'MB2') {
          ifelse(Status == 'F', 4.5347 + (14.4199 * Median) + (18.1660 * Median ^ 2) + (8.1783 * Median ^ 3), NA) 
        } else if (DESIGN == 'MB3') {
          ifelse(Status == 'F', 1.0226 + (2.3939  * Median) + (2.3713  * Median ^ 2) + (0.9340 * Median ^ 3), NA) 
        } else { NA },
      # Calculate wave orbital velocity (based on rolling SD values):
      WaveOrbitalVelocity =
               if (DESIGN == 'MB2') {
          ifelse(Status == 'F', runsd(Median, 60 * 5) * 1.7058 - 0.0103, NA)
        } else { NA }) %>% 
      dplyr::select(-Prox2N)
    
  return(CLASS)
  
}

# Function to generate hydrodynamics statistics:
statistics = function(DATA) {
  
  # total and mean inundation (min):
  s.events = DATA %>%
    group_by(Event) %>%
    summarise(MinInundated = difftime(last(Date), first(Date), units = 'mins')[[1]]) %>%
    na.omit() %>%
    ungroup() %>%
    summarise(SumMinInundated  = sum(MinInundated),
              MeanMinInundated = mean(MinInundated))
  
  # daily flood frequency:
  s.days = DATA %>%
    dplyr::select(Date, Event) %>%
    na.omit() %>%
    summarise_by_time(
      Date,
      'days',
      Frequency = length(unique(Event, na.rm = T))) %>%
    summarise(Frequency = mean(Frequency))
  
  # survey days, total length of survey (min), current and wave orbital velocities (median and upper quantile values):
  s.all = DATA %>%
    summarise(SurveyDays   = difftime(last(Date), first(Date), units = 'days')[[1]],
              SurveyMins   = difftime(last(Date), first(Date), units = 'mins')[[1]],
              MedianCurent = median(CurrentVelocity, na.rm = T),
              UpperCurrent = quantile(CurrentVelocity, 0.75, names = F, na.rm = T),
              MedianWave    = median(WaveOrbitalVelocity, na.rm = T),
              UpperWave     = quantile(WaveOrbitalVelocity, 0.75, names = F, na.rm = T))
  
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
    mutate(MaxWoO = difftime(end, start, units = 'days')[[1]]) %>%
    dplyr::select(MaxWoO)
  
  # flood-ebb velocity:
  flood.ebb = DATA %>%
    group_by(Tide) %>%
    summarise(FloodEbb = median(CurrentVelocity, na.rm = T)) %>%
    na.omit() %>%
    spread(Tide, FloodEbb) %>%
    summarise(FloodEbb = Flood - Ebb)
  
  # Merge:
  HYDRO = bind_cols(s.events, s.days, s.all, max.WoO, flood.ebb) %>%
    mutate(PercInundated = SumMinInundated / SurveyMins * 100) %>%
    dplyr::select(SurveyDays, MeanMinInundated, PercInundated, Frequency, MaxWoO, MedianCurent, UpperCurrent, FloodEbb, MedianWave, UpperWave) %>%
    gather(Parameter, Value, SurveyDays:UpperWave) %>%
    na.omit()
  
  return(HYDRO)
  
}

# Analysis ----------------------------------------------------------------

# Calculate hydrodynamics:
Target    = hydrodynamics(Target, DESIGN)
Reference = if (exists('REFERENCE')) { hydrodynamics(Reference, DESIGN) }

# Calculate summary statistics:
Target.s    = statistics(Target)
Reference.s = if (exists('REFERENCE')) { statistics(Reference) }

# Results -----------------------------------------------------------------

# Statistics table:
Target.s
Reference.s

Statistics = if (exists('REFERENCE')) { Target.s %>% 
    left_join(., Reference.s, 'Parameter') %>%
    rename(Target = Value.x, Reference = Value.y) %>%
    mutate(Difference = Target - Reference)} else { 
      Target.s }

# Daily inundation duration:
Target %>%
  summarise_by_time(
    Date,
    'day',
    InundationMin  = sum(!is.na(Event)) * (.$Date[2] - .$Date[1])) %>%
  ggplot(aes(Date, InundationMin)) +
  geom_bar(stat = 'identity', fill = 'lightblue', colour = 'lightblue') +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
  labs(y = 'Daily inundation (min/day)') + 
  theme(axis.title.x = element_blank())

Reference %>%
  summarise_by_time(
    Date,
    'day',
    InundationMin  = sum(!is.na(Event)) * (.$Date[2] - .$Date[1])) %>%
  ggplot(aes(Date, InundationMin)) +
  geom_bar(stat = 'identity', fill = 'lightblue', colour = 'lightblue') +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
  labs(y = 'Daily inundation (min/day)') + 
  theme(axis.title.x = element_blank())

# Current velocity:
Target %>%
  ggplot(aes(Date, CurrentVelocity)) +
  geom_point(size = 0.2) +
  geom_line(aes(y = rollmean(CurrentVelocity, 20, na.pad = T)), colour = 'blue') +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
  labs(y = 'Median current velocity (m/s)') + 
  theme(axis.title.x = element_blank())

Reference %>%
  ggplot(aes(Date, CurrentVelocity)) +
  geom_point(size = 0.2) +
  geom_line(aes(y = rollmean(CurrentVelocity, 20, na.pad = T)), colour = 'blue') +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
  labs(y = 'Median current velocity (m/s)') + 
  theme(axis.title.x = element_blank())

# Wave orbital velocity:
if (DESIGN == 'MB2') { 
  Target %>%
    ggplot(aes(Date, WaveOrbitalVelocity)) +
    geom_point(size = 0.2) +
    geom_line(aes(y = rollmean(WaveOrbitalVelocity, 20, na.pad = T)), colour = 'blue') +
    scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
    labs(y = 'Median wave orbtial velocity (m/s)') + 
    theme(axis.title.x = element_blank())
} else { message('This Mini Buoy design does not measure wave orbital velocity') }

if (DESIGN == 'MB2') { 
  Reference %>%
    ggplot(aes(Date, WaveOrbitalVelocity)) +
    geom_point(size = 0.2) +
    geom_line(aes(y = rollmean(WaveOrbitalVelocity, 20, na.pad = T)), colour = 'blue') +
    scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
    labs(y = 'Median wave orbtial velocity (m/s)') + 
    theme(axis.title.x = element_blank())
} else { message('This Mini Buoy design does not measure wave orbital velocity') }

# export all results:
write.csv(Target, '~/Downloads/MiniBuoyResults.csv', row.names = F)
