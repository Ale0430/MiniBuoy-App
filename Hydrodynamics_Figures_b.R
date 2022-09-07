

source("Hydrodynamics functions_c.R")

# Daily inundation duration:
#### Figure for Target page"
Bar.Cahrt.Inu.Duration.Tar = Hydrodynamics_df %>%
  filter(Type == 'Target') %>%
#Target %>%
  mutate(date = ceiling_date(datetime, unit = 'days')) %>%
  group_by(date) %>%
  #group_by(date) %>% 
  summarise(InundationMin = sum(!is.na(Event)) * (.$datetime[2] - .$datetime[1]))%>%
    ggplot(aes(date, InundationMin)) +
    geom_bar(stat = 'identity', fill = 'lightblue', colour = 'lightblue') +
    scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
    labs(y = 'Daily inundation (min/day)') + 
    theme(axis.title.x = element_blank())  

#### Figure for Reference page ####
Bar.Cahrt.Inu.Duration.Ref = if (exists('REFERENCE')) { 
  Hydrodynamics_df %>%
  filter(Type == 'Reference') %>%
  #Target %>%
    mutate(date = ceiling_date(datetime, unit = 'days')) %>%
    group_by(date) %>%
    #group_by(date) %>% 
    summarise(InundationMin = sum(!is.na(Event)) * (.$datetime[2] - .$datetime[1]))%>%
    ggplot(aes(date, InundationMin)) +
  geom_bar(stat = 'identity', fill = 'royalblue', colour = 'lightblue') +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
  labs(y = 'Daily inundation (min/day)') + 
  theme(axis.title.x = element_blank())  
  } else { message('No reference site data has been supplied')}

#### Figure for comparison page ####
Bar.Cahrt.Inu.Duration.T_and_R = if (exists('REFERENCE')) { 
  Hydrodynamics_df%>%
    mutate(date = ceiling_date(datetime, unit = 'days')) %>%
    group_by(Type, date) %>%
    #group_by(date) %>% 
    summarise(InundationMin = sum(!is.na(Event)) * (.$datetime[2] - .$datetime[1]))%>%
    ggplot(aes(date, InundationMin, fill = Type)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values = c('lightblue', 'royalblue')) +
    scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
    labs(y = 'Daily inundation (min/day)') + 
    theme(axis.title.x = element_blank(),
          legend.title = element_blank()) 
} else { message('No reference site data supplied: only analysing the target site')}
#This warning message can be yellow- not mistake, just warning








# Current velocity:
Med.Curr.Vel.Tar = Target.h %>%
  ggplot(aes(datetime, CurrentVelocity)) +
  geom_point(size = 0.2) +
  geom_line(aes(y = rollmean(CurrentVelocity, 20, na.pad = T)), colour = 'blue') +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
  labs(y = 'Median current velocity (m/s)') + 
  theme(axis.title.x = element_blank())

Med.Curr.Vel.Ref  = Reference.h %>%
  ggplot(aes(datetime, CurrentVelocity)) +
  geom_point(size = 0.2) +
  geom_line(aes(y = rollmean(CurrentVelocity, 20, na.pad = T)), colour = 'blue') +
  scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
  labs(y = 'Median current velocity (m/s)') + 
  theme(axis.title.x = element_blank())

# Wave orbital velocity:
Wave.Orb.Vel.Tar = if (DESIGN == 'B4+') { 
  Target.h %>%
    ggplot(aes(Date, WaveOrbitalVelocity)) +
    geom_point(size = 0.2) +
    geom_line(aes(y = rollmean(WaveOrbitalVelocity, 20, na.pad = T)), colour = 'blue') +
    scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
    labs(y = 'Median wave orbtial velocity (m/s)') + 
    theme(axis.title.x = element_blank())
} else { message('This Mini Buoy design does not measure wave orbital velocity') }

Wave.Orb.Vel.Ref = if (DESIGN == 'B4+') { 
  Reference.h %>%
    ggplot(aes(Date, WaveOrbitalVelocity)) +
    geom_point(size = 0.2) +
    geom_line(aes(y = rollmean(WaveOrbitalVelocity, 20, na.pad = T)), colour = 'blue') +
    scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
    labs(y = 'Median wave orbtial velocity (m/s)') + 
    theme(axis.title.x = element_blank())
} else { message('This Mini Buoy design does not measure wave orbital velocity') }



###################

