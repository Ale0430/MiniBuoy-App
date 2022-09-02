# Load libraries: @already implemented on ShinyR
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

### LOAD Rda file containing constant values to estimate current velocoty for the different Buoy model types

load("Current_Vel_Constant_Values.rda")
CVC<-Current_Vel_Constant_Values


