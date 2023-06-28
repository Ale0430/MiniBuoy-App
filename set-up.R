
#### LOAD PACKAGES ####

# Basic shiny package
if(!require("shiny")) install.packages("shiny")  
# Package for shiny dashboard layout
if(!require("shinydashboard")) install.packages("shinydashboard")  
# Package to add loading animations when output is recalculating 
if(!require("shinycssloaders")) install.packages("shinycssloaders") 
# Package to manage to browse local folder system
if(!require("shinyFiles")) install.packages("shinyFiles") 
# Package to use markdown
if(!require("markdown")) install.packages("markdown")  
# Package collection to simplify data processing
if(!require("tidyverse")) install.packages("tidyverse") 
# Package to render pretty tables -> renderDataTable()
if(!require("DT")) install.packages("DT")  
# Package to speed up file upload -> fread()
if(!require("data.table")) install.packages("data.table")
# Package to deal with dates and times -> as_date(), ceiling_date(), floor_date()
if(!require("lubridate")) install.packages("lubridate")
# Package to speed up date time transformation -> fastPOSIXct()
if(!require("fasttime")) install.packages("fasttime") 
# Package to calculate running standard deviation -> runsd()
if(!require("caTools")) install.packages("caTools")
# Package to close gaps within inundation events -> na.approx()
if(!require("zoo")) install.packages("zoo")
# Package to read classification algorithms created using caret to classify partially inundated cases in the predict() function
if(!require("caret")) install.packages("caret")
# Package to calculate abrupt shifts in characteristic of partially inundated cases:
if(!require("devtools")) install.packages("devtools")
devtools::install_github("caboulton/asdetect")
if(!require("asdetect")) install.packages("asdetect")
# Package to produce interactive plots
if(!require("plotly")) install.packages("plotly")
# Package to save file in excel format
if(!require("writexl")) install.packages("writexl")


# Suppress warning about group output when using dplyr::summarise
options(dplyr.summarise.inform = FALSE)


#### LOAD APP FUNCTIONS ####
path = "./R_scripts/functions/"
modelFunctions = list.files(path)
lapply(modelFunctions, function(x) source(paste(path, x, sep ="")))
