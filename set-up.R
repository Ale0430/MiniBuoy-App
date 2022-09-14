
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
# Package to deal with dates and times -> as_date(), ceiling_date()
if(!require("lubridate")) install.packages("lubridate")
# Package to deal with time serien -> rollmean()
if(!require("zoo")) install.packages("zoo") 
# Package to speed up date time transformation -> fastPOSIXct()
if(!require("fasttime")) install.packages("fasttime") 
# Package to modify ggplot axis -> args in scale_y_continuous
if(!require("scales")) install.packages("scales")
# ???
if(!require("caTools")) install.packages("caTools")  #from Cai's code
#Packages to run model predictions
if(!require("kernlab")) install.packages("kernlab")
if(!require("caret")) install.packages("caret")

# # Check if the following packages are required
# # Package to speed up processing of date formats -> fastPOSIXct
# if(!require("fasttime")) install.packages("fasttime") 
# if(!require("ggpubr")) install.packages("ggpubr") # draw regression line in ggplot
# if(!require("scales")) install.packages("scales") # modify datetime format in ggpplot
# #if(!require("fontawesome")) install.packages("fontawesome")  #dashboard menu icons (not really needed#)
# ???
# if(!require("kernlab")) install.packages("kernlab")   #from Cai's code


#### LOAD APP FUNCTIONS ####
path= "./R_scripts/functions/"
modelFunctions = list.files(path)
lapply(modelFunctions, function(x) source(paste(path, x, sep ="")))
