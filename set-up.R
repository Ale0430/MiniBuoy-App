
#### LOAD PACKAGES ####

if(!require("shiny")) install.packages("shiny")  
if(!require("shinydashboard")) install.packages("shinydashboard")  
if(!require("shinycssloaders")) install.packages("shinycssloaders") 
if(!require("shinyFiles")) install.packages("shinyFiles") 
if(!require("markdown")) install.packages("markdown")  
if(!require("DT")) install.packages("DT")  
if(!require("zoo")) install.packages("zoo") # rollapplyfunction  #from the first Mini Buoy app (needed?)
if(!require("tidyverse")) install.packages("tidyverse") 
if(!require("ggpubr")) install.packages("ggpubr") # draw regression line in ggplot
if(!require("scales")) install.packages("scales") # modify datetime format in ggpplot
if(!require("kernlab")) install.packages("kernlab")   #from Cai's code
if(!require("caTools")) install.packages("caTools")  #from Cai's code
#if(!require("fontawesome")) install.packages("fontawesome")  #dashboard menu icons (not really needed#)



#### LOAD APP FUNCTIONS ####
path= "./R_scripts/functions/"
modelFunctions = list.files(path)
lapply(modelFunctions, function(x) source(paste(path, x, sep ="")))

path
list.files(path)
