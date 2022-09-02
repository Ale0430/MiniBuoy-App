#' Facet labels
#' @description Helper function to get facets based on column name.
#' @param data: data.frame, long-format
#' @param facet.col: character, name of facet variable
#' @return factor
get.labelledFacets = function(data, facet.col){
   if (is.Date(data[1, facet.col])){
      facet = as.character(data[, facet.col]) 
      facet.factor <- sort(c(unique(facet)))
      labs = facet.factor
   } else {
      facet = as.integer(data[, facet.col]) 
      facet.factor <- sort(c(unique(facet)))
      labs = unlist(lapply(facet.factor, function(x) paste(facet.col, ": ", x, sep = "")))
   }

   # Exclude = NULL includes NA as factor
   return(factor(facet, labels = labs, exclude = NULL))
}

#' Check if value is date format
is.Date <- function(x) {
   inherits(x, c("Date", "POSIXt"))
}

#' Fill colors
#' @description Helper function to get fill colors for discrete data. Colors are either default colors or manually obtained from UI.
#' @param ui.input: UI-input
#' @return vector
get.fillcolors = function(ui.input){
   # If no colors are defined use default set
   print("Discrete color scheme: default")
   col = c("#d8b365", "#260C7D", "#5ab4ac", "#7D410C", 
           "#007D06", '#999999','#E69F00', '#56B4E9')
   if (ui.input$fillColors != ""){
      print("Discrete color scheme: customized")
      cols = ui.input$fillColors
      cols_split = strsplit(cols, ",")[[1]]
      if (length(cols_split) == 1){
         print(cols_split[1])
         try(col <- RColorBrewer::brewer.pal(n = 100,
                                             name = cols_split[1]),
             silent = F)
         print(col)
      } else {
         for (i in 1:length(cols_split)){
            # remove white spaces
            c = gsub(" ", "", cols_split[i], fixed = TRUE)
            col = append(col, c)
         }
      }
   }
   return(col)
}

#' Gradient colors
#' @description Helper function to get gradient colors for continious data. Colors are either default colors or manually obtained from UI.
#' @param ui.input: UI-input
#' @return vector
get.gradientcolors = function(ui.input){
   # If no colors are defined use default set
   if (ui.input$gradientColors == ""){
      print("Gradient color scheme: default")
      col = c("#d8b365", "#5ab4ac")
      
   } else {
      print("Gradient color scheme: customized")
      cols = ui.input$gradientColors
      cols_split = strsplit(cols, ",")[[1]]
      col = c()
      for (i in 1:length(cols_split)){
         # remove white spaces
         c = gsub(" ", "", cols_split[i], fixed = TRUE)
         col = append(col, c)
      }
   }
   return(col)
}

#' Empty diagram
#' @param message: message to be shown
#' @return ggplot-object
plot.emptyMessage = function(message){
   return(p = ggplot() +
             annotate(geom = "text", x = 0, y = 5, 
                      label = message,
                      color = "red", size = 6,
                      hjust = 0) +
             xlim(c(0, 10)) +
             ylim(c(0, 10)) +
             theme_void())
}

######### labels working in shiny ggplot

#' Labels
#' @description Helper variable to get uniform labels.
labels <- list("CurrentVelocity" = expression(paste("Current Velocity (m ", s^-1, ")")),
               "WaveOrbitalVelocity" = expression(paste("Current Velocity (m ", s^-1, ")")),
               "datetime" = "Date",
               "date"="date",
               "time" = "time of day",
               "InundationPerc" = "Inundation duration (daily %)",
               "InundationMin" = expression(paste("Inundation duration (m ", day^-1, ")")),
               "SurveyMin" = expression(paste("Survied time (m ", day^-1, ")")), #complete dqays will hold 1440 min, less SurveyMin should be removed
               "doy" = "Day of year",
               "dTime" = "Time (h)")




#' Statistic summary
#' @description Helper function to produce summary statistics (mean and +/- sd) 
#' Source: http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
#' @param x: vector
#' @return vector
data_summary <- function(x) {
   m <- mean(x)
   ymin <- m-sd(x)
   ymax <- m+sd(x)
   return(c(y=m,ymin=ymin,ymax=ymax))
}

######## FILTER ########

#' Filter diagram
#' @description Shows filtered data as histogram, boxplot, scattered plot or frequency plot
#' @param data: data.frame, long-format
#' @param ui.input: UI-input
#' @return ggplot-object
plot.filteredRawData <- function(data, ui.input) {
   # binwidth = ui.input$filterPlot_binwidth
   bins = ui.input$filterPlot_bins
   type = ui.input$filterPlot_type
   rollingmean = ui.input$filterPlot_rollmean
   steps = ui.input$filterPlot_rollmean_steps

   if (type == "hist") {
      p = data %>%
         ggplot(aes(x = Acceleration)) +
         # geom_histogram(binwidth = binwidth, col = "white") +
         geom_histogram(bins = bins, col = "black") +
         
         labs(x = "Acceleration (g)")
   }
   
   if (type == "line") {
      if (rollingmean){
         p = data %>%
            distinct(datetime, rollm = rollmean(Acceleration, steps, na.pad=TRUE)) %>% 
            ggplot(aes(x = datetime, y = rollm)) +
            geom_line()  + 
            labs(x = "Date", 
                 y = "Acceleration (g)") +
            theme(axis.title.x=element_blank())
      } else {
         p = data %>%
            ggplot(aes(x = datetime, y = Acceleration)) +
            geom_line()  +
            labs(x = "Date",
                 y = "Acceleration (g)") +
            theme(axis.title.x = element_blank())
      }
   }
   if (type == "scatter") {
      p = data %>%
         ggplot(aes(x = datetime, y = Acceleration)) +
         geom_point(pch = '.')  +
         labs(x = "Date", y = "Acceleration (g)") +
         theme(axis.title.x = element_blank())
   }
   

   
   return(p)
}


######## HYDRO: TARGET + REFERENCE ########


######## HYDRO: COMPARISON ########
