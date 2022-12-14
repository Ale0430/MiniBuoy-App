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

defaultColors = c("Target" = "#0072B2", # turquoise: 009E73 , blue:0072B2
                  "Reference" = "#56B4E9")
defaultLineTypes = c("Target" = 1,
                     "Reference" = 2)
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
   bins = ui.input$filterPlot_bins
   type = ui.input$filterPlot_type
   aggwindow = ui.input$filterPlot_window

   if (type == "hist") {
      p = data %>%
         ggplot(aes(x = Acceleration)) +
         geom_histogram(bins = bins, col = "black") +
         
         labs(x = "Acceleration (g)")
   } else {
      data.aggr = data %>% 
         mutate(date = ceiling_date(datetime, unit = aggwindow)) %>%
         group_by(date) %>% 
         summarise(meanAcceleration = mean(Acceleration))

      if (type == "line") {
         p = data.aggr %>%
            ggplot(aes(x = date, y = meanAcceleration)) +
            geom_line()  +
            labs(x = "Date",
                 y = "Acceleration (g)") +
            theme(axis.title.x=element_blank())
      }
      if (type == "scatter") {
         p = data.aggr %>%
            ggplot(aes(x = date, y = meanAcceleration)) +
            geom_point()  + # fast option to create scatter plots but very small dots: pch = '.', 
            labs(x = "Date", y = "Acceleration (g)") +
            theme(axis.title.x = element_blank())
      }
   }
   return(p)
}


######## HYDRO: TARGET + REFERENCE ########

#' Inundation
plot.inundation = function(data) {
   return(
      data %>%
         mutate(date = ceiling_date(datetime, unit = 'days')) %>%
         group_by(date) %>%
         summarise(InundationMin = sum(!is.na(Event)) * (.$datetime[2] - .$datetime[1])) %>%
         ggplot(aes(x = date, y = InundationMin)) +
         geom_bar(
            stat = 'identity',
            fill = 'lightblue',
            colour = 'lightblue'
         ) +
         scale_y_continuous(expand = expansion(mult = c(0, .1)), 
                            label = comma,
                            sec.axis = sec_axis(trans=~./60,
                                                name = 'Daily inundation (hours/day)')) +
         labs(y = 'Daily inundation (min/day)') +
         theme(axis.title.x = element_blank())
   )
}

#' Current velocity
plot.velocity = function(data) {
   return(
      data %>%
         ggplot(aes(x = datetime, y = CurrentVelocity)) +
         geom_point(size = 0.2) +
         geom_line(aes(y = rollmean(CurrentVelocity, 20, na.pad = T)), colour = 'blue') +
         scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
         labs(y = 'Median current velocity (m/s)') + 
         theme(axis.title.x = element_blank())
   )
}

#' Wave orbital velocity
plot.waveVelocity = function(data) {
   return(
      data %>%
         ggplot(aes(x = datetime, y = WaveOrbitalVelocity)) +
         geom_point(size = 0.2) +
         geom_line(aes(y = rollmean(WaveOrbitalVelocity, 20, na.pad = T)), colour = 'blue') +
         scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
         labs(y = 'Median wave orbtial velocity (m/s)') + 
         theme(axis.title.x = element_blank())
   )
}

######## HYDRO: COMPARISON ########

#' Inundation
plot.inundationComparison = function(data.t, data.r){
   hydro = data.t %>% 
      mutate(Type = "Target") %>% 
      bind_rows(data.r %>% 
                   mutate(Type = "Reference")) %>%
      mutate(Type = factor(Type,
                           levels = c("Target", "Reference"))) %>% 
      mutate(date = ceiling_date(datetime, unit = 'days')) %>%
      group_by(Type, date) %>%
      summarise(InundationMin = sum(!is.na(Event)) * (.$datetime[2] - .$datetime[1])) 
   
   return(hydro %>% 
             ggplot(aes(x = date, y = InundationMin, fill = Type)) +
             geom_bar(stat = 'identity', position = 'dodge') +
             scale_fill_manual(values = defaultColors) +
             scale_y_continuous(expand = expansion(mult = c(0, .1)),
                                label = comma,
                                sec.axis = sec_axis(trans=~./60,
                                                    name = 'Daily inundation (hours/day)')) +
             labs(y = 'Daily inundation (min/day)',
                  fill = "Site") + 
             theme(axis.title.x = element_blank())
          )
}


#' Current velocity
plot.velocityComparison = function(data.t, data.r){
   return(
      data.t %>% 
         mutate(Type = "Target") %>% 
         bind_rows(data.r %>% 
                      mutate(Type = "Reference")) %>%
         mutate(Type = factor(Type,
                              levels = c("Target", "Reference"))) %>% 
         group_by(Type) %>% 
         mutate(rmCurrentVel = rollmean(CurrentVelocity, 20, na.pad = T)) %>% 
         ggplot(aes(x = datetime, y = rmCurrentVel, col = Type, linetype = Type)) +
         geom_line(size = 1) + 
         scale_y_continuous(expand = expansion(mult = c(0, .1)), label = comma) +
         scale_linetype_manual(name = "Site", values = defaultLineTypes) +
         scale_color_manual(name = "Site", values = defaultColors) +
         labs(y = 'Median current velocity (m/s)',
              color = "Site",
              linetype = "Site") + 
         theme(axis.title.x = element_blank())
   )
}



#' Parameter bar plot
plot.parameterComparison = function(stats.table){
   return(
      stats.table %>% 
         gather(., Type, Value, Reference, Target) %>% 
         mutate(Type = factor(Type,
                              levels = c("Target", "Reference")),
                Parameter = factor(Parameter,
                                   levels = c("Monitoring period (d)", 
                                              "Average flooding duration (min/d)",
                                              "Time flooded during survey (%)",
                                              "Flooding frequency (f/d)",
                                              "Max. Window of opportunity duration (d)",
                                              "Median current velocity (m/s)",
                                              "75 percentile current velocity (m/s)",
                                              "Flood ebb median velocity (m/s)",
                                              "Median wave orbital velocity (m/s)",
                                              "75 percentile wave orbital velocity (m/s)"))) %>% 
         ggplot(., aes(x = Parameter, y = Value, fill = Type)) +
         geom_bar(stat = "identity", position = "dodge", col = "black") +
         scale_fill_manual(values = defaultColors) +
         facet_wrap(~Parameter, scales = "free", 
                    ncol = round(nrow(stats.table)/2),
                    labeller = label_wrap_gen(width=25)) +
         theme(axis.title.x = element_blank(),
               axis.text.x = element_blank())
   )
}
