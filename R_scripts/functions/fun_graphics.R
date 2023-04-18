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


### NEW VERSION: ###

ggplotly2 = function(x) {
  
  fig = ggplotly(x)
  fig = fig %>% toWebGL()
  suppressWarnings(print(fig))
  
}

data.check = function(data, s = 1) {
  
  data %>% 
    slice(round(seq(1, n(), length.out = (s * n())), 0)) %>%
    ggplot(aes(datetime, Tilt, colour = Status)) +
    geom_point(size = 0.1) +
    scale_x_datetime(date_labels = '%e %b') +
    scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, 30)) +
    scale_color_manual(breaks = c('N', 'P', 'F'),
                       values = c('#A7C7E7', '#C1E1C1', '#3D426B'),
                       name = 'Status:') +
    labs(y     = 'Tilt from horizontal (°)') +
    theme(axis.title.x = element_blank(),
          legend.key = element_blank(),
          panel.background  = element_blank(),
          panel.grid        = element_blank(),
          panel.border      = element_rect(fill = NA, colour = 'black', size = 0.5),
          axis.title        = element_text(colour = 'black'),
          axis.text         = element_text(colour = 'black'),
          axis.ticks.length = unit(-0.1, 'cm'),
          plot.title        = element_text(colour = 'black'),
          legend.text       = element_text(colour = 'black'),
          legend.title      = element_text(colour = 'black'),
          legend.position = 'bottom') +
    guides(colour = guide_legend(override.aes = list(size = 5)))
  
}


####################


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

### Additional: incorporate above:

### velocity stage plot:

# test = data %>%
#   select(Date, Event, Tide, CurrentVelocity) %>%
#   na.omit() %>%
#   group_by(Event, Tide) %>%
#   mutate(
#     HighTide = ifelse(Tide == 'Flood', (0:n()/n()* 100), (n():0)/n() * 100),
#     Velocity = ifelse(Tide == 'Flood', CurrentVelocity, CurrentVelocity * -1)) %>%
#   group_by(Event) %>%
#   mutate(Size = length(Event)) %>%
#   ungroup()
# 
# test %>%
#   filter(Size == max(Size)) %>%
#   ggplot(aes(Velocity, HighTide)) +
#   geom_segment((aes(xend = c(tail(Velocity, n = -1), NA),
#                     yend = c(tail(HighTide, n = -1), NA)))) +
#   geom_rect(aes(xmin = -0.13, xmax = 0.13,
#                 ymin = Inf, ymax = -Inf), 
#             fill = 'grey90') +
#   geom_point(size = 0.2) +
#   geom_vline(xintercept = 0, linetype = 'dashed') +
#   labs(title = 'Velocity stage plot for largest tide detected\n',
#        x     = 'Current velocity (m/s)',
#        y     = 'Duration to high tide (%)')

### target-reference plot

# size.Target    = event.Target    %>% group_by(Parameter) %>% summarise(n = n())
# size.Reference = event.Reference %>% group_by(Parameter) %>% summarise(n = n())
# size.All       = bind_rows(event.Target    %>% group_by(Site, Parameter) %>% summarise(n = n()),
#                            event.Reference %>% group_by(Site, Parameter) %>% summarise(n = n()))
# 
# PLOT = 'Inundation duration'
# 
# event.All %>%
#   filter(Parameter == PLOT) %>%
#   left_join(size.All, by = c('Site', 'Parameter')) %>%
#   mutate(x = paste0(Site, '\n', '(', n, ' events)')) %>%
#   ggplot(aes(x, Value, fill = Site)) +
#   geom_violin(colour = 'white') +
#   geom_boxplot(width = 0.1, colour = 'white', alpha = 0.2) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_x_discrete(limits = rev) +
#   scale_fill_viridis(discrete = T) +
#   labs(y = 'Inundation duration (min)') +
#   theme(legend.position = 'none',
#         axis.title.y = element_blank(),
#         panel.grid        = element_blank(),
#         panel.border      = element_rect(fill = NA, colour = 'black', size = 0.5),
#         panel.background  = element_blank(),
#         axis.ticks.length = unit(-0.1, 'cm'),
#         axis.title        = element_text(colour = 'black', size = 11),
#         axis.text         = element_text(colour = 'black'),
#         axis.text.y       = element_text(size = 11, hjust = 0.5)) +
#   coord_flip()
