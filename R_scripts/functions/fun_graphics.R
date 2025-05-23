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
             annotate(geom = "text", x = 5, y = 5, 
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

   if (type == "Histogram") {
      p = data %>%
         ggplot(aes(x = Acceleration)) +
         geom_histogram(bins = bins, col = "black") +
         
         labs(x = "Acceleration (g)")
   } else {
      data.aggr = data %>% 
         mutate(date = ceiling_date(datetime, unit = aggwindow)) %>%
         group_by(date) %>% 
         summarise(meanAcceleration = mean(Acceleration))

      if (type == "LineGraph") {
         p = data.aggr %>%
            ggplot(aes(x = date, y = meanAcceleration)) +
            geom_line()  +
            labs(x = "Date",
                 y = "Acceleration (g)") +
            theme(axis.title.x=element_blank())
      }
      if (type == "ScatterPlot") {
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

#' Control
plot.control = function(data, chop = 0.25) {
  return(
    data %>%
      slice(round(seq(1, n(), length.out = (chop * n())), 0)) %>%
      mutate(Status = factor(Status,
                             levels = c('N', 'P', 'F'),
                             labels = c("not inundated",
                                        "partially inundated",
                                        "fully inundated"))) %>% 
      ggplot(aes(x = datetime, y = Tilt, colour = Status)) +
      geom_point(size = 0.7) +
      scale_x_datetime(date_labels = '%e %b') +
      scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, 30)) +
      scale_color_manual(values = c('#A7C7E7', '#9AA81D', '#3695F5')) +
      labs(y = 'Tilt from horizontal (degrees)',
           col = "Inundation status") +
      theme(axis.title.x = element_blank(),
            legend.key   = element_blank(),
            legend.position = 'bottom',
            plot.title   = element_text(size = 8))
    )
}

#' Inundation
plot.inundation = function(data) {
   return(
      data %>%
         mutate(date = ceiling_date(datetime, unit = 'days')) %>%
         group_by(date) %>%
         summarise(InundationHrs = sum(!is.na(Event)) * (.$datetime[2] - .$datetime[1]) / 60) %>%
         ggplot(aes(x = date, y = InundationHrs)) +
         geom_bar(
            stat = 'identity',
            fill = 'lightblue',
            colour = 'lightblue'
         ) +
         scale_y_continuous(expand = expansion(mult = c(0, .1)), 
                            sec.axis = sec_axis(trans=~./60,
                                                name = 'Daily inundation (hours/day)')) +
         labs(y = 'Daily inundation (hours/day)') +
         theme(axis.title.x = element_blank())
   )
}

#' Current velocity
plot.velocity = function(data, site) {
  return(
    data %>%
      mutate(Site = site) %>%
      filter(!is.na(Event)) %>% 
      group_by(Event) %>% 
      mutate(min_v   = min(CurrentVelocity, na.rm = T),
             max_v   = max(CurrentVelocity, na.rm = T),
             upper_v = quantile(CurrentVelocity, 0.95, names = F, na.rm = T),
             med_v   = median(CurrentVelocity, na.rm = T),
             n = n()) %>% 
      filter(CurrentVelocity == max_v) %>% 
      ggplot(., aes(x = datetime, y = med_v, col = Site, size = n)) +
      geom_pointrange(aes(ymin = min_v, ymax = upper_v)) +
      scale_color_manual(values = defaultColors) +
      scale_size_continuous(range = c(0.6, 1.6)) +
      guides(col = F, size = F) +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      labs(y = 'Median current velocity (m/s)') + 
      ggtitle('Error bars represent upper 95th percentile and minimum detected value') +
      theme(axis.title.x = element_blank())
  )
}


#' Wave orbital velocity
plot.waveVelocity = function(data, site) {
   return(
     data %>%
       mutate(Site = site) %>% 
       filter(!is.na(Event)) %>% 
       group_by(Event) %>% 
       mutate(min_v   = min(WaveOrbitalVelocity, na.rm = T),
              max_v   = max(WaveOrbitalVelocity, na.rm = T),
              upper_v = quantile(WaveOrbitalVelocity, 0.95, names = F, na.rm = T),
              med_v   = median(WaveOrbitalVelocity, na.rm = T),
              n = n()) %>% 
       filter(WaveOrbitalVelocity == max_v) %>% 
       ggplot(., aes(x = datetime, y = med_v, col = Site, size = n)) +
       geom_pointrange(aes(ymin = min_v, ymax = upper_v)) +
       scale_color_manual(values = defaultColors) +
       scale_size_continuous(range = c(0.6, 1.6)) +
       guides(col = F, size = F) +
       scale_y_continuous(expand = expansion(mult = c(0, .1))) +
       labs(y = 'Median wave orbtial velocity (m/s)') + 
       ggtitle('Error bars represent upper 95th percentile and minimum detected value') +
       theme(axis.title.x = element_blank())
   )
}

#' Velocity stage plot
plot.stage = function(data, design) {
  
  id = unique(
    (data %>%
       filter(Event == as.character(which(table(.$Event) == max(table(.$Event))))))
    $Event)
  
  return(
    data %>%
      filter(Event == as.character(which(table(.$Event) == max(table(.$Event))))) %>%
      mutate(
        HighTide = ifelse(Tide == 'Flood', (0:n() * as.numeric(difftime(.$datetime[2], .$datetime[1], units = 'hours'))), (n():0) * as.numeric(difftime(.$datetime[2], .$datetime[1], units = 'hours'))),
        Velocity = ifelse(Tide == 'Flood', CurrentVelocity, CurrentVelocity * -1)) %>%
      ggplot(aes(Velocity, HighTide)) +
      geom_segment((aes(xend = c(tail(Velocity, n = -1), NA),
                        yend = c(tail(HighTide, n = -1), NA)))) +
      geom_rect(aes(xmin = if(design == 'B4')      { -0.04 }
                    else if(design == 'B4+')     { -0.02 }
                    else if(design == 'Pendant') { -0.05 },
                    xmax = if(design == 'B4')      {  0.04 }
                    else if(design == 'B4+')     {  0.02 }
                    else if(design == 'Pendant') {  0.05 },
                    ymin = Inf, ymax = -Inf),
                fill = 'grey90') +
      geom_vline(xintercept = 0, linetype = 'dashed') +
      labs(title = paste0('Inundation event ', id, ' (largest event detected)'),
           x     = 'Current velocity (m/s)',
           y     = 'Hours to high tide') +
      coord_cartesian(xlim = c(-1, 1)) +
      annotate(geom = 'text', x = -Inf, y = -Inf,
               label = 'Ebb',
               vjust = -1, hjust = -0.2,
               fontface = 'italic') +
      annotate(geom = 'text', x = Inf, y = -Inf,
               label = 'Flood',
               vjust = -1, hjust = 1.2,
               fontface = 'italic') +
      theme(legend.position = 'none',
            panel.grid        = element_blank(),
            panel.border      = element_rect(fill = NA, colour = 'black', linewidth = 0.5),
            panel.background  = element_blank(),
            axis.ticks.length = unit(-0.1, 'cm'),
            axis.title        = element_text(colour = 'black', size = 11),
            axis.text         = element_text(colour = 'black'),
            axis.text.y       = element_text(size = 11, hjust = 0.5))
  )
}

#' Windows of Opportunity
plot.woo = function(data) {
  return(
    data %>%
      get.woo.statistics() %>%
      ggplot(aes(x = EmersionEvent, y = Value)) +
      geom_line() +
      geom_point() +
      labs(x = 'Emersion event',
           y = 'Window of Opportunity duration (hours)')
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
                           levels = c("Reference", "Target"))) %>% 
      mutate(date = ceiling_date(datetime, unit = 'days')) %>%
      group_by(Type, date) %>%
      summarise(InundationHrs = sum(!is.na(Event)) * (.$datetime[2] - .$datetime[1]) / 60) 
   
   return(hydro %>% 
             ggplot(aes(x = date, y = InundationHrs, fill = Type)) +
             geom_bar(stat = 'identity', position = 'dodge') +
             scale_fill_manual(values = defaultColors) +
             scale_y_continuous(expand = expansion(mult = c(0, .1)),
                                sec.axis = sec_axis(trans=~./60,
                                                    name = 'Daily inundation (hours/day)')) +
             labs(y = 'Daily inundation (hours/day)',
                  fill = "Site") + 
             theme(axis.title.x = element_blank())
          )
}


#' Current velocity
plot.currentsComparison = function(data.t, data.r){
   return(
     data.t %>% 
       mutate(Site = "Target") %>% 
       bind_rows(data.r %>% 
                   mutate(Site = "Reference")) %>%
       group_by(Site, Event) %>% 
       mutate(min_v = min(CurrentVelocity, na.rm = T),
              max_v = max(CurrentVelocity, na.rm = T),
              med_v = median(CurrentVelocity, na.rm = T),
              upper_v = quantile(CurrentVelocity, 0.95, names = F, na.rm = T),
              n = n()) %>% 
       filter(CurrentVelocity == max_v) %>% 
       ggplot(., aes(x = datetime, y = med_v, col = Site, size = n)) +
       geom_pointrange(aes(ymin=min_v, ymax=upper_v)) +
       scale_color_manual(values = defaultColors) +
       scale_size_continuous(range = c(0.2, 1)) +
       guides(size = F) +
       scale_y_continuous(expand = expansion(mult = c(0, .1))) +
       labs(y = 'Median current velocity (m/s)') + 
       ggtitle('Error bars represent upper 95th percentile and minimum detected value') +
       theme(axis.title.x = element_blank())
   )
}

#' Wave orbital velocity
plot.wavesComparison = function(data.t, data.r){
  return(
    data.t %>% 
      mutate(Site = "Target") %>% 
      bind_rows(data.r %>% 
                  mutate(Site = "Reference")) %>%
      group_by(Site, Event) %>% 
      mutate(min_v = min(WaveOrbitalVelocity, na.rm = T),
             max_v = max(WaveOrbitalVelocity, na.rm = T),
             med_v = median(WaveOrbitalVelocity, na.rm = T),
             upper_v = quantile(WaveOrbitalVelocity, 0.95, names = F, na.rm = T),
             n = n()) %>% 
      filter(WaveOrbitalVelocity == max_v) %>% 
      ggplot(., aes(x = datetime, y = med_v, col = Site, size = n)) +
      geom_pointrange(aes(ymin=min_v, ymax=upper_v)) +
      scale_color_manual(values = defaultColors) +
      scale_size_continuous(range = c(0.2, 1)) +
      guides(size = F) +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      labs(y = 'Median wave orbital velocity (m/s)') +
      ggtitle('Error bars represent upper 95th percentile and minimum detected value') +
      theme(axis.title.x = element_blank())
  )
}

#' Parameter boxplot
plot.parameterComparison = function(data.t, data.r, design){
  
  event.r = data.r %>% get.event.statistics(design)
  event.t = data.t %>% get.event.statistics(design)
  
  return(
    event.t %>%
      mutate(Site = 'Target') %>%
      bind_rows(event.r %>%
                  mutate(Site = 'Reference')) %>%
      ggplot(., aes(x = Parameter, y = Value, fill = Site, 
                    group = interaction(Parameter, Site))) +
      # stat_boxplot(geom = 'errorbar') + 
      geom_boxplot(position = "dodge2") + 
      coord_flip() +
      scale_fill_manual(values = defaultColors) +
      theme(axis.title.y = element_blank())
  )
}

