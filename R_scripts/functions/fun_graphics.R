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
plot.histogram <- function(data, ui.input){
   variable.col = ui.input$DataSetInput
   Target.col= ui.input$DataSet
   fill.col = ui.input$filterPlot_col
   binwidth = ui.input$filterPlot_binwidth
   type = ui.input$filterPlot_type
   facetGrid = ui.input$filterPlot_facetGrid

   variable = data[, variable.col]

   if (fill.col != "none"){
      if (fill.col == "date"){
         fill = factor(data[, fill.col])
      } else {
         fill = data[, fill.col]
      }
      p = data %>% 
         ggplot(., aes(fill = fill, col = fill, group = fill))# +
         labs(fill = labels[fill.col][[1]],
              col = labels[fill.col][[1]])
   } else {
      p = data %>% 
         ggplot(.) +
         labs(fill = labels[fill.col][[1]],
              col = labels[fill.col][[1]])
   }
   
   if (type == "hist"){
      p = p +
         geom_histogram(mapping=aes(x = Acceleration), 
                        binwidth = binwidth, col = "white") + #, fill = fill
         labs(x = "Acceleration (g) ",
              fill = labels[fill.col][[1]])
      
      
   } 
   
   # Set x axis for violin and boxplot
   if (fill.col == "none"){
      x = 0
   } 
   if (fill.col == "date"){
      x = factor(data[, fill.col])
   }

   if (type == "scatter"){
      p = p +
         geom_point(mapping=aes(x = datetime, y = Acceleration), alpha = 0.5)  + 
         #stat_summary(mapping=aes(x = x, y = variable),
                   #   fun.data=data_summary) +
         labs(x = "Date", y= "Acceleration (g)") +
         theme(axis.title.x=element_blank())
   }
   # if (Target.col == "Target" ){
   #   p = p +
   #     scale_color_gradient(low = gradientcolors()[1],
   #                          high = gradientcolors()[2],
   #                          trans = "doy") +
   #     scale_fill_gradient(low = gradientcolors()[1],
   #                         high = gradientcolors()[2],
   #                         trans = "doy")
   #}
   
   if (fill.col == "date"){
      p = p +
         scale_color_gradient(low = gradientcolors()[1],
                              high = gradientcolors()[2],
                              trans = "date") +
         scale_fill_gradient(low = gradientcolors()[1],
                              high = gradientcolors()[2],
                              trans = "date")
   }
  # if (fill.col == "position"){
     # N = length(unique(data$position))
    #  p = p +
     #    scale_color_manual(values = fillcolors(N)) +
     #    scale_fill_manual(values = fillcolors(N))
  # }
   if (facetGrid){
      p = p +
         facet_grid( ~ date, labeller = label_both, scales = "free_x")
   }
   

   return(p)
}


######## TEMPERATURES ########

#' Returns min, mean and max Doy
#' to build color legend (breaks)
get.doy.legend = function(data.complete){
   myfuns <- list(min, mean, max)
   day_legend <- unlist(lapply(myfuns, function(f) round(f(data.complete$doy), 0)))
   
}

#' Customized diagram settings
#' @description Get UI-settings to render plot
#' @param ui.input: UI-input
#' @return list
get.customizedPlotSettings = function(ui.input){
   return(list(
      x.col = ui.input$rawPlot.xcol,
      y.col = ui.input$rawPlot.ycol,
      col.col = ui.input$rawPlot.col,
      shape.col = ui.input$rawPlot.shape,
      facetWrap = ui.input$rawPlot_facetWrap,
      scales = ui.input$rawPlot_scales,
      facet = ui.input$rawPlot.facet,
      no.cols = ui.input$rawPlot.columns,
      all.dT = ui.input$rawPlot_gathered,
      draw_lines = ui.input$rawPlot_lines
   ))
}

#' Customized diagram
#' @description Shows (filtered) data as customized plot
#' @param data: data.frame, long-format
#' @param ui.input: UI-input
#' @return ggplot-object
plot.customTemperature <- function(data, ui.input.processed){
   x.col = ui.input.processed$x.col
   y.col = ui.input.processed$y.col
   col.col = ui.input.processed$col.col
   shape.col = ui.input.processed$shape.col
   facetWrap = ui.input.processed$facetWrap
   scales = ui.input.processed$scales
   facet = ui.input.processed$facet
   no.cols = ui.input.processed$no.cols
   draw_lines = ui.input.processed$draw_lines
   
   if (draw_lines & col.col == "dTime"){
      p = plot.emptyMessage("Error: Settings not possible. \nDay time can not be selected \nas color in line mode.")
   } else {
      if (!ui.input.processed$all.dT){
         x = data[, x.col]
         y = data[, y.col]
         # Use try() do avoid crash/ error message if col = none
         # or shape = none is chosen
         col = try(data[, col.col], silent = T)
         shape = try(data[, shape.col], silent = T)
   
         p = data %>% 
            ggplot(., aes(x = x, y = y, shape = factor(shape),
                          group = interaction(shape, col),
                          linetype = factor(shape))) +
            labs(x = labels[x.col][[1]],
                 y = labels[y.col][[1]],
                 col = labels[col.col][[1]],
                 shape = labels[shape.col][[1]],
                 linetype = labels[shape.col][[1]]) 
         
         if (draw_lines){
            if (shape.col == "none"){
               p = p + 
                  guides(linetype = F)
            }
            if (col.col == "none"){
               p = p + 
                  geom_line(col = "black") +
                  guides(col = F)
            }
            if (col.col == "position"){
               p = p +
                  geom_line(aes(col = factor(col))) +
                  scale_color_manual(values = fillcolors(length(unique(col))))
            } 
            if (col.col == "date"){
               p = p +
                  geom_line(aes(col = col)) +
                  scale_color_gradient(low = gradientcolors()[1],
                                       high = gradientcolors()[2],
                                       trans = "date") 
            } 
            if (col.col == "doy"){
               p = p +
                  geom_line(aes(col = col)) +
                  scale_color_gradient(low = gradientcolors()[1],
                                       high = gradientcolors()[2],
                                       breaks = get.doy.legend(data))
            }
            
            if (length(unique(shape)) > 6){
               p = p +
                  scale_shape_manual(values = c(1:length(unique(shape))))
            }
            
            
         } else {
            if (shape.col == "none"){
               p = p + 
                  geom_point(shape = 1) +
                  guides(shape = F)
            }
            if (col.col == "none"){
               p = p + 
                  geom_point(col = "black") +
                  guides(col = F)
            } 
            if (col.col == "dTime"){
               p = p +
                  geom_point(aes(col = col)) +
                  scale_color_gradient2(low = gradientcolors()[2], 
                                        high = gradientcolors()[2], 
                                        mid = gradientcolors()[1],
                                        midpoint = 12)
            } 
            if (col.col == "position"){
               p = p +
                  geom_point(aes(col = factor(col))) +
                  scale_color_manual(values = fillcolors(length(unique(col))))
            } 
            if (col.col == "date"){
               p = p +
                  geom_point(aes(col = col)) +
                  scale_color_gradient(low = gradientcolors()[1],
                                       high = gradientcolors()[2],
                                       trans = "date")
            } 
            if (col.col == "doy"){
               p = p +
                  geom_point(aes(col = col)) +
                  scale_color_gradient(low = gradientcolors()[1],
                                       high = gradientcolors()[2],
                                       breaks = get.doy.legend(data))
            }
            
            if (length(unique(shape)) > 6){
               p = p +
                  scale_shape_manual(values = c(1:length(unique(shape))))
            }
         }
      } else {
         p = data %>% 
            gather(., key, value, dTas, dTSym, dTsym.dTas) %>% 
            ggplot(., aes(x = datetime, y = value,
                          col = key, group = key)) +
            geom_line() +
            scale_color_manual(values = fillcolors(3)) +
            labs(x = labels["datetime"][[1]],
                 y = labels["dT"][[1]],
                 col = labels["dT"][[1]]) 
      }
      
      if (facetWrap){
         facet = get.labelledFacets(data, facet) #facet.col #hier
         p = p +
            facet_wrap(~ (facet), scales = scales,
                       ncol = no.cols)
      }
   }
   
   return(p)
}

######## K-ESTIMATION ######## 

get.intersection <- function(data, y.col, x.col1, x.col2){
   m1 = lm(data[, y.col] ~ data[, x.col1], data = data)
   m2 = lm(data[, y.col] ~ data[, x.col2], data = data)
   a = coef(m1) - coef(m2)
   x = -a[[1]] / a[[2]]
   y = coef(m1)[[2]]*x + coef(m1)[[1]]
   return(c(x, y))
}


#' Diurnal dTsym.dTas diagram
#' @description Shows diurnal pattern of dTsym.dTas to determine low-flow times
#' @param data: data.frame, long-format
#' @return ggplot-object
plot.nighttime <- function(data.complete){

   return(ggplot(data.complete, aes(x = dTime, y = dTsym.dTas,
                             col = date, group = date)) +
      # ylim(0, max(data.complete$dTsym.dTas)) +
      geom_hline(yintercept = 0., linetype = "dashed",  col = "#333333") +
      geom_line() +
      scale_color_gradient(low = gradientcolors()[1], 
                           high = gradientcolors()[2], 
                           trans = "date") +
      labs(x = labels["dTime"][[1]], 
           y = labels["dTsym.dTas"][[1]],
           col = labels["date"][[1]]) 
      )
}

#' K-diagram
#' @description Shows temperature differences against dTsym.dTas to determine K
#' @param data: data.frame, long-format, complete data per positions
#' @param data.adj: data.frame, long-format, data per position selected for regression
#' @param ui.input: UI-input
#' @return ggplot-object
plot.kEst1 <- function(data.complete, data.adj, k, ui.input){
   xRange = c(ui.input$k1Plot.x.min, ui.input$k1Plot.x.max)
   fullrange = ui.input$k1Plot.fullrange
   fixedScales = ui.input$k1Plot_scales
   kMethod = ui.input$kMethod
   
   d = data.complete %>% 
      gather(., temp, value, dTsa, dTas, dTSym)
   
   if (min(data.complete$dTsym.dTas, na.rm = T) < 0){
      xmin = min(data.complete$dTsym.dTas, na.rm = T)
   } else {
      xmin = -0.1
   }
   
   p = ggplot() +
      geom_point(d, 
                 mapping=aes(x = dTsym.dTas, y = value, group = temp,
                             col = temp), shape = 1) +
      scale_color_manual(values=fillcolors(3)) +
      xlim(xmin, max(d$dTsym.dTas)) +
      geom_vline(xintercept = 0, linetype = "dashed", col = "#333333") +
      
      labs(x = labels["dTsym.dTas"][[1]], 
           y = labels["dT"][[1]], 
           col = labels["T"][[1]])
      
   if (kMethod == "regression"){
      ad = data.adj %>% 
         gather(., temp, value, dTsa, dTas)
      p = p +
        geom_point(ad, 
                   mapping = aes(x = dTsym.dTas, y = value, group = temp), 
                   col = "black", shape = 4) +
        stat_smooth(ad, method = "lm", formula = 'y ~ x',
                    mapping=aes(x = dTsym.dTas, y = value, group = temp),
                    col = "red") +
        stat_regline_equation(ad,
                              mapping=aes(x = dTsym.dTas, y = value, group = temp,
                                          label =  paste(..eq.label..,
                                                         ..adj.rr.label.., 
                                                         sep = "~~~~")),
                              label.y.npc = c("top", "bottom")) +
        labs(caption = "* Black cross (x): data point used for regression")
   } else {
      p = p +
         geom_point(data.frame(k = c(k, -k)),
                    mapping=aes(x = 0, y = k),
                    size = 4, col = "red", shape = 8)
   }

   
   if (fixedScales){
      p = p +
         xlim(xRange[1], xRange[2])
   }
   if (kMethod == "regression" & fullrange){
      p = p +
         stat_smooth(ad, method = "lm", 
                     mapping=aes(x = dTsym.dTas, y = value, group = temp),
                     col = "#333333", fullrange = T, se = F,
                     size = 0.5)
   }
   return(p)
}

#' K-control diagram 1
#' @param data: data.frame, long-format, complete data per positions
#' @param data.adj: data.frame, long-format, data per position selected for regression
#' @param ui.input: UI-input
#' @param k: K
#' @return ggplot-object
plot.kEst2 <- function(data.complete, data.adj, k, 
                       ui.input){
   if (is.na(k)){
      return(plot.emptyMessage("K is not defined."))
   }
   xRange = c(ui.input$k1Plot.x.min, ui.input$k1Plot.x.max)
   fullrange = ui.input$k1Plot.fullrange
   fixedScales = ui.input$k1Plot_scales
   force = ui.input$k1Plot.forceOrigin
   kMethod = ui.input$kMethod
   
   if (min(data.complete$dTsym.dTas, na.rm = T) < 0){
      xmin = min(data.complete$dTsym.dTas, na.rm = T)
   } else {
      xmin = -0.1
   }
   
   fit = ifelse(force, "y ~ x + 0", "y ~ x")
   
   d = data.complete %>% 
      mutate("K+dTsa" = (dTsa + k)) %>% 
      gather(., temp, value, dTsa, dTas, dTSym, `K+dTsa`)

   p = ggplot() +
      geom_point(d, 
                 mapping=aes(x = dTsym.dTas, y = value, group = temp,
                             col = temp), shape = 1) +
      geom_label(aes(x = 0.9 * max(d$dTsym.dTas), y = 0.9 * max(d$value),
                     label = paste("k = ", round(k, 2))), 
                 fill = "#B8B361", alpha = 0.6) +
      scale_color_manual(values=fillcolors(4)) +
      xlim(xmin, max(d$dTsym.dTas)) +
      ylim(min(d$value), max(d$value)) +
      geom_vline(xintercept = 0, linetype = "dashed", col = "#333333") +
      geom_hline(yintercept = 0, linetype = "dashed", col = "#333333") +
      labs(x = labels["dTsym.dTas"][[1]], 
           y = labels["dT"][[1]], 
           col = labels["T"][[1]])
   
   if (kMethod == "regression"){
      newAdj = data.adj %>% 
         mutate("K+dTsa" = (dTsa + k))%>% 
         gather(., temp, value, dTSym, `K+dTsa`) #dTsa, dTas,
      p = p +
         geom_point(newAdj, 
                    mapping=aes(x = dTsym.dTas, y = value), 
                    shape = 4) +
         stat_smooth(newAdj, method = "lm", formula = fit,
                     mapping=aes(x = dTsym.dTas, y = value, group = temp,
                                 col = temp),
                     col = "red", size = 0.5,
                     fullrange = T, se = F) +
         stat_regline_equation(newAdj,
                               formula = fit, #force through origin x+0
                               mapping=aes(x = dTsym.dTas, y = value, group = temp,
                                           label =  ..adj.rr.label..),
                               label.y.npc = c("top", "bottom")) +
         labs(caption = "* Black cross (x): data point used for regression")
         
   }
   
   if (fixedScales){
      p = p +
         xlim(xRange[1], xRange[2]) +
         geom_label(aes(x = 0.9 * xRange[2], y = 0.9 * max(d$value),
                        label = paste("k = ", round(k, 2))), 
                    fill = "#B8B361", alpha = 0.6)
   }
   if (fullrange){
      p = p +
         stat_smooth(data.adj, method = "lm", 
                     mapping=aes(x = dTsym.dTas, y = dTas),
                     col = "#333333", fullrange = T, se = F,
                     size = 0.5)
   }
   return(p)
}

#' K-control diagram 2
#' @param data: data.frame, long-format, complete data per positions
#' @param data.adj: data.frame, long-format, data per position selected for regression
#' @param ui.input: UI-input
#' @param k: K
#' @return ggplot-object
plot.kEst3 <- function(data.complete, data.adj, k,
                       ui.input){
   if (is.na(k)){
      return(plot.emptyMessage("K is not defined."))
   }
   xRange = c(ui.input$k1Plot.x.min, ui.input$k1Plot.x.max)
   fixedScales = ui.input$k1Plot_scales
   kMethod = ui.input$kMethod
   
   d = data.complete %>%
      mutate(`R = (k + dTsa) / dTas` = (k + dTsa) / dTas) %>% 
      gather(., x.temp, x.value, `dTsym.dTas`, `R = (k + dTsa) / dTas`)
   
   if (min(data.complete$dTsym.dTas, na.rm = T) < 0){
      xmin = min(data.complete$dTsym.dTas, na.rm = T)
   } else {
      xmin = -0.1
   }
   
   p = ggplot() +
      geom_point(d, 
                 mapping=aes(x = x.value, y = dTas, 
                             col = x.temp, shape = "dTas")) +
      geom_point(d, 
                 mapping=aes(x = x.value, y = dTsa, 
                             col = x.temp, shape = "dTsa")) +
      geom_label(aes(x = 0.9 * max(d$x.value), y = 0.9 * max(d$dTas),
                     label = paste("k = ", round(k, 2))), 
                 fill = "#B8B361", alpha = 0.6) + #D2D0AD
      scale_color_manual(values=fillcolors(2)) +
      scale_shape_manual(values = c(21, 24)) +
      xlim(xmin, max(d$x.value)) +
      ylim(-max(d$dTas), max(d$dTas)) +
      geom_vline(xintercept = 0, linetype = "dashed", col = "#333333") +
      labs( x = "dTsym /dTas | R = (k + dTsa) / dTas", 
            y = labels["dT"][[1]], 
            col = "x-axis", 
            shape = labels["T"][[1]])
   
   if (kMethod == "regression"){
      newAdj = data.adj %>% 
         mutate(`R = (k + dTsa) / dTas` = (k + dTsa) / dTas) %>% 
         gather(., x.temp, x.value, `dTsym.dTas`, `R = (k + dTsa) / dTas`)
      p = p +
         geom_point(newAdj, 
                    mapping=aes(x = x.value, y = dTas), 
                    shape = 4) +
         stat_smooth(newAdj, method = "lm", formula = 'y ~ x',
                     mapping=aes(x = x.value, y = dTas,
                                 col = x.temp, group = x.temp),
                     col = "red", size = 0.5,
                     fullrange = T, se = F) +
         geom_point(newAdj, 
                    mapping=aes(x = x.value, y = dTsa), 
                    shape = 4) +
         stat_smooth(newAdj, method = "lm", formula = 'y ~ x',
                     mapping=aes(x = x.value, y = dTsa,
                                 col = x.temp, group = x.temp),
                     col = "red", size = 0.5,
                     fullrange = T, se = F) +
         labs(caption = "* Black cross (x): data point used for regression.
            Gray-shaded values indicate the point of intersection of the two lines.")
      
   }
   if (fixedScales){
      p = p +
         xlim(xRange[1], xRange[2]) +
         geom_label(aes(x = 0.9 * xRange[2], y = 0.9 * max(d$dTas),
                        label = paste("k = ", round(k, 2))), fill = "#B8B361", alpha = 0.6)
   }

   return(p)
}

######## SAP FLOW METRICS ########

assign.customized.groups = function(data, ui.input){
   data$group = "no group assigned"
   
   groups_char = ui.input$sf_grouped_positions
   if (groups_char != ""){
      # remove white spaces
      # g_chars = gsub(" ", "", groups_char, fixed = TRUE)
      # get groups
      g_chars = strsplit(groups_char, ";")[[1]]

      for (i in 1:length(g_chars)){
         g_char = strsplit(g_chars[i], ":")[[1]]
         g_name = g_char[1]
         g_positions = as.numeric(strsplit(g_char[2], ",")[[1]])
         data[data$position %in% g_positions, "group"] = g_name
      }
   }
   return(data)
}

add.group.mean = function(data, y.col){
   y.col = sym(y.col)
   return(data %>% 
             group_by(datetime, group) %>% 
             mutate(
                mean_y = mean({{y.col}})
             ))
}

plot.sf.helper = function(data, ui.input, radial.profile = FALSE){
   data$SFI = data$dTSym

   if (ui.input$sf_y_axis %in% colnames(data)){
      p = plot.sf.function(data = data,
                        ui.input = ui.input, 
                        radial.profile = radial.profile)
   } else {
      p = plot.emptyMessage(message = "Sapwood depth is missing (see 'Project settings')")
   }
   return(p)
}

plot.sf.function = function(data, ui.input, radial.profile = FALSE){
   if (ui.input$sf_style == "sf_facet_wrap"){
      p = plot.sf.facets(data, ui.input, radial.profile)
   } else {
      if (ui.input$sf_style == "sf_grouped"){
         p = plot.sf.groups(data, ui.input, radial.profile)
      } else {
         p = plot.sf.basic(data, ui.input, radial.profile)
      }
   }
   return(p)
}

plot.sf.basic = function(data, ui.input, radial.profile = FALSE){
   y.col = ui.input$sf_y_axis
   
   if (radial.profile){
      p = data %>% 
         ggplot(., aes(x = factor(position), y = get(y.col))) +
         geom_boxplot(aes(col = factor(position))) +
         labs(y = labels[[y.col]],
              x = labels[["position"]],
              col = labels[["position"]]) 
   } else {
      p = data %>% 
         ggplot(., aes(x = datetime, y = get(y.col))) +
         geom_line(aes(col = factor(position))) +
         labs(y = labels[[y.col]],
              x = "",
              col = labels[["position"]])
   }
   N = length(unique(data$position))
   p = p +
      scale_color_manual(values = fillcolors(N))
   return(p)
}

plot.sf.facets = function(data, ui.input, radial.profile = FALSE){
   y.col = ui.input$sf_y_axis
   
   scales = ui.input$sf_facet_scales
   facet.col = ui.input$sf_facet_column
   facet.col.no = ui.input$sf_facet_col_nums
   
   # Remove NA values in facet column if present
   data = data[complete.cases(data[, facet.col]), ]
   # Get facets based on column name
   facet = get.labelledFacets(data, facet.col)
   col.col = ifelse(facet.col == "position", "date", "position")
   if (col.col == "position"){
      col = factor(data[, col.col])
   } else {
      col = data[, col.col]
   }

   if (radial.profile){
      p = data %>% 
         ggplot(., aes(x = col, y = get(y.col))) +
         geom_boxplot(aes(col = col, group = col)) + 
         labs(y = labels[[y.col]],
              x = labels[["position"]],
              col = labels[[col.col]]) 
   } else {
      p = data %>%
         ggplot(aes(x = dTime, y = get(y.col))) +
         geom_line(aes(col = col, group = col)) +
         labs(x = labels["dTime"][[1]],
              y = labels[y.col][[1]],
              col = labels[col.col][[1]])
   }
   
   N = length(unique(data[, col.col]))
   if (col.col == "date"){
      p = p +
         scale_color_gradient(low = gradientcolors()[1],
                              high = gradientcolors()[2],
                              trans = "date")
   } else {
      p = p  +
         scale_color_manual(values = fillcolors(N)) 
   }
   p = p  +
      facet_wrap(~ (facet), scales = scales,
                 ncol = facet.col.no)
   return(p)
}

plot.sf.groups = function(data, ui.input, radial.profile = FALSE){
   y.col = ui.input$sf_y_axis
   
   GroupLegendName = ifelse(ui.input$sf_grouped_name == "", "Group",
                            ui.input$sf_grouped_name)
   
   data = assign.customized.groups(data, ui.input)
   N = length(unique(data$group))
   
   if (radial.profile){
      p = add.group.mean(data, y.col) %>% 
         ggplot(., aes(x = factor(group), col = factor(group), y = get(y.col))) +
         geom_boxplot(aes(col = factor(group))) +
         labs(x = GroupLegendName, 
              y = labels[y.col][[1]],
              col = GroupLegendName) +
         theme(axis.title.x = element_blank())
   } else {
      p = add.group.mean(data, y.col) %>% 
         ggplot(., aes(x = datetime, col = group)) +
         geom_line(aes(y = get(y.col), linetype = factor(position))) +
         geom_line(aes(y = mean_y), size = 1.1) +
         labs(x = "", 
              y = labels[y.col][[1]],
              col = GroupLegendName,
              linetype = labels["position"][[1]])
   }
   p = p +
      scale_color_manual(values = fillcolors(N))
   return(p)
}

# Neg. formula control plot
plot.sf.neg.control = function(data, ui.input){
   scales = ui.input$sf_facet_scales
   facet.col.no = ui.input$sf_facet_col_nums
   
   return(
      data %>% 
         gather(., key, value, SFS, SFSpos) %>% 
         mutate(key = factor(key, 
                             levels = c("SFS", "SFSpos"),
                             labels = c("... with correction",
                                        "... without correction"))) %>% 
         ggplot(., aes(x = datetime, y = value, col = key, linetype = key)) +
         geom_hline(yintercept = 0, col = "#696969") +
         geom_line(size = .8) +
         scale_color_manual(values = c("red", "black")) +
         facet_wrap(~position, ncol = facet.col.no,
                    scales = scales) +
         theme(axis.title.x = element_blank()) +
         labs(col = "Formula",
              y = labels[["SFS"]],
              linetype = "Formula"))
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


######## SAP FLOW RATE ########

#' Upscaling method
#' @description Determine which methods have been selected in UI
#' @param ui.input: UI-input
#' @return vector
get.selectedMethods = function(ui.input){
   groups = c()
   if (ui.input$treeScaleSimple1){
      groups = rbind(groups, "sfM1")
   }
   if (ui.input$treeScaleSimple2){
      groups = rbind(groups, "sfM2")
   }
   if (ui.input$treeScaleSimple3){
      groups = rbind(groups, "sfM3")
   }
   
   groups = groups[,1]
   return(groups)
}

#' Sap flow diagram
#' @param data: data.frame, long-format, complete data per positions
#' @param ui.input: UI-input
#' @return ggplot-object
plot.sapFlowRate = function(data, ui.input){
   N = 0
   p = data %>% 
      ggplot(.) +
      labs(x = "",
           y = labels["SF"][[1]],
           color = "Scaling method",
           linetype = "Scaling method")
   if (ui.input$treeScaleSimple1){
      p = p +
         geom_line(aes(x = datetime, y = sfM1, color = "Method 1",
                       linetype = "Method 1"),
                   size = 0.9)
      N = N + 1
   }
   if (ui.input$treeScaleSimple2){
      p = p +
         geom_line(aes(x = datetime, y = sfM2, color = "Method 2",
                       linetype = "Method 2"),
                   size = 0.9)
      N = N + 1
   }
   if (ui.input$treeScaleSimple3){
      p = p +
         geom_line(aes(x = datetime, y = sfM3, color = "Method 3",
                       linetype = "Method 3"),
                   size = 0.9)
      N = N + 1
   }
   p = p +
      scale_color_manual(values = fillcolors(N))
   
   return(p)
}


#' Water balance diagram
#' @description Shows the area under the curve for measured sap flow density in kg, separated by direct and reverse flow.
#' @param data: data.frame, long-format, complete data per positions
#' @param ui.input: UI-input
#' @return ggplot-object
plot.sapFlowDay = function(data, ui.input){
   groups = get.selectedMethods(ui.input)

   auc.data = data %>% 
      gather(., Method, SFrate, groups) %>% 
      mutate(Method = ifelse(Method == "sfM1", "Method 1",
                             ifelse(Method == "sfM2", "Method 2",
                                    "Method 3")),
             Balance = ifelse(SFrate >= 0, "Positive", "Negative")) %>% 
      mutate(Balance = factor(Balance, levels = c("Positive", "Negative"))) %>% 
      filter(complete.cases(.)) %>%
      group_by(doy, Method, Balance) %>% 
      arrange(dTime) %>% 
      mutate(auc = sum(diff(dTime) * (head(SFrate,-1)+tail(SFrate,-1)))/2) %>% 
      select(doy, Method, Balance, auc) %>% 
      unique(.)
   print(paste("AUC water balance: ", nrow(auc.data)))
   return(auc.data %>% 
      ggplot(., aes(x = factor(doy), fill = Method, y = auc)) +
         geom_bar(position="dodge", stat="identity", 
                  col = "black", alpha = 0.6) +
         scale_fill_manual(values = fillcolors(length(groups))) +
         labs(x = labels["doy"][[1]],
              y = "Area under curve (kg)",
              fill = "Scaling method"))
}

#' Radial profile of sap flow
#' @description Shows the fraction of sap flow in each annuli in kg/L.
#' @param data: data.frame, long-format, complete data per positions
#' @param ui.input: UI-input
#' @return ggplot-object
plot.twu.radialprofile = function(data, ui.input){
   groups = get.selectedMethods(ui.input)
   # Remove sfM2 from groups as method 2 does not allow to estimate
   # radial profiles
   groups = groups[!grepl("sfM2", groups)]
   # Over ride sf values with radial profile
   if ("sfM1" %in% groups){
      data$sfM1 = data$Aring * data$SFDsw
   }
   if ("sfM3" %in% groups){
      data$sfM3 = data$Cring * data$SFS * 1 / length(unique(data$position))
   }
   auc.data = data %>% 
      gather(., Method, sf_i, groups) %>% 
      mutate(Balance = ifelse(sf_i >= 0, "Positive", "Negative"),
             Method = ifelse(Method == "sfM1", "Method 1",
                                    ifelse(Method == "sfM2", "Method 2",
                                           "Method 3"))) %>%  
      mutate(Balance = factor(Balance,
                              levels = c("Positive", "Negative"))) %>%
      filter(complete.cases(.)) %>% 
      group_by(doy, Method, position) %>% 
      arrange(dTime) %>% 
      #distinct(auc = sum(diff(dTime) * (head(sf_i,-1)+tail(sf_i,-1)))/2) 
      mutate(auc = sum(diff(dTime) * (head(sf_i,-1)+tail(sf_i,-1)))/2) %>% 
      select(doy, Method, position, auc) %>% 
      unique(.) %>% data.frame(.)
   
   print(paste("AUC radial profile: ", nrow(auc.data)))
   N = length(unique(auc.data$position))
   
   p = auc.data %>% 
       ggplot(., aes(x = position, y = auc/1000, col = factor(position))) +
       geom_boxplot(fill = NA) +
       geom_jitter() +
       scale_color_manual(values = fillcolors(N)) +
       guides(col = F) +
       labs(x = "Thermometer position",
           y = expression(Tree~water~use~(kg~d^{-1}))) +
      facet_wrap(~Method, ncol = 3, scales = "fixed") 
   
   return(p)
}