
########################
# HYDRODYNAMICS TARGET #
########################

hydTargetOutput = function(){
  return(
    list(
      box(title = "Custom settings",
          width = "100%",
          collapsible = T,
          collapsed = T,
          status = "success",
          hyd.target.box.settings()),
      fluidRow(
        box(title = "Summary",
                   width = "5", height = "95%",
                   collapsible = T, status = "success",
                   hyd.target.box.text()),
        
        box(title = "Table",
                   width = "7", height = "95%",
                   collapsible = T, status = "success",
                   hyd.target.box.table())
      ),
      
      box(title = "Plots",
          width = "100%",
          collapsible = T, status = "success",
          hyd.target.box.figures())
    )
  )
}


hyd.target.box.settings = function(){
  return(
    list(
      uiOutput("hydro.window.target.show"),

      # Default values: gaps = 20, full = 20, part = 90, tilt = 75
      splitLayout(
        numericInput(inputId = "hydro.set.gaps.target",
                     label = HTML("<abbr title='Minimum gap in an inundation event to be closed, where points were misclassified as non-inundated (minutes)'>Minimum gap</abbr>"),
                     value = 20),
        numericInput(inputId = "hydro.set.part.target",
                     label = HTML("<abbr title='Time window to search for partially inundated cases at the start and end of inundation events (minutes).'>Partial inundation window</abbr>"),
                     value = 90)
        ),
      splitLayout(
        numericInput(inputId = "hydro.set.full.target",
                     label = HTML("<abbr title='Minimum duration of a fully inundated event, otherwise event is reclassified as partially inundated (minutes)'>Minimum full inundation</abbr>"),
                     value = 20),
        numericInput(inputId = "hydro.set.tilt.target",
                     label = HTML("<abbr title='Minimum tilt to classify an event as fully inundated, otherwise event is reclassified as partially inundated (degrees)'>Tilt full inundation</abbr>"),
                     value = 75)          
      ),
      # splitLayout(
      #   cellArgs = list(style = "padding-right: 12px; white-space: normal;"),
      #   
      #   tagList(
          actButton("hydro.set.apply.target", "Apply custom settings", "update"),
          actButton("hydro.set.reset.target", "Reset custom settings", "grey")
      #   )
      # )
    )
  )
}

hyd.target.box.text = function(){
  return(
    list(
      output.html("hydro.text.target")
    )
  )
}

hyd.target.box.table = function(){
  return(
    list(
      output.table("hydro.table.target"),
      actButton("hydro.table.target.save", "Download results", "saveCsv")
    )
  )
}

hyd.target.box.figures = function(){
  return(list(
    tabsetPanel(
      tabPanel("Daily inundation", br(),
               plotlyOutput("fig.inundation.target")),
               # actButton("save.fig.inundation.target",
               #           "Save figure", 
               #           "saveFigure")),
      tabPanel("Current velocity", br(),
               plotlyOutput("fig.velocity.target")),
               # actButton("save.fig.velocity.target",
               #           "Save figure", 
               #           "saveFigure")),
      tabPanel("Wave orbital velocity", br(),
               plotlyOutput("fig.wave.velocity.target")),
               # actButton("save.fig.wave.velocity.target",
               #           "Save figure", 
               #           "saveFigure"))
    ),
    actButton("save.figs.target",
              "Download plots", 
              "saveFigure")
    )
  )
}


###########################
# HYDRODYNAMICS REFERENCE #
###########################

hydReferenceOutput = function(){
  return(
    list(
      box(title = "Settings",
          width = "100%",
          collapsible = T,
          collapsed = T,
          status = "success",
          hyd.reference.box.settings()),
      fluidRow(
        box(title = "Summary",
                   width = 3, height = "95%",
                   collapsible = T, status = "success",
                   hyd.reference.box.text()),
        
        box(title = "Table",
                   width = 9, height = "95%",
                   collapsible = T, status = "success",
                   hyd.reference.box.table())
      ),
      
      box(title = "Plots",
          width = "100%",
          collapsible = T, status = "success",
          hyd.reference.box.figures())
    )
  )
  
}

hyd.reference.box.settings = function(){
  return(
    list(
      splitLayout(
        cellWidths = 300,
        checkboxInput( "hydro_set_cust_reference", "Custom settings", F),
        uiOutput("hydro.window.reference.show")
      ),
    conditionalPanel(
      condition = "input.hydro_set_cust_reference == true",
      # Default values: gaps = 20, full = 20, part = 90, tilt = 75
      splitLayout(
        numericInput(inputId = "hydro.set.gaps.reference",
                     label = HTML("<abbr title='Minimum gap in an inundation event to be closed, where points were misclassified as non-inundated (minutes)'>Minimum gap</abbr>"),
                     value = 20),
        numericInput(inputId = "hydro.set.part.reference",
                     label = HTML("<abbr title='Time window to search for partially inundated cases at the start and end of inundation events (minutes).'>Partial inundation window</abbr>"),
                     value = 90)
      ),
      splitLayout(
        numericInput(inputId = "hydro.set.full.reference",
                     label = HTML("<abbr title='Minimum duration of a fully inundated event, otherwise event is reclassified as partially inundated (minutes)'>Minimum full inundation</abbr>"),
                     value = 20),
        numericInput(inputId = "hydro.set.tilt.reference",
                     label = HTML("<abbr title='Minimum tilt to classify an event as fully inundated, otherwise event is reclassified as partially inundated (degrees)'>Tilt full inundation</abbr>"),
                     value = 75)
      ),
      splitLayout(
        actButton("hydro.set.apply.reference", "Apply custom settings", "update"),
        actButton("hydro.set.reset.reference", "Reset custom settings", "grey")
      )
      )
    )
  )
}

hyd.reference.box.text = function(){
  return(
    list(
      output.html("hydro.text.reference")
    )
  )
}

hyd.reference.box.table = function(){
  return(
    list(
      output.table("hydro.table.reference"),
      actButton("hydro.table.reference.save", "Save table", "saveCsv")
    )  )
}

hyd.reference.box.figures = function(){
  return(list(
    tabsetPanel(
      tabPanel("Daily inundation", br(),
               plotlyOutput("fig.inundation.reference"),
               actButton("save.fig.inundation.reference",
                         "Save figure", 
                         "saveFigure")),
      tabPanel("Current velocity", br(),
               plotlyOutput("fig.velocity.reference"),
               actButton("save.fig.velocity.reference",
                         "Save figure", 
                         "saveFigure")),
      tabPanel("Wave orbital velocity", br(),
               plotlyOutput("fig.wave.velocity.reference"),
               actButton("save.fig.wave.velocity.reference",
                         "Save figure", 
                         "saveFigure"))
    ))
  )
}



############################
# HYDRODYNAMICS COMPARISON #
############################

hydComparisonOutput = function(){
  return(
    list(
      fluidRow(
        box(title = "Summary",
                   width = 3, height = "95%",
                   collapsible = T, status = "success",
                   hyd.comparison.box.text()),
        
        box(title = "Table",
                   width = 9, height = "95%",
                   collapsible = T, status = "success",
                   hyd.comparison.box.table())
      ),
      
      box(title = "Select a plot",
          width = "100%",
          collapsible = T, status = "success",
          hyd.comparison.box.figures())
    )
  )
  
}


hyd.comparison.box.text = function(){
  return(
    p("TEXT")
  )
}

hyd.comparison.box.table = function(){
  return(
    list(
      output.table("comparison.table.target"),
      actButton("comparison.table.save", "Download results", "saveCsv")
    )
  )
}

hyd.comparison.box.figures = function(){
  return(list(
    tabsetPanel(
      tabPanel("Daily inundation", br(),
               plotlyOutput("fig.inundation.comparison")),
      
      tabPanel("Current velocity", br(),
               plotlyOutput("fig.velocity.comparison")),
      
      tabPanel("Parameters", br(),
               plotlyOutput("fig.parameter.comparison"))
    ),
    actButton("save.fig.comparison",
              "Download plots", 
              "saveFigure")
    ))
}