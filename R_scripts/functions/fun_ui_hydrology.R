
#################
# HYDRODYNAMICS #
#################

# Target    ####
## Structure     ####
hydTargetOutput = function(){
  return(
    list(
      box(title = "Custom settings",
          width = "100%",
          collapsible = T,
          collapsed = T,
          status = "success",
          hyd.target.box.settings()),
      
      box(title = "Table",
          width = "100%",
          collapsible = T, status = "success",
          hyd.target.box.table()),
      
      box(title = "Plots",
          width = "100%",
          collapsible = T, status = "success",
          hyd.target.box.figures()),
      
      box(title = "Summary for coastal wetlands",
          width = "100%",
          collapsed = T,
          collapsible = T, status = "success",
          hyd.target.box.text())
      
    )
  )
}

## Settings     ####
hyd.target.box.settings = function(){
  return(
    list(
      uiOutput("hydro.window.target.show"),

      # Default values: gaps = 60, full = 60, part = 25, tilt = 75
      splitLayout(
        numericInput(inputId = "hydro.set.gaps.target",
                     label = HTML("<abbr title='Minimum gap in an inundation event to be closed, where points were misclassified as non-inundated'>Minimum gap (minutes)</abbr>"),
                     value = 60),
        numericInput(inputId = "hydro.set.part.target",
                     label = HTML("<abbr title='Proportion of the start and end of inundation events to Window to search for partially inundated cases'>Search window (%)</abbr>"),
                     value = 25)
        ),
      splitLayout(
        numericInput(inputId = "hydro.set.full.target",
                     label = HTML("<abbr title='Minimum duration of a fully inundated event, otherwise event is reclassified as partially inundated'>Minimum duration (minutes)</abbr>"),
                     value = 60),
        numericInput(inputId = "hydro.set.tilt.target",
                     label = HTML("<abbr title='Minimum tilt to classify an event as fully inundated, otherwise event is reclassified as partially inundated'>Minimun tilt (degrees)</abbr>"),
                     value = 75, min = 0, max = 90)          
      ),

      actButton("hydro.set.apply.target", "Apply custom settings", "update"),
      actButton("hydro.set.reset.target", "Reset custom settings", "grey")

    )
  )
}

## Text     ####
hyd.target.box.text = function(){
  return(
    list(
      output.html("hydro.text.target")
    )
  )
}

## Table     ####
hyd.target.box.table = function(){
  return(
    list(
      output.table("hydro.table.target"),
      actButton("hydro.table.target.save", "Download results", "saveCsv")
    )
  )
}

## Plots     ####
hyd.target.box.figures = function(){
  return(list(
    tabsetPanel(
      tabPanel("Raw data", br(),
               plotlyOutput("fig.control.target")),
      tabPanel("Daily inundation", br(),
               plotlyOutput("fig.inundation.target")),
      tabPanel("Current velocity", br(),
               plotlyOutput("fig.velocity.target")),
      tabPanel("Wave orbital velocity", br(),
               plotlyOutput("fig.wave.velocity.target")),
      tabPanel("Ebb-flood comparison", br(),
               plotOutput("fig.stage.target"))
    ),
    actButton("save.figs.target",
              "Download plots", 
              "saveFigure")
    )
  )
}


# Reference    ####
## Structure     ####
hydReferenceOutput = function(){
  return(
    list(
      
      box(
        title = "Custom settings",
        width = "100%",
        collapsible = T,
        collapsed = T,
        status = "success",
        hyd.reference.box.settings()
      ),
      
      
      box(
        title = "Table",
        width = "100%",
        collapsible = T,
        status = "success",
        hyd.reference.box.table()
      ),
      
      box(
        title = "Plots",
        width = "100%",
        collapsible = T,
        status = "success",
        hyd.reference.box.figures()
      ),
      
      box(
        title = "Summary for coastal wetlands",
        width = "100%",
        collapsed = T,
        collapsible = T,
        status = "success",
        hyd.reference.box.text()
      )
    )
  )
  
}

## Settings     ####
hyd.reference.box.settings = function(){
  return(
    list(
      uiOutput("hydro.window.reference.show"),
      
      # Default values: gaps = 60, full = 60, part = 50, tilt = 75
      splitLayout(
        numericInput(inputId = "hydro.set.gaps.reference",
                     label = HTML("<abbr title='Minimum gap in an inundation event to be closed, where points were misclassified as non-inundated'>Minimum gap (minutes)</abbr>"),
                     value = 60),
        numericInput(inputId = "hydro.set.part.reference",
                     label = HTML("<abbr title='Proportion of the start and end of inundation events to Window to search for partially inundated cases'>Search window (%)</abbr>"),
                     value = 25)
      ),
      splitLayout(
        numericInput(inputId = "hydro.set.full.reference",
                     label = HTML("<abbr title='Minimum duration of a fully inundated event, otherwise event is reclassified as partially inundated'>Minimum duration (minutes)</abbr>"),
                     value = 60),
        numericInput(inputId = "hydro.set.tilt.reference",
                     label = HTML("<abbr title='Minimum tilt to classify an event as fully inundated, otherwise event is reclassified as partially inundated'>Minimun tilt (degrees)</abbr>"),
                     value = 75, min = 0, max = 90)          
      ),
      
      actButton("hydro.set.apply.reference", "Apply custom settings", "update"),
      actButton("hydro.set.reset.reference", "Reset custom settings", "grey")
    )
  )
}

## Text     ####
hyd.reference.box.text = function(){
  return(
    list(
      output.html("hydro.text.reference")
    )
  )
}

## Table     ####
hyd.reference.box.table = function(){
  return(
    list(
      output.table("hydro.table.reference"),
      actButton("hydro.table.reference.save", "Save table", "saveCsv")
    )  )
}

## Plots     ####
hyd.reference.box.figures = function(){
  return(list(
    tabsetPanel(
      tabPanel("Raw data", br(),
               plotlyOutput("fig.control.reference")),
      tabPanel("Daily inundation", br(),
               plotlyOutput("fig.inundation.reference")),
      tabPanel("Current velocity", br(),
               plotlyOutput("fig.velocity.reference")),
      tabPanel("Wave orbital velocity", br(),
               plotlyOutput("fig.wave.velocity.reference")),
      tabPanel("Ebb-flood comparison", br(),
               plotOutput("fig.stage.reference"))
    ),
    actButton("save.figs.reference",
              "Download plots", 
              "saveFigure")
  ))
}


# Comparison     ####
## Structure     ####
hydComparisonOutput = function(){
  return(
    list(
      box(title = "Table",
          width = "100%",
          # height = "95%",
          collapsible = T, status = "success",
          hyd.comparison.box.table()),
      
      box(title = "Select a plot",
          width = "100%",
          collapsible = T, status = "success",
          hyd.comparison.box.figures()),
      
      box(title = "Summary for coastal wetlands",
          width = "100%",
          collapsed = T,
          collapsible = T, status = "success",
          hyd.comparison.box.text())
    )
  )
  
}


## Text     ####
hyd.comparison.box.text = function(){
  return(
    output.html('hydro.text.comparison')
  )
}

## Table     ####
hyd.comparison.box.table = function(){
  return(
    list(
      htmlOutput('hydro.text.table'),  # original shiny function to avoid 2nd spinner
      output.table("comparison.table.target"),
      actButton("comparison.table.save", "Download results", "saveCsv")
    )
  )
}

## Plots     ####
hyd.comparison.box.figures = function(){
  return(list(
    tabsetPanel(
      tabPanel("Daily inundation", br(),
               plotlyOutput("fig.inundation.comparison")),
      tabPanel("Current velocity", br(),
               plotlyOutput("fig.currents.comparison")),
      tabPanel("Wave orbital velocity", br(),
               plotlyOutput("fig.waves.comparison"))
    ),
    actButton("save.fig.comparison",
              "Download plots", 
              "saveFigure")
    ))
}