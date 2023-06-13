
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

## Settings     ####
hyd.target.box.settings = function(){
  return(
    list(
      uiOutput("hydro.window.target.show"),

      # Default values: gaps = 20, full = 20, part = 90, tilt = 75, chop = 50
      splitLayout(
        numericInput(inputId = "hydro.set.gaps.target",
                     label = HTML("<abbr title='Minimum gap in an inundation event to be closed, where points were misclassified as non-inundated (minutes)'>Minimum gap</abbr>"),
                     value = 20),
        numericInput(inputId = "hydro.set.part.target",
                     label = HTML("<abbr title='Time window to search for partially inundated cases at the start and end of inundation events (minutes).'>Search window</abbr>"),
                     value = 90)
        ),
      splitLayout(
        numericInput(inputId = "hydro.set.full.target",
                     label = HTML("<abbr title='Minimum duration of a fully inundated event, otherwise event is reclassified as partially inundated (minutes)'>Minimum duration</abbr>"),
                     value = 20),
        numericInput(inputId = "hydro.set.tilt.target",
                     label = HTML("<abbr title='Minimum tilt to classify an event as fully inundated, otherwise event is reclassified as partially inundated (degrees)'>Minimun tilt</abbr>"),
                     value = 75)          
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
      tabPanel("Velocity stage plot", br(),
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
      box(title = "Custom settings",
          width = "100%",
          collapsible = T,
          collapsed = T,
          status = "success",
          hyd.reference.box.settings()),
      fluidRow(
        box(title = "Summary",
                   width = 5, height = "95%",
                   collapsible = T, status = "success",
                   hyd.reference.box.text()),
        
        box(title = "Table",
                   width = 7, height = "95%",
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

## Settings     ####
hyd.reference.box.settings = function(){
  return(
    list(
      uiOutput("hydro.window.reference.show"),
      
      # Default values: gaps = 20, full = 20, part = 90, tilt = 75, chop = 50
      splitLayout(
        numericInput(inputId = "hydro.set.gaps.reference",
                     label = HTML("<abbr title='Minimum gap in an inundation event to be closed, where points were misclassified as non-inundated (minutes)'>Minimum gap</abbr>"),
                     value = 20),
        numericInput(inputId = "hydro.set.part.reference",
                     label = HTML("<abbr title='Time window to search for partially inundated cases at the start and end of inundation events (minutes).'>Search window</abbr>"),
                     value = 90)
      ),
      splitLayout(
        numericInput(inputId = "hydro.set.full.reference",
                     label = HTML("<abbr title='Minimum duration of a fully inundated event, otherwise event is reclassified as partially inundated (minutes)'>Minimum duration</abbr>"),
                     value = 20),
        numericInput(inputId = "hydro.set.tilt.reference",
                     label = HTML("<abbr title='Minimum tilt to classify an event as fully inundated, otherwise event is reclassified as partially inundated (degrees)'>Minimun tilt</abbr>"),
                     value = 75)
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
      tabPanel("Velocity stage plot", br(),
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
               plotlyOutput("fig.velocity.comparison"))
    ),
    actButton("save.fig.comparison",
              "Download plots", 
              "saveFigure")
    ))
}