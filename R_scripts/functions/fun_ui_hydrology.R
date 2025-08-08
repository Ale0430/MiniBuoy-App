
#################
# HYDRODYNAMICS #
#################

# Target    ####
## Structure     ####
hydTargetOutput = function(){
  return(
    list(
      box(title = "Default settings",
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
      
      box(title = "Summary (for coastal wetlands only)",
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

      # Default values: tilt = 75, limit = 9, slope = 0.01, adj_tilt = -1
      splitLayout(
        numericInput(inputId = "hydro.set.tilt.target",
                     label = HTML("<abbr title='Tilt value to consider full inundation'>Min. Tilt for full inundation</abbr>"),
                     value = 75, min = 0, max = 90),
        numericInput(inputId = "hydro.set.slope.target",
                     label = HTML("<abbr title='Max. slope between points to consider non-inundation events (all > slope='F')'>Slope (lower is stricter)</abbr>"),
                     value = 0.01, min = 0.000000, max = 0.7)
        ),
      splitLayout(
        numericInput(inputId = "hydro.set.limit.target",
                     label = HTML("<abbr title='Tilt threshold to identify non-inundation Events'>Tilt limit for emerssion events (degrees)</abbr>"),
                     value = 9),
        numericInput(inputId = "hydro.set.adj_tilt.target",
                     label = HTML("<abbr title='Negative value that fine-tunes non-inundation events, typically between -1 and -5'>Adjusted tilt (negative value)</abbr>"),
                     value = -1, min = -55, max = 0)          
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
               plotOutput("fig.stage.target")),
      tabPanel("Windows of Opportunity", br(),
               plotOutput("fig.woo.target"))
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
        title = "Default settings",
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
        title = "Summary (for coastal wetlands only)",
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
      
      # Default values: tilt = 75, limit = 9, slope = 0.01, adj_tilt = -1
      splitLayout(
        numericInput(inputId = "hydro.set.tilt.reference",
                     label = HTML("<abbr title='Tilt value to consider full inundation'>Min. Tilt for full inundation</abbr>"),
                     value = 75, min = 0, max = 90),
        numericInput(inputId = "hydro.set.slope.reference",
                     label = HTML("<abbr title='Max. slope between points to consider non-inundation events (all > slope='F')'>Slope (lower is stricter)</abbr>"),
                     value = 0.01, min=0.000000, max =0.7)
      ),
      splitLayout(
        numericInput(inputId = "hydro.set.limit.reference",
                     label = HTML("<abbr title='Tilt threshold to identify non-inundation Events'>Tilt limit for emerssion events (degrees)</abbr>"),
                     value = 9, min = 1, max = 87),
        numericInput(inputId = "hydro.set.adj_tilt.reference",
                     label = HTML("<abbr title='Negative value that fine-tunes non-inundation events, typically between -1 and -5'>Adjusted tilt (negative value)</abbr>"),
                     value = -1, min =-55, max=0)          
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
      actButton("hydro.table.reference.save", "Download results", "saveCsv")
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
               plotOutput("fig.stage.reference")),
      tabPanel("Windows of Opportunity", br(),
               plotOutput("fig.woo.reference"))
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
      
      box(title = "Summary (for coastal wetlands only)",
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
      output.table("hydro.table.comparison"),
      actButton("hydro.table.comparison.save", "Download results", "saveCsv")
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
               plotlyOutput("fig.waves.comparison")),
      tabPanel("Comparison of events", br(),
               plotOutput("fig.parameters.comparison"))
    ),
    actButton("save.fig.comparison",
              "Download plots", 
              "saveFigure")
    ))
}