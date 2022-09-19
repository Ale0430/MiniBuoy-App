
########################
# HYDRODYNAMICS TARGET #
########################

hydTargetOutput = function(){
  return(
    list(
      fluidRow(
        box(title = "Results",
                   width = "3", height = "95%",
                   collapsible = T, status = "success",
                   hyd.target.box.text()),
        
        box(title = "Summary table",
                   width = "9", height = "95%",
                   collapsible = T, status = "success",
                   hyd.target.box.table())
      ),
      
      box(title = "Select a plot",
          width = "100%",
          collapsible = T, status = "success",
          hyd.target.box.figures()),
      
      box(title = "Disclaimer",
          width = "100%",
          collapsible = T, status = "info",
          p("Load md. file containing disclaimer. @Cai: please prepare such file and name it 'disclaimer.md' (store it it man)."))
    )
  )
  
}


hyd.target.box.text = function(){
  return(
    list(
      uiOutput("hydro.window.target.show"),
      output.html("hydro.text.target")
    )
  )
}

hyd.target.box.table = function(){
  return(
    list(
      output.table("hydro.table.target"),
      actButton("hydro.table.target.save", "Save table", "saveCsv")
    )
  )
}

hyd.target.box.figures = function(){
  return(list(
    tabsetPanel(
      tabPanel("Daily inundation", br(),
               output.figure("fig.inundation.target"),
               actButton("save.fig.inundation.target",
                         "Save figure", 
                         "saveFigure")),
      tabPanel("Current velocity", br(),
               output.figure("fig.velocity.target"),
               actButton("save.fig.velocity.target",
                         "Save figure", 
                         "saveFigure")),
      tabPanel("Wave orbital velocity", br(),
               output.figure("fig.wave.velocity.target"),
               actButton("save.fig.wave.velocity.target",
                         "Save figure", 
                         "saveFigure"))
    ))
  )
}


###########################
# HYDRODYNAMICS REFERENCE #
###########################

hydReferenceOutput = function(){
  return(
    list(
      fluidRow(
        box(title = "Results",
                   width = 3, height = "95%",
                   collapsible = T, status = "success",
                   hyd.reference.box.text()),
        
        box(title = "Summary table",
                   width = 9, height = "95%",
                   collapsible = T, status = "success",
                   hyd.reference.box.table())
      ),
      
      box(title = "Select a plot",
          width = "100%",
          collapsible = T, status = "success",
          hyd.reference.box.figures()),
      
      box(title = "Disclaimer",
          width = "100%",
          collapsible = T, status = "info",
          p("Load md. file containing disclaimer. @Cai: please prepare such file and name it 'disclaimer.md' (store it it man)."))
    )
  )
  
}


hyd.reference.box.text = function(){
  return(
    list(
      uiOutput("hydro.window.reference.show"),
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
               output.figure("fig.inundation.reference"),
               actButton("save.fig.inundation.reference",
                         "Save figure", 
                         "saveFigure")),
      tabPanel("Current velocity", br(),
               output.figure("fig.velocity.reference"),
               actButton("save.fig.velocity.reference",
                         "Save figure", 
                         "saveFigure")),
      tabPanel("Wave orbital velocity", br(),
               output.figure("fig.wave.velocity.reference"),
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
        box(title = "Results",
                   width = 3, height = "95%",
                   collapsible = T, status = "success",
                   hyd.comparison.box.text()),
        
        box(title = "Summary table",
                   width = 9, height = "95%",
                   collapsible = T, status = "success",
                   hyd.comparison.box.table())
      ),
      
      box(title = "Select a plot",
          width = "100%",
          collapsible = T, status = "success",
          hyd.comparison.box.figures()),
      
      box(title = "Disclaimer",
          width = "100%",
          collapsible = T, status = "info",
          p("Load md. file containing disclaimer. @Cai: please prepare such file and name it 'disclaimer.md' (store it it man)."))
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
      actButton("comparison.table.save", "Save table", "saveCsv")
    )
  )
}

hyd.comparison.box.figures = function(){
  return(list(
    tabsetPanel(
      tabPanel("Daily inundation", br(),
               output.figure("fig.inundation.comparison"),
               actButton("save.fig.inundation.comparison",
                         "Save figure", 
                         "saveFigure")),
      
      tabPanel("Current velocity", br(),
               output.figure("fig.velocity.comparison"),
               actButton("save.fig.velocity.comparison",
                         "Save figure", 
                         "saveFigure"))
    ))
  )
}