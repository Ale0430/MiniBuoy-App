
########################
# HYDRODYNAMICS TARGET #
########################

hydTargetOutput = function(){
  return(
    list(
      fluidRow(
        column(6, 
               box(title = "Results ....",
                   width = "100%",
                   collapsible = T, status = "success",
                   hyd.target.box.text())),
        
        column(6,
               box(title = "Summary table",
                   width = "100%",
                   collapsible = T, status = "success",
                   hyd.target.box.table()))
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
    p("TEXT")
  )
}

hyd.target.box.table = function(){
  return(
    p("TABLE")
  )
}

hyd.target.box.figures = function(){
  return(
    p("FIGURES in tabs")
  )
}


###########################
# HYDRODYNAMICS REFERENCE #
###########################

hydReferenceOutput = function(){
  return(
    list(
      fluidRow(
        column(6, 
               box(title = "Results ....",
                   width = "100%",
                   collapsible = T, status = "success",
                   hyd.reference.box.text())),
        
        column(6,
               box(title = "Summary table",
                   width = "100%",
                   collapsible = T, status = "success",
                   hyd.reference.box.table()))
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
    p("TEXT")
  )
}

hyd.reference.box.table = function(){
  return(
    p("TABLE")
  )
}

hyd.reference.box.figures = function(){
  return(
    p("FIGURES in tabs")
  )
}



############################
# HYDRODYNAMICS COMPARISON #
############################

hydComparisonOutput = function(){
  return(
    list(
      fluidRow(
        column(6, 
               box(title = "Results ....",
                   width = "100%",
                   collapsible = T, status = "success",
                   hyd.comparison.box.text())),
        
        column(6,
               box(title = "Summary table",
                   width = "100%",
                   collapsible = T, status = "success",
                   hyd.comparison.box.table()))
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
    p("TABLE")
  )
}

hyd.comparison.box.figures = function(){
  return(
    p("FIGURES in tabs")
  )
}