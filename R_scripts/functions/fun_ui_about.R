
#### ABOUT ####

introOutput = function(){
  fluidRow(
    column(6, 
           box(title = "The Mini Buoy" ,
               status = "info", solidHeader = F, width = "100%",
               collapsible = T,
               includeMarkdown("./man/MinibuoyIntro.Rmd")),
           box(title = "The Mini Buoy App",
               status = "info", solideHeader = F, width ="100%",
               collapsible = T,
               includeMarkdown("./man/AppDescription.Rmd"),
               img(src='MiniBuoyMotion.png', width = "100%"),
               ),
          
           box(title = "Application Outputs",
               status = "info", solidHeader = F, width="100%",
               collapsible = T,
               includeMarkdown("./man/AppOutputDescription.Rmd"))
           
           
    ),
    column(6,
           
          
           box(title = "Mini Buoy User Manual",
               status = "info", solidHeader = F, width="100%",
               collapsible = T,
               includeMarkdown("./man/Manual.Rmd")),
           box(title = "Mini Buoy Models",
               status = "warning", solidHeader = F,
               collapsible = T, width = "100%",
               
               box.settings_sensors())
           )
  )
}


box.settings_sensors = function(){
  return(list(
    selectInput("sensorType", "Select sensor type to view attributes",
                choices = c("Mini Buoy 1", "Mini Buoy 2", "Mini Buoy 3"),
                selected = "Mini Buoy 1"),
    
    conditionalPanel(
      condition = "input.sensorType == 'Mini Buoy 1'",
      
      
      
      br(),
      p(strong("Schematic representation of Mini Buoy 1 (aka: original) [add here a brief description of MB1 attributes]")),
      img(src='MiniBuoy1.jpeg', width = "80%")),
    
    conditionalPanel(
      condition = "input.sensorType == 'Mini Buoy 2'",
      
      
      
      br(),
      p(strong("Schematic representation of Mini Buoy 2 (aka: black) [add here a brief description of MB2 attributes]")),
      img(src='ChocoLAb.png', width = "80%")),
    
    conditionalPanel(
      condition = "input.sensorType == 'Mini Buoy 3'",
      
      
      
      br(),
      p(strong("Schematic representation of Mini Buoy 3 (aka: Hobo) [add here a brief description of MB3 attributes]")),
      img(src='Golden.jpeg', width = "80%")),
    
    br(),br()
    
  ))
}

