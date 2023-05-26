###############
### GENERAL ###
###############

# Layout blue (boxes, spinner): #3c8dbc
# Button blue: #6CC3F5
# Button green: #57DEA1

output.table = function(outputID){
  return(list(br(), 
              DT::dataTableOutput(outputID) %>% 
                withSpinner(color="#3c8dbc", proxy.height = 150)))
}

output.figure = function(outputID){
  return(plotOutput(outputID) %>% withSpinner(color="#3c8dbc", proxy.height = 200))
}

output.html = function(outputID){
  return(htmlOutput(outputID) %>% withSpinner(color="#3c8dbc", proxy.height = 50))
}

#### STYLES ####

actButton <- function(ID, label, type, class="btn btn-default", addStyling = ""){
  if (type == "saveCsv" | type == "saveFigure"){
    return(actionButton(ID, label,
                        style = buttonStyles("blue", addStyling), 
                        class=class,
                        icon("file-download", style="margin-right:.5em")))
  }
  if (type == "create" | type == "update"){
    return(actionButton(ID, label,
                        style = buttonStyles("green", addStyling), 
                        class=class,
                        icon("tag", style="margin-right:.5em")))
  }
  if (type == "grey"){
    return(actionButton(ID, label,
                        style = buttonStyles("grey",  addStyling), 
                        class=class,
                        icon("broom", style="margin-right:.5em")))
  }
  
}

buttonStyles = function(type = "blue", addStyling = ""){
  cc = "#444"         # font color
  bckc = ""           # background color
  bordc = ""          # border color
  if (type == "blue")
  { 
    cc = "#fff"
    bckc = "#3c8dbc"
    bordc = "#2e6da4"
  }
  if (type == "green")
  {
    cc = "#fff"
    bckc = "#0F8B6E"
    bordc = "#175C4C"
  } 
  return(paste("color: ", cc, "; background-color: ", bckc, "; border-color:  ", bordc, "; margin-bottom: 2rem; margin-top: 2rem; margin-right: 2rem;",
               addStyling, sep = " "))
}

numericInputRow <- function(inputId, label, value = ""){
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value, class="input-small"))
}



#### MENU ####

menuItemOutput = function() {
  return(list(
    menuItem("About", tabName="about", icon = icon("th")),
    menuItem("Settings", tabName = "sett", icon = icon("gear")),
    menuItem("Data", tabName = "data", icon = icon("circle-notch"),
             menuSubItem("Upload", tabName = "dat_upl", icon= icon("upload")),
             menuSubItem("Filter", tabName = "dat_filter", icon = icon("filter"))),
    menuItem("Hydrodynamics", tabName = "hyd_res", icon = icon("water"),
             menuSubItem("Target", tabName = "hyd_target", icon= icon("bullseye")),
             menuSubItem("Reference", tabName = "hyd_reference", icon= icon("asterisk")),
             menuSubItem("Comparison", tabName = "hyd_comparison", icon= icon("not-equal"))
             )
  ))
}