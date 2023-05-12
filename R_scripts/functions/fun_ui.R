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

actButton <- function(ID, label, type, class="btn btn-default"){
  if (type == "saveCsv"){
    return(actionButton(ID, label,
                        style = paste(buttonStyles("blue"), 
                                      "margin-bottom: 2rem", sep = ";"),
                        class=class,
                        icon("file-download")))
  }
  if (type == "saveFigure"){
    return(actionButton(ID, label,
                        style = paste(buttonStyles("blue"), 
                                      "margin-bottom: 2rem", sep = ";"),
                        class=class,
                        icon("file-download")))
  }
  if (type == "create"){
    return(actionButton(ID, label,
                        style = paste(buttonStyles("green"),
                                      "margin-bottom: 2rem", sep = ";"),
                        class=class,
                        icon("tag",style="margin-right:.5em")))
  }
  if (type == "update"){
    return(actionButton(ID, label,
                        style = buttonStyles("green"), 
                        class=class,
                        icon("broom")))
  }
  
}

buttonStyles = function(type = "blue"){
  if (type == "blue")
  { 
    return("color: #fff; background-color: #3c8dbc; border-color: #2e6da4; margin-bottom: 2rem; margin-top: 2rem")
  }
  if (type == "green")
  {
    return("color: #fff; background-color: #0F8B6E; border-color:  #175C4C; margin-bottom: 2rem; margin-top: 2rem")
  }
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