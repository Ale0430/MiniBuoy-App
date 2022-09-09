###############
### GENERAL ###
###############

output.table = function(outputID){
  return(list(br(), 
              DT::dataTableOutput(outputID) %>% 
                withSpinner(color="#0dc5c1")))
}

output.figure = function(outputID){
  return(plotOutput(outputID) %>% withSpinner(color="#0dc5c1"))
}

#### STYLES ####

actButton <- function(ID, label, type){
  if (type == "saveCsv"){
    return(actionButton(ID, label,
                        style = paste(buttonStyles("blue"), "margin-bottom: 2rem", sep = ";"),
                        icon("file-download")))
  }
  if (type == "saveFigure"){
    return(actionButton(ID, label,
                        style = paste(buttonStyles("blue"), "margin-bottom: 2rem", sep = ";"),
                        icon("file-download")))
  }
  if (type == "setValue"){
    return(actionButton(ID, label,
                        style = paste(buttonStyles("red"), "margin-bottom: 2rem", sep = ";"),
                        icon("check-circle")))
  }
  if (type == "create"){
    return(actionButton(ID, label,
                        style = paste(buttonStyles("leafgreen"), "margin-bottom: 2rem",
                                      sep = ";"),
                        icon("tag",style="margin-right:.5em")))
  }
  if (type == "update"){
    return(actionButton(ID, label,
                        style = buttonStyles("green"), 
                        icon("broom")))
  }
  
}

buttonStyles = function(type = "blue"){
  if (type == "blue")
  { ##14B3EE#337ab7
    return("color: #fff; background-color: #14B3EE; border-color: #2e6da4; margin-bottom: 2rem; margin-top: 2rem")
  }
  if (type == "leafgreen")
  {#orange: #F07221 #red:cc0000 (#0F8B6E - Aqua fill, darker border #175C4C)
    return("color: #fff; background-color: #0F8B6E; border-color:  #175C4C; margin-bottom: 2rem; margin-top: 2rem")
  }
  if (type == "green")
  {
    return("color: #fff; background-color: #42C728; border-color: #38A822; margin-bottom: 2rem; margin-top: 2rem")
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