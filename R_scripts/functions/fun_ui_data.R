############
### DATA ###
############

### Upload ###
dataUplOutput = function(){
  return(
    fluidRow(
      column(6,
             box(title = "Upload TARGET file",
                 collapsible = T, width = "100%",
                 status = "warning",
                 box.dat_upl.upload.tar()
             )
      ),
      column(6,
             box(title = "Upload REFERENCE file",
                 collapsible = T, width = "100%",
                 status = "warning",
                 box.dat_upl.upload.ref()
             )
      )
    ))
}

box.dat_upl.upload.tar = function(){
  return(list(
    # Input: Select a file ----
    fileInput("fileTarget", "Choose Target site CSV file",
              buttonLabel = HTML("<span 
                class='btn btn-primary' 
                style='margin: -8px -13px;
                  position: relative;
                  top: -2px;
                  border-radius: 0;margin: -8px -13px;
                   position: relative;
                   top: -2px;
                   border-radius: 0;'>
                                   Browse...
                                   </span>"),
              multiple = F,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    fluidRow(
      column(4,  selectInput("sep.T", "Separator", 
                             choices = c("Comma" = ",",
                                         "Semicolon" = ";",
                                         "Tab" = "\t"))),
      column(4, selectInput("inputType.T", "Input file type",
                  c("Mini Buoy 1" = "MB1", 
                    "Mini Buoy 2" = "MB2",
                    "Mini Buoy 3" = "MB3"))),
      column(4, numericInput("skip.T", "Skip:", min = 0, max = 100, 1))
    ),
    
    h5(strong("Summary data set")),
    output.table("raw.target.sum"),
    actButton("setData.T", "Use data", "create")
   
  ))
}

box.dat_upl.upload.ref = function(){
  return(list(
    # Input: Select a file ----
    fileInput("fileReference", "Choose Target site CSV file",
              buttonLabel = HTML("<span 
                class='btn btn-primary' 
                style='margin: -8px -13px;
                  position: relative;
                  top: -2px;
                  border-radius: 0;margin: -8px -13px;
                   position: relative;
                   top: -2px;
                   border-radius: 0;'>
                                   Browse...
                                   </span>"),
              multiple = F,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    fluidRow(
      column(4,  selectInput("sep.R", "Separator", 
                             choices = c("Comma" = ",",
                                         "Semicolon" = ";",
                                         "Tab" = "\t"))),
      column(4, selectInput("inputType.R", "Input file type",
                            c("Mini Buoy 1" = "MB1", 
                              "Mini Buoy 2" = "MB2",
                              "Mini Buoy 3" = "MB3"))),
      column(4, numericInput("skip.R", "Skip:", min = 0, max = 100, 1))
    ),
    
    h5(strong("Summary data set")),
    output.table("raw.reference.sum"),
    actButton("setData.R", "Use data", "create")
    
  ))
}




### Filter ###

dataFilterOutput = function(){
  return(list(
    fluidRow(
      column(6, box(title = "Deployment time TARGET",
                    collapsible = T,  width = "100%",
                    status = "warning",
                    actButton("LoadFilter.T", "Load filter options", "update"),
                    uiOutput("filterOptions.T")
      )),
      column(6, box(title = "Deployment time REFERENCE",
                    collapsible = T,  width = "100%",
                    status = "warning",
                    actButton("LoadFilter.R", "Load filter options", "update"),
                    uiOutput("filterOptions.R")
      ))
    ),
    fluidRow(
      column(12, box(title = "Raw data figures",
                     collapsible = T, width = "100%",
                     status = "success",
                     box.filter.figures(),
                     actButton("save_dat_filter", "Save csv", "saveCsv"),
                     actButton("save_dat_filter_fig", "Save figure", "saveFigure")))
    ))
  )
}


box.filter.figures = function(){
  return(list(
    
    fluidRow(
      column(3, selectInput(
        "filterPlot_DataSet",
        "View data set",
        choices = c("Target" = "Target",
                    "Reference" = "Reference")
      )),
      column(4,
             radioButtons(
               "filterPlot_type",
               "Diagram type",
               inline = T,
               choices = c(
                 "Histogram" = "hist",
                 "Line plot" = "line",
                 "Scatter plot" = "scatter"
               )), 
             offset = 1
    )), 
    conditionalPanel(
      condition = "input.filterPlot_type == `hist`",
      fluidRow(
        column(3, numericInput("filterPlot_bins", "No. Bins", value = 100))
        ),
    ),
    conditionalPanel(
      condition = "input.filterPlot_type == `line`",
      fluidRow(
        column(3, checkboxInput("filterPlot_rollmean", "Apply rolling average (only line plot)", T)),
        column(3, numericInput("filterPlot_rollmean_steps", "Width rolling window", 10))
      )
    ), 

    actButton("filterPlot_renderPlot", "Render figure", "update"),
    
    output.figure("filterPlot")
  ))
}

