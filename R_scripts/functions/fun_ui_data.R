############
### DATA ###
############

### Upload ###
dataUplOutput = function(){
  return(
    fluidRow(
      column(6,
             box(title = "Target",
                 collapsible = T, width = "100%",
                 status = "warning",
                 box.dat_upl.upload.tar()
             )
      ),
      column(6,
             box(title = "Reference",
                 collapsible = T, width = "100%",
                 status = "warning",
                 box.dat_upl.upload.ref()
             )
      )
    ))
}

box.dat_upl.upload.tar = function() {
  return(list(
    # Input: Select a file ----
    fluidRow(column(
      4, selectInput(
        "inputType_T",
        "Select Mini Buoy design used",
        c(
          "Please select" = "empty",
          "B4" = "MB1",
          "B4+" = "MB2",
          "Pendant" = "MB3"
        )
      )
    ),
    column(
      6,
      offset = 2,
      checkboxInput("raw_default_T", "Use default data set", F)
    )),
    conditionalPanel(
      condition = "input.inputType_T != `empty` & input.raw_default_T == false",
      
      fileInput(
        "fileTarget",
        "Choose the target site data (.csv file)",
        buttonLabel = HTML(
          "<span
      class='btn btn-primary'
      style='margin: -8px -13px;
        position: relative;
        top: -2px;
        border-radius: 0;margin: -8px -13px;
         position: relative;
         top: -2px;
         border-radius: 0;'>
                         Browse...
                         </span>"
        ),
        multiple = F,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      )
    ),
    
    br(),
    h5(strong("Summary of the data")),
    output.table("raw.target.sum"),
    actButton("setData.T", "Use data", "create"),
    span(textOutput("TargetName"), style="color:white")
    
  ))
}

box.dat_upl.upload.ref = function(){
  return(list(
    # Input: Select a file ----
    fluidRow(column(
      4, selectInput(
        "inputType_R",
        "Select Mini Buoy design used",
        c(
          "Please select" = "empty",
          "B4" = "MB1",
          "B4+" = "MB2",
          "Pendant" = "MB3"
        )
      )
    ),
    column(
      6,
      offset = 2,
      checkboxInput("raw_default_R", "Use default data set", F)
    )),
    conditionalPanel(
      condition = "input.inputType_R != `empty` & input.raw_default_R == false",
      
      fileInput(
        "fileReference",
        "(Optional) Choose the reference site data (.csv file)",
        buttonLabel = HTML(
          "<span
      class='btn btn-primary'
      style='margin: -8px -13px;
        position: relative;
        top: -2px;
        border-radius: 0;margin: -8px -13px;
         position: relative;
         top: -2px;
         border-radius: 0;'>
                         Browse...
                         </span>"
        ),
        multiple = F,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      )
    ),

    br(),
    h5(strong("Summary of the data")),
    output.table("raw.reference.sum"),
    actButton("setData.R", "Use data", "create"),
    span(textOutput("ReferenceName"), style="color:white")
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

