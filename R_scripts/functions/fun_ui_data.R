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
    checkboxInput("raw_default_T", "Use default data set", F),
    conditionalPanel(
      condition = "input.raw_default_T == false",
      br(),
      # Input: Select a file ----
      selectInput(
          "inputType_T",
          "Select Mini Buoy design used",
          c(
            "Please select" = "empty",
            "B4" = "B4",
            "B4+" = "B4+",
            "Pendant" = "Pendant"
          )
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
    checkboxInput("raw_default_R", "Use default data set", F),
    conditionalPanel(
      condition = "input.raw_default_R == false",
      br(),
      # Input: Select a file ----
      selectInput(
        "inputType_R",
        "Select Mini Buoy design used",
        c(
          "Please select" = "empty",
          "B4" = "B4",
          "B4+" = "B4+",
          "Pendant" = "Pendant"
        )
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
      column(6, box(title = "Target",
                    collapsible = T,  width = "100%",
                    status = "warning",
                    actButton("LoadFilter.T", "Open/ reload filter options", "update"),
                    uiOutput("filterOptions.T")
      )),
      column(6, box(title = "Reference",
                    collapsible = T,  width = "100%",
                    status = "warning",
                    actButton("LoadFilter.R", "Open/ reload filter options", "update"),
                    uiOutput("filterOptions.R")
      ))
    ),
    fluidRow(
      column(12, box(title = "Check the raw acceleration data",
                     collapsible = T, width = "100%",
                     status = "success",
                     box.filter.figures()))
    ))
  )
}


box.filter.figures = function(){
  return(list(
    
    fluidRow(
      column(3, selectInput(
        "filterPlot_DataSet",
        "View data",
        choices = c("Target" = "TARGET",
                    "Reference" = "REFERENCE"))),
      column(4,
             radioButtons(
               "filterPlot_type",
               "Diagram type",
               inline = T,
               choices = c(
                 "Line plot" = "line",
                 "Scatter plot" = "scatter", 
                 "Histogram" = "hist"
               )), 
             offset = 1),
      column(4,
             conditionalPanel(
               condition = "input.filterPlot_type == `hist`",
               numericInput("filterPlot_bins", "Number of bins for histogram", value = 100)
             ),
             conditionalPanel(
               condition = "input.filterPlot_type != `hist`",
               selectInput("filterPlot_window", "Aggregation window",
                           choices = c("hours" = "hours",
                                       "30 minutes" = "30 minutes",
                                       "20 minutes" = "20 minutes",
                                       "10 minutes" = "10 minutes",
                                       "minutes" = "minutes"))

             ))
    ),

    fluidRow(
      column(2, actButton("filterPlot_renderPlot", "Render figure", "update")),
      column(2, actButton("save_dat_filter", "Save filtered data", "saveCsv")),
      column(1, actButton("save_dat_filter_fig", "Save figure", "saveFigure"))
    ),
    conditionalPanel(
      condition = "input.filterPlot_renderPlot != 0",
      output.figure("filterPlot")
    )
  ))
}

