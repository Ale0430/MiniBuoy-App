#### DATA ####

# Upload ####
## Main ####
dataUplOutput = function(){
  return(
    fluidRow(
      # tags$style(
      #   ".box-header {
      #         padding-bottom: 0;
      #       }
      #   .box-body {
      #         padding-top:0;
      #       }"
      # ),
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

## Target ####
box.dat_upl.upload.tar = function() {
  return(list(
    h5(strong("Select a Mini Buoy design or use the default data")),
    tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              "))),   
    splitLayout(cellWidths = c("50%", "50%"), 
                cellArgs = list(style = "margin-right: 12px; white-space: normal;"),
             
                selectInput(
                  "inputType_T",
                  label = NULL, # "Select Mini Buoy design used",
                  choices = c(
                    "Select design" = "empty",
                    "B4" = "B4",
                    "B4+" = "B4+",
                    "Pendant" = "Pendant"
                  )
                ),
                checkboxInput("raw_default_T", "Use default B4+ data", F)),


    conditionalPanel(
      condition = "input.inputType_T != `empty` & input.raw_default_T == false",
      
      fileInput(
        "fileTarget",
        "Choose target data",
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
    uiOutput("previewTarget"),
    span(textOutput("TargetName"), style="color:white")
  ))
}

## Reference ####
box.dat_upl.upload.ref = function() {
  return(list(
    h5(strong("Select a Mini Buoy design or use the default data")),
    tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              "))),
    splitLayout(cellWidths = c("50%", "50%"),
                cellArgs = list(style = "margin-right: 12px; white-space: normal;"),

                selectInput(
                  "inputType_R",
                  label = NULL, # "Select Mini Buoy design used",
                  choices = c(
                    "Select design" = "empty",
                    "B4" = "B4",
                    "B4+" = "B4+",
                    "Pendant" = "Pendant"
                  )
                ),
                checkboxInput("raw_default_R", "Use default B4+ data", F)),
    
    
    conditionalPanel(
      condition = "input.inputType_R != `empty` & input.raw_default_R == false",

      fileInput(
        "fileReference",
        "Choose reference data",
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
    uiOutput("previewReference"),
    span(textOutput("ReferenceName"), style="color:white")
  ))
}


# Filter ####
## Main ####
dataFilterOutput = function(){
  return(list(
    fluidRow(
      column(6, box(title = "Target",
                    collapsible = T,  width = "100%",
                    status = "warning",
                    uiOutput("filterOptions.T"),
                    textOutput("filterEmpty.T")
      )),
      column(6, box(title = "Reference",
                    collapsible = T,  width = "100%",
                    status = "warning",
                    uiOutput("filterOptions.R"),
                    textOutput("filterEmpty.R")
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

## Plots ####
box.filter.figures = function(){
  return(list(
    splitLayout(
      cellWidths = c("20%", "80%"),
      
      cellArgs = list(style = "padding-right: 12px; white-space: normal;margin-bottom:0px;"),
      selectInput(
          "filterPlot_DataSet",
          "View data",
          choices = c("Target" = "TARGET",
                      "Reference" = "REFERENCE")),
      tagList(
        actButton("filterPlot_renderPlot", "Render figure", "update",
                  addStyling = "margin: 0rem; margin-top: 2.5rem; margin-right: 0.5rem"),
        actButton("save_dat_filter", "Download data", "saveCsv",
                  addStyling = "margin: 0rem; margin-top: 2.5rem; margin-right: 0.5rem"),
        actButton("save_dat_filter_fig", "Download figure", "saveFigure",
                  addStyling = "margin: 0rem; margin-top: 2.5rem; margin-right: 0.5rem"),
        actButton("filterPlot_customizePlot", "Customise figure", "grey",
                  addStyling = "margin: 0rem; margin-top: 2.5rem; margin-right: 0.5rem")
      )
    ),
    
    
    conditionalPanel(
      condition = "input.filterPlot_customizePlot%2==1",
      splitLayout(
        style = "border: 1px solid silver;border-radius: 4px;",
        cellWidths = 300,
        cellArgs = list(style = "padding: 6px"),
        radioButtons(
          "filterPlot_type",
          "Plot type",
          inline = T,
          choices = c(
            "Line plot" = "line",
            "Scatter plot" = "scatter", 
            "Histogram" = "hist"
          )), 
        tagList(
          conditionalPanel(
            condition = "input.filterPlot_type == `hist`",
            numericInput("filterPlot_bins", "Number of bins for histogram", 
                         value = 100)
          ),
          conditionalPanel(
            condition = "input.filterPlot_type != `hist`",
            selectInput("filterPlot_window", "Aggregation window",
                        choices = c("hours" = "hours",
                                    "30 minutes" = "30 minutes",
                                    "20 minutes" = "20 minutes",
                                    "10 minutes" = "10 minutes",
                                    "minutes" = "minutes"))
          )
        )

      )),

    conditionalPanel(
      condition = "input.filterPlot_renderPlot != 0",
      plotlyOutput("filterPlot")
    )
  ))
}

