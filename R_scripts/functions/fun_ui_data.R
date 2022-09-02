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
  return(
    fluidRow(
      column(4, box(title = "Filter options",
                    collapsible = T,  width = "100%",
                    status = "warning",
                    actButton("LoadFilter", "Load filter options", "update"),
                    uiOutput("filterOptions")
      )),
      column(8,
             box(title = "Raw data figures",
                 collapsible = T, width = "100%",
                 status = "success",
                 box.filter.figures(),
                 actButton("save_dat_filter", "Save csv", "saveCsv"),
                 actButton("save_dat_filter_fig", "Save figure", "saveFigure")),
             box(title = "Info",
                 collapsible = T, width = "100%",
                 status = "info")#,
                 #includeMarkdown("./man/des_data_filter.md"))
             
      ))
  )
}


box.filter.figures = function(){
  return(list(
    fluidRow(
      column(4, checkboxInput("filterPlot_facetGrid", 
                              "Facet grid (Acceleration ~ date)", F))
    ),
    
    radioButtons("filterPlot_type", "Diagram type", inline = T,
                 choices = c("Scatter plot" = "scatter",
                             "Histogram" = "hist")),
    
    fluidRow(
      column(4, selectInput("DataSet", "View",
                           choices = c("Reference" = "Reference",
                                        "Target" = "Target"))),
       column(4, selectInput("filterPlot_col", "Color/ Group",
                             choices = c("Acceleration" = "Acceleration",
                                         "doy" = "doy",
                                         "none" = "none",
                                         "date" = "date"))),
      column(4, numericInput("filterPlot_binwidth", "Binwidth", value = 0.1))
    ),
    
    output.figure("filterPlot")
  ))
}

