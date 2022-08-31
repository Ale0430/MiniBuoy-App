############
### DATA ###
############

### Upload ###
dataUplOutput = function(){
  return(
    fluidRow(
      column(6,
             box(title = "Upload data files",
                 collapsible = T, width = "100%",
                 status = "warning",
                 box.dat_upl.upload1(),
                 box.dat_upl.upload2()
             )
      ),
      column(6,
             box(title = "Description", collapsed = T,
                 collapsible = T, width = "100%",
                 status = "info",
                 includeMarkdown("./man/des_data.md")),
             box(title = "Preview data",
                 collapsible = T, width = "100%",
                 status = "success",
                 
                 tabsetPanel(
                   tabPanel("Target", br(),
                            output.table("raw.target")),
                   tabPanel("Reference", br(),
                            #actButton("save_dat_upl", "Save csv", "saveCsv"),
                            br(),
                            output.table("raw.reference")))))
    ))
}

box.dat_upl.upload1 = function(){
  return(list(
    # Input: Select a file ----
    fluidRow(
      column(6, fileInput("file1", "Choose Target site CSV file",
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
                                     ".csv"))),
      column(6,  selectInput("sep", "Separator", 
                             choices = c("Comma" = ",",
                                         "Semicolon" = ";",
                                         "Tab" = "\t")))
    ),
    
    selectInput("inputType", "Input file type",
                c("Mini Buoy 1" = "MB1", 
                  "Mini Buoy 2" = "MB2",
                  "Mini Buoy 3" = "MB3")),
    numericInput("skip", "Skip:", min = 0, max = 100, 1),
    
    # Input: Checkbox if file has header ----
    # checkboxInput("header", "Header", TRUE),
    # Input: Select separator ----
    
    actButton("setData", "Use data", "create")
   
  ))
}

box.dat_upl.upload2 = function(){
  return(list(
    # Input: Select a file ----
    fluidRow(
      column(6, fileInput("file2", "Choose Reference site CSV file",
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
                                     ".csv"))),
      column(6,  selectInput("sep", "Separator", 
                             choices = c("Comma" = ",",
                                         "Semicolon" = ";",
                                         "Tab" = "\t")))
    ),
    
    selectInput("inputType", "Input file type",
                c("Mini Buoy 1" = "MB1", 
                  "Mini Buoy 2" = "MB2",
                  "Mini Buoy 3" = "MB3")),
    numericInput("skip", "Skip:", min = 0, max = 100, 1),
    
    # Input: Checkbox if file has header ----
    # checkboxInput("header", "Header", TRUE),
    # Input: Select separator ----
    
    actButton("setData", "Use data", "create"),
    p(em("<Note> If your data set contains non-numeric rows, e.g. logger warnings,
           they are converted to NA values. Remove them in the 'Data > Filter' section."))
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

