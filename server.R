# Define file upload limit, now 15 MB
options(shiny.maxRequestSize = 15*1024^4)

shinyServer(function(input, output, session) {
  
  ########################
  ### PROJECT SETTINGS ###
  ########################
  
  #### Variables ####
  
  #' Shiny function that returns 'available volumes on the system'
  volumes = getVolumes()
  
  #' Variable holding possible root directories
  roots = c('working directory' = getwd(),
            system = volumes())
  
  #' Shiny function to 'to navigate the filesystem'
  folderInput1 <- shinyDirChoose(input, 'folder',
                                roots = roots, 
                                 filetypes = c('', 'txt'))
  
  
  
  #' Reactive variable holding the current project path
  #' If not project is selected it returns the root directory
  projectPath <- reactive({
    if (!isTruthy(input$folder)){
      roots[[1]]
    } else {
      parseDirPath(roots = roots, selection = input$folder)      
    }
  })
  
  #' Reactive variable holding the current project name
  projectName <- reactive({
    return(tail(unlist(strsplit(as.character(projectPath()), "/")), 1))
  })
  
  
  #' Reactive variable holding the name of plot titles 
  #' (for saved plots)
  #' If not defined in the UI it returns "", i.e. no title appears
  figTitle <- reactive({
    return(input$figTitle)
  })
  
  #' Set default theme
  theme_set(theme_bw())  
  
  #' Reactive variable holding ggplot theme
  #' Can be defined in UI
  plot_theme <- reactive({
    themes[[input$figTheme]]
  })
  
  #' Reactive variable holding fill colors to be used in 
  #' all plots with discrete data
  #' Can be defined in UI
  fillcolors_react = reactive({
    return(get.fillcolors(ui.input = input))
  })
  
  #' Global function (accessible from other scripts) 
  #' that returns a set of N fillcolors
  fillcolors <<- function(N){
    col = fillcolors_react()
    len = length(col)
    return(col[(len-N):len])
  }
  
  #' Reactive variable holding colors to be used in 
  #' all plots with gradient color scale
  #' Can be defined in UI
  gradientcolors_react = reactive({
    return(get.gradientcolors(ui.input = input))
  })
  
  #' Global function (accessible from other scripts) 
  #' that returns 2 colors
  gradientcolors <<- function(){
    col = gradientcolors_react()
    return(col)
  }
  
  
  
  #### UI output ####
  #' Function to render all ggplots with defined theme
  output$theme_output <- renderUI({ 
    #req(input$figTheme)
    theme_set(plot_theme())
    NULL
  })
  
  #' Show project path in Project Settings > Project
  #' if a project folder is selected
  output$prjDir <- renderPrint({
    projectPath()
  })
  
  
  #### Buttons ####
  
  #' Button to create a project (Project Settings > Project)
  #' Requires a folder to be selected (Folder select)
  #' If directory does not exist create two folders:
  #' 'csv-files', 'graphics'
  #' Sets project name = project folder name
  observeEvent(input$crtPrj, {
    # If no folder have been selected show error
    if (!isTruthy(input$folder)){
      showNotification("No folder has been selected yet",
                       type = "error")
    } else{
      req(input$folder)
      csvPath = paste(projectPath(),
                      "/csv-files/", sep = "")
      figPath = paste(projectPath(),
                      "/graphics/", sep = "")
      if (!dir.exists(csvPath)){
        dir.create(csvPath)
      }
      if (!dir.exists(figPath)){
        dir.create(figPath)
      }
      showNotification("A project has been created",
                       type = "message")
    }
    output$prjName <- renderPrint({
      projectName()
    })
  })
  
  






            #### DATA ####


#### Variables ####

#Reactive variables holding raw data
#If no data set is defined, use default data set

rawData_T <- reactive({
  if (is.null(input$file1)){
    defaultData_T = "./data/default_target.csv"
    print("Default data")
    dataT = get.ACCy(defaultData_T,
                                sep = ",",
                                skip = 27)
  } else {
    dataT = get.rawData_T(input)
  }
  return(dataT)
})

  rawData_R <- reactive({
    if (is.null(input$file1)){
      defaultData_R = "./data/default_reference.csv"
      print("Default data")
      dataR = get.ACCy(defaultData_R,
                      sep = ",",
                      skip = 27)
    } else {
      dataR = get.rawData_R(input)
    }
    return(dataR)
  })
  
  
#' Reactive variables holding Mini-Buoy model type- later used 
#' for hydrological parameter computation
Target_NoFilter <- reactive({
  data = rawData_T()
  
  if (input$inputType == "MB1" |
      input$inputType == "MB2"){
    d = data
  }
  if (input$inputType == "MB3"){
    d = data
  }

  return(d)

})

Reference_NoFilter <- reactive({
  data = rawData_R()
  
  if (input$inputType == "MB1" |
      input$inputType == "MB2"){
    d = data
  }
  if (input$inputType == "MB3"){
    d = data
  }
  
  return(d)
  
})

#' Create empty reactive value with a placeholder for the
#' data sets belonging to Target and reference sites
values <- reactiveValues(Target = NULL,
                         Reference = NULL)


#' Reactive variable holding data
#' Assigned to reactive value if empty
Target <- reactive({
  if (is.null(values$Target)){
    values$Target <- Target_NoFilter()
  }
  return(values$Target)
})

Reference <- reactive({
  if (is.null(values$Reference)){
    values$Reference <- Reference_NoFilter()
  }
  return(values$Reference)
})

#' Trigger to update reactive data when 'Use data' 
#' button is pressed
#' Updates data for the whole App
#
observeEvent(input$setData, {
   values$Target <- Target_NoFilter()
   values$Reference <- Reference_NoFilter()
   
 })

  #### FILTER ####

 #' Button to load filter options
  #' Assigns unfiltered  target data as reactive 
  #' value 'Target'
  observeEvent(input$LoadFilter, {
    values$Target <- Target_NoFilter()
    values$Reference <- Reference_NoFilter()
    
  })
  
  #' Button to apply filter
  #' Assigns filter selected options to Target and reference site data sets
  observeEvent(input$FilterApply, {
    values$Target <- get.filteredData(data = values$Target,
                                             ui.input = input)
    values$Reference<- get.filteredData(data = values$Reference,
                                        ui.input = input)
  })
  
  #' Button to delete filter
  #' Assigns unfiltered data for both Target and Reference sites
  observeEvent(input$FilterDelete, {
    values$Target <- Target_NoFilter()
    values$Reference <- Reference_NoFilter()
  })
  
  #### UI ####
  
  #' Reactive variable to get start and end data of data set
  minMaxDatetime <- reactive({
    if (!is.null(input$file1)){
      req(input$setData)
    } 
    d = rawData_T()
    minDate = as.Date(d[which.min(as.POSIXct(d$datetime)),
                        "datetime"])
    maxDate = as.Date(d[which.max(as.POSIXct(d$datetime)),
                        "datetime"])
    return(c(minDate, maxDate))
  })
  
  #' Helper function to built Filter-UI (load or delete)
  filter_helper = function(input, output){
    if (!is.null(input$file1)){
      req(input$setData)
    }
    output = update.filter.ui(ui.output = output, ui.input = input)
    minMaxDatetime = minMaxDatetime()
    updateDateRangeInput(session, "daterange",
                         start = minMaxDatetime[1],
                         end = minMaxDatetime[2],
                         min = minMaxDatetime[1],
                         max = minMaxDatetime[2])
  }
  
  #' Eventlistener to built Filter-UI
  #' Calling helper function, same as delete filter
  observeEvent(input$LoadFilter, {
    filter_helper(input, output)
  })
  
  #' Eventlistener to built Filter-UI
  #' Calling helper function, same as load filter
  observeEvent(input$FilterDelete, {
    filter_helper(input, output)
  })
  
  
  
  
  #### TABLE OUTPUTS ####
  
  
  
  tab.with.file.upload.message = function(){
    m = matrix(data = c("An error occured. Please check your upload settings (e.g. number of lines skipped) and required column names."))
    return(datatable(m) %>%
             formatStyle(1, color = 'red',
                         backgroundColor = 'orange',
                         fontWeight = 'bold'))
  }
  
  
  #' UI Target site Table with , wide-format
  #' (Data > Upload > Preview data)
  output$raw.target <- DT::renderDataTable(rownames = FALSE, {
    rawDataTable_T = rawData_T()
    #if ("dTime" %in% colnames(rawData)){
     # rawDataTable = rawData %>%
      #  mutate(dTime = round(dTime, 2))
    #} else {
     # rawDataTable = tab.with.file.upload.message()
   # }
    return(head(rawDataTable_T, n=1000)) #remove function head() to show all data entries (makes the table render very slow- avoid for now)
  }, options = list(scrollX = TRUE, searching = F), server=FALSE)
  
  

output$raw.reference <- DT::renderDataTable(rownames = FALSE, {
  rawDataTable_R = rawData_R()
  
  return(head(rawDataTable_R, n=1000))
}, options = list(scrollX = TRUE, searching = F), server=FALSE)





#### Text output ####

#' UI Text output of remaining data points after filtering
#' (Data > Filter > Subset data)    
output$dataPoints <- renderText({
  filtered_data = Target()
  n_diff = nrow(Target_NoFilter()) - nrow(filtered_data)
  if (any(is.na(filtered_data))){
    paste("Data set contains NA values. <br/><br/>", 
          n_diff, " data points removed.")
  } else {
    paste(n_diff, " data points removed.")
  }
})
#### Graphics ####

#' Reactive variable holding the 
#' plot with (filtered) data
#' 
DataSetInput<-reactive({
  if (input$DataSet == "Target"){
    dataset<- Target()
  }
  else if (input$DataSet == "Reference"){
    dataset<- Reference()
  }
  return(dataset)
})


filterPlot <- reactive({
  plot.histogram(data = DataSetInput(), 
                 ui.input = input)
})


output$filterPlot <- renderPlot({
  filterPlot()
})





##### Custom View ####

#' Assign empty reactive value holding ui inputs for
#' customized figure
values <- reactiveValues(plotSettings = NULL)

#' Eventlistener assigning ui inputs to customize figure
#' to reactive value
observeEvent(input$renderPlot, {
  values$plotSettings <- get.customizedPlotSettings(ui.input = input)
})

#' Reactive variable holding ui inputs to customize figure
plotSettings <- reactive({
  return(values$plotSettings)
})

#' Reactive variable holding the plot showing customized
#' temperature visualizations
custumPlot <- reactive({
  req(input$renderPlot)
  plot.customTemperature(data =  DataSetInput(),
                         ui.input.processed = plotSettings())
})

#' #' UI of customized plot
#' #' (Data > View > Figure)
output$custumPlot <- renderPlot({
  if (input$renderPlot == 0){
    plot.emptyMessage("Customize your figure (settings).")
  } else {
    custumPlot()
  }
})


#### Buttons ####

#' Eventlistener to save unfiltered, long-format data
#' (Data > Upload > Preview data)
observeEvent(input$save_dat_upl, {
  save.csv(path = projectPath(), 
           name = "dTemp",
           csvObject = DataSet(), 
           ui.input = input)
  
})

#' Eventlistener to save plot with customized temperatures
#' (Data > View > Figure)
observeEvent(input$save.custumPlot, {
  name = paste("g",
               input$rawPlot.xcol, 
               input$rawPlot.ycol, 
               input$rawPlot.col,  
               input$rawPlot.shape,
               input$rawPlot.facet, sep = "_")
  save.figure(path = projectPath(),
              name = name,
              plotObject = custumPlot(), 
              ui.input = input)
})

#' Eventlistener to save filtered, long-format data
#' (Data > Filter > Figures)
observeEvent(input$save_dat_filter, {
  save.csv(path = projectPath(), 
           name = "Acceleration_filtered",
           csvObject = DataSet(), 
           ui.input = input)
})

#' Eventlistener to save plot with filtered data
#' (Data > Filter > Figures)
observeEvent(input$save_dat_filter_fig, {
  name = paste("g_filtered",
               as.character(input$filterPlot_type),
               as.character(input$DataSet),
               as.character(input$filterPlot_col), sep = "_")
  save.figure(path = projectPath(),
              name = name,
              plotObject = filterPlot(), 
              ui.input = input)
})




})
            