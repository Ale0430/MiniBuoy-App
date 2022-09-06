# Define file upload limit, now 15 MB
options(shiny.maxRequestSize = 15 * 1024 ^ 4)

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
  folderInput1 <- shinyDirChoose(input,
                                 'folder',
                                 roots = roots,
                                 filetypes = c('', 'txt'))
  
  
  
  #' Reactive variable holding the current project path
  #' If not project is selected it returns the root directory
  projectPath <- reactive({
    if (!isTruthy(input$folder)) {
      roots[[1]]
    } else {
      parseDirPath(roots = roots, selection = input$folder)
    }
  })
  
  #' Reactive variable holding the current project name
  projectName <- reactive({
    return(tail(unlist(strsplit(
      as.character(projectPath()), "/"
    )), 1))
  })
  
  output$prjName <- renderPrint({
    cat("No project chosen")
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
  fillcolors <<- function(N) {
    col = fillcolors_react()
    len = length(col)
    return(col[(len - N):len])
  }
  
  #' Reactive variable holding colors to be used in
  #' all plots with gradient color scale
  #' Can be defined in UI
  gradientcolors_react = reactive({
    return(get.gradientcolors(ui.input = input))
  })
  
  #' Global function (accessible from other scripts)
  #' that returns 2 colors
  gradientcolors <<- function() {
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
    projectPath = projectPath()
    if (identical(projectPath, character(0))){
      cat("WARNING: No directory has been selected.")
    } else {
      cat(projectPath())
    }
  })
  
  
  #### Buttons ####
  
  #' Button to create a project (Project Settings > Project)
  #' Requires a folder to be selected (Folder select)
  #' If directory does not exist create two folders:
  #' 'csv-files', 'graphics'
  #' Sets project name = project folder name
  observeEvent(input$crtPrj, {
    # If no folder have been selected show error
    if (!isTruthy(input$folder) | is.null(projectName())) {
      showNotification("WARNING: No project has been created. Please select a folder.",
                       type = "error")
      output$prjName <- renderPrint({
        cat("NONE")
      })
    } else {
      req(input$folder)
      csvPath = paste(projectPath(),
                      "/csv-files/", sep = "")
      figPath = paste(projectPath(),
                      "/graphics/", sep = "")
      if (!dir.exists(csvPath)) {
        dir.create(csvPath)
      }
      if (!dir.exists(figPath)) {
        dir.create(figPath)
      }
      showNotification("A project has been created",
                       type = "message")
      output$prjName <- renderPrint({
        cat(projectName())
      })
    }
  })
  
  
  ##############
  #### DATA ####
  ##############
  
  #### UPLOAD ####
  ##### Variables #####

  
  #' Indicates if data were uploaded
  bool.file.upload.target = reactive({
    buoy_and_file = (input$inputType_T != "empty" & !is.null(input$fileTarget$datapath))
    return(!input$raw_default_T & buoy_and_file)
  })
  
  bool.file.upload.reference = reactive({
    buoy_and_file = (input$inputType_R != "empty" & !is.null(input$fileReference$datapath))
    return(!input$raw_default_R & buoy_and_file)
  })
  
  #' Reactive variables holding raw data
  #' If no data set is defined, use default data set
  rawData_T <- reactive({
    dataT = data.frame()
    if (input$raw_default_T){
      defaultData_T = "./data/default_target.csv"
      print("Default TARGET data")
      dataT = get.ACCy(defaultData_T)
    } 
    if (bool.file.upload.target()){
      print("User TARGET data")
      dataT = get.rawData_T(input) 
    }
    return(dataT)
  })
  
  
  rawData_R <- reactive({
    dataR = data.frame()
    if (input$raw_default_R){
      defaultData_R = "./data/default_reference.csv"
      print("Default REFERENCE data")
      dataR = get.ACCy(defaultData_R)
    } 
    if (bool.file.upload.reference()){
      print("User REFERENCE data")
      dataR = get.rawData_R(input)
    }
    return(dataR)
  })
  
  #' Reactive variable holding the target file name
  get.fileName = function(type, input){
    if (type == "Target"){
      if (bool.file.upload.target()){
        return(strsplit(input$fileTarget$name, "[.]")[[1]][1])
      } else if (input$raw_default_T){
        return("Default")
      } else {
        return("None")
      }
    }
    if (type == "Reference"){
      if (bool.file.upload.reference()){
        return(strsplit(input$fileReference$name, "[.]")[[1]][1])
      } else if (input$raw_default_R){
        return("Default")
      } else {
        return("None")
      }
    }
  }

  output$TargetName <- renderText({ 
    get.fileName("Target", input)
  })
  
  output$ReferenceName <- renderText({ 
    get.fileName("Reference", input)
  })

  

  #' Create empty reactive value with a placeholder for the
  #' data sets belonging to Target and reference sites
  values <- reactiveValues(Target = NULL,
                           Reference = NULL)
  
  
  #' Reactive variable holding data
  #' Assigned to reactive value if empty
  Target <- reactive({
    if (is.null(values$Target)) {
      values$Target <- rawData_T()
    }
    if (!input$raw_default_T & !bool.file.upload.target()){
      print("No TARGET data")
      values$Target = data.frame()
    }
    return(values$Target)
  })
  
  Reference <- reactive({
    if (is.null(values$Reference)) {
      values$Reference <- rawData_R()
    }
    if (!input$raw_default_R & !bool.file.upload.reference()){
      print("No REFERENCE data")
      values$Reference = data.frame()
    }
    return(values$Reference)
  })
  

  #' Trigger to update reactive data when 'Use data'
  #' button is pressed
  #' Updates data for the whole App
  message.upload.fail = "Error: Could not read file. Is the file in the correct format? Please refer to the Mini Buoy Handbook how to download data in the correct format for this App."
  message.upload.no.data = "Error: No data were uploaded. "
  
  observeEvent(input$setData.T, {
    print("Set data TARGET")
    if (identical(rawData_T(), data.frame())) {
      if (bool.file.upload.target()) {
        showNotification(
          message.upload.fail,
          type = "error",
          duration = NULL,
          closeButton = T
        )
      } else {
        showNotification(
          message.upload.no.data,
          type = "error",
          duration = NULL,
          closeButton = T
        )
      }
    } else {
      showNotification(
        "Upload of Target data successful.",
        type = "message",
        duration = 3,
        closeButton = T
      )
    }
    Target = Target()
    
  })

  observeEvent(input$setData.R, {
    print("Set data REFERENCE")
    if (identical(rawData_R(), data.frame())) {
      if (bool.file.upload.reference()) {
        showNotification(
          message.upload.fail,
          type = "error",
          duration = NULL,
          closeButton = T
        )
      } else {
        showNotification(
          message.upload.no.data,
          type = "error",
          duration = NULL,
          closeButton = T
        )
      }
    } else {
      Reference = Reference()
      showNotification(
        "Upload of Reference data successful.",
        type = "message",
        duration = 3,
        closeButton = T
      )
    }
  })

  
  #### FILTER ####
  
  #' Buttons to load filter options
  #' Assigns unfiltered  target data as reactive
  #' value 'Target'
  
  #' Eventlistener to built Filter-UI
  #' Calling helper function, same as delete filter
  observeEvent(input$LoadFilter.T, {
    values$Target <- rawData_T()
    filter_helper.T(input, output)
  })
  
  observeEvent(input$LoadFilter.R, {
    values$Reference <- rawData_R()
    filter_helper.R(input, output)
  })
  
  #' Buttons to apply filter
  #' Assigns filter selected options to Target and reference site data sets
  observeEvent(input$FilterApply.T, {
    values$Target <- get.filteredData(data = values$Target,
                                      ui.input = input,
                                      filetype = "T")
  })
  
  observeEvent(input$FilterApply.R, {
    values$Reference <- get.filteredData(data = values$Reference,
                                      ui.input = input,
                                      filetype = "R")
  })
  
  #' Buttons to delete filter
  #' Assigns unfiltered data for both Target and Reference sites
  observeEvent(input$FilterDelete.T, {
    values$Target <- rawData_T()
  })
  
  observeEvent(input$FilterDelete.R, {
    values$Reference <- rawData_R()
  })
  
  #### UI ####
  
  #' Reactive variables to get start and end data of data set
  minMaxDatetime.T <- reactive({
    if (!is.null(input$fileTarget)) {
      req(input$setData.T)
    }
    d = Target()
    minDate = min(d$date)
    maxDate = max(d$date)
    return(c(minDate, maxDate))
  })
  
  minMaxDatetime.R <- reactive({
    if (!is.null(input$fileReference)) {
      req(input$setData.R)
    }
    d = Reference()
    minDate = min(d$date)
    maxDate = max(d$date)
    return(c(minDate, maxDate))
  })
  
  #' Helper function to built Filter-UI (load or delete)
  filter_helper.T = function(input, output) {
    if (!is.null(input$fileTarget)) {
      req(input$setData.T)
    }
    output = update.filter.ui(ui.output = output, ui.input = input,
                              filetype = "T")
    minMaxDatetime = minMaxDatetime.T()
    print(minMaxDatetime)
    updateDateRangeInput(
      session,
      "daterange.T",
      start = minMaxDatetime[1],
      end = minMaxDatetime[2],
      min = minMaxDatetime[1],
      max = minMaxDatetime[2]
    )
  }
  filter_helper.R = function(input, output) {
    if (!is.null(input$fileReference)) {
      req(input$setData.R)
    }
    output = update.filter.ui(ui.output = output, ui.input = input,
                              filetype = "R")
    minMaxDatetime = minMaxDatetime.R()
    print(minMaxDatetime)
    print("Update daterange in R")
    updateDateRangeInput(
      session,
      "daterange.R", #@Marie: bug - not updated when T loaded before
      start = minMaxDatetime[1],
      end = minMaxDatetime[2],
      min = minMaxDatetime[1],
      max = minMaxDatetime[2]
    )
  }
  
  #### TABLE OUTPUTS ####
  
  tab.with.file.upload.message = function(message, color = "red",
                                          backgroundColor = 'orange'){
    m = matrix(
      data = c(
        message
      )
    )
    return(
      datatable(m, options = list(dom = 't'), colnames=NULL) %>%
        formatStyle(
          1,
          color = color,
          backgroundColor = backgroundColor,
          fontWeight = 'bold'
        )
    )
  }
  
  get.rawData.sum = function(data, type){
    no.days = round(max(data$datetime) - min(data$datetime), 2)
    if (no.days < 2){
      showNotification(paste("Error:", type, "data set too small to analyze (< 2 days)!", sep = " "),
                       type = "error", duration = NULL, closeButton = T)
      tab = tab.with.file.upload.message("Error: Data set too small to analyze (< 2 days)!")
    } else {
      if (ncol(data) > 4){
        showNotification(paste("Warning:", type, 
                               "data set contains > 2 columns. Only columns 1 (datetime) and 2 (Acceleration) are used for further processing.",
                               sep = " "),
                         type = "warning", duration = NULL, closeButton = T)
      }
      if (no.days < 15){
        showNotification(paste("Warning:", type, "data set too small to analyze properly (< 15 days)!", sep = " "),
                         type = "warning", duration = NULL, closeButton = T)
      }
      
      coln = paste(colnames(data), collapse = ", ")
      meanAcc = mean(data$Acceleration)
      mAmm = paste(as.character(round(meanAcc, 2)), " (",
                   as.character(round(min(data$Acceleration), 2)), ", ",
                   as.character(round(max(data$Acceleration), 2)), ")", 
                   collapse = "")
      mAqq = paste(as.character(round(median(data$Acceleration), 2)), " (",
                   as.character(round(quantile(data$Acceleration, 0.25), 2)), ", ",
                   as.character(round(quantile(data$Acceleration, 0.55), 2)), ")", 
                   collapse = "")
      
      tab = data.frame(Variable = c("Column names",
                                    "Survey length (days)",
                                    "First date", "Last date",
                                    "Mean Acc. (Min, Max)",
                                    "Median acceleration (1st and 3rd quantile)",
                                    "Number of recordings"),
                       Value = c(coln,
                                 as.character(no.days),
                                 as.character(min(data$datetime)),
                                 as.character(max(data$datetime)),
                                 mAmm,
                                 mAqq,
                                 as.character(nrow(data))))
      colnames(tab) = c("", "")
      if (meanAcc > 0){
        showNotification(paste("Warning: Mean Acceleration in data set", type, 
                               "is > 0. You may installed the Mini-Buoy upside down.", sep = " "),
                         type = "warning",
                         duration = NULL, closeButton = T)
      }
    }
    
    return(tab)
  }

  message.upload.failed = "An error occured. Please check your uploaded csv-file (e.g. needs to contain line starting with '*DATA')."
  output$raw.target.sum <- DT::renderDataTable(
    rownames = F,
    {
      if (!input$raw_default_T & is.null(input$fileTarget$datapath)){
        return(tab.with.file.upload.message("Please upload your data or select a default data set.",
                                            color = "blue", backgroundColor = "white"))
        
      } else {
        rawDataTable_T = rawData_T()
        if (is.numeric(rawDataTable_T$Acceleration)){
          t = get.rawData.sum(rawDataTable_T, "TARGET")
          return(t)
        } else {
          return(tab.with.file.upload.message(message.upload.failed))
        }
      }


    },
    options = list(dom = 't'),
  )
  
  output$raw.reference.sum <- DT::renderDataTable(
    rownames = F,
    {
      if (!input$raw_default_R & is.null(input$fileReference$datapath)){
        return(tab.with.file.upload.message("Please upload your data or select a default data set.",
                                            color = "blue", backgroundColor = "white"))
        
      } else {
        rawDataTable_R = rawData_R()
        if (is.numeric(rawDataTable_R$Acceleration)){
          return(get.rawData.sum(rawDataTable_R, "REFERENCE"))
        } else {
          return(tab.with.file.upload.message(message.upload.failed))
        }
      }
    },
    options = list(dom = 't'),
  )
  
  observeEvent(input$setData.T | input$setData.R, {
    if(input$setData.T==0 || input$setData.R==0){
        return()
    }
    Target = Target()
    Reference = Reference()
    if (nrow(Target) > 0 & nrow(Reference) > 0){
      get.time.overlap(data.t = Target(), 
                       data.r = Reference())
    }

  })
  

  #### Text output ####
  
  #' UI Text output of remaining data points after filtering
  #' (Data > Filter > Subset data)
  get.deleted.points.text = function(data_before, data_after){
    n_diff = nrow(data_before) - nrow(data_after)
    if (any(is.na(data_after))) {
      paste("Data set contains NA values. <br/><br/>",
            n_diff,
            " data points removed.")
    } else {
      paste(n_diff, " data points removed.")
    }
  }
  
  output$dataPoints.T <- renderText({ 
    get.deleted.points.text(data_before = rawData_T(),
                            data_after = Target())
  })
  
  output$dataPoints.R <- renderText({
    get.deleted.points.text(data_before = rawData_R(),
                            data_after = Reference())
  })
  
  #### Graphics ####
  
  #' Reactive variable holding the
  #' plot with (filtered) data
  DataSetInput <- reactive({
    if (input$filterPlot_DataSet == "Target") {
      dataset <- data.frame(Target())
    }
    else if (input$filterPlot_DataSet == "Reference") {
      dataset <- data.frame(Reference())
    }
    return(dataset)
  })
  
  
  filterPlot <- eventReactive(input$filterPlot_renderPlot, {
    plot.filteredRawData(data = DataSetInput(),
                         ui.input = input)
  })
  
  
  output$filterPlot <- renderPlot({
    if (!bool.file.upload.target() & !input$raw_default_T) {
      plot.emptyMessage("No figure available. Please upload data.")
      
    } else {
      if (input$filterPlot_renderPlot == 0) {
        plot.emptyMessage("Customize your figure.")
      } else {
        filterPlot()
      }
      
    }
  })
  
  
  
  
  #### Buttons ####

  #' Eventlistener to save filtered, long-format data
  #' (Data > Filter > Figures)
  observeEvent(input$save_dat_filter, {
    save.csv(
      path = projectPath(),
      name = paste(
        "Acceleration_filtered",
        as.character(input$DataSet),
        sep = "_"
      ),
      csvObject = DataSetInput(),
      ui.input = input
    )
  })
  
  #' Eventlistener to save plot with filtered data
  #' (Data > Filter > Figures)
  observeEvent(input$save_dat_filter_fig, {
    name = paste(
      "g_filtered",
      as.character(input$DataSet),
      as.character(input$filterPlot_type),
      sep = "_"
    )
    save.figure(
      path = projectPath(),
      name = name,
      plotObject = filterPlot(),
      ui.input = input
    )
  })
  
  
  ####################
  #### HYDRODYNAMICS #
  ####################
  
  
})
