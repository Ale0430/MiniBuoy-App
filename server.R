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
  roots = c('folder location' = getwd(),
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

  #' Variable indicating which Mini Buoy Design is selected
  get.design.T <- reactive({
    if (input$raw_default_T){
      return("B4")
    } else {
      return(input$inputType_T)
    }
  })
  
  get.design.R <- reactive({
    if (input$raw_default_R){
      return("B4")
    } else {
      return(input$inputType_R)
    }
  })
  
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
    dataT = NULL
    # Load default file
    if (input$raw_default_T){
      defaultData_T = "./data/default_target.csv"
      print("TARGET data: default")
      dataT = get.ACCy.B4(defaultData_T)
    } else {
      # Load user file if file type is choosen and file path provided
      if (bool.file.upload.target()){
        print("TARGET data: user")
        dataT = get.rawData_T(input) 
      }
    }
    # If column Acceleration is not numeric or is all NA
    # delete data frame
    if (!is.numeric(dataT$Acceleration) | all(is.na(dataT$Acceleration))){
      print("Raw data upload TARGET failed")
      dataT = NULL
    }
    return(dataT)
  })
  
  
  rawData_R <- reactive({
    dataR = NULL
    if (input$raw_default_R){
      defaultData_R = "./data/default_reference.csv"
      print("REFERENCE data: default")
      dataR = get.ACCy.B4(defaultData_R)
    } else {
      if (bool.file.upload.reference()){
        print("REFERENCE data: user")
        dataR = get.rawData_R(input)
      }
    }
    # If column Acceleration is not numeric or is all NA
    # delete data frame
    if (!is.numeric(dataR$Acceleration) | all(is.na(dataR$Acceleration))){
      print("Raw data upload TARGET failed")
      dataR = NULL
    }
    return(dataR)
  })
  
  #' Reactive variable holding the target file name
  get.fileName = function(type, input){
    if (type == "Target"){
      if (bool.file.upload.target()){
        if (!is.null(values$Target)){
          return(strsplit(input$fileTarget$name, "[.]")[[1]][1])
        } else {
          return("None")
        }
      } else if (input$raw_default_T){
        return("Default_Target")
      } else {
        return("None")
      }
    }
    if (type == "Reference"){
      if (bool.file.upload.reference()){
        if (!is.null(values$Reference)){
          return(strsplit(input$fileReference$name, "[.]")[[1]][1])
        } else {
          return("None")
        }
      } else if (input$raw_default_R){
        return("Default_Reference")
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
                           TargetHydro = NULL,
                           Reference = NULL,
                           ReferencetHydro = NULL)
  
  
  #' Reactive variable holding data
  #' Assigned to reactive value if empty
  Target <- reactive({
    if (is.null(values$Target)){
      print("TARGET data: create")
      values$Target <- rawData_T()
    }
    if (!input$raw_default_T & ! bool.file.upload.target()){
      print("TARGET data: delete")
      values$Target = NULL 
    }
    return(values$Target)
  })
  
  Reference <- reactive({
    if (is.null(values$Reference)){
      print("REFERENCE data: create")
      values$Reference <- rawData_R()
    }
    if (!input$raw_default_R & !bool.file.upload.reference()){
      print("REFERENCE data: delete")
      values$Reference = NULL
    }
    return(values$Reference)
  })
  

  #' Trigger to update reactive data when 'Use data'
  #' button is pressed
  #' Updates data for the whole App
  message.upload.fail = "Error: Could not read file. Is the file in the correct format? Please refer to the Mini Buoy Handbook how to download data in the correct format for this App."
  message.upload.no.data = function(type){
    return(paste("Error: No ", type, " data were uploaded.", sep = ""))
  }
  
  observeEvent(input$setData.T, {
    rawData_T = rawData_T()
    if (is.null(rawData_T)) {
      if (bool.file.upload.target()) {
        showNotification(
          message.upload.fail,
          type = "error",
          duration = NULL,
          closeButton = T
        )
      } else {
        showNotification(
          message.upload.no.data("TARGET"),
          type = "error",
          duration = NULL,
          closeButton = T
        )
      }
    } else {
      if (all(is.na(rawData_T$Acceleration))){
        showNotification(
          message.upload.fail,
          type = "error",
          duration = NULL,
          closeButton = T
        )
      } else {
        showNotification(
          "Upload of Target data successful.",
          type = "message",
          duration = 3,
          closeButton = T
        )
      }
    }
    Target = Target()
    
  })

  observeEvent(input$setData.R, {
    rawData_R = rawData_R()
    if (is.null(rawData_R)) {
      if (bool.file.upload.reference()) {
        showNotification(
          message.upload.fail,
          type = "error",
          duration = NULL,
          closeButton = T
        )
      } else {
        showNotification(
          message.upload.no.data("REFERENCE"),
          type = "error",
          duration = NULL,
          closeButton = T
        )
      }
    } else {
      if (all(is.na(rawData_R$Acceleration))){
        showNotification(
          message.upload.fail,
          type = "error",
          duration = NULL,
          closeButton = T
        )
      } else {
        showNotification(
          "Upload of Reference data successful.",
          type = "message",
          duration = 3,
          closeButton = T
        )
      }
    }
    Reference = Reference()
  })
  
  
  #' Reactive variable indicating if Target data is available
  bool.no.target <- reactive({
    Target = values$Target
    return((is.null(Target) | identical(Target, data.frame())))
  })
  
  
  #' Reactive variable indicating if Reference data is available
  bool.no.reference <- reactive({
    Reference = values$Reference
    return((is.null(Reference) | identical(Reference, data.frame())))
  })
  
  bool.overlap <- reactive({
    tar.exists = !bool.no.target()
    ref.exists = !bool.no.reference()
    time.overlap = F
    if (tar.exists & ref.exists){
      time.overlap = !is.null(get.time.overlap(values$Reference, values$Target))
    }
    return(time.overlap)
  })
  
  #### TABLE OUTPUTS ####
  
  tab.with.file.upload.message = function(message, color = "#cc0000",
                                          backgroundColor = '#ffcccc'){
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
  
  
  output$raw.target.sum <- DT::renderDataTable(
    rownames = F,
    {
      if (!input$raw_default_T & is.null(input$fileTarget$datapath)){
        return(tab.with.file.upload.message(text.upload.missing,
                                            color = "blue", backgroundColor = "white"))
        
      } else {
        rawDataTable_T = rawData_T()
        if (is.numeric(rawDataTable_T$Acceleration) & !all(is.na(rawDataTable_T$Acceleration))){
          t = get.rawData.sum(rawDataTable_T, "TARGET")
          return(t)
        } else {
          return(tab.with.file.upload.message(message.upload.fail))
        }
      }
      
      
    },
    options = list(dom = 't'),
  )
  
  output$raw.reference.sum <- DT::renderDataTable(
    rownames = F,
    {
      if (!input$raw_default_R & is.null(input$fileReference$datapath)){
        return(tab.with.file.upload.message(text.upload.missing,
                                            color = "blue", backgroundColor = "white"))
        
      } else {
        rawDataTable_R = rawData_R()
        if (is.numeric(rawDataTable_R$Acceleration)){
          return(get.rawData.sum(rawDataTable_R, "REFERENCE"))
        } else {
          return(tab.with.file.upload.message(message.upload.fail))
        }
      }
    },
    options = list(dom = 't'),
  )
  
  #' Eventlistener: if both, target and reference data, are set
  #' calculate overlapping time window and show warning or error
  #' message
  observeEvent(input$setData.T | input$setData.R, {
    if (input$setData.T == 0 || input$setData.R == 0) {
      return()
    }
    Target = Target()
    Reference = Reference()
    if (!is.null(Target) & !is.null(Reference)){
      time.overlap = get.time.overlap(data.t = Target,
                                      data.r = Reference)
      if (is.null(time.overlap)) {
        showNotification(
          "Warning: Time windows of TARGET and REFERENCE do not overlap",
          type = "warning",
          duration = 5,
          closeButton = T
        )
      } else {
        showNotification(
          paste(
            "Time windows of TARGET and REFERENCE overlap",
            round(time.overlap[2] - time.overlap[1], 2),
            "days.",
            sep = " "
          ),
          type = "message",
          duration = 5,
          closeButton = T
        )
      }
    }
    
  })
  
  
  
  #### FILTER ####
  
  #' Buttons to load filter options
  #' Assigns unfiltered  target data as reactive
  #' value 'Target'
  observeEvent(input$LoadFilter.T, {
    if (is.null(values$Target)){
      showNotification(
        message.upload.no.data("TARGET"),
        type = "error",
        duration = NULL,
        closeButton = T
      )
    } else {
      print("TARGET data: filter load/delete")
      values$Target <- rawData_T()
      filter_helper.T(input, output)
    }

  })
  
  observeEvent(input$LoadFilter.R, {
    if (is.null(values$Reference)){
      showNotification(
        message.upload.no.data("REFERENCE"),
        type = "error",
        duration = NULL,
        closeButton = T
      )
    } else {
      print("REFERENCE data: filter load/delete")
      values$Reference <- rawData_R()
      filter_helper.R(input, output)
    }
  })
  
  #' Buttons to apply filter
  #' Assigns filter selected options to Target and reference site data sets
  observeEvent(input$FilterApply.T, {
    print("TARGET data: filter apply")
    values$Target <- get.filteredData(data = values$Target,
                                      ui.input = input,
                                      filetype = "T")
  })
  
  observeEvent(input$FilterApply.R, {
    print("REFERENCE data: filter apply")
    values$Reference <- get.filteredData(data = values$Reference,
                                      ui.input = input,
                                      filetype = "R")
  })
  
  #' Buttons to delete filter
  #' Assigns unfiltered data for both Target and Reference sites
  observeEvent(input$FilterDelete.T, {
    print("TARGET data: filter delete")
    values$Target <- rawData_T()
  })
  
  observeEvent(input$FilterDelete.R, {
    print("REFERENCE data: filter delete")
    values$Reference <- rawData_R()
  })
  
  #### UI ####
  
  #' Reactive variables to get start and end data of data set
  minMaxDatetime.T <- reactive({
    if (!is.null(input$fileTarget)) {
      req(input$setData.T)
    }
    d = Target()
    minDate = min(d$datetime)
    maxDate = max(d$datetime)
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
    minMaxDatetime = minMaxDatetime.T()
    output = update.filter.ui(ui.output = output, ui.input = input,
                              filetype = "T", minMaxDatetime = minMaxDatetime)
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
    minMaxDatetime = minMaxDatetime.R()
    output = update.filter.ui(ui.output = output, ui.input = input,
                              filetype = "R", minMaxDatetime = minMaxDatetime)
    updateDateRangeInput(
      session,
      "daterange.R", #@Marie: bug - not updated when T loaded before
      start = minMaxDatetime[1],
      end = minMaxDatetime[2],
      min = minMaxDatetime[1],
      max = minMaxDatetime[2]
    )
  }
  

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
  #' data set with (filtered) data
  #' Show notification if no data were uploaded
  DataSetInput <- reactive({
    if (input$filterPlot_DataSet == "TARGET") {
      dataset <- data.frame(Target())
    }
    if (input$filterPlot_DataSet == "REFERENCE") {
      dataset <- data.frame(Reference())
    }
    if (identical(dataset, data.frame())) {
      showNotification(
        message.upload.no.data(input$filterPlot_DataSet),
        type = "error",
        duration = NULL,
        closeButton = T
      )
    }
    
    return(dataset)
  })
  
  #' Reactive variable holding the
  #' plot shown in data > filter
  filterPlot <- eventReactive(input$filterPlot_renderPlot, {
    req(input$filterPlot_renderPlot)
    data = DataSetInput()
    if (identical(data, data.frame())) {
      plot.emptyMessage("No figure available. Please upload data.")
    } else {
      if (input$filterPlot_renderPlot == 0) {
        plot.emptyMessage("Customize your figure.")
      } else {
        plot.filteredRawData(data = data,
                             ui.input = input)
      }
    }
  })

  #' Render plot shown in data > filter
  output$filterPlot <- renderPlot({
    filterPlot()
  })
  
  
  
  
  #### Buttons ####

  #' Eventlistener to save filtered data
  #' (Data > Filter)
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
  #' (Data > Filter)
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
  
  
  ########################
  ##### HYDRODYNAMICS ####
  ########################
  
  
  text.upload.missing = "Please upload your data or select a default data set."
  text.too.short = "Uploaded/ filtered data set < 2 days."
  
  #' Render output option depending on data availability
  output$hydro.window.target.show = renderUI({
    if (!bool.no.target() & !bool.no.reference()) {
      list(
        checkboxInput(
          "hydro.window.target",
          "Use only overlapping times of the target and reference data",
          F
        ),
        hr()
      )
    }
  })
  
  output$hydro.window.reference.show = renderUI({
    if (!bool.no.target() & !bool.no.reference()) {
      list(
        checkboxInput(
          "hydro.window.reference",
          "Use only overlapping times of the target and reference data",
          F
        ),
        hr()
      )
    }
  })
  
  
  get.hydrodynamics.overlap = function(dataset) {
    time.overlap = get.time.overlap(data.t = Target(),
                                    data.r = Reference())
    if (dataset == "Target") {
      Target = Target() %>%
        filter(datetime >= time.overlap[1] &
                 datetime <= time.overlap[2])
      hydroData = get.hydrodynamics(data = Target,
                                    design = get.design.T())
    }
    if (dataset == "Reference") {
      Reference = Reference() %>%
        filter(datetime >= time.overlap[1] &
                 datetime <= time.overlap[2])
      hydroData = get.hydrodynamics(data = Reference,
                                    design = get.design.R())
    }
    return(hydroData)
  }
  
  ##### TARGET ####
  ##### Variables ####

  TargetHydro = reactive({
    if (is.null(values$TargetHydro)){
      print("TARGET hydro: create")
      values$TargetHydro = get.hydrodynamics(data = Target(),
                                             design = get.design.T())
    }
    return(values$TargetHydro)
  })
  
  
  #' Observers to trigger update of hydrodynamic calculation 
  #' if raw data is filtered or the filter is deleted or a change
  #' in the 'use overlapping time...' occurs
  observeEvent(input$FilterApply.T, {
    if (!is.null(input$FilterApply.T[1]) & !is.null(values$TargetHydro)){
      if (input$FilterApply.T[1] != 0){
        print("TARGET hydro: update with filtered data")
        values$TargetHydro = get.hydrodynamics(data = Target(),
                                               design = get.design.T())
      }}
  })
  
  observeEvent(input$FilterDelete.T, {
    if (!is.null(input$FilterDelete.T[1]) & !is.null(values$TargetHydro)){
      if (input$FilterDelete.T[1] != 0){
        print("TARGET hydro: update with full data")
        values$TargetHydro = get.hydrodynamics(data = Target(),
                                               design = get.design.T())
      }}
  })
  
  observeEvent(input$LoadFilter.T, {
    if (!is.null(input$LoadFilter.T[1]) & !is.null(values$TargetHydro)){
      if (input$LoadFilter.T[1] > 1){
        print("TARGET hydro: update with full data")
        values$TargetHydro = get.hydrodynamics(data = Target(),
                                               design = get.design.T())
      }}
  })

  observeEvent(input$hydro.window.target, {
    if (!is.null(input$hydro.window.target) & !is.null(values$TargetHydro)){
      if (input$hydro.window.target) {
        print("TARGET hydro: update with overlapping data")
        values$TargetHydro = get.hydrodynamics.overlap(dataset = "Target")
      } else {
        print("TARGET hydro: update with full data")
        values$TargetHydro = get.hydrodynamics(data = Target(),
                                               design = get.design.T())
      }
    } 
  })
  
 
  TargetHydroStats <- reactive({
    TargetHydro = TargetHydro()
    timewindow = difftime(max(TargetHydro$datetime), min(TargetHydro$datetime), units = "days")
    if (timewindow < 2){
      TargetHydroStats = NULL
    } else {
      TargetHydroStats = get.statistics(TargetHydro())
    }
    return(TargetHydroStats)
  })
  
  ##### Text ####

  output$hydro.text.target <- renderUI({
    if (bool.no.target()){
      print(text.upload.missing)
    } else if (is.null(TargetHydroStats())){
      print(text.too.short)
    } else {
      HTML(get.stats.text(TargetHydroStats()))
    }
  })
  
  ##### Table ####

  #' Render table showing hydrodynamics of target data
  output$hydro.table.target <- DT::renderDataTable(
    rownames = F,
    {
      if (bool.no.target()){
        return(tab.with.file.upload.message(text.upload.missing,
                                            color = "blue", backgroundColor = "white"))
      } else if (is.null(TargetHydroStats())) {
        return(tab.with.file.upload.message(text.too.short,
                                            color = "blue", backgroundColor = "white"))
      } else {
        return(TargetHydroStats() %>%
                 mutate_if(is.numeric, round, 2))
      }
    },
    options = list(dom = 't'),
  )
  
  #' Eventlistener to save hydrodynamics summary target
  #' (Hydrodynamics > Summary table)
  observeEvent(input$hydro.table.target.save, {
    save.csv(
      path = projectPath(),
      name = "Hydrodynamics_Target",
      csvObject = TargetHydroStats(),
      ui.input = input
    )
  })

  #### Figures ####
  #### Inundation #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.inundation.target <- reactive({
    if (bool.no.target()){
      plot.emptyMessage("No figure available. Please upload data.")
    } else if (is.null(TargetHydroStats())) {
      plot.emptyMessage(text.too.short)
      } else {
        plot.inundation(data = TargetHydro())
    }
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.inundation.target <- renderPlot({
    fig.inundation.target()
  })
  
  #' Eventlistener to save plot with inundation data
  #' (Hydrodynamics > Target)
  observeEvent(input$save.fig.inundation.target, {
    name = "DailyInundation_Target"
    save.figure(
      path = projectPath(),
      name = name,
      plotObject = fig.inundation.target(),
      ui.input = input
    )
  })
  
  #### Current velocity #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.velocity.target <- reactive({
    if (bool.no.target()) {
      plot.emptyMessage("No figure available. Please upload data.")
    } else if (is.null(TargetHydroStats())) {
      plot.emptyMessage(text.too.short)
    } else {
      plot.velocity(data = TargetHydro())
    }
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.velocity.target <- renderPlot({
    fig.velocity.target()
  })
  
  #' Eventlistener to save plot with inundation data
  #' (Hydrodynamics > Target)
  observeEvent(input$save.fig.velocity.target, {
    name = "CurrentVelocity_Target"
    save.figure(
      path = projectPath(),
      name = name,
      plotObject = fig.velocity.target(),
      ui.input = input
    )
  })
  
  
  #### Wave orbital velocity #####

  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.wave.velocity.target <- reactive({
    if (bool.no.target()) {
      plot.emptyMessage("No figure available. Please upload data.")
    } else if (is.null(TargetHydroStats())) {
      plot.emptyMessage(text.too.short)
    } else {
      design = get.design.T()
      if (design == "B4+"){
        plot.waveVelocity(data = TargetHydro())
      } else {
        plot.emptyMessage(paste("'", design, "' Mini Buoy design does not measure wave orbital velocity",
                                sep = ""))
      }
    }
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.wave.velocity.target <- renderPlot({
    fig.wave.velocity.target()
  })
  
  #' Eventlistener to save plot with inundation data
  #' (Hydrodynamics > Target)
  observeEvent(input$save.fig.wave.velocity.target, {
    name = "WaveOrbitalVelocity_Target"
    save.figure(
      path = projectPath(),
      name = name,
      plotObject = fig.wave.velocity.target(),
      ui.input = input
    )
  })
  
  
  ##### REFERENCE ####
  ##### Variables ####
  
  ReferenceHydro = reactive({
    if (is.null(values$ReferenceHydro)){
      print("REFERENCE hydro: create")
      values$ReferenceHydro = get.hydrodynamics(data = Reference(),
                                                design = get.design.T())
    }
    return(values$ReferenceHydro)
  })
  
  observeEvent(input$FilterApply.R, {
    if (!is.null(input$FilterApply.R[1]) & !is.null(values$ReferenceHydro)){
      if (input$FilterApply.R[1] != 0){
        print("REFERENCE hydro: update with filtered data")
        values$ReferenceHydro = get.hydrodynamics(data = Reference(),
                                                  design = get.design.R())
      }}
  })
  
  observeEvent(input$FilterDelete.R, {
    if (!is.null(input$FilterDelete.R[1]) & !is.null(values$ReferenceHydro)){
      if (input$FilterDelete.R[1] != 0){
        print("REFERENCE hydro: update with full data")
        values$ReferenceHydro = get.hydrodynamics(data = Reference(),
                                                  design = get.design.R())
      }}
  })
  
  observeEvent(input$LoadFilter.R, {
    if (!is.null(input$LoadFilter.R[1]) & !is.null(values$ReferenceHydro)){
      if (input$LoadFilter.R[1] > 1){
        print("REFERENCE hydro: update with full data")
        values$TargetHydro = get.hydrodynamics(data = Target(),
                                               design = get.design.T())
      }}
  })
  
  observeEvent(input$hydro.window.reference, {
    if (!is.null(input$hydro.window.reference) & !is.null(values$ReferenceHydro)){
      if (input$hydro.window.reference) {
        print("REFERENCE hydro: update with overlapping data")
        values$ReferenceHydro = get.hydrodynamics.overlap(dataset = "Reference")
        
      } else {
        print("REFERENCE hydro: update with full data")
        values$ReferenceHydro = get.hydrodynamics(data = Reference(),
                                                  design = get.design.R())
      }
    } 
  })
  
  
  
  ReferenceHydroStats <- reactive({
    ReferenceHydro = ReferenceHydro()
    timewindow = difftime(max(ReferenceHydro$datetime), min(ReferenceHydro$datetime), units = "days")
    if (timewindow < 2){
      ReferenceHydroStats = NULL
    } else {
      ReferenceHydroStats = get.statistics(ReferenceHydro())
    }
    ReferenceHydroStats = get.statistics(values$ReferenceHydro)
    return(ReferenceHydroStats)
  })
  
  
  ##### Text ####
  
  output$hydro.text.reference <- renderUI({
    if (bool.no.reference()){
      print(text.upload.missing)
    } else if (is.null(ReferenceHydroStats())){
      print(text.too.short)
    } else {
      HTML(get.stats.text(ReferenceHydroStats()))
    }
  })
  
  
  ##### Table ####
  
  #' Render table showing hydrodynamics of reference data
  output$hydro.table.reference <- DT::renderDataTable(
    rownames = F,
    {
      if (bool.no.reference()){
        return(tab.with.file.upload.message(text.upload.missing,
                                            color = "blue", backgroundColor = "white"))
      } else if (is.null(ReferenceHydroStats())){
        print(text.too.short)
      } else {
        return(ReferenceHydroStats() %>%
                 mutate_if(is.numeric, round, 2))
      }
    },
    options = list(dom = 't'),
  )
  
  #' Eventlistener to save hydrodynamics summary reference
  #' (Hydrodynamics > Summary table)
  observeEvent(input$hydro.table.reference.save, {
    save.csv(
      path = projectPath(),
      name = "Hydrodynamics_Reference",
      csvObject = ReferenceHydroStats(),
      ui.input = input
    )
  })
  
  #### Figures ####
  #### Inundation #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Reference
  fig.inundation.reference <- reactive({
    if (bool.no.reference()){
      plot.emptyMessage("No figure available. Please upload data.")
    } else if (is.null(ReferenceHydroStats())){
      print(text.too.short)
    } else {
      plot.inundation(data = ReferenceHydro())
    }
  })
  
  #' Render plot shown in Hydrodynamics > Reference
  output$fig.inundation.reference <- renderPlot({
    fig.inundation.reference()
  })
  
  #' Eventlistener to save plot with inundation data
  #' (Hydrodynamics > Reference)
  observeEvent(input$save.fig.inundation.reference, {
    name = "DailyInundation_Reference"
    save.figure(
      path = projectPath(),
      name = name,
      plotObject = fig.inundation.reference(),
      ui.input = input
    )
  })
  
  #### Current velocity #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > reference
  fig.velocity.reference <- reactive({
    if (bool.no.reference()) {
      plot.emptyMessage("No figure available. Please upload data.")
    } else if (is.null(ReferenceHydroStats())){
      print(text.too.short)
    } else {
      plot.velocity(data = ReferenceHydro())
    }
  })
  
  #' Render plot shown in Hydrodynamics > reference
  output$fig.velocity.reference <- renderPlot({
    fig.velocity.reference()
  })
  
  #' Eventlistener to save plot with inundation data
  #' (Hydrodynamics > reference)
  observeEvent(input$save.fig.velocity.reference, {
    name = "CurrentVelocity_Reference"
    save.figure(
      path = projectPath(),
      name = name,
      plotObject = fig.velocity.reference(),
      ui.input = input
    )
  })
  
  
  #### Wave orbital velocity #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > reference
  fig.wave.velocity.reference <- reactive({
    if (bool.no.reference()) {
      plot.emptyMessage("No figure available. Please upload data.")
    } else if (is.null(ReferenceHydroStats())){
      print(text.too.short)
    } else {
      design = get.design.R()
      if (design == "B4+"){
        plot.waveVelocity(data = ReferenceHydro())
      } else {
        plot.emptyMessage(paste("'", design, 
                                "' Mini Buoy design does not measure wave orbital velocity",
                                sep = ""))
      }
    }
  })
  
  #' Render plot shown in Hydrodynamics > reference
  output$fig.wave.velocity.reference <- renderPlot({
    fig.wave.velocity.reference()
  })
  
  #' Eventlistener to save plot with inundation data
  #' (Hydrodynamics > reference)
  observeEvent(input$save.fig.wave.velocity.reference, {
    name = "WaveOrbitalVelocity_Reference"
    save.figure(
      path = projectPath(),
      name = name,
      plotObject = fig.wave.velocity.reference(),
      ui.input = input
    )
  })
  
  
  ##### COMPARISON ####
  ##### Variables ####
  
  #' Reactive function containing a list the statistical comparison
  #' and hydrodynamics of reference and target for overlapping time
  ComparisonStats <- reactive({
    TargetHydro = get.hydrodynamics.overlap(dataset = "Target")
    ReferenceHydro = get.hydrodynamics.overlap(dataset = "Reference")
    
    TargetHydroStats = get.statistics(data = TargetHydro)
    ReferenceHydroStats = get.statistics(data = ReferenceHydro)
    
    ComparisonStats = get.comparison(stats.t = TargetHydroStats,
                                     stats.r = ReferenceHydroStats)
    return(list(Comparison=ComparisonStats, 
                Target=TargetHydro,
                Reference=ReferenceHydro)) 
    })
  
  
  ##### Table ####
  
  #' Render table showing hydrodynamics of comparison data
  output$comparison.table.target <- DT::renderDataTable(
    rownames = F,
    {
      if (bool.overlap()) {
        ComparisonStats = ComparisonStats()[["Comparison"]]
        return(ComparisonStats %>% 
                 mutate_if(is.numeric,round, 2))
      } else {
        if (bool.no.reference() | bool.no.target()){
          tab.with.file.upload.message("Please upload your target AND reference data or select default data sets.",
                                       color = "blue", backgroundColor = "white")
        } else {
          tab.with.file.upload.message("No overlapping time window.",
                                       color = "blue", backgroundColor = "white")
        }

      }
    },
    options = list(dom = 't'),
  )
  
  #' Eventlistener to save hydrodynamics summary comparison
  #' (Hydrodynamics > Summary table)
  observeEvent(input$comparison.table.save, {
    save.csv(
      path = projectPath(),
      name = "Hydrodynamics_Comparison",
      csvObject = ComparisonStats()[["Comparison"]],
      ui.input = input
    )
  })
  
  
  #### Figures ####
  #### Inundation #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.inundation.comparison <- reactive({
    if (bool.no.target() | bool.no.reference()){
      plot.emptyMessage("No figure available. Please upload data.")
    } else {
      plot.inundationComparison(data.t = ComparisonStats()[["Target"]],
                                data.r = ComparisonStats()[["Reference"]])
    }
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.inundation.comparison <- renderPlot({
    fig.inundation.comparison()
  })
  
  #' Eventlistener to save plot with inundation data
  #' (Hydrodynamics > Target)
  observeEvent(input$save.fig.inundation.comparison, {
    name = "DailyInundation_Comparison"
    save.figure(
      path = projectPath(),
      name = name,
      plotObject = fig.inundation.comparison(),
      ui.input = input
    )
  })
  
  
  #### Current velocity #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.velocity.comparison <- reactive({
    if (bool.no.target() | bool.no.reference()){
      plot.emptyMessage("No figure available. Please upload data.")
    } else {
      plot.velocityComparison(data.t = ComparisonStats()[["Target"]],
                                data.r = ComparisonStats()[["Reference"]])
    }
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.velocity.comparison <- renderPlot({
    fig.velocity.comparison()
  })
  
  #' Eventlistener to save plot with inundation data
  #' (Hydrodynamics > Target)
  observeEvent(input$save.fig.velocity.comparison, {
    name = "CurrentVelocity_Comparison"
    save.figure(
      path = projectPath(),
      name = name,
      plotObject = fig.velocity.comparison(),
      ui.input = input
    )
  })
  
  
  #### Parameter bar plot #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.parameter.comparison <- reactive({
    if (bool.no.target() | bool.no.reference()){
      plot.emptyMessage("No figure available. Please upload data.")
    } else {
      plot.parameterComparison(stats.table = ComparisonStats()[["Comparison"]])
    }
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.parameter.comparison <- renderPlot({
    fig.parameter.comparison()
  })
  
  #' Eventlistener to save plot with inundation data
  #' (Hydrodynamics > Target)
  observeEvent(input$save.fig.parameter.comparison, {
    name = "Parameter_Comparison"
    save.figure(
      path = projectPath(),
      name = name,
      plotObject = fig.parameter.comparison(),
      ui.input = input
    )
  })
})
