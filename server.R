# Define file upload limit, now 15 MB
options(shiny.maxRequestSize = 15 * 1024 ^ 4)

shinyServer(function(input, output, session) {
  # Project Settings  ####
  ## Variables        ####
  
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
    cat("No project selected")
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
  
  
  ## UI output        ####
  
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
      cat("No directory selected")
    } else {
      cat(projectPath())
    }
  })
  
  ## Buttons        ####
  
  #' Button to create a project (Project Settings > Project)
  #' Requires a folder to be selected (Folder select)
  #' If directory does not exist create two folders:
  #' 'table-files', 'graphics'
  #' Sets project name = project folder name
  observeEvent(input$crtPrj, {
    # If no folder have been selected show error
    if (!isTruthy(input$folder) | is.null(projectName())) {
      showNotification("No project has been created. Please first select a folder directory.",
                       type = "error")
      output$prjName <- renderPrint({
        cat("No project selected")
      })
    } else {
      req(input$folder)
      csvPath = paste(projectPath(),
                      "/tables/", sep = "")
      figPath = paste(projectPath(),
                      "/figures/", sep = "")
      if (!dir.exists(csvPath)) {
        dir.create(csvPath)
      }
      if (!dir.exists(figPath)) {
        dir.create(figPath)
      }
      showNotification("A project has been created.",
                       type = "message")
      output$prjName <- renderPrint({
        cat(projectName())
      })
    }
  })
  
  
  # Data        ####
  ## Upload        ####
  ### Variables        ####
  
  #' Variable indicating which Mini Buoy Design is selected
  get.design.T <- reactive({
    if (input$raw_default_T){
      return("B4+")
    } else {
      return(input$inputType_T)
    }
  })
  
  get.design.R <- reactive({
    if (input$raw_default_R){
      return("B4+")
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
      print("Uploading raw target data failed.")
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
      print("Uploading raw reference data failed.")
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
  message.upload.fail = "Could not read file. Is the file in the correct format? Please refer to the Mini Buoy Handbook."
  message.upload.no.data = function(type){
    return(paste(type, " data not uploaded.", sep = ""))
  }
  
  observeEvent(input$setData.T, {
    rawData_T = rawData_T()
    if (is.null(rawData_T)) {
      if (bool.file.upload.target()) {
        showNotification(
          message.upload.fail,
          type = "error",
          duration = 5,
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
          duration = 5,
          closeButton = T
        )
      } else {
        showNotification(
          "Uploaded target data successfully.",
          type = "message",
          duration = 5,
          closeButton = T
        )
      }
    }
    Target = Target()
    filter_helper.T(input, output)
    
    
  })

  observeEvent(input$setData.R, {
    rawData_R = rawData_R()
    if (is.null(rawData_R)) {
      if (bool.file.upload.reference()) {
        showNotification(
          message.upload.fail,
          type = "error",
          duration = 5,
          closeButton = T
        )
      } else {
        showNotification(
          message.upload.no.data("REFERENCE"),
          type = "error",
          duration = 5,
          closeButton = T
        )
      }
    } else {
      if (all(is.na(rawData_R$Acceleration))){
        showNotification(
          message.upload.fail,
          type = "error",
          duration = 5,
          closeButton = T
        )
      } else {
        showNotification(
          "Uploaded reference data successfully.",
          type = "message",
          duration = 5,
          closeButton = T
        )
      }
    }
    Reference = Reference()
    filter_helper.R(input, output)
    
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
  
  
  ### Table Outputs        ####

  tab.with.file.upload.message = function(message, color = "#cc0000",
                                          backgroundColor = '#ffcccc'){
    m = matrix(
      data = c(
        message
      )
    )
    return(
      datatable(m, options = list(dom = 't', ordering = F), colnames=NULL) %>%
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
      rawDataTable_T = rawData_T()
      if (is.numeric(rawDataTable_T$Acceleration) & !all(is.na(rawDataTable_T$Acceleration))){
        t = get.rawData.sum(rawDataTable_T, "TARGET")
        return(t)
      } else {
        return(tab.with.file.upload.message(message.upload.fail))
      }
    },
    options = list(dom = 't', ordering=F),
  )
  
  output$raw.reference.sum <- DT::renderDataTable(
    rownames = F,
    {
      rawDataTable_R = rawData_R()
      if (is.numeric(rawDataTable_R$Acceleration) & !all(is.na(rawDataTable_R$Acceleration))){
        t = get.rawData.sum(rawDataTable_R, "REFERENCE")
        return(t)
      } else {
        return(tab.with.file.upload.message(message.upload.fail))
      }
    },
    options = list(dom = 't'),
  )
  
  
  output$previewTarget <- renderUI({
    if (input$raw_default_T | !is.null(input$fileTarget$datapath)){
      list(tags$head(tags$style(HTML("#previewTarget > br{display:none}"))),
           output.table("raw.target.sum"),
           actButton("setData.T", "Use data", "create"))
    }
  })
  
  output$previewReference <- renderUI({
    if (input$raw_default_R | !is.null(input$fileReference$datapath)){
      list(tags$head(tags$style(HTML("#previewReference > br{display:none}"))),
           output.table("raw.reference.sum"),
           actButton("setData.R", "Use data", "create"))
    }
  })
  
  #' Eventlistener: if both, target and reference data, are set
  #' calculate overlapping time window and show warning or error
  #' message
  observeEvent(input$setData.T | input$setData.R, {
    # &req needed because buttons are set on server side and not 
    # available from beginning
    req(isTruthy(input$setData.T) & isTruthy(input$setData.R))
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
          "Time windows of the target and reference data do not overlap.",
          type = "warning",
          duration = 5,
          closeButton = T
        )
      } else {
        showNotification(
          paste(
            "Time windows of the target and reference data overlap by",
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
  
  
  ## Filter        ####
  
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
  
  ### UI        ####
  
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
  

  ### Text        ####
  
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
  
  output$filterEmpty.T <- renderText({ 
    if (is.null(rawData_T())){
      paste(message.upload.no.data("TARGET"))
    } else if (input$setData.T == 0){
      paste(message.upload.no.data("TARGET"),
            "Hint: you might forgot to click `Use data` when uploading your data set.")
    }
  })
  
  output$filterEmpty.R <- renderText({ 
    if (is.null(rawData_R())){
      paste(message.upload.no.data("REFERENCE"))
    } else if (input$setData.R == 0){
      paste(message.upload.no.data("REFERENCE"),
            "Hint: you might forgot to click `Use data` when uploading your data set.")
    }
  })
  
  ### Plots        ####
  
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
        duration = 5,
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
        plot.emptyMessage("Customise your figure.")
      } else {
        plot.filteredRawData(data = data,
                             ui.input = input)
      }
    }
  })

  #' Render plot shown in data > filter
  output$filterPlot <- renderPlotly({
    filterPlot()
  })
  
  
  
  
  ### Buttons        ####
  
  #' Eventlistener to save filtered data
  #' (Data > Filter)
  #' Not saved as excel format as max. number of rows in limited to ~1 Mil.
  observeEvent(input$save_dat_filter, {
    save.csv(path = projectPath(), 
              name =  paste(
                as.character(input$filterPlot_DataSet),
                "Raw",
                sep = "_"
              ),
              csvObject =  DataSetInput(),
              ui.input = input)
  })
  
  #' Eventlistener to save plot with filtered data
  #' (Data > Filter)
  observeEvent(input$save_dat_filter_fig, {
    name = paste(
      as.character(input$filterPlot_DataSet),
      'Raw',
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
  
  
  # Hydrodynamics        ####
  ## Info Texts        ####
  
  
  
  text.upload.missing = "No analysis available. Please upload data."
  text.too.short = "Uploaded/ filtered data set < 2 days."
  
  ## Functions for both R&T   ####
  
  #' Shortens data set to overlapping time window
  get.overlapping.data = function(dataset) {
    time.overlap = get.time.overlap(data.t = Target(),
                                    data.r = Reference())
    return(dataset %>%
             filter(datetime >= time.overlap[1] &
                      datetime <= time.overlap[2]))
  }
  
  ## Target        ####
  ### UI        ####
  
  #' Render output option depending on data availability
  output$hydro.window.target.show = renderUI({
    if (!bool.no.target() & !bool.no.reference()) {
      checkboxInput(
        "hydro.window.target",
        "Use overlapping target and reference data",
        F
      )
    }
  })
  
  output$hydro.text.target <- renderUI({
    if (bool.no.target()){
      print(text.upload.missing)
    } else if (is.null(TargetHydroStats())){
      print(text.too.short)
    } else {
      HTML(get.stats.text(TargetHydroStats(), design = get.design.T()))
    }
  })
  
  ### Variables        ####
  
  TargetCustomInput = reactive({
    return(data.frame(gaps = input$hydro.set.gaps.target,
                      full = input$hydro.set.full.target,
                      part = input$hydro.set.part.target,
                      tilt = input$hydro.set.tilt.target)
    )
  })
  
  TargetHydro = reactive({
    if (is.null(values$TargetHydro)){
      print("TARGET hydro: create")
      values$TargetHydro = get.hydrodynamics(data = Target(),
                                             design = get.design.T(),
                                             ui.input_settings = "Default")
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
                                               design = get.design.T(),
                                               ui.input_settings = "Default")
      }}
  })
  
  observeEvent(input$FilterDelete.T, {
    if (!is.null(input$FilterDelete.T[1]) & !is.null(values$TargetHydro)){
      if (input$FilterDelete.T[1] != 0){
        print("TARGET hydro: update with full data")
        values$TargetHydro = get.hydrodynamics(data = Target(),
                                               design = get.design.T(),
                                               ui.input_settings = "Default")
      }}
  })
  

  observeEvent(input$hydro.set.apply.target, {
    if (input$hydro.set.tilt.target < 0 | input$hydro.set.tilt.target > 90){
      showNotification("Minimum tilt needs to be a value between 0 and 90°.",
                       type = "error")
    } else {
      print("TARGET hydro: update with custom settings")
      Target = Target()
      if (!is.null(input$hydro.window.target)){
        if (input$hydro.window.target) {
          print("TARGET hydro: update with overlapping data")
          Target = get.overlapping.data(dataset = Target)
        }
      }
      values$TargetHydro = data.frame()
      values$TargetHydro = get.hydrodynamics(data = Target,
                                             design = get.design.T(),
                                             ui.input_settings = TargetCustomInput())      
    }
  })
  
  observeEvent(input$hydro.set.reset.target, {
    print("TARGET hydro: update with default settings")
    values$TargetHydro = data.frame()
    if (!is.null(input$hydro.window.target)){
      updateCheckboxInput(session, "hydro.window.target", value = F)
    }
    values$TargetHydro = get.hydrodynamics(data = Target(),
                                           design = get.design.T(),
                                           ui.input_settings = "Default")
  })
 
  TargetHydroStats <- reactive({
    TargetHydro = TargetHydro()
    timewindow = difftime(max(TargetHydro$datetime), min(TargetHydro$datetime), units = "days")
    if (timewindow < 2){
      TargetHydroStats = data.frame()
    } else {
      TargetHydroStats = get.summary.statistics(TargetHydro, design = get.design.T())
      }
    return(TargetHydroStats)
  })
  
  ### Table     ####

  #' Render table showing hydrodynamics of target data
  output$hydro.table.target <- DT::renderDataTable(
    rownames = F,
    {
      if (bool.no.target()){
        return(tab.with.file.upload.message(text.upload.missing,
                                            color = "black", backgroundColor = "white"))
      } else if (is.null(TargetHydroStats())) {
        return(tab.with.file.upload.message(text.too.short,
                                            color = "black", backgroundColor = "white"))
      } else {
        return(TargetHydroStats() %>%
                 mutate_if(is.numeric, round, 2))
      }
    },
    options = list(dom = "ltip"), 
  )
  
  get.xlsx.object.target = reactive({
    TargetHydro = TargetHydro() %>% dplyr::select(-'FullDay')
    stats.daily = get.daily.statistics(TargetHydro, design = get.design.T()) %>% left_join(TargetHydro() %>% group_by(floor_date(datetime, 'days')) %>% summarise(FullDay = unique(FullDay)) %>% rename(datetime = 1), by = 'datetime')
    stats.event = get.event.statistics(TargetHydro, design = get.design.T())
    stats.summary = TargetHydroStats()
    stats.tidal = get.tidal.statistics(TargetHydro)
    stats.woo = get.woo.statistics(TargetHydro)
    settings = data.frame(gaps = input$hydro.set.gaps.target,
                          full = input$hydro.set.full.target,
                          part = input$hydro.set.part.target,
                          tilt = input$hydro.set.tilt.target)
    sheets = list('Summary' = stats.summary, 
                  'Daily'   = stats.daily, # %>% rename('Date' = 'datetime')
                  'Events'  = stats.event,
                  'Tides'   = stats.tidal,
                  'Windows' = stats.woo,
                  'All' = TargetHydro, # %>% rename('Date' = 'datetime')
                  'Settings' = settings)
    return(sheets)
  })
  
  #' Eventlistener to save hydrodynamics summary target
  #' (Hydrodynamics > Summary table)
  observeEvent(input$hydro.table.target.save, {
    tryCatch(
      {
        sheets = get.xlsx.object.target()
        if (input$fileFor == "xlsx"){
          save.xlsx(path = projectPath(), 
                    name = "Target",
                    csvObject = sheets,
                    ui.input = input)
        } else {
          for (n in names(sheets)) {
            noMessage = F
            if (n != names(sheets)[1]){
              noMessage = T
            }
            save.csv(path = projectPath(), 
                     name = paste("Target", n, sep ="_"),
                     csvObject = data.frame(sheets[[n]]),
                     ui.input = input)
          }
        }
      },
      error=function(e) {
        showNotification("No results available.",
                         type = "error")
      }
    )
  })

  ### Figures ####
  #### Inundation #####
  
  fig.helper.target = function(actual.plot.function){
    if (bool.no.target()){
      plot.emptyMessage("No figure available. Please upload data.")
    } else if (is.null(TargetHydroStats())) {
      plot.emptyMessage(text.too.short)
    } else {
      actual.plot.function
    }
  }
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.inundation.target <- reactive({
    fig.helper.target(plot.inundation(data = TargetHydro()))
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.inundation.target <- renderPlotly({
    fig.inundation.target()
  })
  

  #### Control ####
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.control.target <- reactive({
    fig.helper.target(plot.control(data = TargetHydro()))
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.control.target <- renderPlotly({
    fig.control.target()
  })
  
  
  #### Current velocity #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.velocity.target <- reactive({
    fig.helper.target(plot.velocity(data = TargetHydro(), 
                                    site = "Target"))
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.velocity.target <- renderPlotly({
    fig.velocity.target()
  })
  

  #### Wave orbital velocity #####

  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.wave.velocity.target <- reactive({
    design = get.design.T()
    if (design == "B4+"){
      fig.helper.target(plot.waveVelocity(data = TargetHydro(), 
                                          site = "Target"))
    } else {
      plot.emptyMessage(paste("'", design, 
                              "' Mini Buoy design does not measure wave orbital velocity",
                              sep = ""))
    }
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.wave.velocity.target <- renderPlotly({
    fig.wave.velocity.target()
  })
  
  
  
  #### Velocity stage plot #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.stage.target <- reactive({
    fig.helper.target(plot.stage(data   = TargetHydro(),
                                 design = get.design.T()))
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.stage.target <- renderPlot({
    fig.stage.target()
  })
  
  #### Windows of Opportunity plot #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.woo.target <- reactive({
    fig.helper.target(plot.woo(data = TargetHydro()))
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.woo.target <- renderPlot({
    fig.woo.target()
  })
  
  #### Save all plots #####
  
  #' Eventlistener to save hydro plots
  #' (Hydrodynamics > Target)
  observeEvent(input$save.figs.target, {
    save.figure(
      path = projectPath(),
      name = "Target_Classified",
      plotObject = fig.control.target(),
      ui.input = input
    )
    save.figure(
      path = projectPath(),
      name = "Target_DailyInundation",
      plotObject = fig.inundation.target(),
      ui.input = input
    )
    save.figure(
      path = projectPath(),
      name = "Target_CurrentVelocity",
      plotObject = fig.velocity.target(),
      ui.input = input
    )
    save.figure(
      path = projectPath(),
      name = "Target_WaveOrbitalVelocity",
      plotObject = fig.wave.velocity.target(),
      ui.input = input
    )
    save.figure(
      path = projectPath(),
      name = "Target_EbbFlood",
      plotObject = fig.stage.target(),
      ui.input = input
    )
    save.figure(
      path = projectPath(),
      name = "Target_Window",
      plotObject = fig.woo.target(),
      ui.input = input
    )
  })
  
  
  ## Reference      ####
  ### UI            ####
  
  
  output$hydro.window.reference.show = renderUI({
    if (!bool.no.target() & !bool.no.reference()) {
      checkboxInput(
        "hydro.window.reference",
        "Use overlapping target and reference data",
        F
      )
    }
  })
  
  ### Variables     ####
  
  ReferenceCustomInput = reactive({
    return(data.frame(gaps = input$hydro.set.gaps.reference,
                      full = input$hydro.set.full.reference,
                      part = input$hydro.set.part.reference,
                      tilt = input$hydro.set.tilt.reference)
    )
  })
  
  ReferenceHydro = reactive({
    if (is.null(values$ReferenceHydro)){
      print("REFERENCE hydro: create")
      values$ReferenceHydro = get.hydrodynamics(data = Reference(),
                                                design = get.design.R(),
                                                ui.input_settings = "Default")
    }
    return(values$ReferenceHydro)
  })
  
  observeEvent(input$FilterApply.R, {
    if (!is.null(input$FilterApply.R[1]) & !is.null(values$ReferenceHydro)){
      if (input$FilterApply.R[1] != 0){
        print("REFERENCE hydro: update with filtered data")
        values$ReferenceHydro = get.hydrodynamics(data = Reference(),
                                                  design = get.design.R(),
                                                  ui.input_settings = "Default")
      }}
  })
  
  observeEvent(input$FilterDelete.R, {
    if (!is.null(input$FilterDelete.R[1]) & !is.null(values$ReferenceHydro)){
      if (input$FilterDelete.R[1] != 0){
        print("REFERENCE hydro: update with full data")
        values$ReferenceHydro = get.hydrodynamics(data = Reference(),
                                                  design = get.design.R(),
                                                  ui.input_settings = "Default")
      }}
  })
  

  
  observeEvent(input$hydro.set.apply.reference, {
    if (input$hydro.set.tilt.reference < 0 | input$hydro.set.tilt.reference > 90){
      showNotification("Minimum tilt needs to be a value between 0 and 90°.",
                       type = "error")
    } else {
      print("REFERENCE hydro: update with custom settings")
      Reference = Reference()
      if (!is.null(input$hydro.window.reference)) {
        if (input$hydro.window.reference) {
          print("REFERENCE hydro: update with overlapping data")
          Reference = get.overlapping.data(dataset = Reference())
        }
      }
      values$ReferenceHydro = data.frame()
      values$ReferenceHydro = get.hydrodynamics(data = Reference,
                                                design = get.design.R(),
                                                ui.input_settings = ReferenceCustomInput())
    }
  })
  

  observeEvent(input$hydro.set.reset.reference, {
    print("REFERENCE hydro: update with default settings")
    values$ReferenceHydro = data.frame()
    if (!is.null(input$hydro.window.reference)){
      updateCheckboxInput(session, "hydro.window.reference", value = F)
    }
    values$ReferenceHydro = get.hydrodynamics(data = Reference(),
                                              design = get.design.R(),
                                              ui.input_settings = "Default")
  })
  
  
  ReferenceHydroStats <- reactive({
    ReferenceHydro = ReferenceHydro()
    timewindow = difftime(max(ReferenceHydro$datetime), min(ReferenceHydro$datetime), units = "days")
    if (timewindow < 2){
      ReferenceHydroStats = NULL
    } else {
      ReferenceHydroStats = get.summary.statistics(ReferenceHydro, design = get.design.R())
    }
    ReferenceHydroStats = get.summary.statistics(values$ReferenceHydro, design = get.design.R())
    return(ReferenceHydroStats)
  })
  
  
  ### Text          ####
  
  output$hydro.text.reference <- renderUI({
    if (bool.no.reference()){
      print(text.upload.missing)
    } else if (is.null(ReferenceHydroStats())){
      print(text.too.short)
    } else {
      HTML(get.stats.text(ReferenceHydroStats(), design = get.design.R()))
    }
  })
  
  
  ### Table         ####
  
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
    options = list(dom = 'ltip'),
  )
  
  get.xlsx.object.reference = reactive({
    ReferenceHydro = ReferenceHydro() %>% dplyr::select(-'FullDay')
    stats.daily = get.daily.statistics(ReferenceHydro, design = get.design.R()) %>% left_join(ReferenceHydro() %>% group_by(floor_date(datetime, 'days')) %>% summarise(FullDay = unique(FullDay)) %>% rename(datetime = 1), by = 'datetime')
    stats.event = get.event.statistics(ReferenceHydro, design = get.design.R())
    stats.summary = ReferenceHydroStats()
    stats.tidal = get.tidal.statistics(ReferenceHydro)
    stats.woo = get.woo.statistics(ReferenceHydro)
    settings = data.frame(gaps = input$hydro.set.gaps.reference,
                          full = input$hydro.set.full.reference,
                          part = input$hydro.set.part.reference,
                          tilt = input$hydro.set.tilt.reference)
    
    sheets = list('Summary' = stats.summary, 
                  'Daily'   = stats.daily, # %>% rename('Date' = 'datetime')
                  'Events'  = stats.event,
                  'Tides'   = stats.tidal,
                  'Windows' = stats.woo,
                  'All' = ReferenceHydro, # %>% rename('Date' = 'datetime')
                  'Settings' = settings)
    return(sheets)
  })
  
  #' Eventlistener to save hydrodynamics summary reference
  #' (Hydrodynamics > Summary table)
  observeEvent(input$hydro.table.reference.save, {
    tryCatch(
      {
        sheets = get.xlsx.object.reference()
        if (input$fileFor == "xlsx"){
          save.xlsx(path = projectPath(), 
                    name = "Reference",
                    csvObject =  sheets,
                    ui.input = input)
        } else {
          for (n in names(sheets)) {
            noMessage = F
            if (n != names(sheets)[1]){
              noMessage = T
            }
            save.csv(path = projectPath(), 
                     name = paste("Reference", n, sep ="_"),
                     csvObject = data.frame(sheets[[n]]),
                     ui.input = input)
          }
        }
      },
      error=function(e) {
        showNotification("No results available.",
                         type = "error")
      }
    )
  })
  
  ### Figures         ####
  
  fig.helper.reference = function(actual.plot.function){
    if (bool.no.reference()){
      plot.emptyMessage("No figure available. Please upload data.")
    } else if (is.null(ReferenceHydroStats())) {
      plot.emptyMessage(text.too.short)
    } else {
      actual.plot.function
    }
  }
  
  #### Inundation #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Reference
  fig.inundation.reference <- reactive({
    fig.helper.reference(plot.inundation(data = ReferenceHydro()))
  })
  
  #' Render plot shown in Hydrodynamics > Reference
  output$fig.inundation.reference <- renderPlotly({
    fig.inundation.reference()
  })
  
  #### Control ####
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.control.reference <- reactive({
    fig.helper.reference(plot.control(data = ReferenceHydro()))
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.control.reference <- renderPlotly({
    fig.control.reference()
  })
  

  #### Current velocity #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > reference
  fig.velocity.reference <- reactive({
    fig.helper.reference(plot.velocity(data = ReferenceHydro(), 
                                       site = "Reference"))
  })
  
  #' Render plot shown in Hydrodynamics > reference
  output$fig.velocity.reference <- renderPlotly({
    fig.velocity.reference()
  })
  

  #### Wave orbital velocity #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > reference
  fig.wave.velocity.reference <- reactive({
    design = get.design.R()
    if (design == "B4+"){
      fig.helper.reference(plot.waveVelocity(data = ReferenceHydro(), 
                                             site = "Reference"))
    } else {
      plot.emptyMessage(paste("'", design, 
                              "' Mini Buoy design does not measure wave orbital velocity",
                              sep = ""))
    }
  })
  
  #' Render plot shown in Hydrodynamics > reference
  output$fig.wave.velocity.reference <- renderPlotly({
    fig.wave.velocity.reference()
  })
  
  #### Velocity stage plot #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Reference
  fig.stage.reference <- reactive({
    fig.helper.reference(plot.stage(data   = ReferenceHydro(),
                                    design = get.design.R()))
  })
  
  #' Render plot shown in Hydrodynamics > Reference
  output$fig.stage.reference <- renderPlot({
    fig.stage.reference()
  })
  
  #### Windows of Opportunity plot #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Reference
  fig.woo.reference <- reactive({
    fig.helper.reference(plot.woo(data = ReferenceHydro()))
  })
  
  #' Render plot shown in Hydrodynamics > Reference
  output$fig.woo.reference <- renderPlot({
    fig.woo.reference()
  })
  
  #### Save all plots #####
  
  #' Eventlistener to save hydro plots
  #' (Hydrodynamics > Target)
  observeEvent(input$save.figs.reference, {
    save.figure(
      path = projectPath(),
      name = "Reference_DailyInundation",
      plotObject = fig.inundation.reference(),
      ui.input = input
    )
    save.figure(
      path = projectPath(),
      name = "Reference_Classified",
      plotObject = fig.control.reference(),
      ui.input = input
    )
    save.figure(
      path = projectPath(),
      name = "Reference_CurrentVelocity",
      plotObject = fig.velocity.reference(),
      ui.input = input
    )
    save.figure(
      path = projectPath(),
      name = "Reference_WaveOrbitalVelocity",
      plotObject = fig.wave.velocity.reference(),
      ui.input = input
    )
    save.figure(
      path = projectPath(),
      name = "Reference_EbbFlood",
      plotObject = fig.stage.reference(),
      ui.input = input
    )
    save.figure(
      path = projectPath(),
      name = "Reference_Window",
      plotObject = fig.woo.reference(),
      ui.input = input
    )
  })
  
  
  ## Comparison       ####
  ### Variables       ####
  
  #' Reactive function containing a list the statistical comparison
  #' and hydrodynamics of reference and target for overlapping time
  ComparisonStats <- reactive({ 
    TargetHydro = get.overlapping.data(dataset = TargetHydro())
    ReferenceHydro = get.overlapping.data(dataset = ReferenceHydro())
    
    TargetHydroStats = get.summary.statistics(data = TargetHydro, design = get.design.T())
    ReferenceHydroStats = get.summary.statistics(data = ReferenceHydro, design = get.design.R())
    
    ComparisonStats = get.comparison(hydro.t  = TargetHydro,
                                     hydro.r  = ReferenceHydro,
                                     stats.t  = TargetHydroStats,
                                     stats.r  = ReferenceHydroStats,
                                     design.t = get.design.T(),
                                     design.r = get.design.R())
    return(list(Comparison=ComparisonStats, 
                Target=TargetHydro,
                Reference=ReferenceHydro)) 
    })
  
  
  ### Text          ####
  
  output$hydro.text.comparison <- renderUI({
    if (bool.overlap()){
      HTML(get.comparison.text(ComparisonStats()[["Comparison"]]))
    } else if (bool.no.reference() | bool.no.target()){
      HTML("Please upload your target AND reference data or select default data sets.")
    } else {
      HTML("No overlapping time window.")
    }
  })
  
  output$hydro.text.table <- renderUI({
    if (bool.overlap()){
      HTML('Hydrodynamics of the reference and target sites are compared using <b>',
           round(ComparisonStats()[["Comparison"]][1, "Reference"], 1),
           '</b> days of overlapping data.<br/>')
    }
  })
  
  ### Table         ####
  

  #' Helper funtion to render background color in table according to comparison
  #' color with opacity = 50%
  table.background.js <- "(/higher/).test(value) ? '#56B4E950' : (/lower/).test(value) ? '#0072B250' : ''"
  
  
  #' Render table showing hydrodynamics of comparison data
  output$hydro.table.comparison <- DT::renderDataTable(
    rownames = F,
    {
      if (bool.overlap()) {
        ComparisonStats = ComparisonStats()[["Comparison"]] %>% 
          slice(2:n()) %>% 
          mutate_if(is.numeric,round, 2) %>% 
          rename("Meaningfully different" = "SignificantlyDifferent",
                 "Target is" = "TargetIs")
        return(ComparisonStats)
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
    options = list(dom = 't', ordering = F, 
                   columnDefs = list(list(className = 'dt-center', targets = 1:5))),
  )

  get.xlsx.object.comparison = reactive({
    ComparisonHydro = ComparisonStats()[["Comparison"]] %>% 
      slice(2:n()) %>% 
      mutate_if(is.numeric,round, 2) %>% 
      rename("Meaningfully different" = "SignificantlyDifferent",
             "Target is" = "TargetIs")
    sheets = list('Summary' = ComparisonHydro)
    return(sheets)
  })
  
  #' Eventlistener to save hydrodynamics summary comparison
  #' (Hydrodynamics > Summary table)
  observeEvent(input$hydro.table.comparison.save, {
    tryCatch(
      {
        sheets = get.xlsx.object.comparison()
        if (input$fileFor == "xlsx"){
          save.xlsx(path = projectPath(), 
                    name = "Comparison",
                    csvObject = sheets,
                    ui.input = input)
        } else {
          for (n in names(sheets)) {
            noMessage = F
            if (n != names(sheets)[1]){
              noMessage = T
            }
            save.csv(path = projectPath(), 
                     name = paste("Comparison", n, sep ="_"),
                     csvObject = data.frame(sheets[[n]]),
                     ui.input = input)
          }
        }
      },
      error=function(e) {
        showNotification("No results available.",
                         type = "error")
      }
    )
  })
  
  
  ### Figures           ####
  #### Inundation       ####
  
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
  output$fig.inundation.comparison <- renderPlotly({
    fig.inundation.comparison()
  })

  #### Current velocity #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.currents.comparison <- reactive({
    if (bool.no.target() | bool.no.reference()){
      plot.emptyMessage("No figure available. Please upload data.")
    } else {
      plot.currentsComparison(data.t = ComparisonStats()[["Target"]],
                              data.r = ComparisonStats()[["Reference"]])
    }
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.currents.comparison <- renderPlotly({
    fig.currents.comparison()
  })

  #### Wave orbital velocity #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.waves.comparison <- reactive({
    if (bool.no.target() | bool.no.reference()){
      plot.emptyMessage("No figure available. Please upload data.")
    } else {
      plot.wavesComparison(data.t = ComparisonStats()[["Target"]],
                           data.r = ComparisonStats()[["Reference"]])
    }
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.waves.comparison <- renderPlotly({
    fig.waves.comparison()
  })
  
  #### Parameter comparison (by event) #####
  
  #' Reactive variable holding the
  #' plot shown in Hydrodynamics > Target
  fig.parameters.comparison <- reactive({
    if (bool.no.target() | bool.no.reference()){
      plot.emptyMessage("No figure available. Please upload data.")
    } else {
      plot.parameterComparison(data.t = ComparisonStats()[["Target"]],
                               data.r = ComparisonStats()[["Reference"]],
                               design = get.design.T())
    }
  })
  
  #' Render plot shown in Hydrodynamics > Target
  output$fig.parameters.comparison <- renderPlotly({
    fig.parameters.comparison()
  })
  
  #### Save all plots #####

  #' Eventlistener to save comparison plots
  #' (Hydrodynamics > Comparison)
  observeEvent(input$save.fig.comparison, {
    
    save.figure(
      path = projectPath(),
      name = "Comparison_DailyInundation",
      plotObject = fig.inundation.comparison(),
      ui.input = input
    )

    save.figure(
      path = projectPath(),
      name = "Comparison_CurrentVelocity",
      plotObject = fig.currents.comparison(),
      ui.input = input
    )

    save.figure(
      path = projectPath(),
      name = "Comparison_WaveOrbitalVelocity",
      plotObject = fig.waves.comparison(),
      ui.input = input
    )
    
    save.figure(
      path = projectPath(),
      name = "Comparison_Parameters",
      plotObject = fig.parameters.comparison(),
      ui.input = input
    )
  })
  
})
