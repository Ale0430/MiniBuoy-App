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
    if (!isTruthy(input$folder)) {
      showNotification("No folder has been selected yet",
                       type = "error")
    } else{
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
    }
    output$prjName <- renderPrint({
      projectName()
    })
  })
  
  
  ##############
  #### DATA ####
  ##############
  
  #### UPLOAD ####
  ##### Variables #####
  
  #' Reactive variables holding raw data
  #' If no data set is defined, use default data set
  
  rawData_T <- reactive({
    #print(input$fileTarget)
    if (is.null(input$fileTarget$datapath)) {
      defaultData_T = "./data/default_target.csv"
      print("Default TARGET data")
      dataT = get.ACCy(defaultData_T,
                       sep = ",",
                       skip = 27)
    } else {
      print("User TARGET data")
      dataT = get.rawData_T(input)
    }
    return(dataT)
  })
  
  rawData_R <- reactive({
    if (is.null(input$fileReference)) {
      defaultData_R = "./data/default_reference.csv"
      print("Default REFERENCE data")
      dataR = get.ACCy(defaultData_R,
                       sep = ",",
                       skip = 27)
    } else {
      print("User REFERENCE data")
      dataR = get.rawData_R(input)
    }
    return(dataR)
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
    return(values$Target)
  })
  
  Reference <- reactive({
    if (is.null(values$Reference)) {
      values$Reference <- rawData_R()
    }
    return(values$Reference)
  })
  
  #' Trigger to update reactive data when 'Use data'
  #' button is pressed
  #' Updates data for the whole App
  observeEvent(input$setData.R, { #@Marie: does not make sense with the previous functions
    print("Set data REFERENCE")
    Reference()
  })
  observeEvent(input$setData.T, {
    print("Set data TARGET")
    Target()
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
  
  tab.with.file.upload.message = function(message){
    m = matrix(
      data = c(
        message
      )
    )
    return(
      datatable(m) %>%
        formatStyle(
          1,
          color = 'red',
          backgroundColor = 'orange',
          fontWeight = 'bold'
        )
    )
  }
  
  get.rawData.sum = function(data, type){
    no.days = length(unique(data$date))
    if (no.days < 2){
      showNotification(paste("Error:", type, "data set too small to analyze (< 2 dayss)!", sep = " "),
                       type = "error",
                       duration = NULL, closeButton = T)
      tab = tab.with.file.upload.message("Error: Data set too small to analyze (< 2 dayss)!")
    } else {
      if (no.days < 15){
        showNotification(paste("Warning:", type, "data set too small to analyze properly (< 15 days)!", sep = " "),
                         type = "warning",
                         duration = NULL, closeButton = T)
      }
      tab = data.frame(Variable = c("No. rows", "No. columns",
                                    "No. days",
                                    "First date", "Last date",
                                    "Mean Acc. (Min, Max)"),
                       Value = c(as.character(nrow(data)),
                                 paste(ncol(data), " (", 
                                       paste(colnames(data), collapse = ", "), ")",
                                       sep = ""),
                                 as.character(no.days),
                                 as.character(min(data$datetime)),
                                 as.character(max(data$datetime)),
                                 paste(as.character(round(mean(data$Acceleration), 3)), " (",
                                       as.character(min(data$Acceleration)), ", ",
                                       as.character(max(data$Acceleration)), ")", 
                                       collapse = "")))
    }
    
    return(tab)
  }
  
  output$raw.target.sum <- DT::renderDataTable(
    rownames = F,
    {
      rawDataTable_T = rawData_T()
      
      if (is.numeric(rawDataTable_T$Acceleration)){
        return(get.rawData.sum(rawDataTable_T, "TARGET"))
      } else {
        return(tab.with.file.upload.message("An error occured. Please check your upload settings (e.g. number of lines skipped) and required column names."))
      }
    },
    options = list(dom = 't'),
  )
  
  output$raw.reference.sum <- DT::renderDataTable(
    rownames = F,
    {
      rawDataTable_R = rawData_R()
      if (is.numeric(rawDataTable_R$Acceleration)){
        return(get.rawData.sum(rawDataTable_R, "REFERENCE"))
      } else {
        return(tab.with.file.upload.message("An error occured. Please check your upload settings (e.g. number of lines skipped) and required column names."))
      }
    },
    options = list(dom = 't'),
  )
  
  
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
    if (input$filterPlot_renderPlot == 0){
      plot.emptyMessage("Customize your figure.")
    } else {
      filterPlot()
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
  
  
  
  
})
