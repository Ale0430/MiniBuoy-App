#' Wrapper function to get uploaded raw data based on selected
#' method
#' @param UI-input: file, file-args, method, etc.
#' @return data.frame
get.rawData_T = function(input) {
   return(get.rawData(inputType = input$inputType_T, 
                      file = input$fileTarget$datapath))
}

get.rawData_R = function(input) {
   return(get.rawData(inputType = input$inputType_R, 
                      file = input$fileReference$datapath))
}

get.rawData = function(inputType, file) { # @Marie: needs to be checked when we receive raw data
   an.error.occured = F
   if (inputType == "B4" |
       inputType == "B4+") {
      tryCatch({
         rawData  = get.ACCy.B4(file = file)
      },
      error = function(e) {
         an.error.occured <<- TRUE
      })
   }
   if (inputType == "Pendant") {
      tryCatch({
         rawData  = get.ACCy.Pendant(file = file)
      },
      error = function(e) {
         an.error.occured <<- TRUE
      })
   }
   if (an.error.occured) {
      return(data.frame())
   } else {
     # Transform datetime
     rawData = suppressWarnings({
       rawData%>%
       mutate(across(starts_with("datetime"), 
                     ~lubridate::parse_date_time(.,orders = c("mdy_HMS", "ymd_HMS"))))
     })
     # Remove NA rows
     rawData = rawData[complete.cases(rawData),]
     return(rawData)
   }
}


#' Reads raw data (datetime & Acceleration (ACCy)) .
#' @param file: uploaded file
#' @return data.frame
get.ACCy.B4 = function(file) {
  # read filtered data:
  if(readLines(file, n = 1) == c('datetime,Acceleration')) { rawData = fread(file)
  # read raw data:
  } else {
    # find column names:
    names = as.character(
      fread(file, 
            skip   = grep('*CHANNEL', readLines(file, n = 100)),
            nrows  = 1,
            header = F))
    # read the raw data:
    rawData = fread(file, skip = "*DATA", header = F)
    # assign column names:
    colnames(rawData) <- names
    # merge date and time if columns are separate:
    if(is.POSIXct(rawData$TIME) == F) {
      rawData = rawData %>%
        unite('TIME',  c(1, 2), sep = ' ', remove = T) %>%
        mutate(TIME = ymd_hms(TIME)) }
    # ensure only datetime and y-axis acceleration columns are used:
    rawData = rawData %>% select(any_of(c('TIME', 'ACC y', 'Ay')))
    colnames(rawData) <- c('datetime', 'Acceleration') }
  return(rawData)
}


#' Reads raw data (datetime & Acceleration (ACCx)) .
#' @param file: uploaded file
#' @return data.frame
get.ACCy.Pendant = function(file) {
  # read filtered data:
  if(readLines(file, n = 1) == c('datetime,Acceleration')) { rawData = fread(file) 
  # read raw data:
  # read raw data:
  } else {
    # select data from the row containing headers;
    rawData = fread(file, skip = "#")
    # select only date/time and x-axis acceleration columns:
    rawData = select(rawData, contains(c('Date', 'X Accel')))
    # assign column names to the first 2 columns:
    colnames(rawData)[c(1,2)] <- c("datetime", "Acceleration") }
  return(rawData)
}



#' Function to calculate the time window overlap of 
#' target and reference data in days
#' Shows notification with result 
#' @param data.t data.frame containing target data
#' @param data.r data.frame containing reference data
#' @return vector with start and end of overlapping time window
get.time.overlap = function(data.t, data.r){
   timerange.t = c(min(data.t$datetime, na.rm = T), max(data.t$datetime, na.rm = T))
   timerange.r = c(min(data.r$datetime, na.rm = T), max(data.r$datetime, na.rm = T))
   # Test if recordings of reference and target overlap
   if ((min(timerange.t) <= max(timerange.r)) & (max(timerange.t) >= min(timerange.r))){
      sorted.times = sort(c(timerange.t, timerange.r))[c(2,3)]
   } else {
      sorted.times = c()
   }
   return(sorted.times)
}


#' Creates a table with a summary of raw data
#' (shown in Data > Upload > Summary of the data)
#' @param data data.frame containing data set
#' @param type reference or target
#' @return dataframe with summary
get.rawData.sum = function(data, type){
   no.days = round(max(data$datetime, na.rm = T) - min(data$datetime, na.rm = T), 2)
   sampling.rate = as.numeric(data$datetime[2] - data$datetime[1], units = 'secs')
   if (no.days < 2){
      showNotification(paste(type, "data is too short to analyse. A minimum duration of 2 days is needed.", sep = " "),
                       type = "error", duration = 5, closeButton = T)
      tab = tab.with.file.upload.message("Data is too short to analyse. A minimum duration of 2 days is needed.")
   } else {
      if (ncol(data) > 4){
         showNotification(paste(type, 
                                "data contains more than 2 columns. Only columns 1 (datetime) and 2 (acceleration) are used for further processing.",
                                sep = " "),
                          type = "warning", duration = 5, closeButton = T)
      }
      if (no.days < 15){
         showNotification(paste(type, 
                                "data is less than 15 days, which is too short for a robust analysis.", 
                                sep = " "),
                          type = "warning", duration = 5, closeButton = T)
      }
      
      coln = paste(tolower(colnames(data)), collapse = ", ")
      meanAcc = mean(data$Acceleration, na.rm = T)
      mAmm = paste(as.character(round(meanAcc, 2)), " (",
                   as.character(round(min(data$Acceleration, na.rm = T), 2)), ", ",
                   as.character(round(max(data$Acceleration, na.rm = T), 2)), ")", 
                   collapse = "")
      mAqq = paste(as.character(round(median(data$Acceleration, na.rm = T), 2)), " (",
                   as.character(round(quantile(data$Acceleration, 0.25, na.rm = T), 2)), ", ",
                   as.character(round(quantile(data$Acceleration, 0.55, na.rm = T), 2)), ")", 
                   collapse = "")
      
      tab = data.frame(Variable = c("Column names",
                                    "Sampling rate (seconds)",
                                    "Survey length (days)",
                                    "First date and time", "Last date and time",
                                    "Mean acceleration (min, max)",
                                    "Median acceleration (1st and 3rd quantile)",
                                    "Number of recordings"),
                       Value = c(coln,
                                 as.numeric(difftime(data$datetime[2], data$datetime[1], units = 'secs')),
                                 as.character(no.days),
                                 as.character(min(data$datetime, na.rm = T)),
                                 as.character(max(data$datetime, na.rm = T)),
                                 mAmm,
                                 mAqq,
                                 as.character(nrow(data))))
      colnames(tab) = c("Data summary", "")
      if (meanAcc > 0){
         showNotification(paste("Mean Acceleration in data set", type, 
                                "is more than 0. Did you install the acceleration loger upside down inside the Mini Buoy?", sep = " "),
                          type = "warning",
                          duration = 5, closeButton = T)
      }
   }
   
   return(tab)
}


#' ########### FILTER #############

#' Filter by time
#' 'replace_na(TRUE)' in filter function avoids removing NA-rows of the
#' variable under consideration
#' @description Function to filter uploaded data set
#' @param data: data.frame with long-format data
#' @param ui.input: UI-input
#' @return data.frame
get.filteredData <- function(data, ui.input, filetype) {
   # Filter by date and time of first and last day
   daterange = ui.input[[paste("daterange", filetype, sep = ".")]]
   start = ui.input[[paste("timerangeStart", filetype, sep = ".")]]
   end = ui.input[[paste("timerangeEnd", filetype, sep = ".")]]

   startdt = fastPOSIXct(paste(daterange[1], start, sep = " "), tz="GMT")
   endtdt = fastPOSIXct(paste(daterange[2], end, sep = " "), tz="GMT")
   
   data = data %>%
      filter((datetime >= startdt) %>% replace_na(TRUE)) %>%
      filter((datetime <= endtdt) %>% replace_na(TRUE))
   
   return(data)
}

#' Filter UI
#' #' @description Function to update data filter in UI based on uploaded data set
#' #' @param ui.input: UI-input
#' #' @param ui.output: UI-output
#' #' @param filetype: "R" or "T"
#' #' @return UI-output
update.filter.ui = function(ui.output, ui.input, filetype, minMaxDatetime) {
   fO = paste("filterOptions", filetype, sep = ".")
   
   time.start = as.character(format(minMaxDatetime[1], format = "%H:%M:%S"))
   time.end = as.character(format(minMaxDatetime[2], format = "%H:%M:%S"))
   
   ui.output[[fO]] <- renderUI({
      tagList(
         # Date and time range
         h5(strong("Select start and end datetimes of the survey")),
         
         fluidRow(# Date
            column(2, p(strong('Date'))),
            column(10, 
                   dateRangeInput(inputId = paste("daterange", filetype, sep = "."), 
                                  label = "Range",
                                  start = minMaxDatetime[1],
                                  end = minMaxDatetime[2],
                                  min = minMaxDatetime[1],
                                  max = minMaxDatetime[2])
            )),
         
         fluidRow(
            # Time of day
            column(2, p(strong('Time'))),
            column(5, textInput(paste("timerangeStart", filetype, sep = "."),
                                "Start", value = time.start, placeholder = time.start)),
            column(5, textInput(paste("timerangeEnd", filetype, sep = "."),
                                "End", value = time.end, placeholder = time.end))
         ),
         
         actButton(ID =paste("FilterApply", filetype, sep = "."),
                   label="Apply filter", type="update"),
         actButton(ID =paste("FilterDelete", filetype, sep = "."),
                   label="Remove filter", type="update"),

         output.html(paste("dataPoints", filetype, sep = "."))
      )
   })
   return(ui.output)
}


#' ########### SAVE #############

get.notifications = function(ui.input, path) {
   # Check if project directory is defined
   # If not show warning and set path to root directory
   if (!isTruthy(ui.input$folder)) {
      noti_note = "No project selected. File has saved in the root directory."
      noti_type = "warning"
      
   } else {
      noti_note = "File saved successfully"
      noti_type = "message"
   }
   return(list(noti_note, noti_type))
}

get.filename = function(path, name, format, ui.input, file=NULL) {
   format = paste(".", format, sep = "")
   fileAppendix = get.fileAppendix(ui.input, file)
   # Add appendix to file name and replace whitespace
   if (fileAppendix != "") {
      fileAppendix = gsub(" ", "_", fileAppendix, fixed = TRUE)
      name = paste(fileAppendix, name, sep = "_")
   }
   filename = paste(path, "/", name, format, sep = "")
   return(filename)
}


#' Get string/name added to file
get.fileAppendix = function(ui.input, file = NULL) {
  if (ui.input$fileAppend == "manual") {
    return(ui.input$fileAppendName)
  } else if (ui.input$fileAppend == "inputName") {
    if (is.null(file)) {
      return("")
    } else {
      # Extract file name (additionally remove file extension using sub)
      return(sub(".csv$", "", basename(file$name)))
    }
  } else {
    return("")
  }
}

#' Save figure
#' @description Handler to save ggplots as as jpg, svg or pdf
#' @return success-message
#' @param name: file name
#' @param plotObject: object to be saved, i.e. ggplot-object
#' @param fileAppendix: character to be appended to file name
#' @param format: file format
#' @param prjName: project name, added as title to plot
save.figure = function(path, name, plotObject, ui.input, file=NULL, noMessage=F) {
   showNotification("Saving...", type = "default")
   plotObject = plotObject +
      ggtitle(ui.input$figTitle) +
      theme(text = element_text(size = 14))

   format = ui.input$figFor
   nots = get.notifications(ui.input)
   if (nots[[2]] == "message") {
      path = paste(path, "figures", sep = "/")
   }
   filename = get.filename(
      path = path,
      name = name,
      format = format,
      ui.input = ui.input,
      file = file
   )

   if (format == "rdata") {
      res = try(save(plotObject, file = filename))
   } else {
      res = try(ggsave(
         plotObject,
         filename = filename,
         width = 12,
         height = 6,
         dpi = 900
      ))
   }
   if (!noMessage){
     if (is.null(res) || (res == filename)) {
       showNotification(nots[[1]],
                        type = nots[[2]])
     } else {
       showNotification("Error: File not saved!",
                        type = "error")
     }
   }
}


#' #' Save csv
#' #' @description Handler to save data.frames as csv-file.
#' #' @return success-message
#' #' @param name: file name
#' #' @param csvObject: object to be saved, i.e. data.frame
#' #' @param fileAppendix: character to be appended to file name
save.csv = function(path, name, csvObject, ui.input, file = NULL, noMessage=F) {
  showNotification("Saving...",
                   type = "default")
   # Gets list(noti_note, noti_type, path)
   nots = get.notifications(ui.input)
   if ("datetime" %in% colnames(csvObject)){
     csvObject$datetime = as.character(csvObject$datetime)
   }
   if (nots[[2]] == "message") {
      path = paste(path, "tables", sep = "/")
   }
   filename = get.filename(path, name, "csv", ui.input, file)
   
   res = try(fwrite(csvObject,
                    file = filename))
   if (!noMessage){
     if (is.null(res)) {
       showNotification(nots[[1]],
                        type = nots[[2]])
     } else {
       showNotification("File not saved.",
                        type = "error")
     }   
   }
  
}

save.xlsx = function(path, name, csvObject, ui.input, file = ""){
  format = ui.input$fileFor
  
  # Gets list(noti_note, noti_type, path)
  nots = get.notifications(ui.input)
  if (nots[[2]] == "message"){
    path = paste(path, "tables", sep = "/")
  }
  
  filename = get.filename(path, name, "xlsx", ui.input, file)
  res = try(write_xlsx(csvObject,
                       path = filename))
  
  if (file.exists(filename)){
    showNotification(nots[[1]], 
                     type = nots[[2]])
  } else {
    showNotification("File not saved.",
                     type = "error")
  }
}
