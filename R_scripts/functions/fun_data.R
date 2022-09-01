#' Wrapper function to get uploaded raw data based on selected
#' method
#' @param UI-input: file, file-args, method, etc.
#' @return data.frame
get.rawData_T = function(input) {
   an.error.occured = F
   if (input$inputType == "MB1") {
      tryCatch({
         rawDataTarget  = get.ACCy(input$file1$datapath,
                                   sep = input$sep,
                                   skip = input$skip)
      },
      error = function(e) {
         an.error.occured <<- TRUE
      })
      # print(an.error.occurred)
      
   }
   if (input$inputType == "MB2" |
       input$inputType == "MB3") {
      tryCatch({
         rawDataTarget  = get.ACCy(input$file1$datapath,
                                   sep = input$sep,
                                   skip =  input$skip)
      },
      error = function(e) {
         an.error.occured <<- TRUE
      })
      
   }
   
   if (an.error.occured) {
      return(data.frame())
   } else {
      return(rawDataTarget)
   }
}

get.rawData_R = function(input) {
   an.error.occured = F
   if (input$inputType == "MB1") {
      tryCatch({
         rawDataRef  = get.ACCy(input$file2$datapath,
                                sep = input$sep,
                                skip = input$skip)
      },
      error = function(e) {
         an.error.occured <<- TRUE
      })
      # print(an.error.occurred)
      
   }
   if (input$inputType == "MB2" |
       input$inputType == "MB3") {
      tryCatch({
         rawDataRef  = get.ACCy(input$file2$datapath,
                                sep = input$sep,
                                skip = input$skip)
      },
      error = function(e) {
         an.error.occured <<- TRUE
      })
      
   }
   
   if (an.error.occured) {
      return(data.frame())
   } else {
      return(rawDataRef)
   }
}

#' Reads raw data (datetime & Acceleration (ACCy)) .
#' @param file: uploaded file
#' @param sep: symbol to use as separator
#' @param skip: number of rows to skip
#' @return data.frame
get.ACCy = function(file, sep, skip) {
   rawData <- read.csv(
      file,
      header = FALSE,
      sep = sep,
      fileEncoding = "latin1",
      skip = skip
   )
   rawData <- rawData[, 1:2]
   colnames(rawData) <- c("datetime", "Acceleration")
   
   
   rawData = suppressWarnings(unify.datetime(rawData))
   
   return(rawData)
}


unify.datetime = function(rawData) {
   if ("datetime" %in% tolower(names(rawData))) {
      print("Transform datetime to date and time")
      datetimeformat = get.datetime.format(rawData[1, ]$datetime)
      rawData$datetime = as.POSIXct(rawData$datetime,
                                    format = datetimeformat)
      rawData$date = strftime(rawData$datetime, format = "%d-%m-%Y")
      rawData$time = strftime(rawData$datetime, format = "%H:%M:%S")
      
      dateformat = get.date.format(rawData[1, ]$date)
      rawData$date = as.Date(rawData$date,
                             format =  "%d-%m-%Y")
      
   } else {
      if (all(c(tolower("time"), tolower("date")) %in% tolower(names(rawData)))) {
         print("Add datetime based on date and time")
         
         date_col = rawData[grepl("(?i)date", colnames(rawData))]
         time_col = rawData[grepl("(?i)time", colnames(rawData))]
         # Remove upper case columns if exist
         rawData[, names(date_col)] = NULL
         rawData[, names(time_col)] = NULL
         # Define columns with lower case names for further processing
         rawData$date = date_col[[1]]
         rawData$time = time_col[[1]]
         
         datetimeformat = get.datetime.format(rawData[1, ]$date, rawData[1, ]$time)
         dateformat = get.date.format(rawData[1, ]$date)
         
         rawData = rawData %>%
            rowwise() %>%
            mutate(
               datetime = as.POSIXct(x = paste(date, time),
                                     format = datetimeformat),
               date = as.Date(date,
                              format = dateformat)
            )
      }
   }
   
   # add hour of day and date of year
   rawData$dTime = convertTimeToDeci(as.character(rawData$time))
   rawData$doy <-
      as.numeric(strftime(rawData$datetime, format = "%j"))
   #round(dTime, 2)
   # Reorder columns
   rawData = data.frame(rawData) %>%
      relocate(datetime, date, time, doy, dTime)
   
   return(rawData)
}

#' #' Function to convert string to data-time format
#' #' @param date: datetime or date as string
#' #' @param time: time as string
#' #' @return datetime object
get.datetime.format = function(date, time = "") {
   # datetime = rawData[1, c("Date", "Time")]
   if (!is.na(as.POSIXct(x = paste(date, time),
                         format = "%d.%m.%Y %H:%M:%S"))) {
      return("%d.%m.%Y %H:%M:%S")
   }
   if (!is.na(as.POSIXct(x = paste(date, time),
                         format = "%d/%m/%Y %H:%M:%S"))) {
      return("%d/%m/%Y %H:%M:%S")
   }
   if (!is.na(as.POSIXct(x = paste(date, time),
                         format = "%d.%m.%Y %H:%M"))) {
      return("%d.%m.%Y %H:%M")
   }
   if (!is.na(as.POSIXct(x = paste(date, time),
                         format = "%d/%m/%Y %H:%M"))) {
      return("%d/%m/%Y %H:%M")
   }
   if (!is.na(as.POSIXct(x = paste(date, time),
                         format = "%Y-%m-%d %H:%M:%S"))) {
      return("%Y-%m-%d %H:%M:%S")
   }
}

#' Function to convert string to data format
#' @param date: datetime or date as string
#' @return datetime object
get.date.format = function(date) {
   # datetime = rawData[1, c("Date", "Time")]
   if (!is.na(as.POSIXct(x = date,
                         format = "%d.%m.%Y"))) {
      return("%d.%m.%Y")
   }
   if (!is.na(as.POSIXct(x = date,
                         format = "%d/%m/%Y"))) {
      return("%d/%m/%Y")
   }
   if (!is.na(as.POSIXct(x = date,
                         format = "%d.%m.%Y"))) {
      return("%d.%m.%Y")
   }
   if (!is.na(as.POSIXct(x = date,
                         format = "%d/%m/%Y"))) {
      return("%d/%m/%Y")
   }
   if (!is.na(as.POSIXct(x = date,
                         format = "%Y-%m-%dS"))) {
      return("%Y-%m-%d")
   }
}

#' Convert character time HH:MM:SS to decimal time
#' @param time: time object or character
#' @return numeric
#'
convertTimeToDeci <- function(time) {
   dt = sapply(strsplit(time, ":"),
               function(x) {
                  x <- as.numeric(x)
                  x[1] + x[2] / 60 + x[3] / 3600
               })
   dt = round(dt, 2)
   return(dt)
}

#' ########### CLEAN #############
#'
#' #' Remove outlier
#' #' @description Function that removes outlier of a defined variable
#' #' @param data: data.frame with long-format data
#' #' @param data.vector: character indicating column name of selected variable
#' #' @return data.frame
remove.outlier <-
   function(data,
            data.vector,
            data.group,
            group.value) {
      data.sub = data
      if (data.group != "none") {
         data.sub = data %>%
            filter(get(data.group) == group.value)
      }
      
      d = data.sub %>%
         select(data.vector) %>% unlist(.)
      
      Q <- quantile(d, probs = c(.25, .75), na.rm = T)
      iqr <- IQR(d, na.rm = T) # = Q[2]-Q[1]
      up <-  Q[2] + 1.5 * iqr # Upper Range
      low <- Q[1] - 1.5 * iqr # Lower Range
      
      data.sub <- subset(data.sub, data.sub[, data.vector] > low &
                            data.sub[, data.vector] < up)
      return(data.sub)
   }


#' Filter
#' 'replace_na(TRUE)' in filter function avoids removing NA-rows of the
#' variable under consideration
#' @description Function to filter uploaded data set
#' @param data: data.frame with long-format data
#' @param ui.input: UI-input
#' @return data.frame
get.filteredData <- function(data, ui.input) {
   # remove na-values
   if (ui.input$removeNA) {
      data = data[complete.cases(data),]
   }
   # #    data$doy <- as.numeric(data$doy)   # filter by day/ doy and day time
   minDoy = as.numeric(strftime(ui.input$daterange[1], format = "%j"))
   maxDoy = as.numeric(strftime(ui.input$daterange[2], format = "%j"))
   
   data = data %>%
      filter((doy >= minDoy) %>% replace_na(TRUE)) %>%
      filter((doy <= maxDoy) %>% replace_na(TRUE))
   
   start = ui.input$timerangeStart
   end = ui.input$timerangeEnd
   if (start < end) {
      data = data %>%
         filter((dTime >= start &
                    dTime <= end) %>% replace_na(TRUE))
      
   } else {
      data = data %>%
         filter((dTime >= start |
                    dTime <= end) %>% replace_na(TRUE))
      
   }
   
   # remove outlier
   if (ui.input$removeOutlier) {
      data.vector = ui.input$filterPlot_X
      
      # Grouping variable = Color
      data.group = ui.input$filterPlot_col
      # Set number of groups = 1 if grouping variable does not
      # exist in data
      group.values = if (data.group %in% colnames(data))
         unique(data[, data.group])
      else
         1
      
      data = do.call(rbind,
                     lapply(group.values, function(x)
                        remove.outlier(data, data.vector, data.group, x)))
   }
   
   return(data)
}

#' Filter UI
#' #' @description Function to update data filter in UI based on uploaded data set
#' #' @param ui.input: UI-input
#' #' @param ui.output: UI-output
#' #' @return UI-output
update.filter.ui = function(ui.output, ui.input) {
   ui.output$filterOptions <- renderUI({
      req(ui.input$LoadFilter)
      
      tagList(
         checkboxInput("removeNA", "Remove NA-rows", T),
         
         # Date and time range
         h4(strong("Time filters")),
         
         fluidRow(# Date
            column(2, p(
               strong('Date')
            )),
            column(
               10, dateRangeInput("daterange", "Range")
            )),
         
         fluidRow(
            # Time of day
            column(2, p(strong('Time'))),
            column(5, numericInput("timerangeStart", "Start", value = 0)),
            column(5, numericInput("timerangeEnd", "End", value = 24)),
         ),
         
         
         # General filters
         checkboxInput("removeOutlier", "Remove outliers of plot variable", F),
         
         
         
         
         # Buttons
         fluidRow(column(5, (
            actButton("FilterApply", "Apply filter", "update")
         )),
         column(5, (
            actButton("FilterDelete", "Delete filter", "update")
         )),),
         htmlOutput("dataPoints")
      )
   })
   return(ui.output)
}

#' ########### SAVE #############

get.notifications = function(ui.input, path) {
   # Check if project directory is defined
   # If not show warning and set path to root directory
   if (!isTruthy(ui.input$folder)) {
      noti_note = "No project selected. File saved in root directory."
      noti_type = "warning"
      
   } else {
      noti_note = "File saved successfully!"
      noti_type = "message"
   }
   return(list(noti_note, noti_type))
}

get.filename = function(path, name, format, ui.input) {
   format = paste(".", format, sep = "")
   fileAppendix = get.fileAppendix(ui.input)
   # Add appendix to file name and replace whitespace
   if (fileAppendix != "") {
      fileAppendix = gsub(" ", "_", fileAppendix, fixed = TRUE)
      name = paste(fileAppendix, name, sep = "_")
   }
   
   # Check if file already exists, if yes append unique number
   # based on system time
   if (file.exists(paste(path, "/", name, format, sep = ""))) {
      unique_number = gsub("\\.", "", as.numeric(Sys.time()))
      name = paste(name, unique_number, sep = "_")
   }
   
   filename = paste(path, "/", name, format, sep = "")
   return(filename)
}


#' Get string/name added to file
get.fileAppendix = function(ui.input) {
   if (ui.input$fileAppend == "manual") {
      return(ui.input$fileAppendName)
   } else if (ui.input$fileAppend == "inputName") {
      if (!is.null(ui.input$file1$datapath)) {
         # Extract file name (additionally remove file extension using sub)
         return(sub(".csv$", "", basename(ui.input$file1$name)))
      } else {
         return("DefaultFile")
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
save.figure = function(path, name, plotObject, ui.input) {
   plotObject = plotObject +
      ggtitle(ui.input$figTitle) +
      theme(text = element_text(size = 14))
   
   format = ui.input$figFor
   nots = get.notifications(ui.input)
   if (nots[[2]] == "message") {
      path = paste(path, "graphics", sep = "/")
   }
   filename = get.filename(
      path = path,
      name = name,
      format = format,
      ui.input = ui.input
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
   
   if (is.null(res) || (res == filename)) {
      showNotification(nots[[1]],
                       type = nots[[2]])
   } else {
      showNotification("Error: File not saved!",
                       type = "error")
   }
}


#' #' Save csv
#' #' @description Handler to save data.frames as csv-file.
#' #' @return success-message
#' #' @param name: file name
#' #' @param csvObject: object to be saved, i.e. data.frame
#' #' @param fileAppendix: character to be appended to file name
save.csv = function(path, name, csvObject, ui.input) {
   # Gets list(noti_note, noti_type, path)
   nots = get.notifications(ui.input)
   if (nots[[2]] == "message") {
      path = paste(path, "csv-files", sep = "/")
   }
   filename = get.filename(path, name, "csv", ui.input)
   
   res = try(write.csv(csvObject,
                       file = filename,
                       row.names = FALSE))
   if (is.null(res)) {
      showNotification(nots[[1]],
                       type = nots[[2]])
   } else {
      showNotification("Error: File not saved!",
                       type = "error")
   }
}
