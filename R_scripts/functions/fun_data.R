#' Wrapper function to get uploaded raw data based on selected
#' method
#' @param UI-input: file, file-args, method, etc.
#' @return data.frame
get.rawData_T = function(input) {
   return(get.rawData(inputType = input$inputType.T, 
                      file = input$fileTarget$datapath, 
                      sep = input$sep.T, 
                      skip = input$skip.T))
}

get.rawData_R = function(input) {
   return(get.rawData(inputType = input$inputType.R, 
                      file = input$fileReference$datapath, 
                      sep = input$sep.R, 
                      skip = input$skip.R))
}

get.rawData = function(inputType, file, sep, skip) { # @Marie: needs to be checked when we receive raw data
   an.error.occured = F
   if (inputType == "MB1") {
      tryCatch({
         rawData  = get.ACCy(file = file,
                             sep = sep,
                             skip = skip)
      },
      error = function(e) {
         an.error.occured <<- TRUE
      })
   }
   if (inputType == "MB2" |
       inputType == "MB3") {
      tryCatch({
         rawData  = get.ACCy(file = file,
                             sep = sep,
                             skip =  skip)
      },
      error = function(e) {
         an.error.occured <<- TRUE
      })
      
   }
   if (an.error.occured) {
      return(data.frame())
   } else {
      return(rawData)
   }
}

#' Reads raw data (datetime & Acceleration (ACCy)) .
#' @param file: uploaded file
#' @param sep: symbol to use as separator
#' @param skip: number of rows to skip
#' @return data.frame
get.ACCy = function(file, sep, skip) {
   rawData <- fread(
      file,
      header = FALSE,
      sep = sep,
      skip = skip
   )
   rawData <- rawData[, 1:2]
   #@Ale: always 1+2? > yes, except used downloaded wrong data > @Marie: throw warning if ncol > 2
   colnames(rawData) <- c("datetime", "Acceleration")
   
   rawData = suppressWarnings(unify.datetime(rawData))
   
   return(rawData)
}


unify.datetime = function(rawData){
   print("Transform datetime to date and time")
   rawData$datetime = fastPOSIXct(rawData$datetime, tz="GMT")
   rawData$date = lubridate::as_date(rawData$datetime)
   rawData$time = format(rawData$datetime, format = "%H:%M:%S") #@Marie @Ale: too slow?
   return(rawData)
}


#' Convert character time HH:MM:SS to decimal time
#' @param time: time object or character
#' @return numeric
convertTimeToDeci <- function(time) { #@Marie: delete?
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
remove.outlier <- #@Marie: delete? > ask cai and thorsten
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
get.filteredData <- function(data, ui.input, filetype) {
   # remove na-values
   if (ui.input[[paste("removeNA", filetype, sep = ".")]]) {
      data = data[complete.cases(data),]
   }

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
update.filter.ui = function(ui.output, ui.input, filetype) {
   fO = paste("filterOptions", filetype, sep = ".")
   ui.output[[fO]] <- renderUI({
      #req(ui.input$LoadFilter)
      print("In load filters")
      tagList(
         checkboxInput(paste("removeNA", filetype, sep = ".")
                       , "Remove NA-rows", T),
         
         # Date and time range
         h5(strong("Time filters")),
         
         fluidRow(# Date
            column(2, p(
               strong('Date')
            )),
            column(
               10, dateRangeInput(paste("daterange", filetype, sep = ".")
                                  , "Range")
            )),
         
         fluidRow(
            # Time of day
            column(2, p(strong('Time'))),
            # column(5, numericInput(paste("timerangeStart", filetype, sep = ".")
            #                        , "Start", value = 0)),
            # column(5, numericInput(paste("timerangeEnd", filetype, sep = ".")
            #                        , "End", value = 24)),
            column(5, textInput(paste("timerangeStart", filetype, sep = "."),
                                "Start", value = "00:00:00", placeholder = "00:00:00")),
            column(5, textInput(paste("timerangeEnd", filetype, sep = "."),
                                "End", value = "00:00:00", placeholder = "23:59:00"))
         ),
         
         
         # Buttons
         fluidRow(column(5, (
            actButton(paste("FilterApply", filetype, sep = ".")
                      , "Apply filter", "update")
         )),
         column(5, (
            actButton(paste("FilterDelete", filetype, sep = ".")
                      , "Delete filter", "update")
         )),),
         htmlOutput(paste("dataPoints", filetype, sep = "."))
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
