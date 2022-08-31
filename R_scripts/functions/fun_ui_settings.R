themes <- list("Bw" = theme_bw(),
               "Classic" = theme_classic(),
               "Grey" = theme_gray(),
               "Dark" = theme_dark(),
               "Light" = theme_light(),
               "Minimal" = theme_minimal(),
               "Void" = theme_void()
)
################
### SETTINGS ###
################
settingsOutput = function(){
  return(
    fluidRow(
      column(7,
             fluidRow(
               box(title = "Project",
                   status = "warning", solidHeader = F, #height = 300,
                   collapsible = T, width = 12,
                   
                   p("Select a folder to create or set a project"),
                   
                   shinyDirButton('folder', 
                                  'Folder select', 
                                  'Please select a folder', 
                                  # multiple = FALSE,
                                  style = buttonStyles("red"),
                                  icon = icon("folder-open")),
                   actButton("crtPrj", "Create/set project", "create"),
                   br(), br(),
                   h4("Current project"),
                   verbatimTextOutput("prjName"),
                   h4("Current project directory"),
                   verbatimTextOutput("prjDir")
               )
                            )),
      column(5,
             fluidRow(
               box(title="File output",
                   status="warning", solidHeader = F,
                   collapsible = T, width=12,
                   p(em("Define attributes for output files (optional)")),
                   selectInput("figFor", "Figure format",
                               c("jpg"="jpg",
                                 "rdata" = "rdata",
                                 "pdf" = "pdf")),
                   textInput("figTitle", "Figure title",
                             placeholder = "e.g. study site"),
                   radioButtons("fileAppend", "String added to file names",
                             choices = c("Use file name" = "inputName",
                                         "Manual" = "manual",
                                         "None" = "none")),
                   conditionalPanel(condition = "input.fileAppend == `manual`",
                                    textInput("fileAppendName", "File name",
                                              placeholder = "e.g. sampling dates"))),
               box("Visualization",
                   status="warning", solidHeader = F,
                   collapsible = T, width=12,
                   selectInput("figTheme", "Figure theme (ggplot)",
                               choices=names(themes),
                               selected = themes["Light"]),
                   uiOutput('theme_output'),
                   textInput("fillColors", "Fill colors for discrete data*",
                             placeholder = 'Hex colors, comma delimited: #CD5C5C, #FFBF00, #6495ED'),
                   textInput("gradientColors", "Colors for gradient color scale**",
                             placeholder = 'Hex colors, comma delimited: #CD5C5C, #FFBF00'),
                   p("* Colors can be either hex colors or a RColorBrewer palette, 
                         e.g. 'Blues'"),
                   p("** Two colors representing low and high values."),
                   
                 
               
             )))
    )
  )
}


