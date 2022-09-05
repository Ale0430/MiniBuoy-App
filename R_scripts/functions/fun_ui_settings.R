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
                   
                   p("Select a folder to create a project. The project folder will contain all the files that will be saved during the further use of the app."),
                   
                   shinyDirButton('folder', 
                                  'Browse to select a project folder', 
                                  'Please select a folder'),
                   actButton("crtPrj", "Create project", "create"),
                   br(), br(),
                   h4("Project folder"),
                   verbatimTextOutput("prjName"),
                   h4("Working directory"),
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


