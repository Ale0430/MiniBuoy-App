themes <- list("Black & White" = theme_bw(),
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
                   
                   p("1. Select a folder to create a project. The project folder will contain all the files that will be saved during the further use of the app."),
                   
                   shinyDirButton('folder', 
                                  'Browse to select a project folder', 
                                  'Please select a folder',
                                  style = paste(buttonStyles("leafgreen"),
                                                "margin-bottom: 2rem",
                                                sep = ";"),
                                  icon = icon("folder-open",
                                              style = "margin-right:.5em")),
                   p("2. Now create a new project in this folder location"),
                   actButton("crtPrj", "Create project", "create"),
                   br(),
                   h4("Project folder"),
                   verbatimTextOutput("prjName"),
                   h4("Working directory"),
                   verbatimTextOutput("prjDir")
               )
                            )),
      column(5,
             fluidRow(
               box(title="File output (optional)",
                   status="warning", solidHeader = F,
                   collapsible = T, collapsed = T, 
                   width=12,
                   p(em("Define attributes for output files (optional)")),
                   selectInput("figFor", "Figure format",
                               c("jpg"="jpg",
                                 "rdata" = "rdata",
                                 "pdf" = "pdf")),
                   textInput("figTitle", "Figure title",
                             placeholder = "e.g. study site"),
                   radioButtons("fileAppend", "Prefix for all files exported in this App",
                             choices = c("Use file name" = "inputName",
                                         "Manual" = "manual",
                                         "None" = "none")),
                   conditionalPanel(condition = "input.fileAppend == `manual`",
                                    textInput("fileAppendName", "Prefix",
                                              placeholder = "e.g. sampling dates"))),
               box(title = "Visualization (optional)",
                   status="warning", solidHeader = F,
                   collapsible = T, collapsed = T, 
                   width=12,
                   selectInput("figTheme", "Figure theme",
                               choices=names(themes),
                               selected = themes["Light"])#,
                   # uiOutput('theme_output'),
                   # textInput("fillColors", "Fill colors for discrete data*",
                   #           placeholder = 'Hex colors, comma delimited: #CD5C5C, #FFBF00, #6495ED'),
                   # textInput("gradientColors", "Colors for gradient color scale**",
                   #           placeholder = 'Hex colors, comma delimited: #CD5C5C, #FFBF00'),
                   # p("* Colors can be either hex colors or a RColorBrewer palette, 
                   #       e.g. 'Blues'"),
                   # p("** Two colors representing low and high values."),
                   
                 
               
             )))
    )
  )
}


