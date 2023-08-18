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
  return(list(
    fluidRow(
      column(12,
             fluidRow(
               box(title = "Project",
                   status = "warning", solidHeader = F, #height = 300,
                   collapsible = T, width = 12,
                   
                   h5(strong("Select folder to save output")), # Step 1: Select a folder to create a project where all output will be saved
                   
                   shinyDirButton('folder', 
                                  'Select folder', 
                                  'Please select a folder',
                                  style = paste(buttonStyles("green"),
                                                "margin-bottom: 2rem",
                                                sep = ";"),
                                  icon = icon("folder-open",
                                              style = "margin-right:.5em")),
                   verbatimTextOutput("prjDir"),
                   br(),
                   h5(strong("Create a new project folder in this location")),
                   actButton("crtPrj", "Create project", "create"),
                   br(),
                   verbatimTextOutput("prjName")
               )
                            ))),
    fluidRow(
      column(12,
             fluidRow(
               box(title="Default settings",
                   status="warning", solidHeader = F,
                   collapsible = T, collapsed = T, 
                   width=12,
                   # p(em("Define attributes for output files (optional)")),
                   selectInput("fileFor", "File format",
                               c("csv"="csv",
                                 "xlsx" = "xlsx")),
                   radioButtons("fileAppend", "File prefix",
                                choices = c("Use file name" = "inputName",
                                            "Manual" = "manual",
                                            "None" = "none")),
                   conditionalPanel(condition = "input.fileAppend == `manual`",
                                    textInput("fileAppendName", "Prefix",
                                              placeholder = "e.g. sampling dates")),
                   selectInput("figFor", "Figure format",
                               c("jpg"="jpg",
                                 "rdata" = "rdata",
                                 "pdf" = "pdf")),
                   textInput("figTitle", "Figure title",
                             placeholder = "e.g. study site"),
                   selectInput("figTheme", "Figure theme",
                               choices=names(themes),
                               selected = themes["Light"]),
                   uiOutput('theme_output'))
             )))
  ))
}
