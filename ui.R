

source("set-up.R")

shinyUI(
  dashboardPage(
    skin="green",
    dashboardHeader(title = "The Mini Buoy App"),
    dashboardSidebar(
      br(), br(), br(),
      sidebarMenu(menuItemOutput()),
      br(), br(), br(),
      br(), br(), br(), tags$hr(),
      tags$footer("Geographical and Earth Sciences", align = "center"),
      tags$footer("University of Glasgow", align = "center"),
      tags$footer("2021 -2023", align = "center"),
      tags$hr()
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "about",
                introOutput()),
        tabItem(tabName = "sett",
                settingsOutput()),
        
        tabItem(tabName = "dat_upl",
                dataUplOutput()),
        tabItem(tabName = "dat_filter",
               dataFilterOutput()),
        tabItem(tabName = "hidr_res",
                HidrologyOutput())
      
    ),
    
    fluidPage(
      tags$script(src = "project-settings.js"),
      tags$link(href = "styles.css",
                rel = "stylesheet")
    )
    
    )
))