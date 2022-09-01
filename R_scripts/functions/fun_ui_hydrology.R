

HidrologyOutput<- function(){
  return(list(
    fluidRow(
      box(title="Some claibration param here (change to warning)",
          collapsible=T, status = "info",
          includeMarkdown("./man/MiniBuoyIntro.Rmd"))
    )
  ))
}