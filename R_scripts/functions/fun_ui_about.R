

#### ABOUT ####

introOutput = function(){
  fluidRow(
    box(title = "The Mini Buoy" ,
        status = "info", 
        solidHeader = F, 
        width = 6, 
        height = "100%",
        collapsible = T,
        includeMarkdown("./man/MinibuoyIntro.md"),
        column(12, img(src = 'MiniBuoyMotion.png', width = "100%"), align = "center")
    ),
    box(title = "Mini Buoy Models",
        status = "warning", 
        solidHeader = F,
        collapsible = T, 
        width = 6, 
        height = "100%",
        box.settings_sensors())
  )
}


box.settings_sensors = function() {
  return(
    list(
      selectInput(
        "sensorType",
        "Select sensor type to view attributes",
        choices = c("B4", "B4+", "Pendant"),
        selected = "B4"
      ),
      
      conditionalPanel(
        condition = "input.sensorType == 'B4'",
        br(),
        p(
          "The original Mini Buoy design featured in Balke et al. (2021) that contains an MSR145 B4 acceleration data logger inside a self-standing centrifuge tube attached to an anchor via a fishing swivel. The B4 measures inundation duration and current velocity only."
        ),
        column(12, img(src = 'B4.jpg', width = "80%"), align = "center")
      ),
      
      conditionalPanel(
        condition = "input.sensorType == 'B4+'",
        br(),
        p(
          "A more durable version of the original Mini Buoy ideal for long term deployments. The B4+ has a UV-resistant casing without the skirt and a metal eye bolt connected to a mooring by crimped fishing line rings. In addition to measuring inundation duration and current velocity, the B4+ has been calibrated to measure wave orbital velocities. Whilst the durability and functionality may be improved, the B4+ requires more effort to assemble."
        ),
        column(12, img(src = 'B4plus.jpeg', width = "80%"), align = "center")
      ),
      
      conditionalPanel(
        condition = "input.sensorType == 'Pendant'",
        br(),
        p(
          "An integrated accelerometer data logger, float, and anchor point, attached to a pole by a fishing swivel. The Pendant is less expensive and easier to assemble than the B4 and B4+, however memory capacity and sampling rates are lower. Because of the low sampling rate, the Pendant measures inundation duration and current velocity only."
        ),
        column(12, img(src = 'Pendant.jpg', width = "80%"), align = "center")
      ),
      
      br(),
      br()
      
    )
  )
}
