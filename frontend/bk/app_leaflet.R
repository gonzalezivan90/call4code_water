# install.packages(c('shiny', 'leaflet'))
library(shiny)
library(leaflet)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("ff", width = "100%", height = "100%")
)
server <- function(input, output,session) {
  output$ff <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron")%>%
      setView(lng = -111.65, lat = 35.18, zoom = 20)
  })
  #Show popup on click
  observeEvent(input$ff_click, {
    click <- input$ff_click
    text<-paste("Lattitude:", round(click$lat, 2),
                "<br>Longtitude:", round(click$lng, 4),
                "<br>Date:", Sys.Date())
    proxy <- leafletProxy("ff")
    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
}

shinyApp(ui = ui, server = server)
