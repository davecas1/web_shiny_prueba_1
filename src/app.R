library(shiny)
library(quantmod)
library(stochvol)

seleccion_datos <- c("PDX.ST", "CABK.MC", "UBI.PA")

ui <- fluidPage(
  selectInput("dataset", label = "Selecciona la acción que quieras analizar", choices = seleccion_datos),
  verbatimTextOutput("summary"),
  tableOutput("table"),
  plotOutput("plot_id"),
  verbatimTextOutput("vol_msg"),
  plotOutput("plot_vol")
)

server <- function(input, output, session) {
  datos_reactivos <- reactive({
    getSymbols(input$dataset, src = "yahoo", auto.assign = FALSE)
  })
  datos_close <- reactive({
    precios <- Cl(datos_reactivos())
    rend <- diff(log(precios))
    rend <- rend[!is.na(rend)]
    rend <- rend[rend != 0]
    as.numeric(rend)
  })
  modelo_reactivo <- reactive({
    svlsample(y = datos_close(),draws = 5000,burnin = 500)
  })
  
  output$summary <- renderPrint({
    summary(datos_reactivos())
  })
  
  output$table <- renderTable({
    head(datos_reactivos())
  })
  
  output$plot_id <- renderPlot({
    datos <- datos_reactivos()
    chartSeries(datos, name = input$dataset, theme = chartTheme("white"))
  })
  
  output$vol_msg <- renderPrint({
    "Se procederá a realizar una estimación de la volatilidad de forma estocástica."
  })
  
  output$plot_vol <- renderPlot({
    plot(modelo_reactivo())
  })
}

shinyApp(ui, server)
