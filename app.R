library(shiny)
library(DT)
library(plotly)
library(data.table)
library(crosstalk)

### UI function ---------
ui <- fluidPage(
  fluidRow(
    plotlyOutput('my_graph', height = '400px')
  ),
  fluidRow(
    dataTableOutput('my_table')
  )
)

### Server function -------
server <- function(input, output, session) {
  
  ### SharedData object ----
  filtered_data <- reactive({
    data.table(mtcars, keep.rownames = TRUE)
  }) 
  
  shared_data <- reactive({
    req(filtered_data())
    SharedData$new(filtered_data(), ~rn)
  })
  
  ### my_graph ----
  output$my_graph <- renderPlotly({
    plot_ly(shared_data(),
            x = ~disp,
            y = ~mpg,
            color = ~factor(carb))
    
  }) 
  
  ### my_table --------- 
  output$my_table <- renderDataTable({
    datatable(shared_data()$data(),
              selection = 'single')
  })
  
  observe({
    click_detect = plotly::event_data('plotly_hover')
    str(click_detect)
    
    dataTableProxy('my_table') %>%
      selectRows(match(click_detect$key, shared_data()$data()$rn))
  })
  
  observeEvent(input$my_table_rows_selected, {
    row_selected <- shared_data()$data()[input$my_table_rows_selected, ]
    str(row_selected)
    plotlyProxy('my_graph', session) %>%
      plotlyProxyInvoke('addTraces', list(x = list(row_selected$disp), 
                                          y = list(row_selected$mpg),
                                          type = 'scatter',
                                          mode = 'markers',
                                          showlegend = FALSE,
                                          color = '#000000'))
  })
  
  
}

shinyApp(ui, server)