server <- function(input, output){
  
  values <- reactiveValues()
  values$disciplinas <- disciplinas
  
  
  observe({
    if(input$selectAllBottom > 0) {
      updateCheckboxGroupInput(session=session, inputId="disciplinas", 
                               choices=disciplinas, selected=disciplinas)
      values$disciplinas <- disciplinas
    }
  })
  
  
  observe({
    if(input$clearAllBottom > 0) {
      updateCheckboxGroupInput(session=session, inputId="disciplinas", 
                               choices=disciplinas, selected=NULL)
      values$disciplinas <- c()
    }
  })
  
  # Create event type checkbox
  output$disciplinasControl <- renderUI({
    checkboxGroupInput('disciplinas', 'Selecione as disciplinas:', 
                       disciplinas, selected = values$disciplinas)
  })
  
  # Prepare dataset
  dataTable <- reactive({
    groupByDisciplina(fy, input$year[1], 
                      input$year[2], input$disciplinas)
  })
  
  output$dTable <- renderDataTable({
    dataTable()
  })
  
  output$media <- renderDataTable({media1})
  
  selectData <- reactive({
    if(input$anos %in% fy$`Ano Ingresso`)
      return(fy %>% 
               filter(`Ano Ingresso` == input$anos))
  })
  
  output$plot <- renderPlot({
    p <-ggplot(selectData(), aes(x = CR, y = Notas, fill = Período)) + geom_boxplot() + facet_grid(. ~ Período) + 
      theme(legend.position = "none")
    
    print(p)
  })
  
}