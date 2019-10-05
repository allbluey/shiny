



#tenho que fazer uma mistura do select input e do checkboxinput para construir o painel

disciplinas <- unique(fy$Disciplinas)

sidebarLayout(
  sidebarPanel(
    sliderInput("year", "Selecione o ano de ingresso:", min = min(fy$`Ano Ingresso`), max = max(fy$`Ano Ingresso`),
                value = 2005),
    actionButton(inputId = "clearAllTop", 
                 label = "Clear selection", 
                 icon = icon("square-o")),
    actionButton(inputId = "selectAllTop",
                 label = "Select all",
                 icon = icon("check-square-o")),
    uiOutput("disciplinasControl"), # the id
    actionButton(inputId = "clearAllBottom",
                 label = "Clear selection",
                 icon = icon("square-o")),
    actionButton(inputId = "selectAllBottom",
                 label = "Select all",
                 icon = icon("check-square-o"))
  ),
  mainPanel(
      dataTableOutput(outputId="dTable")
    )
)
    #checkboxGroupInput("disciplinas", "Selecione a disciplina:", fy$Disciplinas, selected = fy$Disciplinas)))






#server
values <- reactiveValues()
values$disciplinas <- disciplinas

observe({
  if(input$selectAllTop > 0) {
    updateCheckboxGroupInput(session=session, inputId="disciplinas", 
                             choices= disciplinas, selected=  disciplinas)
    values$disciplinas <- disciplinas
  }
})
observe({
  if(input$selectAllBottom > 0) {
    updateCheckboxGroupInput(session=session, inputId="disciplinas", 
                             choices=disciplinas, selected=disciplinas)
    values$disciplinas <- disciplinas
  }
})

observe({
  if(input$clearAllTop > 0) {
    updateCheckboxGroupInput(session=session, inputId="disciplinas", 
                             choices=disciplinas, selected=NULL)
    values$disciplinas <- c()
  }
})
observe({
  if(input$clearAllBottom > 0) {
    updateCheckboxGroupInput(session=session, inputId="disciplinas", 
                             choices= disciplinas, selected=NULL)
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
  groupBydisciplinas(fy, input$year[1], 
               input$year[2], input$disciplinas)
})


output$dTable <- renderDataTable({
  dataTable()
} #, options = list(bFilter = FALSE, iDisplayLength = 50)
)

#output$notas <- DT::renderDataTable({
  #DT::datatable(fy[, input$disciplinas, drop = FALSE])})




