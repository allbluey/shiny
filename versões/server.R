server <- function(input, output){
  output$notas = DT::renderDataTable({fy})
  
  output$media = renderDataTable({media})
}