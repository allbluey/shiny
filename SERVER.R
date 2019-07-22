function(input, output) {
  
  
  # Selecoes graficos
  selected_indplot <- reactive({
    if(input$continente == "Africa")
      return(Ind_tab2%>% 
               filter(Country == input$paisAf & Year %in% c(1990,1995,2000,2005, 2010:2015)))
    
    else if (input$continente == "America")
      return(Ind_tab2%>% 
               filter(Country == input$paisAm & Year %in% c(1990,1995,2000,2005, 2010:2015)))
    
    else if (input$continente == "Asia")
      return(Ind_tab2%>% 
               filter(Country == input$paisAs & Year %in% c(1990,1995,2000,2005, 2010:2015)))
    
    else if (input$continente == "Europe")
      return(Ind_tab2%>% 
               filter(Country == input$paisEu & Year %in% c(1990,1995,2000,2005, 2010:2015)))
    
    else if (input$continente == "Oceania")
      return(Ind_tab2%>% 
               filter(Country == input$paisOc & Year %in% c(1990,1995,2000,2005, 2010:2015)))
  })
  
  
  ##Graficos DANI
  selected_cont <- reactive({
    if(input$cont == "Africa")
      return(africa)
    else if (input$cont == "America")
      return(america)
    else if (input$cont == "Asia")
      return(asia)
    else if (input$cont == "Europa")
      return(europa)
    else if (input$cont == "Oceania")
      return(oceania)
  })
  
  selected_leg <- reactive({
    if(input$cont == "Africa")
      return(africa_labels)
    else if (input$cont == "America")
      return(america_labels)
    else if (input$cont == "Asia")
      return(asia_labels)
    else if (input$cont == "Europa")
      return(europa_labels)
    else if (input$cont == "Oceania")
      return(oceania_labels)
  })
  
  #Tabelas
  output$tabind = DT::renderDataTable({Ind_tab2})
  
  output$tabhiv = renderDataTable({hiv})
  
  #output$tabcont = renderDataTable({ selected_cont() })
  
  
  #Grafico indices - aba2
  output$plotind <- renderDygraph({
    dygraph(selected_indplot()[,-c(1)]) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"), drawGrid = FALSE)
  })
  output$tind <- renderTable(selected_indplot()[,-1])
  
  
  #Grafico Doenças
  output$doe_brasil <- renderD3heatmap({
    d3heatmap(brazil, colors = "Spectral", labRow = names, scale = "row")
  })
  output$doe_cont <- renderD3heatmap({
    d3heatmap(selected_cont(), colors = "Spectral", labRow = selected_leg(), scale = "row")
  })
  
  #Grafico IDH x Genero
  output$ind_gen <- renderPlotly({
    p <- ggplot(genidh, aes(IDHF, IDHM, color = Country)) +
      geom_point(aes(size = IDH, frame = Year)) + labs(x = "IDH Feminino", y = "IDH Masculino")
    
    p <- ggplotly(p)
    p
  })
  
  
  #Mapa America do Sul com HIV - aba4
  output$mapaAShiv <- renderHighchart({
    
    colors <- colorRampPalette(brewer.pal(9,"Oranges"))(12)
    hcmap("custom/south-america", data = data_map2,value = "value", 
          joinBy = c("name", "code"), name = "IDH 2015",
          borderColor = "#FAFAFA", borderWidth = 0.1,
          tooltip = list(valueDecimals = 3)) %>% 
      hc_add_series(data = HIV, type = "mapbubble", name = "HIV/AIDS", maxSize = '10%', color = "darkblue") %>% 
      hc_mapNavigation(enabled = TRUE) %>% hc_colorAxis(stops = color_stops(n=length(colors), colors = colors)) %>% 
      hc_add_theme(hc_theme_538())
    
  })
  
  #Mapa Mundo X Doença Cardiaca
  output$mapaM <- renderHighchart({
    
    colors <- colorRampPalette(brewer.pal(9,"RdBu"))(6)
    p <- hcmap("custom/world-continents", data = cont, value = "val", joinBy = c("name","location"), name = "Números de mortes por Doenças Cardiovasculares",
               dataLabels = list(enabled = TRUE, format = "{point.name}"), borderWidth = 0.1) %>%  hc_colorAxis(stops = color_stops(n=length(colors), colors = colors))%>%
      hc_add_theme(hc_theme_db())
    p
    
  })
  
}