
load("lista_mapa.rda")

output$porto_page <- renderUI(fluidPage(

         sidebarLayout(
           sidebarPanel(
             
             selectInput("sigla", label = "Selecione o estado",
                         choices = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                                     "MS", "MT", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", 
                                     "RO", "RR", "SP", "SC", "SE", "TO"),
                         selected = "SP"),
             
             uiOutput("uI"),
             
             
             dateInput("inicio",
                       label="Data inicial",
                       value="2017-01-01",
                       min="2017-01-01",
                       max="2017-07-31",
                       format="dd/mm/yyyy"),
             
             dateInput("fim",
                       label="Data final",
                       value="2017-01-31",
                       min="2017-01-01",
                       max="2017-07-31",
                       format="dd/mm/yyyy"),
             actionButton("mostrar","Ir para mapa")
           ),
           
           mainPanel("",status="primary",
                     leaflet::leafletOutput("mapa1"),height="500px")))
)
  
  ## Reatividade



s<-eventReactive(input$sigla,{
  lista_mapa %>% 
    magrittr::extract2(input$sigla) %>% 
    magrittr::extract("municipio") %>% 
    unique()
})

output$uI <- renderUI({
  if (is.null(input$sigla))
    return()
  selectInput("municipio","Selecione o município",choices=s())
})

tabela<-eventReactive(input$mostrar,{
  
  lista_mapa %>% 
    magrittr::extract2(input$sigla) %>% 
    dplyr::filter(data_mes>=input$inicio,data_mes<=input$fim,municipio==input$municipio) %>% 
    #extract2(input$mes) %>% 
    purrr::map_if(is.numeric,~round(.x,digits=2)) %>% 
    tibble::as_tibble()
  
})
  
  #### Plota mapa ####
  observe({
    
    output$mapa1 <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>%
        leaflet::addTiles() %>% 
        leaflet::fitBounds(-73.9872354804, -33.7683777809, -34.7299934555, 5.24448639569)
      
    })
  })
  
  observeEvent(input$mostrar,{
    
    ll_map<-tabela()
    
    proxy<-leaflet::leafletProxy("mapa1",data=ll_map) %>% 
      leaflet::clearMarkers() %>% 
      leaflet::addMarkers(lng=~lon,lat=~lat,
                 #label=~muni,
                 popup=sprintf("Município: %s <br>Homicídios: %s <br>Roubos: %s <br> Roubos de veículos: %s <br> Furtos: %s <br> Furtos de veículos: %s  <br> Tráfico: %s <br> Fraudes: %s", ll_map$municipio,ll_map$homicidio,ll_map$total_roubo,ll_map$roubo_veiculo, ll_map$total_furto, ll_map$furto_veiculo, ll_map$trafico,ll_map$fraude),
                 labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,textsize="7px"),
                 icon=~icons(
                   iconUrl="fogo.jpg",iconWidth = 10,  shadowHeight = 20))
    
  })
  
  observeEvent(input$mostrar,{
    
    l_map<-tabela()
    
    proxy <- leaflet::leafletProxy("mapa1",data=l_map)
    
    proxy %>% 
      leaflet::setView(lng=mean(l_map$lon),lat=mean(l_map$lat), zoom=6)
    
  })
  