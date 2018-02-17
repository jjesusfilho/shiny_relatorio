
load("sp_porto.rda")

output$porto_page <- renderUI(fluidPage(
         sidebarLayout(
           sidebarPanel(
             selectInput("sigla_s",
                         label = "Selecione o estado",
                         choices = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                                     "MS", "MT", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", 
                                     "RO", "RR", "SP", "SC", "SE", "TO"),
                         selected = "SP"),
             
             selectInput("sucursal",
                         label="Selecione a sucursal",
                         choices = unique(sp_porto$SP$unidade),
                         selected = "Edifício Sede"),
             
             # dateInput("inicio_s",
             #           label="Data inicial",
             #           value="2017-01-01",
             #           min="2017-01-01",
             #           max="2017-07-31",
             #           format="mm/yyyy"),
             # 
             # dateInput("fim_s",
             #           label="Data final",
             #           value="2017-02-28",
             #           min="2017-01-01",
             #           max="2017-07-31",
             #           format="mm/yyyy"),
             # 
             
             selectInput("ano",
                         label="Selecione o ano",
                         choices = 2017,
                         selected=2017),
             
             selectInput("mes_inicial",
                         label="Selecione o mês inicial",
                         choices = unique(sp_porto$SP$mes),
                         selected="janeiro"),
             
             selectInput("mes_final",
                         label="Selecione o mês final",
                         choices = unique(sp_porto$SP$mes),
                         selected="fevereiro"),
             
             actionButton("mostrar","Ir para mapa"),
             
             selectInput("crime_s",
                         label = "Selecione o crime",
                         choices = c("fraude","furto","homicidio","lesao_corporal","roubo","trafico"),
                         selected = "furto"),
             
           
            downloadButton('relatorio_sucursal',"Baixar relatório em Word")),
           
           mainPanel(status="primary",
                     leafletOutput("mapa"),height="500px"))
         ))



## Criminalidade na sucursal 

map<- eventReactive(input$mostrar,{
  
  sp_porto %>% 
    magrittr::extract2(input$sigla_s) %>% 
    dplyr::filter(unidade==input$sucursal,ano==input$ano, mes>=input$mes_inicial,mes<=input$mes_final) %>% 
    dplyr::mutate_at(10:15,sum) %>% 
    dplyr::mutate(mes=stringr::str_c(mes,collapse=" a ")) %>% 
    dplyr::distinct()
})


observe({
  output$mapa <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      leaflet::fitBounds(-73.9872354804, -33.7683777809, -34.7299934555, 5.24448639569)
    #  setView(lng =map_ps1()$longitude, lat = map_ps1()$latitude, zoom = 12)
    
  })
  
})

observeEvent(input$mostrar,{
  
  map_ps<-map()
  
  proxy<-leaflet::leafletProxy("mapa",data=map_ps) %>% 
    leaflet::clearMarkers() %>% 
    leaflet::addMarkers(lng = ~lon, lat = ~lat,
               popup=sprintf("Unidade: %s <br> Endereço: %s <br> Mês: %s <br>Homicídios: %s  <br>Roubos: %s <br> Furtos: %s <br> Tráfico: %s <br> Fraudes: %s", map_ps$unidade, map_ps$endereco,map_ps$mes,map_ps$homicidio,map_ps$roubo,map_ps$furto, map_ps$trafico, map_ps$fraude),
               icon=~leaflet::icons(
                 iconUrl="http://4.bp.blogspot.com/_BR-RpcDHucY/TJSob8vWuNI/AAAAAAAAAK8/0xm2qtKrTgo/s1600/porto_seguro_seguros_logo.jpg",
                 iconWidth = 20))
  
  
})


observeEvent(input$mostrar,{
  map_ps<-map()

  proxy <- leaflet::leafletProxy("mapa",data=map_ps)

  proxy %>%
    leaflet::setView(lng=mean(map_ps$lon),lat=mean(map_ps$lat), zoom=6)

})



#### Relatório sucursal ####

output$relatorio_sucursal = downloadHandler(
  filename = 'relatorio_sucursal.doc',
  
  content = function(file) {
    src <- normalizePath('capa.png')
    tempReport <- file.path(tempdir(), "relatorio_sucursal.Rmd")
    tempCapa<-file.path(tempdir(),"capa.png")
    file.copy(src, tempCapa,overwrite = TRUE) #NEW
    file.copy("relatorio_sucursal.Rmd", tempReport, overwrite = TRUE)
    params <- list(sucursal = input$sucursal, inicio=input$inicio_s,fim=input$fim_s,crime=input$crime_s)
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



