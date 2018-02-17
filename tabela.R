


#### Tabela ####
load("lista_mapa.rda")

output$porto_page <- renderUI(fluidPage(

         sidebarLayout(
           sidebarPanel(
             selectInput("sigla_df", label = "Selecione o estado",
                         choices = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                                     "MS", "MT", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", 
                                     "RO", "RR", "SP", "SC", "SE", "TO"),
                         selected = "SP"),
             
             dateInput("ini_df",
                       label="Data inicial",
                       value="2017-01-01",
                       min="2017-01-01",
                       max="2017-07-31",
                       format="dd/mm/yyyy"),
             
             dateInput("fim_df",
                       label="Data final",
                       value="2017-01-31",
                       min="2017-01-01",
                       max="2017-07-31",
                       format="dd/mm/yyyy"),
             
             actionButton("mostrar_df","Pesquisar"),
             
             downloadButton("downloadData", "Baixar excel")),
           
           mainPanel(title="Crimes por 100 mil habitantes",
                     status="primary",
                     DT::dataTableOutput("crime")))
))



observeEvent(input$mostrar_df,{
  
  df<-lista_mapa %>% 
    extract2(input$sigla_df) %>% 
    filter(data_mes>=input$ini_df,data_mes<=input$fim_df) %>% 
    #extract2(input$mes) %>% 
    purrr::map_if(is.numeric,~round(.x,digits=2)) %>% 
    tibble::as_tibble()
  
  output$crime <- DT::renderDataTable({
    DT::datatable(df[c("municipio","total_furto_100mil_hab","furto_veiculo_100mil_hab","total_roubo_100mil_hab","roubo_veiculo_100mil_hab","homidicio_100mil_hab","fraude_100mil_hab","trafico_100mil_hab")],
                  rownames=FALSE,
                  colnames=c("Furtos","Furtos de veículos","Roubos","Roubo de veículos","Homicídio","Fraudes-estelionatos","Tráfico"),
                  filter="top", options=list(
                    language=list(search="Busca",
                                  info="Mostrando de _START_ a _END_ de _TOTAL_ ocorrências",
                                  lengthMenu="Mostre _MENU_ ocorrências",
                                  paginate=list(previous="anterior",
                                                'next'="próximo"))))
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      
      paste(input$sigla_df,"crimes.xlsx",sep="_")},
    
    content = function(file) {
      write_xlsx(df,file)
    }
  )
})



