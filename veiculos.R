
load("veiculos.rda")

output$porto_page <- renderUI(fluidPage(
  

         sidebarLayout(
           sidebarPanel(
             
             selectInput("sigla",
                         label = "Selecione o estado",
                         choices =  c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                                      "MS", "MT", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", 
                                      "RO", "RR", "SP", "SC", "SE", "TO"),
                         selected = "SP"),
             
             selectInput("crime",
                         label="Selecione o crime",
                         choices=c("Furto","Roubo"),
                         selected="Roubo"),
             
             selectInput("ano",
                         label="Selecione o ano do BO",
                         choices=c(2015:2017),
                         selected=2016),
             
             selectizeInput("colunas", 
                            label="Selecione as colunas",
                            selected=NULL,
                            choices=v_nomes,
                            multiple = TRUE,
                            options = NULL),
             
             
             actionButton("mostrar","Mostrar tabela"),
             
             downloadButton("downloadData", "Baixar excel")
             
           ),
           
           mainPanel(title="Furto e roubo de veículos",
                     status="primary",
                     DT::dataTableOutput("veiculos")))

))



  df<-eventReactive(input$mostrar,{
           
           #req(input$colunas)
           #arg_list <- rlang::parse_quosures(paste(input$colunas, collapse = ";"))
           
           
           df<-veiculos %>% 
             extract2(input$crime) %>% 
             filter(Ano.do.Bo == input$ano)
           
           df<-df[,input$colunas]
           
           return(df)
         })
         
         
         
observeEvent(input$mostrar,{
  
  output$veiculos <- DT::renderDataTable({
    
    DT::datatable(df(),
                  rownames=FALSE,
                  colnames=stringr::str_replace_all(names(df()),"\\."," "),
                  filter="top", 
                  options=list(language=list(search="Busca",
                                             info="Mostrando de _START_ a _END_ de _TOTAL_ ocorrências",
                                             lengthMenu="Mostre _MENU_ ocorrências",
                                             paginate=list(previous="anterior",
                                                           'next'="próximo")),processing=FALSE))
  })
  
  
  output$downloadData_v <- downloadHandler(
    filename = function(){
      
      paste(input$crime_v,".xlsx",sep="_")},
    
    content = function(file) {
      write_xlsx(df_v(),file)
    }
  )
  
  
})




