
load("serie.rda")

output$porto_page <- renderUI(fluidPage(
  
         sidebarLayout(
           sidebarPanel(
             selectInput("sigla", label = "Selecione o estado",
                         choices = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                                               "MS", "MT", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", 
                                               "RO", "RR", "SP", "SC", "SE", "TO"), selected = "SP"),
             
             selectInput("crime", label = "Selecione o crime",
                         choices = crime, selected = "roubo"),
             
             conditionalPanel(
               condition = "input.sigla != 'SP'",
               selectInput("mes", label = "Selecione o mês",
                           choices = meses, selected = "Janeiro")
             )),
           
           mainPanel("",
                     status="primary",
                     dygraphOutput("dygraph"))))
)


serie<-reactive({
  if(input$sigla!="SP"){
    l %>% 
      extract2(input$sigla) %>% 
      do.call(rbind,.)
  }else{
    
    sp %>% 
      extract(c("data_ocorrencia_bo",input$crime)) %>% 
      xts(order.by=.$data_ocorrencia_bo)
  }
})

output$dygraph<-renderDygraph({
  
  
  dygraph(serie(),main="Ocorrências mensais de crimes no estado",ylab="Número de crimes") %>%
    dyRangeSelector() %>% 
    dyLegend(width=450)
  
})

