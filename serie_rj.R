
output$porto_page <- renderUI(fluidPage(


sidebarLayout(
  sidebarPanel(
    selectInput("delito",
                label = "Selecione o delito",
                choices = c("homicídio doloso", "roubo no comércio", "roubo em residência", 
                            "roubo de veículos", "roubo de transeuntes", "roubo em coletivos", 
                            "roubo de celulares", "outros roubos", "furto de veículos", 
                            "outros furtos", "estelionato", "apreensão de drogas"),
                selected = "roubo de veículos"),
    selectInput("risp", label = "Selecione a RISP",
                choices = 1:7, selected = 2),
    selectInput("aisp", label = "Selecione a AISP",
                choices = 1:41, selected = 27),
    downloadButton('relatorio',"Baixar relatório em Word")),
  mainPanel(""))
))




output$relatorio = downloadHandler(
  filename = 'relatorio_crimes_rj.doc',
  
  content = function(file) {
    src <- normalizePath('capa.png')
    tempReport <- file.path(tempdir(), "relatorio_rj.Rmd")
    tempCapa<-file.path(tempdir(),"capa.png")
    file.copy(src, tempCapa,overwrite = TRUE) #NEW
    file.copy("relatorio_rj.Rmd", tempReport, overwrite = TRUE)
    params <- list(delito = input$delito, risp=input$risp,aisp=input$aisp)
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)
