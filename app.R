### uiPorto
### v1.3 - José de Jesus Filho - Fev 2018

### v1.1 fixes a security issue
###    for details, see: https://groups.google.com/forum/?utm_source=digest&utm_medium=email#!topic/shiny-discuss/zxoLK1kKTCw)
### v1.2 adds some tagList() functions that aren't strictly required here, but make it
###    much easier to expand this code.
### v1.3 added more informative console messages.

### Simple demo program that "moves" the ui into the server, thereby allowing the use of
###    the URL "search" protocol to create multi-page web sites. The advantage is that
###    each page can have its own file (to keep things organized) and only one file is
###    loaded into Shiny Server at a time, limiting memory usage.

### Note, the URL "path" protocol currently doesn't work, because Shiny Server currently
###    returns a "not found" error rather than passing the path to the session.


#### Configurações ####

#.libPaths("/home/rstudio/R/x86_64-pc-linux-gnu-library/3.4")
library(shiny)
library(leaflet)
library(xts)
library(dygraphs)
library(magrittr)
library(dplyr)
library(sf)
library(DT)
#library(networkD3)
library(purrr)

options(shiny.sanitize.errors = FALSE)

## Ui


# put a message in console or server log; note this happens only when the app is started!
cat("uiPorto iniciou apalicação...\n")

ui <- uiOutput("Uiporto")                                # single-output stub ui


## Servidor
server <- function(input, output,session){
  
  cat("Início da sessão.\n")                               # this prints when a session starts
  onSessionEnded(function() {cat("Fim da sessão.\n\n")})  # this prints when a session ends
  
  # build menu; same on all pages
  output$Uiporto <- renderUI(tagList(             # a single-output stub ui basically lets you
    fluidPage(                                  #     move the ui into the server function
      fluidRow(
        column(12,
               HTML("<h3> <a href='?home'></a>",
                    "<a href='?mapa'></a>",
                    "<a href='?serie'></a>",
                    "<a href='?tabela'></a>",
                    "<a href='?sucursal'></a>",
                    "<a href='?veiculos'></a>",
                    "<a href='?serie_rj'></a>",
                    "</h3>")
        )
      ),
      uiOutput("porto_page")                     # loaded server code should render the
    )                                           #    rest of the page to this output$
    
  ))
  
  
# load server code for page specified in URL
validFiles = c("home.R",                             # valid files must be hardcoded here
               "mapa.R",
               "serie.R",
               "tabela.R",
               "sucursal.R",
               "veiculos.R",
               "serie_rj.R")                     #    for security (use all lower-case

#    names to prevent Unix case problems)
fname = isolate(session$clientData$url_search)       # isolate() deals with reactive context
if(nchar(fname)==0) { fname = "?mapa" }              # blank means home page
fname = paste0(substr(fname, 2, nchar(fname)), ".R") # remove leading "?", add ".R"

cat(paste0("Nome da sessão: ", fname, ".\n"))      # print the URL for this session

if(!fname %in% validFiles){                          # is that one of our files?
  output$porto_page <- renderUI(tagList(              # 404 if no file with that name
    fluidRow(
      column(5,
             HTML("<h2>404 Not Found Error:</h2><p>That URL doesn't exist. Use the",
                  "menu above to navigate to the page you were looking for.</p>")
      )
    )
  ))
  return()    # to prevent a "file not found" error on the next line after a 404 error
}
source(fname, local=TRUE)                            # load and run server code for this page
}
  
  
shinyApp(ui, server)