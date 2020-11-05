library(shiny)
#library(shinydashboard)
library(shinyWidgets)
library(stringr)
library(DT)
library(shinythemes)

#setwd("/home/anders/Pictures/Anders digitale herbarium/Karplanter")
paths <-list.files(recursive = T, full.names = F)
lib <- as.data.frame(paths)
lib$temp <-  substr(paths, 5, nchar(paths))

lib <- tidyr::separate(data=lib,
                        col = temp,
                        into = c("Vitenskapelig navn", 
                                 "Norsk navn",
                                 "Funnsted",
                                 "dato"),
                        extra = "drop",
                        sep = "-")
lib <- tidyr::separate(data=lib,
                       col = "Vitenskapelig navn",
                       into = c("Slekt", 
                                "epitet"),
                       extra = "drop",
                       sep = " ",
                       remove=F)

slekterDT <- aggregate(data = lib, paths~Slekt, FUN=length)
slekterDTx <- aggregate(data = lib, epitet~Slekt, FUN=function(x) length(unique(x)))
colnames(slekterDT) <- c("Slekt", "bilder")
slekterDT$arter <- slekterDTx$epitet[match(slekterDT$Slekt, slekterDTx$Slekt)]

# UI ----------------------------------------------------------------------


ui <- navbarPage(theme = shinytheme("spacelab"),
                 title = "digiHerb",
                tabPanel("Søk etter bilde",    
                  
  fluidRow(
column(width=3,
    h3("Velg slekt(er)"),
    DTOutput('slektstabell')),


column(width = 5, offset = 1,
    h3("Velg bilde"),
    DTOutput('funntabell'))),

fluidRow(
  column(width=10, offset = 1,
  imageOutput('picture')))

))
                    




# Server ------------------------------------------------------------------


server <- function(input, output, session) {
 
  
  #output$art <- renderUI({
  #  selectizeInput('name', 
  #                 label="Art", 
  #                 choices =  unique(filteredLib2()$`Vitenskapelig navn`), 
  #                 multiple = F, 
  #                 selected = unique(filteredLib2()$`Vitenskapelig navn`)[1])  })
  
  valgteSlekter <- reactive({
    slekterDT$Slekt[input$slektstabell_rows_selected]
    })
  
  funnAvValgtArt <- reactive({
    lib[lib$Slekt %in% valgteSlekter(),]
  })
  
  
  output$funntabell <- renderDT(
   dplyr::select(funnAvValgtArt(),"Vitenskapelig navn", "Norsk navn", Funnsted, dato),
   selection = 'single'
 )
 
  
  
  output$slektstabell <- renderDT(
    slekterDT,
   selection = 'multiple',
   options = list(pageLength = 5)
  )



output$picture<-renderImage({
  index <- input$funntabell_rows_selected
  outfile <- funnAvValgtArt()$paths[index]
  list(src = paste(outfile),
       contentType = 'image/jpeg',
       width = 800,
       height = 'auto',
       alt = "This is alternate text")
 }, deleteFile = F)
 
 }

shinyApp(ui = ui, server = server)

