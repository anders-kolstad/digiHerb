library(shiny)
library(shinyWidgets)
library(stringr)
library(DT)
library(shinythemes)
#library(slickR)
library(ggplot2)
library(magick)

#setwd("/home/anders/Pictures/Anders digitale herbarium/Karplanter")
paths <-list.files(path = "../bilder/",recursive = T, full.names = F)
lib <- as.data.frame(paths)
lib$temp <-  substr(paths, 5, nchar(paths))

lib <- tidyr::separate(data=lib,
                        col = temp,
                        into = c("Vitenskapelig navn", 
                                 "Norsk navn",
                                 "Funnsted",
                                 "dato"),
                        extra = "drop",
                        sep = " - ")
lib <- tidyr::separate(data=lib,
                       col = "Vitenskapelig navn",
                       into = c("Slekt", 
                                "epitet"),
                       extra = "merge",
                       sep = " ",
                       remove=F)

# get year (last four characters)
lib$dato <- substr(lib$dato, 1, nchar(lib$dato)-4)
lib$år <-  substr(lib$dato, nchar(lib$dato)-4, nchar(lib$dato))

# Hent familienavn
fam <- readRDS('slektOGfam.RData')
lib$Familie <- fam$familie[match(lib$Slekt, fam$slekt)]
lib <- cbind(
  lib[,1:2],
  lib[,"Familie"],
  lib[,4:ncol(lib)-1]
)
colnames(lib)[3] <- "Familie"

# antall arter per slekt
slekterDT <- aggregate(data = lib, epitet~Slekt+Familie, 
                       FUN=function(x) length(unique(x)))
colnames(slekterDT) <- c("Slekt", "Familie", "arter")

# antall slekter og arter per familie
famDT <- stats::aggregate(data = lib, Slekt~Familie, 
                   FUN=function(x) length(unique(x)),
                   drop=F)
famDT2 <- stats::aggregate(data = lib, epitet~Familie, 
                    FUN=function(x) length(unique(x)),
                    drop=F)


famDT$arter <- famDT2$epitet
colnames(famDT) <- c("Familie", "slekter", "arter")
lib$år <- as.numeric(lib$år)

rekke <- seq(min(lib$år, na.rm = T), 
             max(lib$år, na.rm = T),1)
antTaxa <- data.frame("År"= rekke,
            "Unike_taxa" <- as.numeric(rep(NA, length(rekke))),
            "Nye_taxa" <- as.numeric(rep(NA, length(rekke))))

for(i in 1:length(rekke)){
  antTaxa[i,2] <- length(unique(lib$`Vitenskapelig navn`[lib$år<=rekke[i]]))
  ifelse(rekke[i] > min(rekke),  
         antTaxa[i,3] <- antTaxa[i,2]-antTaxa[i-1,2],
         antTaxa[i,3] <- antTaxa[i,2])
}
colnames(antTaxa) <- c("År", "Antall_arter", "Antall_nye_arter")

# UI ----------------------------------------------------------------------


ui <- navbarPage(theme = shinytheme("slate"),
                 title = "digiHerb",
                tabPanel("Søk etter bilde",    
   
 tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"), 
  fluidRow(
column(width=4,
    h3("Velg slekt(er)"),
    DTOutput('slektstabell')),

column(width=4, offset=0,
       h3("Velg art(er)"),
       DTOutput('artstabell')),

column(width = 4, offset=0,
    h3("Velg bilde"),
    DTOutput('funntabell'))),

fluidRow(
  column(width=10, offset = 1,
         br(),
         sliderInput('width', label = "Bildebredde", min=200, max=4000, step=100,
                     value = 800),
         radioGroupButtons(
           inputId = "rotation",
           label = "Rotasjon",
           choiceNames = c("<-","0","->"), 
           choiceValues = c("270", "0", "90"),
           selected = "0"
         ),
  imageOutput('picture')
  #slickROutput('slick', width = 500)
  ))

),
tabPanel("Quiz",
      column(width = 3,
         actionButton("nyttBilde", "Nytt bilde"),
         actionButton("svaret", "Vis svar"),
         sliderInput('width2', label = "Bildebredde", min=200, max=4000, step=100,
                     value = 600, width = "100%"),
         radioGroupButtons(
           inputId = "rotation2",
           label = "Rotasjon",
           choiceNames = c("<-","0","->"), 
           choiceValues = c("270", "0", "90"),
           selected = "0"
         ),
         tableOutput('svaret')),
      column(width = 8, offset = 0,
         tags$head(tags$script(src = "message-handler.js")),
         imageOutput('quizbilde'))
         ),
tabPanel("Tabell",
         DTOutput('alleRader')),
tabPanel("Statistikk",
        column(width = 6,
               plotOutput("barFam"),br(),br(),
               plotOutput('trends')),
        column(width = 6,
               plotOutput("barFam2"), br(), br(),
               plotOutput('trends2')))
)
                    




# Server ------------------------------------------------------------------


server <- function(input, output, session) {


# Quiz --------------------------------------------------------------------
  v <- reactiveValues(data = NULL)
  
observeEvent(input$nyttBilde, {
  v$index <- runif(1,1,nrow(lib))
})

output$quizbilde<-renderImage({
  image <- image_read(paste0("../bilder/", lib$paths[v$index]))
  tmp <- image %>%
    image_rotate(as.numeric(input$rotation2)) %>%
    image_border("grey", "20x10") %>%
    image_write(tempfile(fileext='jpg'), format = 'jpg')
  list(src = paste(tmp),
       contentType = 'image/jpeg',
       width = input$width2,
       height = 'auto',
       alt = "This is alternate text")
}, deleteFile = F)

#observeEvent(input$svaret, {
#  session$sendCustomMessage(type = 'testmessage',
#                            message = paste(lib$`Vitenskapelig navn`[v$index],
#                                            lib$Familie[v$index],
#                                            lib$`Norsk navn`[v$index],
#                                            lib$Funnsted[v$index],
#                            sep=" | "))
#})

#observeEvent(input$svaret, {
#  output$svaret <- renderText(paste(lib$`Vitenskapelig navn`[v$index],
#                                            lib$Familie[v$index],
#                                            lib$`Norsk navn`[v$index],
#                                            lib$Funnsted[v$index],
#                                            sep=" | "))
#})

observeEvent(input$svaret, {
  output$svaret <- renderTable(t(lib[v$index,c(2,3,6,7,8)]), colnames=F)
})

observeEvent(input$nyttBilde, {
  output$svaret <- renderTable(NULL, colnames=F)
})

# Utvalg ------------------------------------------------------------------

  valgteSlekter <- reactive({
    slekterDT$Slekt[input$slektstabell_rows_selected]
  })

  valgteArter2 <- reactive({
    valgteArter()$`Vitenskapelig navn`[input$artstabell_rows_selected]
  })
# Filtrer data ------------------------------------------------------------

  
  valgteArter <- reactive({
    dat <- lib[lib$Slekt %in% valgteSlekter(),]
    aggregate(data = dat, paths~`Vitenskapelig navn`, FUN=length)
  })
  
  funnAvValgtArt <- reactive({
    lib[lib$`Vitenskapelig navn` %in% valgteArter2(),]
  })
  
  
 

# Tabeller ----------------------------------------------------------------
  
  output$slektstabell <- renderDT({
    datatable(slekterDT,
   rownames = FALSE,
   selection = 'single',
   options = list(pageLength = 500, scrollY = "300px",
     scrollX = TRUE)) %>% 
      formatStyle(c(1:ncol(slekterDT)), 
                  color = 'black',
                  backgroundColor = 'lightgreen') 
  })


  output$artstabell <- renderDT({
    datatable(valgteArter(),
              rownames = FALSE,
              selection = 'single',
              options = list(pageLength = 50, scrollY = "300px",
                             scrollX = TRUE)) %>% 
      formatStyle(c(1:ncol(slekterDT)), 
                  color = 'black',
                  backgroundColor = 'lightgreen') 
  })
  
  output$funntabell <- renderDT({
    datatable(dplyr::select(funnAvValgtArt(),"Norsk navn", Funnsted, dato),
    rownames = FALSE,
    selection = 'single',
    options = list(pageLength = 50, scrollY = "300px",
                   scrollX = TRUE)) %>% 
    formatStyle(c(1:ncol(slekterDT)), 
                color = 'black',
                background = 'lightgreen') 
  })

  output$alleRader <- renderDT({
  datatable(lib[,-1],
  options = list(pageLength = 2000, scrollY = "300px")) %>%
      formatStyle(c(1:ncol(lib[,-1])), 
                  color = 'black',
                  backgroundColor = 'lightgreen') 
    })
# Bilder ------------------------------------------------------------------

# hvordan få bilde til å åpne slik at man kan zoome?
# https://shiny.rstudio.com/articles/modal-dialogs.html


#image <- image_read(paste0("/home/anders/Pictures/Anders digitale herbarium/Karplanter/bilder/", lib$paths[1]))
  
# evt som bildegalleri - se slickR
output$picture<-renderImage({
  index <- input$funntabell_rows_selected
  image <- image_read(paste0("../bilder/", funnAvValgtArt()$paths[index]))
  tmp <- image %>%
    image_rotate(as.numeric(input$rotation)) %>%
    image_border("grey", "20x10") %>%
    image_write(tempfile(fileext='jpg'), format = 'jpg')
  
  list(src = tmp,
       contentType = 'image/jpeg',
       width = input$width,
       height = 'auto',
       alt = "This is alternate text")
 }, deleteFile = F)
  
  
#output$slick <- renderSlickR({
#  index <- input$funntabell_rows_selected
#  outfile <- funnAvValgtArt()$paths[index]
#  slickR(outfile)
#})
 
  

# Stats -------------------------------------------------------------------

topFam <- reactive({famDT[order(famDT$arter, decreasing = T),]})
  
  output$barFam <- renderPlot({
    ggplot(data = topFam()[1:25,], height = 500,width = 100)+
      geom_bar(aes(x=reorder(Familie, arter), y=arter), 
               stat = "identity", width = 0.8)+
      coord_flip()+
      theme_bw()+
      theme(axis.title.y = element_blank())
    
  })  
 

output$barFam2 <- renderPlot({
  ggplot(data = topFam()[1:25,], height = 900,width = 100)+
    geom_bar(aes(x=reorder(Familie, arter), y=slekter), 
             stat = "identity", width = 0.8)+
    coord_flip()+
    theme_bw()+
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank())
  
})

output$trends <- renderPlot({
  ggplot(data=lib)+
    geom_histogram(aes(x=år), stat="count")+
    scale_x_continuous(breaks=seq(2010,2021,1))+
    xlab("")+ylab("antall bilder")
})

output$trends2 <- renderPlot({
  ggplot(data=antTaxa)+
    geom_histogram(aes(x=År, y=Antall_nye_arter), stat="identity")+
    scale_x_continuous(breaks=seq(2010,2021,1))+
    xlab("")+ylab("antall nye arter")+
    theme(axis.text.x = element_text(angle=45))+
    geom_line(aes(x=År, y=Antall_arter/2))+
    scale_y_continuous(name = "Søyler: Antall nye arter per år",
                       
  # Add a second axis and specify its features
    sec.axis = sec_axis(~.*2, name="Linje: Totalt antall arter"))
})
}

shinyApp(ui = ui, server = server)

