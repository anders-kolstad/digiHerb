library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rdrop2)
library(stringr)
library(DT)
token <- readRDS(file = "token.rds")
entries <- drop_dir(path = "/fotoherbarium")[,'name']
class(entries)
entries <- data.frame(entries)
names <- as.character(NA)

for(i in 1:nrow(entries)){
  names[i] <- stringr::str_split(entries[i,'name'], pattern = "-")[[1]][1]  
}
names <- as.data.frame(names)

ui <- dashboardPage(title = "digiHerb",
                    
                    dashboardHeader(title = "digiHerb",
                                    titleWidth = 400),
                    dashboardSidebar(),
                    dashboardBody(
                      column(width=8,
                      DTOutput('filtered')),
                      column(width=4,
                      textOutput('out'))
                      )
                    )
                    

server <- function(input, output, session) {
 output$filtered <- renderDT(
   names
 )
 
 output$out <- renderText({
   s <- input$filtered_rows_selected
   
   if (length(s)) {
     print(paste('These rows were selected:', s, sep=" "))
     
   }
 }
 )
 
  }

shinyApp(ui = ui, server = server)

