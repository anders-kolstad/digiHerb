library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rdrop2)
library(stringr)
library(DT)
library(httr)

drop_auth(rdstoken = "token.rds")

entries <- drop_dir(path = "/fotoherbarium")[,'name']
entries <- data.frame(entries)
names <- as.character(NA)

for(i in 1:nrow(entries)){
  names[i] <- stringr::str_split(entries[i,'name'], pattern = "-")[[1]][1]  
}
names <- as.data.frame(names)

ui <- dashboardPage(title = "digiHerb",
                    
                    dashboardHeader(title = "digiHerb",
                                    titleWidth = 400),
                    
                    dashboardSidebar(disable = T),
                    
                    dashboardBody(
                      column(width=12,
                             
                             DTOutput('filtered'),
                             
                             box(width = NULL,
                                 textOutput('path')),
                             
                             # Method 1
                             box(width = NULL,
                                 htmlOutput("picture"))
                             
                             # Method 2 . iframe
                             ,
                             box(width=NULL,
                                 uiOutput('myIFrame'))
                             
                             #alternative method
                             ,
                             box(width = NULL,
                                 uiOutput("img"))
                             
                             #alternative method 2
                             #,
                             #box(width = NULL,
                             #    htmlOutput("includeHTML"))
                      ))
                    
)


server <- function(input, output, session) {
  output$filtered <- renderDT(
    names, selection = 'single'
  )
  
  myPath <- paste0("/fotoherbarium/", 
                   entries[2,1])
  
  # Use an ifelse to find shared pictured using drop_list_shared_links(), or else make a shared link using drop_share
  
  shared <- drop_list_shared_links(verbose = F)
  l <- length(shared$links)
  sharedBefore <- data.frame()
  for(i in 1:l){
    sharedBefore[i,1] <- shared$links[[i]]$name
    sharedBefore[i,2] <- shared$links[[i]]$url
  }
  
  
  colnames(sharedBefore) <- c("names", "url")
  
  #preview <- drop_share(myPath) 
  #preview2 <- drop_media(myPath)
  #preview2$link
  
  preview <- sharedBefore[1,"url"]
  #edit (see https://help.dropbox.com/files-folders/share/force-download):
  preview <- paste0(preview, "raw=1")
  output$path <- renderText(print(preview))
  
  # Method 1
  output$picture<-renderText({
    c('<img src="',
      preview,
      '">')
  })
  
  # Method 2 - iframe
  output$myIFrame<-renderUI({
    tags$iframe(src = preview)
  })
  
  # Alternative method:
  output$img <- renderUI({
    tags$img(src =  preview, width="100%")
  })
  
  # Alternative method 2:
  #request <- httr::GET(url=preview)
  #dropB <- httr::content(request, as="text")
  #output$includeHTML <- renderText({
  #  dropB
  #})
  
  
}

shinyApp(ui = ui, server = server)

