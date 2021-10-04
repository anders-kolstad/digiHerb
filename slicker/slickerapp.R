library(shiny)
library(slickR)
library(svglite)

# slickR krever java. Får ikke det fra NINA tror jeg

(myfiles <- list.files(path = "../../bilder/test/",
                      recursive = T, full.names = F))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      ####
    ),
    
    mainPanel(
      style="background:red;",
      ("slickr", width="500px", height = "500px")
    )
  )
)

server <- function(input, output) {
  
  output$slickr <- renderSlickR({
    
  
    
    slickR(myfiles)
    
  })
  
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)