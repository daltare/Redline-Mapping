# this app just redirects to a different website - in this case, it redirects to a 
# shiny app at a different URL (https://cawaterdatadive.shinyapps.io/Redline-Mapping)

# source: https://gist.github.com/jobonaf/c5cb2773f2aa623a0ddf9b6d25448008

library(shiny)

server <- function(input, output, session) {}
ui <- fluidPage(singleton(tags$head(tags$script('window.location.replace("https://cawaterdatadive.shinyapps.io/Redline-Mapping");'))))
shinyApp(ui = ui, server = server)
