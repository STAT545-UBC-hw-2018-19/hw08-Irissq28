library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(dplyr)
library(ggplot2)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # theme = "bootstrap.css",
  # choose a theme
  theme = shinytheme("cyborg"),
  # import header style and apply it
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    "))
    ),
  headerPanel(h1("BC Liquor price app",style = "font-family: 'Lobster', cursive;
        font-weight: 500; line-height: 1.1; 
             color: #4d3a7d;")),
  sidebarLayout(
    sidebarPanel(
      img(src = "giphy.gif",align = "left", width = 400),
      sliderInput("priceInput", "Select your desired price range",
                  min = 0, max = 100, value = c(15, 30), pre="$"),
      checkboxInput("sortbyPrice", "Sort by price (Descending)", FALSE),
      sliderInput("sweetness", "Select your desired sweetness",
                  min = 0, max = 10, value = c(0, 5), pre="*"),
      # add multiselect input
      multiInput("typeInput", "Select your alcohole beverge type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = c("BEER","WINE"),
                 options = list(
                   enable_search = FALSE,
                   non_selected_header = "Choose between:",
                   selected_header = "You have selected:"
                 )),
      uiOutput("countryOutput")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("price_hist")),
        tabPanel("Summary", verbatimTextOutput("summ")),
        tabPanel("Table", tableOutput("bcl_data"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # reactive functions
  bcl_filterd <- reactive({
    # check if the country input exists
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    # filter by the range of sweetness
    bcl_filterd <- bcl %>% filter(Sweetness >= input$sweetness[1],
                                  Sweetness <= input$sweetness[2])
    # filter by the type
    bcl_filterd <- bcl_filterd %>% filter(Type == input$typeInput)
    # filter by the country
    bcl_filterd <-bcl_filterd %>% filter(Country == input$countryInput)
    # add an option ---sort by price
    if (input$sortbyPrice) {
      bcl_filterd <- dplyr::arrange(bcl_filterd,Price)
    }  
    else{
      bcl_filterd <- dplyr::arrange(bcl_filterd,desc(Price))
    }
  })
  bcl_summ <-reactive({
    bcl_filterd() %>%
      summary()
  })
  #  render the outputs
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })
  output$price_hist <- renderPlot({
    if (is.null(bcl_filterd())) {
      return()
    }
    bcl_filterd() %>%
      ggplot(aes(Price)) +
      geom_histogram() +
      theme_bw()
  })
  output$bcl_data <- renderTable(
    bcl_filterd()
  )
  output$summ <- renderText(
    knitr::kable(bcl_summ())
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

