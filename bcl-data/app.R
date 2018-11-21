library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DT)
library(ggthemes)


bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # theme = "bootstrap.css",
  # choose a theme
  theme = shinytheme("cyborg"),
  # import header style and apply it
  includeCSS("style.css"),
  headerPanel("BC Liquor price app"),
  sidebarLayout(
    sidebarPanel(
      img(src = "giphy.gif",align = "left", width = 400),
      sliderInput("priceInput", "Select your desired price range",
                  min = 0, max = 130, value = c(15, 30), pre="$"),
      checkboxInput("sortbyPrice", "Sort by price (Ascending)", FALSE),
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
      uiOutput("countryOutput"),
      downloadButton('downloadData', "Download results", style="display: block; margin: 0 auto; width: 230px;color: black;")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("price_hist")),
        tabPanel("Options", verbatimTextOutput("summ")),
        tabPanel("Table", dataTableOutput(outputId = "bcl_data", width = "100%", height = "auto"))
      ),
      br(),
      br(),
      br(),
      # add license
      uiOutput("author"),
      uiOutput("improved")
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
    # filter by the range of price
    bcl_filterd <- bcl %>% 
        dplyr::filter(Price >= input$priceInput[1],
                      Price <= input$priceInput[2])  %>%
    # filter by the range of sweetness
        dplyr::filter(Sweetness >= input$sweetness[1],
                      Sweetness <= input$sweetness[2]) %>%
    # filter by the type
        dplyr::filter(Type == input$typeInput) %>%
    # filter by the country
        dplyr::filter(Country == input$countryInput) 
    # add an option ---sort by price
    if (input$sortbyPrice) {
      bcl_filterd <- dplyr::arrange(bcl_filterd,Price)
    }  
    else{
      bcl_filterd <- dplyr::arrange(bcl_filterd,desc(Price))
    }

  })
  #  render the outputs
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })
  output$price_hist <- renderPlot({
    if (is.null(bcl_filterd())) {
      return(NULL)
    }
      bcl_filterd() %>%
          ggplot(aes(x = Alcohol_Content, fill = Type)) +
          geom_histogram(alpha = 0.5) +
          ggthemes::theme_solarized() +
          labs(x = "Alcohol_Content", y = "Count", 
               title = "Distribution of the Alcohol Content for different alcohol") +
          # size and position of title changed
          theme(plot.title = element_text(size = 20,hjust = 0.5),
                # size of axis text changed
                axis.text = element_text(size = 15))
  })
  output$bcl_data <- renderDataTable(
     bcl_filterd <- datatable(bcl_filterd()) %>%
      formatStyle(
        names(bcl_filterd()),
        color = 'white',
        backgroundColor = 'black',
        fontWeight = 'bold'
      )
  )
  output$summ <- renderText({
      numOpts <- nrow(bcl_filterd())
      if (is.null(numOpts)) {
          numOpts <- 0
      }
      paste0("Hi, we found ", numOpts, " options for you!")
  })
  
  price_country <- reactive({
      bcl_filterd() %>%
      group_by(Country) %>%
      summarise(
          AvgPrice = mean(Price)
      )
  })
  output$downloadData <- downloadHandler(
      filename = function() {
          paste("bcl-data.csv")
      },
      content = function(file) {
          write.csv(bcl_filterd(), file)
      }
  )
  url_au <- a("Code and data are from ", "Dean Attali", href="https://deanattali.com/blog/building-shiny-apps-tutorial/")
  output$author <- renderUI({
      tagList(url_au)
  })
  url_im <- a("Improved by Siqi An", href="https://github.com/STAT545-UBC-students/hw08-Irissq28")
  output$improved <- renderUI({
      tagList(url_im)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

