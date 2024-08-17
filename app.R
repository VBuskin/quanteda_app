
# This R Shiny app provides a simple, yet effective user interface for the R package "quanteda". 


# Load libraries
library(shiny) # web application framework
library(shinythemes) # themes
library(quanteda) # quantitative analysis of textual data
library(quanteda.textstats) # supplementary package
library(DT) # for rendering data frames
library(openxlsx) # for exporting data as MS Excel files
library(tidyverse) # for data wrangling

# Define a reactive value for the corpus
corpus <- reactiveVal(NULL)

#### UI ####

# Define UI
ui <- navbarPage(
  title = "Quanteda Corpus App",
  theme = shinytheme("cerulean"),  # You can change the theme as desired
  
  # Main tab
  tabPanel("Query",
    sidebarLayout(
      # Sidebar
      sidebarPanel(
        textInput("query", "Enter your query:", value = ""),
        sliderInput("window", "Select window size:", min = 1, max = 50, value = 10),
        checkboxInput("caseSensitive", "Case Sensitive", value = FALSE),
        fileInput("uploadRds", "Upload your RDS file:", accept = ".rds"),
        
        # Add a header for the export options
        tags$hr(),  # Horizontal line for visual separation
        tags$p(tags$strong("Choose your export format:")),  # Bold text for the header
        
        # Export buttons
        downloadButton("downloadCsv", "Download CSV (Tab-delimited)"),
        downloadButton("downloadXlsx", "Download XLSX"),
        downloadButton("downloadRds", "Download RDS")
      ),
      
      # Main Content
      mainPanel(
        # Tabset for Query Results, Keyword Statistics, and Keyword Plot
        tabsetPanel(
          tabPanel("Query Results", DTOutput("queryResult")),
          tabPanel("Keyword Statistics", DTOutput("keywordStats")),
          tabPanel("Keyword Plot", plotOutput("keywordPlot"))
        )
      )
    )
  ),
  
  # About tab
  tabPanel("About",
    h2("About This App"),
    p("This is a Shiny app for querying and analyzing text corpora using the quanteda package.")
  ),
  
  # Help tab
  tabPanel("Help",
    h2("How to Use This App"),
    p("Instructions on how to use the app can go here.")
  ),
  
  # Dropdown menu
  navbarMenu("More",
    tabPanel("Contact",
      h2("Contact Information"),
      p("You can reach us at...")
    ),
    tabPanel("References",
      h2("References"),
      p("List of references and citations can go here.")
    )
  )
)

#### SERVER ####

# Define server logic
server <- function(input, output, session) {
  # Observe file input and update corpus
  observe({
    if (!is.null(input$uploadRds)) {
      new_corpus <- readRDS(input$uploadRds$datapath)
      corpus(new_corpus)
    }
  })
  
  query_result <- reactive({
    query <- input$query
    window_size <- input$window
    case_sensitive <- input$caseSensitive
    corpus_data <- corpus()
    
    if (is.null(corpus_data)) {
      return(data.frame(Message = "Please upload an RDS file to load the corpus."))
    }
    
    if (query != "") {
      # Tokenize and query the corpus
      result <- kwic(corpus_data, pattern = query, valuetype = "regex", window = window_size, case_insensitive = !case_sensitive)
      
      if (nrow(result) > 0) {
        result
      } else {
        data.frame(Message = "No matches found.")
      }
    }
  })
  
  # Compute keyword statistics
  keyword_stats <- reactive({
    result <- query_result()
    if (nrow(result) > 0 && !("Message" %in% names(result))) {
      keyword_freq <- result %>%
        as_tibble() %>%
        count(keyword) %>%
        mutate(Percentage = (n / sum(n)) * 100)
      return(keyword_freq)
    } else {
      return(data.frame(Keyword = character(), Count = integer(), Percentage = numeric()))
    }
  })
  
  # Modify the renderDT for query results to include the expand option
  # Render data table for query results
  output$queryResult <- renderDT({
    datatable(query_result(),
              options = list(pageLength = 10,
                             scrollX = TRUE,
                             autoWidth = TRUE))
  })
  
  
  
  # Render data table for keyword statistics
  output$keywordStats <- renderDT({
    datatable(keyword_stats(),
              options = list(pageLength = 10,
                             scrollX = TRUE,
                             autoWidth = TRUE))
  })
  
  # Render plot for keyword statistics
  output$keywordPlot <- renderPlot({
    keyword_freq <- keyword_stats()
    if (nrow(keyword_freq) > 0) {
      ggplot(keyword_freq, aes(x = keyword, y = Percentage)) +
        geom_point(color = "steelblue") +
        labs(x = "Keyword", y = "Percentage", title = "Keyword Frequency Plot") +
        coord_flip() +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Download handlers for results
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("query_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.table(query_result(), file, sep = "\t", row.names = FALSE)
    }
  )
  
  output$downloadXlsx <- downloadHandler(
    filename = function() {
      paste("query_results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(query_result(), file, rowNames = FALSE)
    }
  )
  
  output$downloadRds <- downloadHandler(
    filename = function() {
      paste("query_results_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(query_result(), file)
    }
  )
}

# Create Shiny app
shinyApp(ui = ui, server = server)