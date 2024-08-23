
#' Run Quanteda GUI App
#'
#' This function launches a Shiny app that provides a graphical user interface for the Quanteda package.
#'
#' @return A Shiny app object
#' @export
#'
#' @examples
#' if(interactive()){
#'   run_quanteda_app()
#' }

run_quanteda_app <- function(...) {
  
  # Load libraries
 # library(shiny) # web application framework
#  library(shinythemes) # themes
 # library(quanteda) # quantitative analysis of textual data
#  library(quanteda.textstats) # supplementary package
 # library(DT) # for rendering data frames
#  library(openxlsx) # for exporting data as MS Excel files
 # library(dplyr) # for data wrangling
  
  # Define a reactive value for the corpus
  corpus <- shiny::reactiveVal(NULL)
  
  #### UI ####
  # Define UI
  ui <- shiny::navbarPage(
    title = "Quanteda Corpus App",
    theme = shinythemes::shinytheme("cerulean"),
    # You can change the theme as desired
    
    # Main tab
    shiny::tabPanel("Query",
                    shiny::sidebarLayout(
                      # Sidebar
                      shiny::sidebarPanel(
                        shiny::textInput("query", "Enter your query:", value = ""),
                        shiny::sliderInput("window", "Select window size:", min = 1, max = 50, value = 10),
                        shiny::checkboxInput("caseSensitive", "Case Sensitive", value = FALSE),
                        shiny::fileInput("uploadRds", "Upload your RDS file:", accept = ".rds"),
                        
                        # Add a header for the export options
                        shiny::tags$hr(),  # Horizontal line for visual separation
                        shiny::tags$p(shiny::tags$strong("Choose your export format:")),  # Bold text for the header
                        
                        # Export buttons
                        shiny::downloadButton("downloadCsv", "Download CSV (Tab-delimited)"),
                        shiny::downloadButton("downloadXlsx", "Download XLSX"),
                        shiny::downloadButton("downloadRds", "Download RDS")
                      ),
                      
                      # Main Content
                      shiny::mainPanel(
                        # Tabset for Query Results, Keyword Statistics, and Keyword Plot
                        shiny::tabsetPanel(
                          shiny::tabPanel("Query Results", DT::DTOutput("queryResult")),
                          shiny::tabPanel("Keyword Statistics", DT::DTOutput("keywordStats")),
                          shiny::tabPanel("Keyword Plot", shiny::plotOutput("keywordPlot"))
                        )
                      )
                    )
    ),
    
    # About tab
    shiny::tabPanel("About",
                    shiny::h2("About This App"),
                    shiny::p("This is a Shiny app for querying and analyzing text corpora using the quanteda package.")
    ),
    
    # Help tab
    shiny::tabPanel("Help",
                    shiny::h2("How to Use This App"),
                    shiny::p("Instructions on how to use the app can go here.")
    ),
    
    # Dropdown menu
    shiny::navbarMenu("More",
                      shiny::tabPanel("Contact",
                                      shiny::h2("Contact Information"),
                                      shiny::p("This app is maintained by Vladimir Buskin (VBuskin@ku.de).")
                      ),
                      shiny::tabPanel("References",
                                      shiny::h2("References"),
                                      shiny::p("List of references and citations can go here.")
                      )
    )
  )
  
  #### SERVER ####
  # Define server logic
  server <- function(input, output, session) {
    # Observe file input and update corpus
    shiny::observe({
      if (!is.null(input$uploadRds)) {
        new_corpus <- readRDS(input$uploadRds$datapath)
        corpus(new_corpus)
      }
    })
    
    query_result <- shiny::reactive({
      query <- input$query
      window_size <- input$window
      case_sensitive <- input$caseSensitive
      corpus_data <- corpus()
      
      if (is.null(corpus_data)) {
        return(data.frame(Message = "Please upload an RDS file to load the corpus."))
      }
      
      if (query != "") {
        # Tokenize and query the corpus
        result <- quanteda::kwic(corpus_data, pattern = query, valuetype = "regex", window = window_size, case_insensitive = !case_sensitive)
        
        if (nrow(result) > 0) {
          result
        } else {
          data.frame(Message = "No matches found.")
        }
      }
    })
    
    # Compute keyword statistics
    keyword_stats <- shiny::reactive({
      result <- query_result()
      if (nrow(result) > 0 && !("Message" %in% names(result))) {
        keyword_freq <- result %>%
          dplyr::as_tibble() %>%
          dplyr::count(keyword) %>%
          dplyr::mutate(Percentage = (n / sum(n)) * 100)
        return(keyword_freq)
      } else {
        return(data.frame(Keyword = character(), Count = integer(), Percentage = numeric()))
      }
    })
    
    # Modify the renderDT for query results to include the expand option
    # Render data table for query results
    output$queryResult <- DT::renderDT({
      DT::datatable(query_result(),
                    options = list(pageLength = 10,
                                   scrollX = TRUE,
                                   autoWidth = TRUE))
    })
    
    # Render data table for keyword statistics
    output$keywordStats <- DT::renderDT({
      DT::datatable(keyword_stats(),
                    options = list(pageLength = 10,
                                   scrollX = TRUE,
                                   autoWidth = TRUE))
    })
    
    # Render plot for keyword statistics
    output$keywordPlot <- shiny::renderPlot({
      keyword_freq <- keyword_stats()
      if (nrow(keyword_freq) > 0) {
        ggplot2::ggplot(keyword_freq, ggplot2::aes(x = keyword, y = Percentage)) +
          ggplot2::geom_point(color = "steelblue") +
          ggplot2::labs(x = "Keyword", y = "Percentage", title = "Keyword Frequency Plot") +
          ggplot2::coord_flip() +
          ggplot2::theme_light() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      }
    })
    
    # Download handlers for results
    output$downloadCsv <- shiny::downloadHandler(
      filename = function() {
        paste("query_results_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        utils::write.table(query_result(), file, sep = "\t", row.names = FALSE)
      }
    )
    
    output$downloadXlsx <- shiny::downloadHandler(
      filename = function() {
        paste("query_results_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        openxlsx::write.xlsx(query_result(), file, rowNames = FALSE)
      }
    )
    
    output$downloadRds <- shiny::downloadHandler(
      filename = function() {
        paste("query_results_", Sys.Date(), ".rds", sep = "")
      },
      content = function(file) {
        saveRDS(query_result(), file)
      }
    )
  }
  
  # Create Shiny app
  shiny::shinyApp(ui = ui, server = server)

}
