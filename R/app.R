
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
  
  # Define a reactive value for the corpus
  corpus <- shiny::reactiveVal(NULL)
  

# UI ----------------------------------------------------------------------

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
                        shiny::checkboxInput("caseSensitive", "Case sensitive", value = FALSE),
                        shiny::fileInput("uploadRds", "Upload your corpus in RDS format:", accept = ".rds"),
                        
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
                    shiny::p("This is a Shiny app for querying and analyzing text corpora using the quanteda package. It offers a user-friendly interface to perform corpus linguistic analyses without the need for manual coding."),
                    
                    shiny::h3("Key Features and Uses"),
                    shiny::tags$ul(
                      shiny::tags$li(shiny::strong("Educational Tool:"), "It is primarily intended for intermediate and advanced students of linguistics who are learning how to conduct corpus linguistic analyses. It provides a hands-on way to explore corpus linguistics techniques."),
                      
                      shiny::tags$li(shiny::strong("Research Assistant:"), "Open to anyone requiring a fast and lightweight interface to the quanteda package without the need to code queries manually in R. As such, it is ideal for anyone who needs to analyse text data swiftly"),
                      
                      shiny::tags$li(shiny::strong("Quick Export Options:"), "This app offers easy export functionality without the need for additional coding. Users can immediately save their results for further analysis or annotation."),
                      
                      shiny::tags$li(shiny::strong("Suitable for Small to Medium-sized Corpora:"), "Particularly useful for working with corpora that are small to medium-sized and available to the user on disk. As such, it provides efficient processing for these types of datasets."),
                      
                      shiny::tags$li(shiny::strong("Streamlined Workflow:"), "The GUI minimises the work required to go from entering a query to performing data annotation on the exported spreadsheet document, thereby allowing users to  move quickly from data exploration to detailed analysis."),
                      
                      shiny::h3("Open Source"),
  shiny::p("This app is open source and distributed under the GNU General Public License version 3 (GPLv3). This means you are free to use, modify, and distribute the software under the terms of this license. For full details of the GPLv3 license, please visit: ", 
    shiny::a("https://www.gnu.org/licenses/gpl-3.0.html", href="https://www.gnu.org/licenses/gpl-3.0.html", target="_blank")),
  shiny::p("The full source code for this app is available on GitHub at: ",
    shiny::a("https://github.com/VBuskin/quanteda_app/blob/main/R/app.R", href="https://github.com/VBuskin/quanteda_app/blob/main/R/app.R", target="_blank")),
  shiny::p("Users are to explore the code, contribute improvements, and adapt it for their own needs, in accordance with the GPLv3 license.")
                    ),
    ),
    
    
    # Help tab
    shiny::tabPanel("Help",
                    shiny::h2("How to Use This App"),
                    shiny::h3("Menu Bar"),
                    shiny::tags$ul(
                      shiny::tags$li(shiny::strong("Query:"), "Enter your search terms here. The app accepts regular expressions, allowing for complex pattern matching."),
                      shiny::tags$li(shiny::strong("Window size:"), "Adjust this slider to set the number of characters displayed to the left and right of the keyword in the search results. A larger window provides more context."),
                      shiny::tags$li(shiny::strong("Upload your RDS file:"), "Use this to supply your corpus file. The file should be in RDS format and contain a quanteda corpus object. Note that this object should be the result of applying quanteda::tokens() and quanteda::corpus() functions to your collection of .txt files."),
                      shiny::tags$li(shiny::strong("Choose your export format:"), "Use these buttons to save your query results in your preferred format (CSV, XLSX, or RDS).")
                    ),
                    shiny::h3("Results Panels"),
                    shiny::tags$ul(
                      shiny::tags$li(shiny::strong("Query Results:"), "This panel displays the output of your search expression. It shows each occurrence of the keyword in context, based on the window size you've set."),
                      shiny::tags$li(shiny::strong("Keyword Statistics:"), "Here you'll find a breakdown of the absolute and proportional frequencies of each unique match in the corpus. This helps you understand how often different keywords appear relative to each other."),
                      shiny::tags$li(shiny::strong("Keyword Plot:"), "This panel provides a simple dotplot visualization of the keyword statistics. It offers a quick, visual way to compare the frequencies of different keywords.")
                    ),
                    shiny::h3("Tips for Effective Use"),
                    shiny::tags$ul(
                      shiny::tags$li("Start with a small window size and increase it if you need more context."),
                      shiny::tags$li("Use regular expressions in your queries for more powerful searches. For example, use '\\b' to match word boundaries."),
                      shiny::tags$li("Export your results for further analysis in other tools.")
                    )
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
  
  

# Server ------------------------------------------------------------------

  # Define server logic
  server <- function(input, output, session) {
    # Observe file input and update corpus
    shiny::observe({
      if (!is.null(input$uploadRds)) {
        new_corpus <- readRDS(input$uploadRds$datapath)
        corpus(new_corpus)
      }
    })
    
    # Query results
    query_result <- shiny::reactive({
      query <- input$query
      window_size <- input$window
      case_sensitive <- input$caseSensitive
      corpus_data <- corpus()
      
      # Upload .RDS file
      if (is.null(corpus_data)) {
        return(data.frame(Message = "Please upload an RDS file to load the corpus."))
      }
      
      if (query != "") {
        # Perform actual query on corpus object
        result <- quanteda::kwic(corpus_data, 
                                 pattern = query, 
                                 valuetype = "regex", 
                                 window = window_size, 
                                 case_insensitive = !case_sensitive)
        
        if (nrow(result) > 0) {
          result
        } else {
          # else return nothing
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
            # Count unique keywords
            dplyr::count(keyword) %>%
            # Convert n to pct
            dplyr::mutate(Percentage = (n / sum(n)) * 100)
          return(keyword_freq)
        } else {
          # else return nothing
          return(data.frame(Keyword = character(), Count = integer(), Percentage = numeric()))
        }
      })
    
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
  
  # Create Shiny app object
  shiny::shinyApp(ui = ui, server = server)

}
