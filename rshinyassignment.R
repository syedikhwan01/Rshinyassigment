library(shiny)
library(bslib)
library(readxl)
library(ChainLadder)

ui <- page_sidebar(
  title = "Cumulative Claim Paid",
  sidebar = sidebar(
    fileInput("upload", "Choose File"),
    sliderInput("tail", label = "Tail Factor:", min = 0, max = 3, value = 1.1, step = 0.1)
  ),
  navset_card_underline(
    title = "Summary",
    nav_panel("Plot", plotOutput("plot")),
    nav_panel("Table", tableOutput("table"))
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)
    inFile <- input$upload
    if (is.null(inFile)) {
      return(NULL)
    }
    raw_data <- readxl::read_excel(inFile$datapath)
    claim_data <- data.frame(loss_year = raw_data$loss_year, 
                             development_year = raw_data$development_year, 
                             claim_paid = raw_data$claim_paid)
    incrementaltriangle <- as.triangle(claim_data, 
                                       origin = "loss_year", 
                                       dev = "development_year", 
                                       value = "claim_paid")
    ####add
    cummulativetriangle <- incr2cum(incrementaltriangle, na.rm = FALSE)
    ####add
    n <- ncol(cummulativetriangle)
    f <- sapply(1:(n - 1), function(i) {
      sum(cummulativetriangle[c(1:(n - i)), i + 1]) / sum(cummulativetriangle[c(1:(n - i)), i])
    })
    tail <- input$tail
    list(f = f, cummulativetriangle = cummulativetriangle, tail = tail,loss_year = raw_data$loss_year)
  })
  
  output$table <- renderTable({
    data_list <- data()
    f <- c(data_list$f, data_list$tail)
    full_triangle <- cbind(data_list$cummulativetriangle, Ult = rep(0, ncol(data_list$cummulativetriangle)))
    for (k in 1:ncol(data_list$cummulativetriangle)) {  
      full_triangle[(ncol(data_list$cummulativetriangle) - k + 1):ncol(data_list$cummulativetriangle), k + 1] <- 
        full_triangle[(ncol(data_list$cummulativetriangle) - k + 1):ncol(data_list$cummulativetriangle), k] * f[k]
    }
    round_full_triangle <- round(full_triangle)
    unique_loss_years <- unique(data_list$loss_year)
  
  
    round_full_triangle <- cbind("Accident Year" = unique_loss_years, round_full_triangle)
  
    colnames(round_full_triangle)[2:ncol(round_full_triangle)] <- as.character(1:(ncol(round_full_triangle)-1))
    colnames(round_full_triangle)[2:5] <- paste("Development Year", 1:4)
    round_full_triangle
  }, digits = 0)
  
  output$plot <- renderPlot({
    data_list <- data()
    f <- c(data_list$f, data_list$tail)
    full_triangle <- cbind(data_list$cummulativetriangle, Ult = rep(0, ncol(data_list$cummulativetriangle)))
    for (k in 1:ncol(data_list$cummulativetriangle)) {  
      full_triangle[(ncol(data_list$cummulativetriangle) - k + 1):ncol(data_list$cummulativetriangle), k + 1] <- 
        full_triangle[(ncol(data_list$cummulativetriangle) - k + 1):ncol(data_list$cummulativetriangle), k] * f[k]
    }
    round_full_triangle <- round(full_triangle)
    colnames(round_full_triangle) <- as.character(1:ncol(round_full_triangle))  # Set column names dynamically
    round_full_triangle
    
    full_triangle <- round_full_triangle
    graph <- plot(full_triangle[1, ], type = "l", col = "red", ylim = c(400000, 1400000), 
                  xlab = "Development Year", ylab = "Amount of Claim ($)", main = "Cumulative Paid Claims", xaxt = "n")
    lines(full_triangle[2, ], col = "blue")
    lines(full_triangle[3, ], col = "green")
    axis(1, at = 1:ncol(full_triangle), labels = 1:ncol(full_triangle)) 
    legend("bottomright", c("2017", "2018", "2019"), col = c("red", "blue", "green"), lty = 1)
    text(x = 1:length(full_triangle[1, ]), y = full_triangle[1, ], labels = full_triangle[1, ], pos = 3, col = "red")
    text(x = 1:length(full_triangle[2, ]), y = full_triangle[2, ], labels = full_triangle[2, ], pos = 3, col = "blue")
    text(x = 1:length(full_triangle[3, ]), y = full_triangle[3, ], labels = full_triangle[3, ], pos = 3, col = "green")
    grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
    graph
  })
}

shinyApp(ui, server)