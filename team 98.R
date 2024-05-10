
# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(arules)
# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "My first app",

                  tabPanel("K-means",
                           sidebarPanel(
                             tags$h3("Input:"), 
                             fileInput("data", "Choose CSV File",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             sliderInput("clusters", "Number of Clusters",
                                         min = 2, max =4,
                                         value = 4, animate = TRUE)    ,                         
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1(" Clusters"),
                             tableOutput("cluster_table"),
                             tableOutput("point_cluster_table"),
                             plotOutput("cluster_plot")
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Data Analysis Visualization",
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("file", "Upload CSV file"),
                               selectInput("plot_type", "Select Plot Type", choices = c("Pie Chart", "Scatter Plot", "Bar Plot", "Box Plot", "Histogram")),
                               conditionalPanel(
                                 condition = "input.plot_type != 'Pie Chart'",
                                 sliderInput("num_bins", "Number of Bins", min = 1, max = 50, value = 10)
                               )
                             ),
                             mainPanel(
                               plotOutput("plot")
                             )
                           )),
                  tabPanel("Apriori Algorithm",
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("file", "Upload CSV file"),
                               sliderInput("confidence", "Confidence", min = 0.001, max = 1, value = 0.5, step = 0.001),
                               sliderInput("support", "Support", min = 0.001, max = 1, value = 0.1, step = 0.001)
                             ),
                             mainPanel(
                               tableOutput("summary_table"),
                               tableOutput("apriori_results")
                             )
                           ))
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  processed_data <- reactive({
    # Remove duplicated rows
    data <- unique(data())
    
    # Remove rows with missing values
    data <- na.omit(data)
    
    # Convert data types to integer
    data$total <- as.integer(data$total)
    data$age <- as.integer(data$age)
    
    # Outlier detection and removal
    outliers_total <- boxplot(data$total, plot = FALSE)$out
    outliers_age <- boxplot(data$age, plot = FALSE)$out
    
    data <- data[!(data$total %in% outliers_total | data$age %in% outliers_age), ]
    
    return(data)
  })
  kmeanData <- reactive({
    req(data())
    data() %>% 
      select(total, age) %>% 
      scale()
  })
  
  clusters <- reactive({
    req(kmeanData())
    kmeans(kmeanData(), centers = input$clusters, iter.max = 100, nstart = 50)
  })
  
  output$cluster_plot <- renderPlot({
    req(clusters())
    # Extract cluster centers
    centers <- clusters()$centers
    
    # Add cluster assignment to original data
    data_with_clusters <- data() %>%
      select(total, age) %>%
      mutate(cluster = clusters()$cluster)
    
    # Plot the data points with cluster centers
    ggplot(data_with_clusters, aes(x = total, y = age, color = factor(cluster))) +
      geom_point() +
      geom_point(data = as.data.frame(centers), aes(x = total, y = age), color = "black", size = 3, shape = 17) +
      labs(title = "K-means Clustering",
           x = "Total Spending",
           y = "Age",
           color = "Cluster") +
      theme_minimal()
  })
  output$cluster_table <- renderTable({
    req(clusters())
    data_with_clusters <- data() %>%
      select(names) %>%
      mutate(cluster = clusters()$cluster)
    data_with_clusters
  })
  output$point_cluster_table <- renderTable({
    req(clusters())
    data_with_clusters <- data() %>%
      select(names) %>%
      mutate(cluster = clusters()$cluster)
    data_with_clusters
  })
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  output$plot <- renderPlot({
    plot_type <- input$plot_type
    
    if (plot_type == "Pie Chart") {
      paymentType <- table(data()$paymentType)
      percentage <- paste0(round(100 * paymentType / sum(paymentType), 2), "%")
      pie(paymentType, labels = percentage, edges = 50, radius = 0.9, clockwise = TRUE,
          main = "Comparison of Cash and Credit Card Total", col = c("green", "blue"))
      legend("bottomright", legend = c("Cash", "Credit cards"), fill = c("green", "blue"))
    } else if (plot_type == "Scatter Plot") {
      ggplot(data(), aes(x = total, y = age)) +
        geom_point() +
        labs(title = "Scatter Plot of Total Spending vs Age", x = "Total Spending", y = "Age")
    } else if (plot_type == "Bar Plot") {
      ggplot(data(), aes(x = city, y = total)) +
        geom_bar(stat = "identity") +
        labs(title = "Total Spending by City", x = "City", y = "Total Spending") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (plot_type == "Box Plot") {
      ggplot(data(), aes(x = "", y = total)) +
        geom_boxplot(fill = "skyblue") +
        labs(title = "Distribution of Total Spending", x = NULL, y = "Total Spending")
    } else if (plot_type == "Histogram") {
      ggplot(data(), aes(x = total)) +
        geom_histogram(bins = input$num_bins, fill = "blue", color = "black") +
        labs(title = "Histogram of Total Spending", x = "Total Spending", y = "Frequency")
    }
  })
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  cleaned_data <- reactive({
    # Assuming 'items' is the column containing itemsets
    data() %>% select(items)
  })
  
  output$summary_table <- renderTable({
    summary(cleaned_data())
  })
  
  output$apriori_results <- renderTable({
    # Write itemsets to a text file
    write.table(cleaned_data(), "ItemSet.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
    
    # Read transactions from the text file
    txtBasedCleaned_data <- read.transactions("ItemSet.txt", format = "basket", sep = ",")
    
    # Apply Apriori algorithm
    ruleNo1 <- apriori(txtBasedCleaned_data, parameter = list(minlen = 2, support = input$support, conf = input$confidence))
    
    # Return the results
    inspect(head(ruleNo1, 15))
  })
  
} # server
# Create Shiny object
shinyApp(ui = ui, server = server)
