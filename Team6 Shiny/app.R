# Team 6 Ponsetto, Schoonover, Takeda
#add libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(randomForest)
library(cluster)

# Load and preprocess data
joined_air_quality <- read.csv("joined_air_quality.csv")

# Ensure appropriate column names and data handling
joined_air_quality <- joined_air_quality %>%
  rename(
    PM.Levels = PM.Levels, 
    population = population,
    Year = Year,
    Borough = Borough
  ) %>%
  drop_na()

# UI
ui <- fluidPage(
  titlePanel("NYC Air Quality Analysis"),
  
  #Side panel for input
  sidebarLayout(
    sidebarPanel(
      selectInput("model_type", "Select Model Type:", 
                  choices = c("Linear Regression", "Random Forest", "Clustering")),
      sliderInput("train_split", "Training Split (%):", min = 50, max = 90, value = 80),
      numericInput("clusters", "Number of Clusters (for Clustering Model):", value = 3, min = 2, max = 10),
      selectInput("xvar", "Choose X-axis variable:", 
                  choices = c("population", "Year", "Borough")),
      selectInput("colorvar", "Choose Color variable:", 
                  choices = c("Borough", "Year", "cluster"), selected = "Borough"),
      actionButton("run_model", "Run Model")
    ),
    
    #Main panel for displaying outputs via tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Data Overview", tableOutput("data_table")),
        tabPanel("Model Summary", verbatimTextOutput("model_summary")),
        tabPanel("Visualization", plotOutput("model_plot")),
        tabPanel("Clustering Results", tableOutput("cluster_results")),
        tabPanel("Data Visualizations",
                 fluidRow(
                   plotOutput("density_plot"),
                   plotOutput("boxplot"),
                   plotOutput("scatter_plot"),
                   plotOutput("line_plot")
                 )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Ensure required columns are present
  validate_data <- reactive({
    required_cols <- c("PM.Levels", "population", "Year", "Borough")
    missing_cols <- setdiff(required_cols, colnames(joined_air_quality))
    if (length(missing_cols) > 0) {
      stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    }
    joined_air_quality
  })
  
  # Reactive dataset for processing
  data_reactive <- reactive({
    validate_data() %>% drop_na()
  })
  
  # Reactive scaling for clustering
  data_s <- reactive({
    scale(data_reactive() %>% select_if(is.numeric))
  })
  
  # Reactive data split for training and testing
  split_data <- reactive({
    set.seed(100)
    train_ind <- sample(1:nrow(data_reactive()), size = input$train_split / 100 * nrow(data_reactive()))
    list(
      train = data_reactive()[train_ind, ],
      test = data_reactive()[-train_ind, ]
    )
  })
  
  # Visualizations
  # Density plot of PM levels by borough
  output$density_plot <- renderPlot({
    ggplot(data_reactive(), aes(x = PM.Levels, fill = Borough)) +
      geom_density(alpha = 0.5) +
      labs(title = "Density Plot of PM Levels by Borough", x = "PM Levels", y = "Density") +
      theme_minimal() +
      scale_fill_manual(values = c("red", "blue", "green", "orange", "purple"))
  })
  
  #Boxplot of PM levels by year
  output$boxplot <- renderPlot({
    ggplot(data_reactive(), aes(x = factor(Year), y = PM.Levels, fill = factor(Year))) +
      geom_boxplot() +
      labs(title = "PM Levels by Year", x = "Year", y = "PM Levels") +
      theme_minimal()
  })
  #Scatter plot of population vs PM levels
  output$scatter_plot <- renderPlot({
    ggplot(data_reactive(), aes(x = population, y = PM.Levels)) +
      geom_point(aes(color = Borough), alpha = 0.7) +
      labs(title = "Population vs PM Levels", x = "Population", y = "PM Levels") +
      scale_x_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  #Line plot of PM levels over time
  output$line_plot <- renderPlot({
    avg_pm_year_borough <- data_reactive() %>%
      group_by(Year, Borough) %>%
      summarise(avg_PM = mean(PM.Levels, na.rm = TRUE), .groups = "drop")
    
    ggplot(avg_pm_year_borough, aes(x = Year, y = avg_PM, color = Borough, group = Borough)) +
      geom_line(size = 1) +
      labs(title = "PM Levels Over Time by Borough", x = "Year", y = "Average PM Levels") +
      theme_minimal() +
      theme(legend.title = element_blank()) +
      scale_color_brewer(palette = "Set1") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Run selected model when the "run model" button is clicked
  observeEvent(input$run_model, {
    output$model_summary <- renderPrint({
      if (input$model_type == "Linear Regression") {
        model <- lm(PM.Levels ~ population + Year, data = split_data()$train)
        summary(model)
      } else if (input$model_type == "Random Forest") {
        model <- randomForest(PM.Levels ~ population + Year + Borough, data = split_data()$train)
        print(model)
      } else {
        kmeans_result <- kmeans(data_s(), centers = input$clusters)
        list(Centers = kmeans_result$centers, ClusterCounts = table(kmeans_result$cluster))
      }
    })
    
    #Render model plot based on selected model type
    output$model_plot <- renderPlot({
      if (input$model_type == "Linear Regression") {
        model <- lm(PM.Levels ~ population + Year, data = split_data()$train)
        ggplot(split_data()$train, aes_string(x = input$xvar, y = "PM.Levels", color = input$colorvar)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE) +
          labs(title = "Linear Regression: PM Levels", x = input$xvar, y = "PM Levels") +
          theme_minimal()
      } else if (input$model_type == "Random Forest") {
        test <- split_data()$test
        model <- randomForest(PM.Levels ~ population + Year + Borough, data = split_data()$train)
        test$predictions <- predict(model, newdata = test)
        ggplot(test, aes(x = PM.Levels, y = predictions)) +
          geom_point(color = "lightblue", alpha = 0.7) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgreen") +
          labs(title = "Random Forest: Actual vs Predicted", x = "Actual PM Levels", y = "Predicted PM Levels") +
          theme_minimal()
      } else {
        kmeans_result <- kmeans(data_s(), centers = input$clusters)
        data_reactive()$cluster <- as.factor(kmeans_result$cluster)
        ggplot(data_reactive(), aes_string(x = input$xvar, y = "PM.Levels", color = "cluster")) +
          geom_point() +
          labs(title = "Clustering: PM Levels", x = input$xvar, y = "PM Levels") +
          theme_minimal()
      }
    })
    
    output$cluster_results <- renderTable({
      if (input$model_type == "Clustering") {
        kmeans_result <- kmeans(data_s(), centers = input$clusters)
        as.data.frame(table(kmeans_result$cluster))
      } else {
        NULL
      }
    })
  })
  
  # Display data overview
  output$data_table <- renderTable({
    head(data_reactive())
  })
}

# Run the app
shinyApp(ui, server)
