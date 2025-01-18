install.packages("dplyr")
install.packages("shiny")
install.packages("randomForest")
install.packages("DT")
# Load necessary libraries
library(shiny)
library(randomForest)
library(dplyr)
library(DT)


data <- read.csv(file.choose())

# Summarize missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
missing_values <- missing_values[missing_values > 0]

# Removing columns with high missing values
cols_high_missing <- c("Alley", "Pool.QC", "Fence", "Misc.Feature")
data <- data %>% select(-any_of(cols_high_missing))

# Handling missing values in 'Lot.Frontage'
data$Lot.Frontage[is.na(data$Lot.Frontage)] <- 0  # Replacing NA values in Lot Frontage with 0


# Removing rows with missing values in specific columns
columns_to_remove <- c("BsmtFin.SF.1", "BsmtFin.SF.2", "Bsmt.Unf.SF", "Total.Bsmt.SF", "Bsmt.Full.Bath", "Bsmt.Half.Bath",
                       "Garage.Cars", "Garage.Area")
data <- data[complete.cases(data[, columns_to_remove]), ]

# Replacing null values in basement and fireplace columns
basement_cols <- c("Bsmt.Exposure", "Bsmt.Qual", "Bsmt.Cond", "BsmtFin.Type.1", "BsmtFin.Type.2")
data[basement_cols] <- lapply(data[basement_cols], function(x) ifelse(is.na(x), "No", x))

data$Mas.Vnr.Area[is.na(data$Mas.Vnr.Area)] <- mean(data$Mas.Vnr.Area, na.rm = TRUE)

data <- data[complete.cases(data[c("Garage.Type", "Garage.Yr.Blt", "Garage.Finish", 
                                   "Garage.Cond", "Garage.Qual")]), ]

data$Fireplace.Qu <- ifelse(is.na(data$Fireplace.Qu), "No", data$Fireplace.Qu)

# Creating Total Bathrooms column
data$Total.Bathrooms <- with(data, Bsmt.Full.Bath + 0.5 * Bsmt.Half.Bath + Full.Bath + 0.5 * Half.Bath)

# Selecting relevant features and adding a Garage indicator
data$Garage <- ifelse(data$Garage.Cars > 0, "Yes", "No")
selected_features <- c("Bedroom.AbvGr", "Total.Bathrooms", "Garage", "Year.Built", "SalePrice")
data <- data[selected_features]

# Final missing values check
sum(is.na(data))

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
training_rows <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[training_rows, ]
test_data <- data[-training_rows, ]

# Train the Random Forest model
model <- randomForest(SalePrice ~ ., data=train_data, importance=TRUE, ntree=500)


# R Shiny Application with updated features
ui <- fluidPage(
  titlePanel("Real Estate Price Prediction Tool"),
  sidebarLayout(
    sidebarPanel(
      numericInput("bedrooms", "Number of Bedrooms", value = 3, min = 1, max = 10),
      sliderInput("totalBathrooms", "Total Bathrooms", min = 0, max = 10, value = 2, step = 0.5),
      selectInput("garage", "Garage", choices = c("Yes" = "Yes", "No" = "No")),
      numericInput("yearBuilt", "Year Built", value = 1990, min = 1900, max = 2020),
      actionButton("predict", "Predict Sale Price")
    ),
    mainPanel(
      textOutput("predictedPrice"),
      textOutput("rating"),
      DT::dataTableOutput("similarProperties")
    )
  )
)

server <- function(input, output) {
  prediction <- eventReactive(input$predict, {
    new_data <- data.frame(
      Bedroom.AbvGr = input$bedrooms,
      Total.Bathrooms = input$totalBathrooms,
      Garage = input$garage,
      Year.Built = input$yearBuilt
    )
    predict(model, newdata=new_data)
  })
  output$predictedPrice <- renderText({
    paste("Predicted Sale Price: $", round(prediction(), 2))
  })
  
  output$rating <- renderText({
    predicted_price <- prediction()
    rating <- if(predicted_price > 300000) {
      "A"
    } else if(predicted_price > 200000) {
      "B"
    } else if(predicted_price > 100000) {
      "C"
    } else {
      "D"
    }
    paste("Rating: ", rating)
  })
  
  output$similarProperties <- DT::renderDataTable({
    similar <- data %>%
      filter(
        Bedroom.AbvGr == input$bedrooms,
        Total.Bathrooms >= input$totalBathrooms - 0.5 & Total.Bathrooms <= input$totalBathrooms + 0.5,
        Garage == input$garage,
        Year.Built >= input$yearBuilt - 5 & Year.Built <= input$yearBuilt + 5
      ) %>%
      head(5)
    DT::datatable(similar, options = list(pageLength = 5))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
