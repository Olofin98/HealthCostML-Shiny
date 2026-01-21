
# Load required libraries for Shiny app, data manipulation, and string handling
library(shiny)
library(dplyr)
library(stringr)
library(dplyr)
library(forcats)
library(readr)


# Load insurance dataset and preprocess variables
data <- read_csv("expenses.csv") %>%
  mutate(
    # Convert numerical variables to appropriate types
    Age = as.numeric(age),
    BMI = as.numeric(bmi),
    Charges = as.numeric(charges),
    
    # Standardize and convert categorical variables to factors with fixed reference levels
    Sex = factor(stringr::str_to_title(sex), levels = c("Female","Male")),
    Smoker = factor(stringr::str_to_title(smoker), levels = c("No","Yes")),
    Region = factor(stringr::str_to_title(region),
                    levels = c("Northeast","Northwest","Southeast","Southwest")),
    
    # Convert number of children to integer and create labeled factor version (for reporting/EDA)
    Children = as.integer(children),
    Children_fct = factor(Children, levels = 0:5,
                          labels = c("Zero","One","Two","Three","Four","Five")),
    
    # Log-transform annual medical charges for modeling
    log_cost = log(Charges)
  ) %>%
  # Retain only variables required for modeling and prediction
  select(Age, Sex, BMI, Children, Children_fct, Smoker, Region, Charges, log_cost)

# Fit log–linear multiple regression model for annual medical expenditures
model <- lm(log_cost ~ Age + Sex + BMI + Children + Smoker + Region, data = data)

# Extract model frame to obtain factor levels used during model training
mf <- model.frame(model)
sex_levels    <- levels(mf$Sex)
smoker_levels <- levels(mf$Smoker)
region_levels <- levels(mf$Region)

# Define the user interface for the Shiny application
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("ACME Insurance: Medical Charges Estimator"),
  sidebarLayout(
    sidebarPanel(
      # User input section
      HTML("<h3>Input parameters</h3>"),
      selectInput("sex", "Sex:", choices = sex_levels, selected = sex_levels[1]),
      selectInput("smoker", "Smoking:", choices = smoker_levels, selected = smoker_levels[1]),
      numericInput("age", "Age:", min = 18, max = 64, value = 30, step = 1),
      numericInput("bmi", "BMI:", value = 30, min = 10, max = 60, step = 0.1),
      numericInput("children", "Number of Children", value = 0, min = 0, max = 5, step = 1),
      selectInput("region", "Region", choices = region_levels, selected = region_levels[1]),
      sliderInput("loading", "Risk Loading (optional)", min = 0, max = 0.50, value = 0.15, step = 0.01),
      actionButton("go", "Estimate Charges", class = "btn-primary")
    ),
    mainPanel(
      # Output display section
      h4("Estimated Annual Charges"),
      verbatimTextOutput("annual"),
      h4("Estimated Monthly Premium"),
      verbatimTextOutput("monthly"),
      hr(),
      tags$small(
        "Note: This is an estimate based on historical data (Kaggle insurance dataset) ",
        "and may differ from actual medical costs."
      )
    )
  )
)

# Define server-side logic for prediction and output rendering
server <- function(input, output, session) {
  
  # Trigger prediction only when the action button is clicked
  pred <- eventReactive(input$go, {
    
    # Construct a new observation matching the model’s variable structure
    new_customer <- data.frame(
      Age      = as.numeric(input$age),
      Sex      = factor(input$sex, levels = sex_levels),
      BMI      = as.numeric(input$bmi),
      Children = as.integer(input$children),
      Smoker   = factor(input$smoker, levels = smoker_levels),
      Region   = factor(input$region, levels = region_levels)
    )
    
    # Predict log-scale charges and back-transform to original scale
    pred_log <- predict(model, newdata = new_customer)
    annual_charges <- exp(pred_log)
    
    # Compute monthly charges by dividing annual estimate by 12
    list(
      annual = annual_charges,
      monthly = annual_charges / 12
    )
  })
  
  # Render estimated annual charges
  output$annual <- renderText({
    req(pred())
    paste0("$", format(round(pred()$annual, 2), big.mark = ","))
  })
  
  # Render estimated monthly charges
  output$monthly <- renderText({
    req(pred())
    paste0("$", format(round(pred()$monthly, 2), big.mark = ","))
  })
  
}

# Launch the Shiny application
shiny::shinyApp(ui, server)


