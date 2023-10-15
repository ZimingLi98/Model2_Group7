library(shiny)
library(caret)
library(rpart)

# UI design
ui <- fluidPage(
  
  # For images
  tags$style(type="text/css", 
             ".rightCenteredImage { 
               position: absolute; 
               top: 350px;
               transform: translateY(-50%);
               right: 80px;
            }"),
  
  tags$head(
    tags$style(".navbar-default {background-color: #8B0000; border-color: #8B0000;} 
                   .navbar-default .navbar-brand {color: white;}
                   .navbar-default .navbar-brand:hover {color: white;}
                   .navbar-default .navbar-nav > li > a {color: white;}
                   .navbar-default .navbar-nav > li > a:hover {color: white;}")
  ),
  
  navbarPage(title = "Test your body fat today!", id = "navbar",
             tabPanel("Welcome!",
                      h3("Free body fat predictor", style = "color: black;"),
                      h3("Press menu to start!", style = "color: black;"),
                      p("Use this application to predict your body fat percentage based on various models.", 
                        style = "font-size: 16px;"),
                      shiny::fluidPage(
                        shiny::tags$img(src = "2.jpg", height = "700px", width = "1200px")
                      )),
             
             # For menu
             navbarMenu("Menu",
                        tabPanel("Instructions",
                                 h3("How to use:", style = "color: black;"),
                                 p("1. Navigate to the 'Prediction' tab.", style = "font-size: 16px;"),
                                 p("2. Choose a model based on how many variables you know.", style = "font-size: 16px;"),
                                 p(" -Select Model 1 if you know a little about your body parameters",style = "font-size: 16px;"),
                                 p(" -Select Model 2 if you know a lot about your body parameters",style = "font-size: 16px;"),
                                 p("3. Input your value.", style = "font-size: 16px;"),
                                 p("4. Click 'Predict' to get your results.", style = "font-size: 16px;")),
                        
                        # Design the input 
                        tabPanel("Prediction",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("model_choice", "Model:", choices = c("Model 1",
                                                                                       "Model 2")),
                                     conditionalPanel(
                                       condition = "input.model_choice == 'Model 1'",
                                       numericInput("weight1", "Weight (lbs):", 160, min = 1, max = 300),
                                       numericInput("wrist1", "Wrist (cm):", 17, min = 14, max = 20),
                                       numericInput("abdomen1", "Abdomen (cm):", 85, min = 50, max = 150)
                                     ),
                                     conditionalPanel(
                                       condition = "input.model_choice == 'Model 2'",
                                       numericInput("weight2", "Weight (lbs):", 150, min = 1, max = 300),
                                       numericInput("height2", "Height (inch):", 67, min = 50, max = 250),
                                       numericInput("age2", "Age:", 30, min = 0, max = 100),
                                       numericInput("adiposity", "Adiposity", 25, min = 1, max = 100),
                                       numericInput("abdomen2", "Abdomen (cm):", 85, min = 50, max = 150),
                                       numericInput("wrist2", "Wrist (cm):", 17, min = 14, max = 20),
                                       numericInput("thigh", "Thigh (cm):", 55, min = 20, max = 100),
                                       numericInput("hip", "HIP(cm):", 95, min = 70, max = 120),
                                       numericInput("ankle", "Ankle(cm):", 22, min = 15, max = 30),
                                       numericInput("forearm", "Forearm(cm):", 28, min = 20, max = 35),
                                       numericInput("neck", "Neck(cm):", 40, min = 30, max = 50)),
                                     actionButton("predict_btn", "Predict", class = "btn btn-danger btn-lg")
                                   ),
                                   mainPanel(
                                     textOutput("errorMsg"),
                                     h3("Result:", style = "color: #8B0000;"), 
                                     verbatimTextOutput("prediction"),
                                     uiOutput("adviceImage")
                                   )
                                 )
                        ),
                        
                        tabPanel("Contact",
                                 h3("Have questions?", style = "color: black;"),
                                 p("Please reach out to any of our group member:", style = "font-size: 16px;"),
                                 p(tags$a(href = "mailto:bchen342@wisc.edu", "Baiheng Chen: bchen342@wisc.edu")),
                                 p(tags$a(href = "mailto:wtu25@wisc.edu", "Wanxin Tu: wtu25@wisc.edu")),
                                 p(tags$a(href = "mailto:zli2543@wisc.edu", "Ziming Li: zli2543@wisc.edu")),
                                 p(tags$a(href = "https://www.vectorstock.com/royalty-free-vector/body-fat-composition-vector-40785532", "Some images from: VectorStock", target="_blank"))
                        )
             )
  )
)

server <- function(input, output) {
  
  # For model loading, include store models
  model_1_reactive <- reactiveVal()
  model_2_reactive <- reactiveVal()

  env1 <- new.env()
  load("model_with3.RData", envir = env1)
  model_1_reactive(env1$model_with3)
  
  env2 <- new.env()
  load("model_2.RData", envir = env2)
  model_2_reactive(env2$step_model_aic) 
  
  
  # store results of prediction
  predictionValue <- reactiveVal()
  
  # For input
  observeEvent(input$predict_btn, {
    if(input$weight1 < 0 || input$wrist1 < 0 || input$abdomen1 < 0 || 
       input$weight2 < 0 || input$height2 < 0 || input$age2 < 0 ||
       input$adiposity < 0 || input$abdomen2 < 0 || input$wrist2 < 0 ||
       input$thigh < 0 || input$hip < 0 || input$ankle < 0 ||
       input$forearm < 0 || input$neck < 0) {
      output$errorMsg <- renderText({ "Please enter valid positive numbers." })
      return()
    }
    output$errorMsg <- renderText({ "" })
    if(input$model_choice == "Model 1") {
      new_data <- data.frame(ABDOMEN = input$abdomen1, WRIST = input$wrist1,WEIGHT = input$weight1)
      prediction <- predict(model_1_reactive(), new_data)
      predictionValue(round(prediction, 2))
    } else if(input$model_choice == "Model 2") {
      new_data <- data.frame(WEIGHT = input$weight2, 
                             HEIGHT = input$height2, 
                             AGE = input$age2, 
                             ADIPOSITY = input$adiposity,
                             NECK = input$neck,
                             ABDOMEN = input$abdomen2, 
                             HIP = input$hip,
                             ANKLE = input$ankle,
                             FOREARM = input$forearm,
                             WRIST = input$wrist2, 
                             THIGH = input$thigh)
      prediction <- predict(model_2_reactive(), new_data)  
      predictionValue(round(prediction, 2))
    }
  })
  
  # For output
  output$prediction <- renderText({
    result <- predictionValue()
    if (is.null(result)) return(NULL)
    if (result <= 15) {
      paste("Prediction: ", result, "% body fat. \nYour body fat percentage is in the very low range!")
    } else if (result > 15 && result <= 18) {
      paste("Prediction: ", result, "% body fat.\nYour body fat percentage is in the lower range!")
    } else if ( result > 18 && result <= 22) {
      paste("Prediction: ", result, "% body fat.\nYour body fat percentage is within the normal range!")
    } else if (result > 22 && result <= 30){
      paste("Prediction: ", result, "% body fat.\nYour body fat percentage is a bit high!")
    } else if (result >30){
      paste("Prediction: ", result, "% body fat.\nYour body fat percentage is too high!")
    }
  })
  
  output$adviceImage <- renderUI({
    result <- predictionValue()
    if (is.null(result)) return(NULL)
    if (result <= 15 ) {
      tags$img(src = "a.jpg", height = "450px", width = "300px", class = "rightCenteredImage")
    } else if (result > 15 && result <= 18) {
      tags$img(src = "b.jpg", height = "450px", width = "300px", class = "rightCenteredImage")
    } else if ( result > 18 && result <= 22){
      tags$img(src = "c.jpg", height = "450px", width = "300px", class = "rightCenteredImage")
    } else if ( result > 22 && result <= 30){
      tags$img(src = "d.jpg", height = "450px", width = "300px", class = "rightCenteredImage")
    } else if ( result >30 ){
      tags$img(src = "e.jpg", height = "450px", width = "300px", class = "rightCenteredImage")
    }
  })
  
}


shinyApp(ui, server)
