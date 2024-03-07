library(shiny)
library(epidemics)
library(ggplot2)
library(dplyr)
library(socialmixr) # Ensure this package is installed for `socialmixr::polymod`

# Define UI
ui <- fluidPage(
  titlePanel("Epidemic Modeling with Interventions"),
  sidebarLayout(
    sidebarPanel(
      actionButton("run_model", "Run Epidemic Model")
    ),
    mainPanel(
      plotOutput("epidemic_curve")
    )
  )
)
