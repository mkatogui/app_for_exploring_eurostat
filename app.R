library(shiny)
library(eurostat)
library(stringr)
library(tidyverse)
library(dplyr)
library(DT)
library(shinydashboard)

# Loading table
table_of_contents_en <- read.delim("table_of_contents_en.txt")

ui <- dashboardPage( 
  dashboardHeader(title = "Eurostat Data"),  
  dashboardSidebar(
    p("This app allows the user to explore datasets from Eurostat and download them."),
    tags$a(href = "https://github.com/mkatogui/app_for_exploring_eurostat", "Click here to visit the repository to run it locally in your computer. github.com/mkatogui"),
    textInput("input1", label = "Select your topic", value = "agriculture"),    
    selectInput("input2", label = "Second Input (select table 'code')",
                choices = table_of_contents_en$code,   
                multiple = TRUE,
                selected = "aact_eaa03"),
    selectInput("input3", label = "Third Input (select country)",
                choices = unique(get_eurostat("aact_eaa03")$geo),   
                multiple = TRUE,
                selected = "DE"),
    textInput("input4", label = "File Name", value = "Output")  
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("1. Select data set from here", DTOutput("output1")),
      tabPanel("2. Displaying all data found", DTOutput("output2")),
      tabPanel("3. Filtered by Country/Geo", DTOutput("output3")),
      tabPanel("Download", downloadButton("downloadData", "Download"))
    )  
  )
)

# Define global output variables
Output1 <- NULL
Output2 <- NULL
Output3 <- NULL

# Define server
server <- function(input, output) {
  
  # First stage
  output$output1 <- renderDT({
    # Generating first Outputs based on input1
    Selection1 <- str_detect(table_of_contents_en$title, pattern = input$input1)
    Output1 <<- table_of_contents_en[Selection1,]
    Output1
  })
  
  # Second stage
  output$output2 <- renderDT({
    # Generating second Outputs based on input2
    Output2 <<- data.frame(get_eurostat(input$input2))
    Output2
  })
  
  # Third stage
  output$output3 <- renderDT({
    # Generating third Outputs based on input3
    #Output2 <<- data.frame(get_eurostat(input$input2))
    Output3 <<- subset(Output2, geo %in% input$input3) %>% 
      pivot_wider(names_from = geo, values_from = values)
    Output3
  })
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$input4, ".xlsx")
    },
    content = function(file) {
      # Generating final output and saving to file
     # Output2 <<- data.frame(get_eurostat(input$input2))
      Output3 <<- subset(Output2, geo %in% input$input3) %>% 
        pivot_wider(names_from = geo, values_from = values)
      openxlsx::write.xlsx(Output3, file)
    }
  )
}


# Run the app
shinyApp(ui, server)

