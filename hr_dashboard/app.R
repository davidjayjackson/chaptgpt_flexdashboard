library(shiny)
library(tibble)
library(tidyverse)

# Create the employee table
employee <- tribble(
  ~department_id, ~employee_id, ~name, ~hire_date, ~gender, ~salary,
  1, 101, "Alice", "2015-01-01", "Female", 50000,
  1, 102, "Bob", "2016-02-01", "Male", 60000,
  1, 103, "Charlie", "2017-03-01", "Male", 70000,
  2, 201, "Dave", "2018-04-01", "Male", 80000,
  2, 202, "Eve", "2019-05-01", "Female", 90000
)

# Create the department table
department <- tribble(
  ~department_id, ~department_name,
  1, "Sales",
  2, "Engineering"
)

# Define UI
ui <- fluidPage(
  titlePanel("HR Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "department",
                  label = "Select department:",
                  choices = c("All", unique(employee$department_id)),
                  selected = "All")
    ),
    mainPanel(
      tableOutput("employee_table"),
      verbatimTextOutput("summary_stats")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Filter employee data based on selected department
  department_data <- reactive({
    if(input$department == "All") {
      employee
    } else {
      employee %>%
        filter(department_id == input$department)
    }
  })
  
  # Output table of employees in selected department
  output$employee_table <- renderTable({
    department_data()
  })
  
  # Output summary statistics of salary in selected department
  output$summary_stats <- renderPrint({
    department_data() %>%
      summarize(mean_salary = mean(salary),
                median_salary = median(salary),
                min_salary = min(salary),
                max_salary = max(salary))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

