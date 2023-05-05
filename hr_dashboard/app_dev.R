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
                  choices = c("All", as.character(department$department_name)),
                  selected = "All"),
      selectInput(inputId = "gender",
                  label = "Select gender:",
                  choices = c("All", "Male", "Female"),
                  selected = "All"),
      selectInput(inputId = "gender_bar",
                  label = "Select gender:",
                  choices = c("All", "Male", "Female"),
                  selected = "All")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Employees", 
                 tableOutput("employee_table")),
        tabPanel("Department Summary",
                 plotOutput("pie_chart")),
        tabPanel("Salary Summary",
                 plotOutput("bar_chart"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Convert department_id in employee table to department_name
  employee_data <- reactive({
    employee %>%
      left_join(department, by = "department_id") %>%
      mutate(department_name = if_else(is.na(department_name), "All", department_name))
  })
  
  # Filter employee data based on selected department and gender
  filtered_data <- reactive({
    temp_data <- employee_data()
    
    if (input$department != "All") {
      temp_data <- temp_data %>%
        filter(department_name == input$department)
    }
    
    if (input$gender != "All") {
      temp_data <- temp_data %>%
        filter(gender == input$gender)
    }
    
    if (input$gender_bar != "All") {
      temp_data <- temp_data %>%
        filter(gender == input$gender_bar)
    }
    
    temp_data
  })
  
  # Output table of employees in selected department and gender
  output$employee_table <- renderTable({
    filtered_data()
  })
  
  # Output bar chart of employee salaries in selected department and gender
  output$salary_chart <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = name, y = salary, fill = gender)) +
      geom_col() +
      labs(title = "Employee Salaries by Gender",
           x = "Employee Name",
           y = "Salary",
           fill = "Gender") +
      theme_minimal()
  })
  
  # Output pie chart of employee counts by department
  output$pie_chart <- renderPlot({
    employee_data() %>%
      group_by(department_name) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = "", y = count, fill = department_name)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Employee Counts by Department",
           fill = "Department") +
      theme_void() +
      theme(legend.position = "bottom")
  })
}

# Run the app
shinyApp(ui, server)