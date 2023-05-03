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
  
  # Filter employee data based on selected department
  department_data <- reactive({
    if (input$department == "All") {
      employee
    } else {
      employee %>%
        filter(department_id == department$department_id[department$department_name == input$department])
    }
  })
  
  # Output table of employees in selected department
  output$employee_table <- renderTable({
    department_data()
  })
  
  # Output pie chart of total salary by department
  output$pie_chart <- renderPlot({
    department_data() %>%
      group_by(department_id) %>%
      summarize(total_salary = sum(salary)) %>%
      inner_join(department, by = "department_id") %>%
      ggplot(aes(x = department_name, y = total_salary, fill = department_name)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Salary by Department", 
           x = "Department", y = "Total Salary")
  })
  
  # Output bar chart of employee salaries in selected department
  output$bar_chart <- renderPlot({
    department_data() %>%
      ggplot(aes(x = name, y = salary)) +
      geom_bar(stat = "identity") +
      labs(title = "Employee Salaries",
           x = "Employee Name", y = "Salary")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
