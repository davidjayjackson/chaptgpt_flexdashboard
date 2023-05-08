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
                  choices = c("All", unique(employee$gender)),
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
  department_data <- reactive({
    if (input$department == "All" & input$gender == "All") {
      employee_data()
    } else if (input$department == "All") {
      employee_data() %>%
        filter(gender == input$gender)
    } else if (input$gender == "All") {
      employee_data() %>%
        filter(department_name == input$department)
    } else {
      employee_data() %>%
        filter(department_name == input$department, gender == input$gender)
    }
  })
  
  # Output table of employees in selected department
  output$employee_table <- renderTable({
    department_data()
  })
  
  # Output pie chart of total salary by department
  output$pie_chart <- renderPlot({
    employee_data() %>%
      group_by(department_name) %>%
      summarise(total_salary = sum(salary)) %>%
      ggplot(aes(x = "", y = total_salary, fill = department_name)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start=0) +
      labs(title = "Salary Summary", fill = "Department") +
      theme_void() +
      theme(legend.position = "bottom")
  })
  
  # Output bar chart of employee salaries in selected department
  output$bar_chart <- renderPlot({
    department_data() %>%
      ggplot(aes(x = name, y = salary, fill = department_name)) +
      geom_col() +
      labs(title = "Employee Salaries by Department", x = "Employee Name", y = "Salary", fill = "Department") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "right")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
