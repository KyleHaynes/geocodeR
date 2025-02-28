library(shiny)
library(DT)
library(bslib)

# Define initial categories
categories <- reactiveVal(c("Work", "Personal", "Urgent", "Later"))
projects <- reactiveVal(c("Project A", "Project B"))
file_path <- "checklist.rds"  # File to store the checklist

# Load existing data if available
if (file.exists(file_path)) {
  checklist <- readRDS(file_path)
} else {
  checklist <- data.frame(Task = character(), Tag = character(), Project = character(), Weight = integer(), DueDate = as.Date(character()), Completed = logical(), stringsAsFactors = FALSE)
}

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Checklist App"),
  sidebarLayout(
    sidebarPanel(
      textInput("task", "Task"),
      selectizeInput("tag", "Tag", choices = NULL, multiple = FALSE, options = list(create = TRUE)),
      selectizeInput("project", "Project", choices = NULL, multiple = FALSE, options = list(create = TRUE)),
      numericInput("weight", "Weight (1-10)", value = 5, min = 1, max = 10),
      dateInput("due_date", "Due Date", value = Sys.Date()),
      actionButton("add", "Add Task"),
      actionButton("save", "Save Checklist"),
      downloadButton("export_ics", "Export to Outlook")
    ),
    mainPanel(
      DTOutput("taskTable")
    )
  )
)

server <- function(input, output, session) {
  checklist_data <- reactiveVal(checklist)

  observe({
    updateSelectizeInput(session, "tag", choices = categories(), server = TRUE)
    updateSelectizeInput(session, "project", choices = projects(), server = TRUE)
  })

  observeEvent(input$add, {
    if (input$task != "") {
      if (!(input$tag %in% categories())) categories(c(categories(), input$tag))
      if (!(input$project %in% projects())) projects(c(projects(), input$project))
      new_entry <- data.frame(Task = input$task, Tag = input$tag, Project = input$project, Weight = input$weight, DueDate = input$due_date, Completed = FALSE, stringsAsFactors = FALSE)
      checklist_data(rbind(checklist_data(), new_entry))
    }
  })

  observeEvent(input$save, {
    saveRDS(checklist_data(), file_path)
    showNotification("Checklist saved!", type = "message")
  })

  output$taskTable <- renderDT({
    datatable(
      checklist_data(),
      selection = "none",
      rownames = FALSE,
      filter = "top",
      callback = JS(
        "table.on('click', 'input[type=\"checkbox\"]', function() {",
        "  var rowIdx = $(this).closest('tr').index();",
        "  var checked = this.checked;",
        "  Shiny.setInputValue('checkbox_toggle', {row: rowIdx, value: checked}, {priority: 'event'});",
        "});"
      ),
      options = list(
        dom = 'ftip',
        paging = TRUE,
        columnDefs = list(list(
          targets = 3,  # Assuming the weight column is the 4th column (index 3)
          render = JS(
            "function(data, type, row, meta) {",
            "  var color = 'rgb(' + (255 * (data - 1) / 9) + ', ' + (255 * (10 - data) / 9) + ', 0)';",
            "  return '<span style=\"color:' + color + '\">' + data + '</span>';",
            "}"
          )
        ))
      )
    )
  }, server = FALSE)

  observeEvent(input$checkbox_toggle, {
    df <- checklist_data()
    df[input$checkbox_toggle$row + 1, "Completed"] <- input$checkbox_toggle$value
    checklist_data(df)
  })

  output$export_ics <- downloadHandler(
    filename = function() { "checklist.ics" },
    content = function(file) {
      df <- checklist_data()
      ics_content <- c("BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//Checklist App//EN")
      for (i in 1:nrow(df)) {
        ics_content <- c(ics_content,
          "BEGIN:VEVENT",
          paste0("SUMMARY:", df$Task[i]),
          paste0("DTSTART;VALUE=DATE:", format(df$DueDate[i], "%Y%m%d")),
          paste0("DTEND;VALUE=DATE:", format(df$DueDate[i], "%Y%m%d")),
          paste0("DESCRIPTION:Tag: ", df$Tag[i], " Project: ", df$Project[i]),
          "END:VEVENT"
        )
      }
      ics_content <- c(ics_content, "END:VCALENDAR")
      writeLines(ics_content, file)
    }
  )
}

shinyApp(ui, server)
