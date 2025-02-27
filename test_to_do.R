library(shiny)
library(DT)  # Use DT for table rendering
library(bslib)
library(timevis)  # Add timevis for Gantt chart

# Define initial categories
categories <- reactiveVal(c("Work", "Personal", "Urgent", "Later"))
projects <- reactiveVal(c("Project A", "Project B"))
file_path <- "checklist.rds"  # File to store the checklist

# Load existing data if available
if (file.exists(file_path)) {
  checklist <- readRDS(file_path)
} else {
  checklist <- data.frame(Task = character(), Tag = character(), Project = character(), Weight = integer(), StartDate = as.Date(character()), DueDate = as.Date(character()), Description = character(), Completed = logical(), stringsAsFactors = FALSE)
}

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Roboto", wght = "300")),  # Make font smaller
  titlePanel("Checklist App"),
  sidebarLayout(
    sidebarPanel(
      textInput("task", "Task"),
      selectizeInput("tag", "Tag", choices = NULL, multiple = FALSE, options = list(create = TRUE)),
      selectizeInput("project", "Project", choices = NULL, multiple = FALSE, options = list(create = TRUE)),
      numericInput("weight", "Weight (1-10)", value = 5, min = 1, max = 10),
      dateInput("start_date", "Start Date", value = Sys.Date()),  # Add Start Date input
      dateInput("due_date", "Due Date", value = NA),
      textAreaInput("description", "Description", ""),  # Add Description input
      actionButton("add", "Add Task"),
      actionButton("save", "Save Checklist"),
      downloadButton("export_ics", "Export to Outlook"),
      width = 3  # Make sidebar collapsible
    ),
    mainPanel(
      DTOutput("taskTable"),  # Use DTOutput for table
      actionButton("save_changes", "Save Changes"),  # Add a button to save changes
      timevisOutput("ganttChart")  # Add output for Gantt chart
    )
  )
)

server <- function(input, output, session) {
  checklist_data <- reactiveVal(checklist)
  selected_row <- reactiveVal(NULL)

  observe({
    updateSelectizeInput(session, "tag", choices = categories(), server = TRUE)
    updateSelectizeInput(session, "project", choices = projects(), server = TRUE)
  })

  observeEvent(input$add, {
    if (input$task != "") {
      if (!(input$tag %in% categories())) categories(c(categories(), input$tag))
      if (!(input$project %in% projects())) projects(c(projects(), input$project))
      new_entry <- data.frame(Task = input$task, Tag = input$tag, Project = input$project, Weight = input$weight, StartDate = input$start_date, DueDate = input$due_date, Description = input$description, Completed = FALSE, stringsAsFactors = FALSE)
      if (is.null(selected_row())) {
        checklist_data(rbind(checklist_data(), new_entry))
      } else {
        df <- checklist_data()
        df[selected_row(), ] <- new_entry
        checklist_data(df)
        selected_row(NULL)
      }
      updateTextInput(session, "task", value = "")
      updateSelectizeInput(session, "tag", selected = NULL)
      updateSelectizeInput(session, "project", selected = NULL)
      updateNumericInput(session, "weight", value = 5)
      updateDateInput(session, "start_date", value = Sys.Date())
      updateDateInput(session, "due_date", value = Sys.Date())
      updateTextAreaInput(session, "description", value = "")
    }
  })

  observeEvent(input$save, {
    saveRDS(checklist_data(), file_path)
    showNotification("Checklist saved!", type = "message")
  })

  output$taskTable <- renderDT({
    datatable(
      checklist_data(),
      selection = "single",
      rownames = FALSE,
      filter = "top",
      options = list(
        dom = 'ftip',
        paging = TRUE,
        columnDefs = list(
          list(
            targets = 3,  # Assuming the weight column is the 4th column (index 3)
            render = JS(
              "function(data, type, row, meta) {",
              "  var color = 'rgb(' + (255 * (data - 1) / 9) + ', ' + (255 * (10 - data) / 9) + ', 0)';",
              "  return '<span style=\"color:' + color + '\">' + data + '</span>';",
              "}"
            )
          ),
          list(
            targets = 7,  # Assuming the Completed column is the 8th column (index 7)
            render = JS(
              "function(data, type, row, meta) {",
              "  return '<input type=\"checkbox\" ' + (data ? 'checked' : '') + '>';",
              "}"
            )
          )
        )
      )
    )
  })

  observeEvent(input$taskTable_rows_selected, {
    selected <- input$taskTable_rows_selected
    if (length(selected) > 0) {
      selected_row(selected)
      df <- checklist_data()
      updateTextInput(session, "task", value = df[selected, "Task"])
      updateSelectizeInput(session, "tag", selected = df[selected, "Tag"])
      updateSelectizeInput(session, "project", selected = df[selected, "Project"])
      updateNumericInput(session, "weight", value = df[selected, "Weight"])
      updateDateInput(session, "start_date", value = df[selected, "StartDate"])
      updateDateInput(session, "due_date", value = df[selected, "DueDate"])
      updateTextAreaInput(session, "description", value = df[selected, "Description"])
    }
  })

  observeEvent(input$save_changes, {
    saveRDS(checklist_data(), file_path)
    showNotification("Changes saved!", type = "message")
  })

  output$ganttChart <- renderTimevis({
    df <- checklist_data()
    df_filtered <- df[df$Completed == FALSE, ]  # Filter out completed tasks
    gantt_data <- data.frame(
      id = 1:nrow(df_filtered),
      content = df_filtered$Task,
      start = df_filtered$StartDate,
      end = df_filtered$DueDate
    )
    timevis(gantt_data)
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
          paste0("DTSTART;VALUE=DATE:", format(df$StartDate[i], "%Y%m%d")),
          paste0("DTEND;VALUE=DATE:", format(df$DueDate[i], "%Y%m%d")),
          paste0("DESCRIPTION:Tag: ", df$Tag[i], " Project: ", df$Project[i], " Description: ", df$Description[i]),
          "END:VEVENT"
        )
      }
      ics_content <- c(ics_content, "END:VCALENDAR")
      writeLines(ics_content, file)
    }
  )
}

shinyApp(ui, server)
