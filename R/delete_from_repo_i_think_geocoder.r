library(shiny)
library(shinythemes)
library(readxl)
library(DT)
library(writexl)
library(data.table)
require(dirp)
options(shiny.maxRequestSize = 1E10)

# Preloaded dataset (replace with actual data)
preloaded_data <- data.frame(ID = 1:10, Value = letters[1:10], Num1 = rnorm(10, 50, 10), Num2 = rnorm(10, 100, 20))

ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("CSV/XLSX Merger", windowTitle = "Data Merger"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload CSV/XLSX File",
                accept = c(".csv", ".xlsx", ".txt"),
                buttonLabel = "Browse...", placeholder = "No file selected"
            ),
            uiOutput("var_select"),
            actionButton("merge_btn", "Merge Data", class = "btn btn-primary"),
            hr(),
            uiOutput("filter1"),
            uiOutput("filter2"),
            downloadButton("download", "Download Filtered Data", class = "btn btn-success")
        ),
        mainPanel(
            h3("Merged Data Table"),
            div(
                style = "width: 100%; overflow-x: auto; max-width: 100%;",
                DTOutput("filtered_table")
            )
        )
    )
)

server <- function(input, output, session) {
    data <- reactive({
        req(input$file)
        ext <- tools::file_ext(input$file$name)
        if (ext %in% c("csv", "txt")) {
            d <- fread(input$file$datapath)
        } else if (ext == "xlsx") {
            d <- read_excel(input$file$datapath)
        } else {
            NULL
        }
        setDT(d)
        # browser()
        d[, a := st(toupper(paste(PG1_ResidentialAddrLine1, PG1_ResidentialAddrLine2, PG1_ResidentialSuburb, PG1_ResidentialPostcode)))]
        d[, a := st(gsub("^UNIT|TNHS|DU*PLX", "", a))]
        d <- d[a != ""]
        d <- d %sample% 2E4
        d
    })

    output$var_select <- renderUI({
        req(data())
        selectInput("merge_var", "Select Column to Merge",
            choices = names(data()),
            selectize = TRUE, multiple = FALSE
        )
    })

    merged_data <- eventReactive(input$merge_btn, {
        req(data(), input$merge_var)
        merge_col <- input$merge_var
        # browser()
        lookup_address(data()[[merge_col]])
        # data() %>% inner_join(preloaded_data, by = setNames("ID", merge_col))
    })

    output$filter1 <- renderUI({
        req(merged_data())
        selectInput("filter_var1", "Select First Filter Column",
            choices = names(merged_data()),
            selected = NULL
        )
    })

    output$filter2 <- renderUI({
        req(merged_data())
        selectInput("filter_var2", "Select Second Filter Column",
            choices = names(merged_data()),
            selected = NULL
        )
    })

    filtered_data <- reactive({
        req(merged_data())
        df <- merged_data()
        if (!is.null(input$filter_var1) && input$filter_var1 %in% names(df)) {
            df <- df[df[[input$filter_var1]] > 50, ]
        }
        if (!is.null(input$filter_var2) && input$filter_var2 %in% names(df)) {
            df <- df[df[[input$filter_var2]] <= 100, ]
        }
        df
    })

    output$filtered_table <- renderDT(expr = 
        {
            # browser()
            req(filtered_data())
            filtered_data()
        }
        , options = list(pageLength = 10, autoWidth = TRUE)
    )

    output$download <- downloadHandler(
        filename = function() {
            paste("filtered_data", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(filtered_data(), file, row.names = FALSE)
        }
    )
}

# Z:\Project\QGSO\Data Integration\QHF\Data (supplementary)\DET\2023

if (F) {
    d <- fread("Z:\\Project\\QGSO\\Data Integration\\QHF\\Data (supplementary)\\DET\\2023\\JMS 50630 - Current Student Addresses for QGSO 24-04-2022(lohan.wolf@qed.qld.gov.au).txt")

    lookup_map <<- source_gnaf()
    set.seed(1)
    d <- d[a != ""]
    d <- d %sample% 2E4
    d[, a := st(toupper(paste(PG1_ResidentialAddrLine1, PG1_ResidentialAddrLine2, PG1_ResidentialSuburb, PG1_ResidentialPostcode)))]
    d[, a := st(gsub("^UNIT|TNHS|DU*PLX", "", a))]
    d <<- d
    x <- lookup_address(d$a)
    x[, .N, matched]
}

if(F){
    source("C:/Users/kzhayn@treasury.qld.gov.au/local/git/dirv/R/geocoder.r")
    source("C:/Users/kzhayn@treasury.qld.gov.au/local/git/code/code/202404_address_matching.R")
    shinyApp(ui, server)
}
