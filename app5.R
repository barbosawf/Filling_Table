library(shiny)
library(DT)

# Function to validate email
is_valid_email <- function(email) {
  grepl("^[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}$", email)
}

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Table with Editable Rows"),
  
  # Tabs for different views
  tabsetPanel(
    id = "main_tabs",  # Add an ID for tab switching
    tabPanel("Edit Table", value = "edit_tab", fluidRow(
      column(6, DTOutput("editable_table")),
      column(6, actionButton("finish", "Finish"))
    )),
    tabPanel("Final Table", value = "final_tab", DTOutput("final_table"))
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive value to store table data
  rv <- reactiveValues(
    data = data.frame(Name = "", Email = "", stringsAsFactors = FALSE),
    finished = FALSE  # To control editing
  )
  
  # Render the editable table
  output$editable_table <- renderDT({
    datatable(
      rv$data,
      editable = !rv$finished,  # Disable editing when finished
      options = list(dom = 't', paging = FALSE, ordering = FALSE),
      rownames = FALSE
    )
  })
  
  # Handle edits to the table
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    req(info)
    
    row <- info$row
    col <- info$col + 1
    value <- info$value
    
    if (row > 0 && row <= nrow(rv$data) && col > 0 && col <= ncol(rv$data)) {
      # Validate email if editing the Email column
      if (col == 2 && !is_valid_email(value)) {
        showModal(modalDialog(
          title = "Invalid Email",
          "Please enter a valid email address.",
          easyClose = TRUE
        ))
        return()  # Don't update the table
      }
      
      # Update the specific cell in the data frame
      rv$data[row, col] <- value
    }
    
    # Add a new row if the last row is completely filled
    last_row <- rv$data[nrow(rv$data), ]
    if (all(trimws(last_row) != "") && !rv$finished) {
      rv$data <- rbind(rv$data, data.frame(Name = "", Email = "", stringsAsFactors = FALSE))
    }
  })
  
  # Stop the filling process and switch tabs
  observeEvent(input$finish, {
    rv$finished <- TRUE
    
    # Save the finalized table as an RDS file
    save_path <- file.path(getwd(), "final_table.rds")
    saveRDS(rv$data, save_path)
    
    # Render the final table in the second tab
    output$final_table <- renderDT({
      datatable(
        rv$data,
        options = list(dom = 'Bfrtip', paging = TRUE, ordering = TRUE),
        rownames = FALSE
      )
    })
    
    # Automatically switch to the "Final Table" tab
    updateTabsetPanel(session, "main_tabs", selected = "final_tab")
  })
}

# Run the app
shinyApp(ui, server)
