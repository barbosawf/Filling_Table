library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Table with Editable Rows"),
  fluidRow(
    column(6, DTOutput("editable_table"))
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive value to store table data
  rv <- reactiveValues(data = data.frame(Name = "", Email = "", stringsAsFactors = FALSE))
  
  # Render the editable table
  output$editable_table <- renderDT({
    datatable(
      rv$data,
      editable = TRUE,
      options = list(dom = 't', paging = FALSE, ordering = FALSE),
      rownames = FALSE
    )
  })
  
  # Handle edits to the table
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    req(info)  # Ensure edit info exists
    
    # Convert column index from 0-based to 1-based (used in R)
    row <- info$row
    col <- info$col + 1
    value <- info$value
    
    # Safeguard against invalid indices
    if (row > 0 && row <= nrow(rv$data) && col > 0 && col <= ncol(rv$data)) {
      # Update the specific cell in the data frame
      rv$data[row, col] <- value
    }
    
    # Check if the last row is completely filled
    last_row <- rv$data[nrow(rv$data), ]
    if (all(trimws(last_row) != "")) {
      rv$data <- rbind(rv$data, data.frame(Name = "", Email = "", stringsAsFactors = FALSE))
    }
  })
}

# Run the app
shinyApp(ui, server)
