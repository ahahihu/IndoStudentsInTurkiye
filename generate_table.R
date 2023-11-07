generate_table <- function(type, .data) {
  format_int <- reactable::colFormat(
    digits = 0,
    separators = TRUE,
    locales = "id-ID"
  )
  
  format_pct <- reactable::colFormat(
    digits = 1,
    separators = TRUE,
    percent = TRUE,
    locales = "id-ID"
  )
  
  is_searchable <- ifelse(type == "univ_details", FALSE, TRUE)

  is_filterable <- ifelse(type == "univ_details", TRUE, FALSE)
  filter_text_case_insensitive <-
    JS("function(rows, columnId, filterValue) {
          const pattern = new RegExp(filterValue, 'i')

          return rows.filter(function(row) {
            return pattern.test(row.values[columnId])
          })
    }")
  
  filter_number_bigger_than <-
    JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }")
  
  col_opts <- NULL
  if (type == "univ_details") {
    filter_categorical_select <-
      function(values, name) {
        tags$select(
          class = "filter-column-table",
          onchange = sprintf("Reactable.setFilter('prosp_stud-univ_details', '%s', event.target.value || undefined)", name),
          tags$option(value = "", "All"),
          lapply(unique(values), tags$option),
          "aria-label" = sprintf("Filter %s", name),
        )
      }
    
    col_opts <- list(
      logo_src = colDef(
        name = "Logo",
        width = 50,
        filterable = FALSE,
        cell = htmlwidgets::JS('
          function(cellInfo) {
            return `<img style = "width:45px;height:45px" src="${cellInfo.value}">`
          }
        '),
        html = TRUE
      ),
      university_name = colDef(
        name = "University",
        minWidth = 250,
        filterable = TRUE,
        filterMethod = filter_text_case_insensitive
      ),
      university_type = colDef(
        name = "Type",
        minWidth = 75,
        filterable = TRUE,
        filterInput = filter_categorical_select
      ),
      city = colDef(
        name = "City",
        minWidth = 100,
        filterable = TRUE,
        filterInput = filter_categorical_select
      ),
      university_website = colDef(
        name = "Website",
        minWidth = 200,
        filterable = FALSE,
        cell = htmlwidgets::JS('
          function(cellInfo) {
            return `<a href="${cellInfo.value}" target="_blank">${cellInfo.value}</a>`
          }
        '),
        html = TRUE
      ),
      total = colDef(
        name = "Total Student (22-23)",
        minWidth = 150,
        cell = JS('function(cellInfo) {
        let pct = (100.0 * cellInfo.value / 667).toFixed(1) + "%"
        // Pad single-digit numbers
        let value = cellInfo.value
        // Render bar chart
        return `
          <div class="bar-cell">
            <span class="number">${value}</span>
            <div class="bar-chart">
              <div class="bar" style="width: ${pct}; background-color: #20B2AA"></div>
            </div>
          </div>
        `
      }'),
        html = TRUE,
        sortNALast = TRUE,
        filterable = TRUE,
        filterMethod = filter_number_bigger_than
        
      ),
      qs_2024 = colDef(
        name = "QS 2024 Rank",
        minWidth = 100,
        format = format_int,
        sortNALast = TRUE,
        filterable = TRUE,
        filterMethod = filter_number_bigger_than
        
      ),
      times_2024 = colDef(
        name = "Times HE 2024 Rank",
        minWidth = 100,
        format = format_int,
        sortNALast = TRUE,
        filterable = TRUE,
        filterMethod = filter_number_bigger_than
        
      ),
      female = colDef(
        name = "Female Student (22-23)",
        minWidth = 100,
        format = format_int,
        filterable = TRUE,
        filterMethod = filter_number_bigger_than
        
      ),
      female_pct = colDef(
        name = "Female Student Ratio (22-23)",
        minWidth = 150,
        cell = JS('function(cellInfo) {
        // Format as percentage
        let pct = (cellInfo.value * 100).toFixed(1) + "%"
        // Pad single-digit numbers
        let value = pct.padStart(5)
        // Render bar chart
        return `
          <div class="bar-cell">
            <span class="number">${value}</span>
            <div class="bar-chart" style="background-color: #e1e1e1">
              <div class="bar" style="width: ${pct}; background-color: #FFB6C1"></div>
            </div>
          </div>
        `
      }'),
        html = TRUE,
        sortNALast = TRUE,
        filterable = TRUE,
        filterMethod = filter_number_bigger_than
      ),
      male = colDef(
        name = "Male Student (22-23)",
        minWidth = 100,
        format = format_int,
        filterable = TRUE,
        filterMethod = filter_number_bigger_than
      ),
      details_url = colDef(
        name = "Detailed Links",
        minWidth = 120,
        filterable = FALSE,
        cell = htmlwidgets::JS('
          function(cellInfo) {
            return `<a href="${cellInfo.value}" target="_blank">StudyInTurkey Profile</a>`
          }
        '),
        html = TRUE,
        sticky = "right"
      ),
      program_uid = colDef(
        name = "Program List",
        minWidth = 100,
        filterable = FALSE,
        cell = htmlwidgets::JS('
          function(cellInfo) {
            return `<a href="${cellInfo.value}" target="_blank">Program Lists</a>`
          }
        '),
        html = TRUE,
        sticky = "right"
      )
    )
  }
  
  if (type == "city_details") {
    col_opts <- list(
      city = colDef(
        name = "City",
        minWidth = 100,
        sticky = "left"
      ),
      student_count = colDef(
        name = "Total Student (22-23)",
        minWidth = 100,
        cell = JS('function(cellInfo) {
        let pct = (100.0 * cellInfo.value / 667).toFixed(1) + "%"
        // Pad single-digit numbers
        let value = cellInfo.value
        // Render bar chart
        return `
          <div class="bar-cell">
            <span class="number">${value}</span>
            <div class="bar-chart">
              <div class="bar" style="width: ${pct}; background-color: #20B2AA"></div>
            </div>
          </div>
        `
      }'),
        html = TRUE
      ),
      female_student_count = colDef(
        name = "Female Student (22-23)",
        minWidth = 60,
        format = format_int
      ),
      female_ratio = colDef(
        name = "Female Student Ratio (22-23)",
        minWidth = 100,
        cell = JS('function(cellInfo) {
        // Format as percentage
        let pct = (cellInfo.value * 100).toFixed(1) + "%"
        // Pad single-digit numbers
        let value = pct.padStart(5)
        // Render bar chart
        return `
          <div class="bar-cell">
            <span class="number">${value}</span>
            <div class="bar-chart" style="background-color: #e1e1e1">
              <div class="bar" style="width: ${pct}; background-color: #FFB6C1"></div>
            </div>
          </div>
        `
      }'),
        html = TRUE
      ),
      male_student_count = colDef(
        name = "Male Student (22-23)",
        minWidth = 60,
        format = format_int
      ),
      university_count = colDef(
        name = "University with Indonesian Student",
        minWidth = 75,
        align = "center"
      ),
      ppi_region = colDef(
        name = "PPI Region",
        minWidth = 75,
        sticky = "right",
        cell = htmlwidgets::JS('
          function(cellInfo) {
            return `<a href="${cellInfo.row["ppi_region_instagram"]}" target="_blank">${cellInfo.value}</a>`
          }
        '),
        html = TRUE
      ),
      ppi_region_instagram = colDef(
        show = FALSE
      )
    )
  }
  reactable(
    data = .data,
    columns = col_opts,
    defaultSortOrder = "desc",
    searchable = is_searchable,
    filterable = is_filterable,
    showPageInfo = FALSE,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50),
    defaultPageSize = 10,
    wrap = TRUE,
    highlight = TRUE,
    borderless = TRUE,
    striped = TRUE,
    compact = TRUE
  )
}
