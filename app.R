library(shiny)
library(readr)
library(dplyr)
library(bs4Dash)
library(leaflet)
library(reactable)
library(echarts4r)
library(htmlwidgets)
library(glue)
library(htmltools)
library(sf)
library(shinyjs)
library(waiter)

yearly_count <- read_rds("data/students_yearly_count.rds")
city_map <- read_rds("data/city_map.rds")
student_by_city <- read_rds("data/student_by_city.rds")
student_by_univ <- read_rds("data/student_by_univ.rds")
ppi_regions <- read_rds("data/ppi_regions.rds")
univ_details <- read_rds("data/univ_details.rds")
invisible(lapply(list.files("R", full.names = TRUE), source))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  help = NULL,
  dark = NULL,
  scrollToTop = TRUE,
  title = "Indonesian University Students in Turkey",
  header = bs4DashNavbar(
    title = dashboardBrand(
      color = "maroon",
      title = "Indonesian University Students in Turkiye"
    ),
    navbarMenu(
      id = "navbar",
      navbarTab(
        tabName = "tab_student_population",
        text = "Student Population"
      ),
      navbarTab(
        tabName = "tab_prospective_student",
        text = "Prospective Student"
      )
    )
  ),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "tab_student_population",
        student_population_ui("stud_pop")
      ),
      tabItem(
        tabName = "tab_prospective_student",
        prospective_student_ui("prosp_stud")
      )
    ),
    useShinyjs(),
    useWaiter(),
    includeCSS("www/ui.css")
  ),
  preloader = list(html = spin_3(), color = transparent(.5)),
  footer = dashboardFooter(
    left = "Prepared by Ahmad Habib Batama Putra, alumnus of Orta Doğu Teknik Üniversitesi",
    right = span(
      span("Got any feedbacks? Please contact me via"),
      a("ahmadhabibbp@gmail.com", href = "mailto:ahmadhabibbp@gmail.com?subject=Indonesian%20Student%20in%20Turkiye%20Feedback", target = "_blank")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$navbar, {
    if (input$navbar == "tab_student_population") {
      student_population_server(
        "stud_pop",
        student_by_city = student_by_city,
        yearly_count = yearly_count,
        city_map = city_map,
        student_by_univ = student_by_univ
      )
    }
    else if (input$navbar == "tab_prospective_student") {
      prospective_student_server(
        "prosp_stud",
        student_by_city = student_by_city,
        student_by_univ = student_by_univ,
        ppi_regions = ppi_regions,
        univ_details = univ_details
      )
    }
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
