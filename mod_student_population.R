student_population_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h3("University Students Population"),
    span(
      span("Students population metrics are sourced from"),
      a("Turkish Council of Higher Education (Yükseköğretim Kurulu)", href = "https://istatistik.yok.gov.tr/", target = "_blank"),
      span("with the latest data provided for 2022-2023 Academic Year.")
    ),
    br(),
    br(),
    fluidRow(
      infoBoxOutput(ns("current_student_cnt")),
      infoBoxOutput(ns("current_female_student_cnt")),
      infoBoxOutput(ns("current_male_student_cnt"))
    ),
    box(
      title = "Indonesian Students Growth",
      echarts4rOutput(ns("yearly_trend"), height = "300px"),
      width = 12,
      height = "330px"
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Indonesian Students Year on Year Growth",
          echarts4rOutput(ns("yoy_growth"), height = "300px"),
          width = 12,
          height = "330px"
        )
      ),
      column(
        width = 6,
        box(
          title = "Student Gender Ratio by Year",
          echarts4rOutput(ns("distribution_gender"), height = "300px"),
          width = 12,
          height = "330px"
        )
      )
    ),
    br(),
    br(),
    h3("Student Distribution by Academic Year"),
    selectInput(
      ns("student_population_year"),
      "Select Academic Year",
      choices = list(
        "2022-2023", "2021-2022", "2020-2021", "2019-2020", "2018-2019", 
        "2017-2018", "2016-2017", "2015-2016", "2014-2015", "2013-2014"
      ),
      selected = "2022-2023"
    ),
    fluidRow(
      infoBoxOutput(ns("student_cnt")),
      infoBoxOutput(ns("female_student_cnt")),
      infoBoxOutput(ns("male_student_cnt"))
    ),
    fluidRow(
      infoBoxOutput(ns("distinct_city")),
      infoBoxOutput(ns("distinct_uni"))
    ),
    box(
      title = "Student Distribution Map",
      width = 12,
      leafletOutput(ns("distribution_map"))
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Top 10 City by Indonesian Student Count",
          echarts4rOutput(ns("top_10_city"), height = "300px"),
          width = 12,
          height = "330px"
        )
      ),
      column(
        width = 6,
        box(
          title = "Top 10 University by Indonesian Student Count",
          echarts4rOutput(ns("top_10_univ_by_student"), height = "300px"),
          width = 12,
          height = "330px"
        )
      )
    )
  )
}


student_population_server <- function(id, student_by_city, yearly_count, city_map, student_by_univ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    w <- Waiter$new(
      id = c("stud_pop-distribution_map"),
      html = spin_3(), 
      color = transparent(.5)
    )
    
    current_metric <- prep_city_data(
      student_by_city, 
      "2022-2023",
      "metric"
    )
    
    output$current_student_cnt <- renderInfoBox(
      infoBox(
        title = "Total Student (2022-2023)",
        value = pull(current_metric, student_count),
        icon = icon("user", class = "fa-solid")
      )
    )
    
    output$current_male_student_cnt <- renderInfoBox(
      infoBox(
        title = "Male Student (2022-2023)",
        value = pull(current_metric, male_student_count),
        icon = icon("person", class = "fa-solid", style = "color: #1cb2f2;")
      )
    )
    output$current_female_student_cnt <- renderInfoBox(
      infoBox(
        title = "Female Student (2022-2023)",
        value = pull(current_metric, female_student_count),
        icon = icon("person", class = "fa-solid", style = "color: #ff9ec8;")
      )
    )
    output$yearly_trend <- renderEcharts4r({
      generate_viz(
        type = "yearly_student_count",
        .data = yearly_count
      )
    })
    output$yoy_growth <- renderEcharts4r({
      generate_viz(
        type = "yoy_student_growth",
        .data = yearly_count
      )
    })
    output$distribution_gender <- renderEcharts4r({
      generate_viz(
        type = "distribution_gender",
        .data = yearly_count
      )
    })
    
    selected_metrics <- reactive({
      prep_city_data(
        student_by_city, 
        input$student_population_year,
        "metric"
      )
    })
    
    observeEvent(input$student_population_year, {
      if (input$student_population_year == "2022-2023") {
        hide("student_cnt")
        hide("male_student_cnt")
        hide("female_student_cnt")
      }
      else if (input$student_population_year != "2022-2023") {
        shinyjs::show("student_cnt")
        shinyjs::show("male_student_cnt")
        shinyjs::show("female_student_cnt")
      }
    })
    
    output$student_cnt <- renderInfoBox({
      infoBox(
        title = paste0("Total Student (", input$student_population_year, ")"),
        value = pull(selected_metrics(), student_count),
        icon = icon("user", class = "fa-solid")
      )
    })
    output$male_student_cnt <- renderInfoBox({
      infoBox(
        title = paste0("Male Student (", input$student_population_year, ")"),
        value = pull(selected_metrics(), male_student_count),
        icon = icon("person", class = "fa-solid", style = "color: #1cb2f2;")
      )
    })
    output$female_student_cnt <- renderInfoBox({
      infoBox(
        title = paste0("Female Student (", input$student_population_year, ")"),
        value = pull(selected_metrics(), female_student_count),
        icon = icon("person", class = "fa-solid", style = "color: #ff9ec8;")
      )
    })
    output$distinct_city <- renderInfoBox(
      infoBox(
        title = paste0("City Count (", input$student_population_year, ")"),
        value = pull(selected_metrics(), city_count),
        icon = icon("city", class = "fa-solid")
      )
    )
    output$distinct_uni <- renderInfoBox(
      infoBox(
        title = paste0("University Count (", input$student_population_year, ")"),
        value = pull(selected_metrics(), university_count),
        icon = icon("school-flag", class = "fa-solid")
      )
    )
    
    delay(
      1000,
      output$distribution_map <- renderLeaflet({
        city_stat_map <- prep_city_data(
          student_by_city, 
          input$student_population_year,
          "map_distribution",
          city_map = city_map
        )
        
        generate_viz(
          type = "distribution_leaflet",
          .data = city_stat_map
        )
      })
    )
    
    output$top_10_city <- renderEcharts4r({
      top10_city <- prep_city_data(
        student_by_city,
        input$student_population_year,
        "graph_top10"
      )
      generate_viz(
        type = "top_10_city",
        .data = top10_city
      )
    })
    output$top_10_univ_by_student <- renderEcharts4r({
      top10_univ <- prep_univ_data(
        student_by_univ,
        input$student_population_year,
        "graph_top10",
        univ_details = list(NULL)
      )
      generate_viz(
        type = "top_10_univ_by_student",
        .data = top10_univ
      )
    })
  })
}