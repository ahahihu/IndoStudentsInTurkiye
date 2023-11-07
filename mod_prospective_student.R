prospective_student_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h4("Choosing the right University"),
    span(
      span("There are several things to consider for your next journey. If you want to experience the best facilities and study with the best professors in the country then you should consider university ranking from website like"),
      a("QS University Ranking", href = "https://www.topuniversities.com/university-rankings/world-university-rankings/2024?countries=tr", target = "_blank"),
      span("or"),
      a("Times Higher Education", href = "https://www.timeshighereducation.com/student/best-universities/best-universities-turkey", target = "_blank"),
      span(". Having many fellow Indonesian students in town can really helps you to easily settle in and explore
      many opportunities beyond your campus life.")
    ),
    br(),
    br(),
    span(
      span("You can find some"),
      a("guides", href = "https://www.studyinturkiye.gov.tr/StudyinTurkey/StudyinTurkey#item-2", target = "_blank"),
      span("about applying universities in Turkiye courtesy of the Turkish Council of Higher Education or 
      check the reference link from the table below to directly jump into your university of choice.")
    ),
    br(),
    br(),
    fluidRow(
      box(
        title = "Explore More about Turkish Universities",
        width = 12,
        reactableOutput(ns("univ_details"))
      )
    ),
    br(),
    p("If you need more assistance, you can also ask for more information from the regional Indonesian Students Association (Perhimpunan Pelajar Indonesia) from their social media but be sure that you have done some research first before asking"),
    fluidRow(
      box(
        title = "More about Turkish Cities",
        width = 12,
        reactableOutput(ns("city_details"))
      )
    )
  )
}

prospective_student_server <- function(id, student_by_city, student_by_univ, ppi_regions, univ_details) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    w <- Waiter$new(
      id = c("prosp_stud-city_details", "prosp_stud-univ_details"),
      html = spin_3(), 
      color = transparent(.5)
    )
    
    output$city_details <- renderReactable({
      city_details_table <- prep_city_data(
        student_by_city,
        "2022-2023",
        "table_detailed",
        ppi_regions = ppi_regions
      )
      generate_table(
        type = "city_details",
        .data = city_details_table
      )
    })
    
    output$univ_details <- renderReactable({
      univ_details_table <- prep_univ_data(
        student_by_univ,
        "2022-2023",
        "table_detailed",
        univ_details = univ_details
      )
      generate_table(
        type = "univ_details",
        .data = univ_details_table
      )
    })
  })
}