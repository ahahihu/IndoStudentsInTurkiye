library(dplyr)

prep_city_data <- function(.data, selected_year, purpose, city_map = list(NULL), ppi_regions = list(NULL)) {
  res <- .data %>% 
    filter(year == selected_year)
  
  if (purpose == "graph_top10") {
    res <- res %>% 
      arrange(desc(student_count)) %>% 
      slice_head(n = 10) %>% 
      select(city, female_student_count, male_student_count)
  }
  
  if (purpose == "map_distribution") {
    res <- res %>% 
      select(city, student_count, university_count) %>% 
      left_join(
        city_map,
        by = "city"
      )
  }
  
  if (purpose == "metric") {
    res <- res %>% 
      summarize(
        student_count = sum(student_count, na.rm = TRUE),
        male_student_count = sum(male_student_count, na.rm = TRUE),
        female_student_count = sum(female_student_count, na.rm = TRUE),
        city_count = n_distinct(city),
        university_count = sum(university_count, na.rm = TRUE)
      )
  }
  
  if (purpose == "table_detailed") {
    res <- res %>% 
      left_join(
        ppi_regions,
        by = "city"
      ) %>% 
      relocate(
        university_count,
        .before = student_count
      ) %>% 
      relocate(
        female_ratio,
        .before = female_student_count
      ) %>% 
      arrange(desc(student_count)) %>% 
      select(-c(year, male_ratio))
  }
  
  return(res)
}

prep_univ_data <- function(.data, selected_year, purpose, univ_details = list(NULL)) {
  res <- .data %>% 
    filter(year == selected_year)
  
  if (purpose == "graph_top10") {
    res <- res %>% 
      arrange(desc(total)) %>% 
      slice_head(n = 10) %>% 
      select(university_name, female, male)
  }
  
  if (purpose == "table_detailed") {
    res <- res %>% 
      left_join(
        univ_details,
        by = "university_name"
      ) %>% 
      mutate(
        female_pct = round(1.0*female/total, 3) 
      ) %>% 
      select(
        logo_src,
        university_name,
        university_type,
        city,
        total,
        female_pct,
        female,
        male,
        qs_2024,
        times_2024,
        university_website,
        details_url,
        program_uid
      ) %>% 
      arrange(desc(total))
  }
  
  return(res)
}
