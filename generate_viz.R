generate_viz <- function(type, .data) {
  
  if (type == "distribution_leaflet") {
    city_map <- .data
    mylabel <- glue::glue("
    City: <strong>{city_map$city}</strong>
    <br/>
    Student Count: <strong>{city_map$student_count}</strong>
    <br/>
    University Count: <strong>{city_map$university_count}</strong>
    ") %>%
      lapply(htmltools::HTML)
    
    res <- leaflet(data = city_map) %>% 
      addProviderTiles(providers$CartoDB.VoyagerLabelsUnder) %>% 
      addCircleMarkers(
        lat = city_map$latitude,
        lng = city_map$longitude,
        opacity = 1,
        fillColor = "maroon",
        label = mylabel,
        labelOptions = labelOptions(
          opacity = 0.8,
          fontsize = 14
        ),
        radius = 8 + city_map$student_count/30,
        stroke = TRUE,
        weight = 2,
        color = "white"
      )
  }
  if (type == "top_10_univ_by_student") {
    res <- .data %>%
      e_charts(x = university_name) %>% 
      e_bar(
        serie = female,
        stack = "grp",
        name = "Female",
        barWidth = "60%",
        barCategoryGap = "100%"
      ) %>% 
      e_bar(
        serie = male,
        stack = "grp",
        name = "Male",
        barWidth = "60%",
        barCategoryGap = "100%"
      ) %>% 
      echarts4r::e_color(c("pink", "lightblue")) %>% 
      e_flip_coords() %>% 
      e_x_axis(
        show = TRUE,
        position = "bottom",
      ) %>% 
      e_y_axis(
        inverse = TRUE,
        axisLine = list(show = FALSE),
        axisTick = list(
          show = TRUE,
          alignWithLabel = TRUE,
          inside = TRUE,
          length = 8
        ),
        axisLabel = list(
          overflow = "truncate",
          width = "125"
        ),
        splitLine = list(
          show = TRUE,
          lineStyle = list(
            cap = "square",
            opacity = 0.5,
            color = "#F2F2F2"
          )
        ),
        margin = 5
      ) %>%
      e_grid(
        containLabel = TRUE,
        left = "3%",
        right = "3%",
        top = "12%",
        bottom = "3%"
      ) %>% 
      echarts4r::e_tooltip(
        trigger = "axis",
        confine = TRUE,
        formatter = htmlwidgets::JS('
                  formatter : (params) => {
                    let tooltip = 
                      `<div style="font-size:16px; font-weight:600; line-height:24px">
                        ${params[0].axisValue}
                      </div>`;
                    let total_student = 0;
                    params.forEach(({ marker, seriesName, value}) => {
                      value = value || [0, 0];
                      total_student += parseFloat(value[0])
                      tooltip += 
                        `<div style="font-size:14px; font-weight:400; margin-top: 8px">
                            ${marker}
                            ${seriesName}
                            <span style="float:right;font-size:14px; font-weight:600">
                                \u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0 ${parseFloat(parseFloat(value[0]).toFixed(1)).toLocaleString("id-ID", { useGrouping: true })}
                            </span>
                        </div>`;
                    });
                    tooltip +=
                      `<div style="font-size:14px; font-weight:400; margin-top: 8px">
                        Total
                        <span style="float:right;font-size:14px; font-weight:600">
                                \u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0 ${total_student.toLocaleString("id-ID", { useGrouping: true })}
                        </span>          
                      </div>`
                    return tooltip;
                    }
                '
        )
      ) %>% 
      e_dims(height = "300px")
  }
  if (type == "top_10_city") {
    res <- .data %>%
      e_charts(x = city) %>% 
      e_bar(
        serie = female_student_count,
        stack = "grp",
        name = "Female",
        barWidth = "60%",
        barCategoryGap = "100%"
      ) %>% 
      e_bar(
        serie = male_student_count,
        stack = "grp",
        name = "Male",
        barWidth = "60%",
        barCategoryGap = "100%"
      ) %>% 
      echarts4r::e_color(c("pink", "lightblue")) %>% 
      e_flip_coords() %>% 
      e_x_axis(
        show = TRUE,
        position = "bottom"
      ) %>% 
      e_y_axis(
        inverse = TRUE,
        axisLine = list(show = FALSE),
        axisTick = list(
          show = TRUE,
          alignWithLabel = TRUE,
          inside = TRUE,
          length = 8
        ),
        splitLine = list(
          show = TRUE,
          lineStyle = list(
            cap = "square",
            opacity = 0.5,
            color = "#F2F2F2"
          )
        ),
        margin = 5
      ) %>%
      echarts4r::e_grid(
        containLabel = TRUE,
        left = "0%",
        top = "12%",
        bottom = "3%"
      ) %>%
      echarts4r::e_tooltip(
        trigger = "axis",
        confine = TRUE,
        formatter = htmlwidgets::JS('
                  formatter : (params) => {
                    let tooltip = 
                      `<div style="font-size:16px; font-weight:600; line-height:24px">
                        ${params[0].axisValue}
                      </div>`;
                    let total_student = 0;
                    params.forEach(({ marker, seriesName, value}) => {
                      value = value || [0, 0];
                      total_student += parseFloat(value[0])
                      tooltip += 
                        `<div style="font-size:14px; font-weight:400; margin-top: 8px">
                            ${marker}
                            ${seriesName}
                            <span style="float:right;font-size:14px; font-weight:600">
                                \u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0 ${parseFloat(parseFloat(value[0]).toFixed(1)).toLocaleString("id-ID", { useGrouping: true })}
                            </span>
                        </div>`;
                    });
                    tooltip +=
                      `<div style="font-size:14px; font-weight:400; margin-top: 8px">
                        Total
                        <span style="float:right;font-size:14px; font-weight:600">
                                \u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0 ${total_student.toLocaleString("id-ID", { useGrouping: true })}
                        </span>          
                      </div>`
                    return tooltip;
                    }
                '
        )
      ) %>% 
      e_dims(height = "300px")
  }
  
  if (type == "distribution_gender") {
    res <- .data %>% 
      e_charts(x = year) %>% 
      e_bar(
        serie = female_ratio,
        stack = "grp",
        name = "Female"
      ) %>% 
      e_bar(
        serie = male_ratio,
        stack = "grp",
        name = "Male"
      )  %>% 
      echarts4r::e_color(c("pink", "lightblue")) %>% 
      e_y_axis(
        show = TRUE,
        position = "top",
        axisLine = list(show = FALSE),
        axisTick = list(
          show = TRUE,
          alignWithLabel = TRUE,
          inside = TRUE,
          length = 8
        ),
        splitLine = list(
          show = TRUE,
          lineStyle = list(
            cap = "square",
            opacity = 0.5,
            color = "#F2F2F2"
          )
        ),
        margin = 2,
        formatter = "{value}%"
      ) %>%
      e_x_axis(
        axisLabel = list(
          rotate = 45
        )
      ) %>% 
      echarts4r::e_tooltip(
        trigger = "axis",
        formatter = htmlwidgets::JS('
                  formatter : (params) => {
                    let tooltip = 
                      `<div style="font-size:16px; font-weight:600; line-height:24px">
                        ${params[0].axisValue}
                      </div>`;
                    let total_student = 0;
                    params.forEach(({ marker, seriesName, value}) => {
                      value = value || [0, 0];
                      total_student += parseFloat(value[1])
                      tooltip += 
                        `<div style="font-size:14px; font-weight:400; margin-top: 8px">
                            ${marker}
                            ${seriesName}
                            <span style="float:right;font-size:14px; font-weight:600">
                                \u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0 ${parseFloat(parseFloat(value[1]).toFixed(1)).toLocaleString("id-ID", { useGrouping: true })}%
                            </span>
                        </div>`;
                    });
                    return tooltip;
                    }
                ')
      ) %>% 
      echarts4r::e_grid(
        containLabel = TRUE,
        left = "3%",
        right = "3%",
        top = "10%",
        bottom = "6%"
      ) %>%
      e_dims(height = "300px")
  }
  
  if (type == "yoy_student_growth") {
    res <- .data %>% 
      e_charts(x = year) %>% 
      e_line(
        serie = yoy_change_female,
        name = "Female Student Growth",
        lineStyle = list(
          type = "dashed",
          width = 2
        )
      ) %>% 
      e_line(
        serie = yoy_change_male,
        name = "Male Student Growth",
        lineStyle = list(
          type = "dashed",
          width = 2
        )
      ) %>% 
      e_line(
        serie = yoy_change,
        name = "Overall Growth",
        lineStyle = list(
          width = 3
        )
      ) %>%
      e_x_axis(
        axisTick = list(show = FALSE),
        axisLabel = list(
          rotate = 45
        )
      ) %>% 
      e_y_axis(
        show = TRUE,
        position = "top",
        axisLine = list(show = FALSE),
        axisTick = list(
          show = TRUE,
          alignWithLabel = TRUE,
          inside = TRUE,
          length = 8
        ),
        formatter = "{value}%"
      ) %>% 
      e_legend(
        show = TRUE
      ) %>% 
      echarts4r::e_color(c("pink", "lightblue", "black")) %>%
      e_tooltip(
        trigger = "axis",
        confine = TRUE,
        formatter = htmlwidgets::JS('
          formatter : (params) => {
            let tooltip = 
              `<p style="font-size:16px; font-weight:600">
                ${params[0].axisValue}
              </p>`;
            params.forEach(({ marker, seriesName, value }) => {
              value = value || [0, 0];
              tooltip += 
                `<div style="font-size:14px; font-weight:400">
                  ${marker} ${seriesName}
                    <span style="float:right;font-size:14px; font-weight:600">
                      \u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0${parseFloat(parseFloat(value[1]).toFixed(1)).toLocaleString("id-ID")}%
                    </span>
                </div>`;
            });
          return tooltip;
        }
      ')
    ) %>% 
      echarts4r::e_grid(
        containLabel = TRUE,
        left = "3%",
        right = "3%",
        top = "10%",
        bottom = "6%"
      ) %>%
    e_dims(height = "300px")
  }
  
  if (type == "yearly_student_count") {
    res <- .data %>% 
      e_charts(x = year) %>% 
      e_bar(
        serie = female,
        stack = "grp",
        name = "Female"
      ) %>% 
      e_bar(
        serie = male,
        stack = "grp",
        name = "Male"
      ) %>% 
      echarts4r::e_color(c("pink", "lightblue")) %>% 
      e_y_axis(
        show = TRUE,
        position = "top",
        axisLine = list(show = FALSE),
        axisTick = list(
          show = TRUE,
          alignWithLabel = TRUE,
          inside = TRUE,
          length = 8
        ),
        splitLine = list(
          show = TRUE,
          lineStyle = list(
            cap = "square",
            opacity = 0.5,
            color = "#F2F2F2"
          )
        ),
        margin = 2
      ) %>% 
      echarts4r::e_tooltip(
        trigger = "axis",
        order = "seriesAsc",
        formatter = htmlwidgets::JS('
                  formatter : (params) => {
                    let tooltip = 
                      `<div style="font-size:16px; font-weight:600; line-height:24px">
                        ${params[0].axisValue}
                      </div>`;
                    let total_student = 0;
                    params.forEach(({ marker, seriesName, value}) => {
                      value = value || [0, 0];
                      total_student += parseFloat(value[1])
                      tooltip += 
                        `<div style="font-size:14px; font-weight:400; margin-top: 8px">
                            ${marker}
                            ${seriesName}
                            <span style="float:right;font-size:14px; font-weight:600">
                                \u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0 ${parseFloat(parseFloat(value[1]).toFixed(1)).toLocaleString("id-ID", { useGrouping: true })}
                            </span>
                        </div>`;
                    });
                    tooltip +=
                      `<div style="font-size:14px; font-weight:400; margin-top: 8px">
                        Total
                        <span style="float:right;font-size:14px; font-weight:600">
                                \u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0 ${total_student.toLocaleString("id-ID", { useGrouping: true })}
                        </span>          
                      </div>`
                    return tooltip;
                    }
                ')
      ) %>% 
      e_dims(height = "300px") %>% 
      e_grid(
        containLabel = TRUE,
        left = "3%",
        right = "3%",
        top = "12%",
        bottom = "3%"
      )
  }
  return(res)
}
