ptExposuresUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "pt_exposures",
          fluidRow(
            box(
              title = "Filter Military Exposures Table by Deployment and Age",
              collapsible = TRUE,
              collapsed = FALSE,
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(
                  6,
                  selectInput(
                    ns("milConflictCodeExpoTbl"),
                    div(
                      style = "display: flex; align-items: center;",
                      "Deployment Conflict (will only include deployed vets): ",
                      tags$i(
                        class = "fas fa-info-circle",
                        title = "This filter applies only to veterans who reported being deployed and identified their conflict participation."
                      )
                    ),
                    choices = c(
                      "All",
                      "Bosnia",
                      "Croatia",
                      "Grenada",
                      "Korea",
                      "Kosovo",
                      "Lebanon",
                      "ODSS",
                      "OIF/OEF/OND",
                      "Other",
                      "Somalia",
                      "Vietnam",
                      "WWII"
                    ),
                    selected = "All"
                  )
                ),
                column(
                  6,
                  sliderInput(
                    ns("ageExpoTbl"),
                    "Age Range:",
                    min = min(full_demo_dat$age, na.rm = TRUE),
                    max = max(full_demo_dat$age, na.rm = TRUE),
                    value = c(
                      min(full_demo_dat$age, na.rm = TRUE),
                      max(full_demo_dat$age, na.rm = TRUE)
                    )
                  )
                )
              )
            ),
            box(
              title = "Filter Military Exposures Table by Gender, Ethnicity, and Race",
              collapsible = TRUE,
              collapsed = FALSE,
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(
                  4,
                  selectInput(
                    ns("genderExpoTbl"),
                    "Gender:",
                    choices = c("All", "Male", "Female"),
                    selected = "All"
                  )
                ),
                column(
                  4,
                  selectizeInput(
                    ns("ethnicityExpoTbl"),
                    "Ethnicity:",
                    choices = c(
                      "All", 
                      "Non Hispanic or Non Latino", 
                      "Hispanic or Latino"
                    ),
                    multiple = TRUE,
                    selected = "All"
                  )
                ),
                column(
                  4,
                  selectizeInput(
                    ns("raceExpoTbl"),
                    "Race:",
                    choices = c(
                      "All", 
                      "American Indian or Alaskan Native", 
                      "Asian",
                      "Black or African-American",
                      "Native Hawaiian or Pacific Islander",
                      "White"
                    ),
                    multiple = TRUE,
                    selected = "All"
                  )
                )
              )
            )
          ),
          fluidRow(
            tabBox(
              width = 12,
              tabPanel(
                title = HTML("<strong>Military Exposures Table</strong>"),
                width = 12,
                collapsible = TRUE,
                collapsed = FALSE,
                solidHeader = TRUE,
                reactableOutput(
                  ns("militaryExposuresTable"))
            ),
            tabPanel(
              title = HTML("<strong>Civilian Exposures Table</strong>"),
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              solidHeader = TRUE,
              reactableOutput(
                ns("civilianExposuresTable"))
            )
          )
        
        )
    )
}

ptExposuresServer <- function(id, selected_patient) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Military Exposures Table ----
    ## Filter IDs based on inputs ----
    filtered_ids <- reactive({
      req(
        input$ageExpoTbl,
        input$genderExpoTbl,
        input$milConflictCodeExpoTbl,
        input$ethnicityExpoTbl,
        input$raceExpoTbl
      )
      
      ## Get relevant IDs from mil_conflict_dat based on the selected milConflictCode ----
      conflict_filtered_ids <- if (input$milConflictCodeExpoTbl == "All") {
        unique(full_demo_dat$id)  # Include all IDs if no conflict filter is applied
      } else {
        unique(
          mil_conflict_dat %>%
            filter(milConflictCode == input$milConflictCodeExpoTbl) %>%
            pull(id)
        )
      }
      
      ## Filter full_demo_dat based on age, gender, and IDs from mil_conflict_dat ----
      filtered <- full_demo_dat %>%
        filter(
          age >= input$ageExpoTbl[1] & age <= input$ageExpoTbl[2],
          (
            input$genderExpoTbl == "All" | gender == input$genderExpoTbl
          ),
          (
            input$ethnicityExpoTbl == "All" |
              ethnicity %in% input$ethnicityExpoTbl # Allow multiple ethnicities
          ),
          (
            input$raceExpoTbl == "All" |
              race %in% input$raceExpoTbl # Allow multiple races
          ),
          id %in% conflict_filtered_ids
        )
      unique(filtered$id)
    }) %>%
      bindCache(
        input$ageExpoTbl,
        input$genderExpoTbl,
        input$milConflictCodeExpoTbl,
        input$ethnicityExpoTbl,
        input$raceExpoTbl
      )
    
    ## Filter mil_expo_dat based on filtered IDs ----
    filtered_mil_expo_dat <- reactive({
      req(filtered_ids())
      mil_expo_dat %>%
        filter(id %in% filtered_ids())
    }) %>%
      bindCache(filtered_ids())
    
    ## Calculate summary statistics for the group ----
    mil_expo_dat_summary <- reactive({
      result <- filtered_mil_expo_dat() %>%
        group_by(exposureName) %>%
        summarise(
          n = n_distinct(id),
          median_freq = median(exposureFreq, na.rm = TRUE),
          percentile_75_freq = quantile(exposureFreq, 0.75, na.rm = TRUE),
          percentile_90_freq = quantile(exposureFreq, 0.90, na.rm = TRUE),
          prop_zeros_freq = mean(exposureFreq == 0, na.rm = TRUE),
          conditional_median_freq = median(exposureFreq[exposureFreq > 0], na.rm = TRUE),
          conditional_mean_freq = mean(exposureFreq[exposureFreq > 0], na.rm = TRUE),
          median_intensity = median(exposureIntensity, na.rm = TRUE),
          percentile_75_intensity = quantile(exposureIntensity, 0.75, na.rm = TRUE),
          percentile_90_intensity = quantile(exposureIntensity, 0.90, na.rm = TRUE),
          prop_zeros_intensity = mean(exposureIntensity == 0, na.rm = TRUE),
          conditional_median_intensity = median(exposureIntensity[exposureIntensity > 0], na.rm = TRUE),
          conditional_mean_intensity = mean(exposureIntensity[exposureIntensity > 0], na.rm = TRUE),
          median_concern = median(exposureConcern, na.rm = TRUE),
          percentile_75_concern = quantile(exposureConcern, 0.75, na.rm = TRUE),
          percentile_90_concern = quantile(exposureConcern, 0.90, na.rm = TRUE),
          prop_zeros_concern = mean(exposureConcern == 0, na.rm = TRUE),
          conditional_median_concern = median(exposureConcern[exposureConcern > 0], na.rm = TRUE),
          conditional_mean_concern = mean(exposureConcern[exposureConcern > 0], na.rm = TRUE)
        )
      result
    }) %>% bindCache(filtered_mil_expo_dat())
    
    ## Function to calculate percentile rank ----
    calculate_percentile_rank <-
      function(individual_score, group_scores) {
        group_scores <-
          group_scores[!is.na(group_scores)] # Remove NA values from group_scores
        (
          sum(group_scores < individual_score) + 0.5 * sum(group_scores == individual_score)
        ) / length(group_scores) * 100
      }
    
    ## Function to calculate percentile ranks for an individual ----
    calculate_individual_percentiles <-
      function(individual_data, group_data) {
        individual_data %>%
          group_by(exposureName) %>%
          mutate(
            percentile_rank_freq = if_else(
              !is.na(exposureFreq) & exposureFreq > 0,
              calculate_percentile_rank(exposureFreq,
                                        group_data$exposureFreq[group_data$exposureName == exposureName]),
              NA_real_
            ),
            percentile_rank_intensity = if_else(
              !is.na(exposureIntensity) & exposureIntensity > 0,
              calculate_percentile_rank(exposureIntensity,
                                        group_data$exposureIntensity[group_data$exposureName == exposureName]),
              NA_real_
            ),
            percentile_rank_concern = if_else(
              !is.na(exposureConcern) & exposureConcern > 0,
              calculate_percentile_rank(exposureConcern,
                                        group_data$exposureConcern[group_data$exposureName == exposureName]),
              NA_real_
            ),
            conditional_percentile_rank_intensity = if_else(
              !is.na(exposureIntensity) & exposureIntensity > 0,
              calculate_percentile_rank(exposureIntensity,
                                        group_data$exposureIntensity[group_data$exposureName == exposureName &
                                                                       group_data$exposureIntensity > 0]),
              NA_real_
            ),
            conditional_percentile_rank_concern = if_else(
              !is.na(exposureConcern) & exposureConcern > 0,
              calculate_percentile_rank(exposureConcern,
                                        group_data$exposureConcern[group_data$exposureName == exposureName &
                                                                     group_data$exposureConcern > 0]),
              NA_real_
            )
          ) %>%
          ungroup()
      }
    
    ## Calculate individual percentiles ----
    individual_percentiles <- reactive({
      req(selected_patient(), filtered_mil_expo_dat())
      individual_data <-
        mil_expo_dat %>% filter(id == selected_patient())
      group_data <- filtered_mil_expo_dat()
      
      calculate_individual_percentiles(individual_data, group_data) %>%
        arrange(desc(exposureFreq))
    }) %>% bindCache(selected_patient(), filtered_mil_expo_dat())
    
    ## Function to generate the summary table for each exposure ----
    generate_summary_table <- function(exposure_name) {
      mil_expo_dat_summary() %>%
        filter(exposureName == exposure_name) %>%
        select(
          n,
          median_freq,
          prop_zeros_freq,
          conditional_median_freq,
          conditional_mean_freq,
          percentile_75_freq,
          percentile_90_freq,
          median_intensity,
          prop_zeros_intensity,
          conditional_median_intensity,
          conditional_mean_intensity,
          percentile_75_intensity,
          percentile_90_intensity,
          median_concern,
          prop_zeros_concern,
          conditional_median_concern,
          conditional_mean_concern,
          percentile_75_concern,
          percentile_90_concern
        ) %>%
        pivot_longer(cols = -n,
                     names_to = "stat_type",
                     values_to = "value") %>%
        separate(stat_type,
                 into = c("stat", "type"),
                 sep = "_(?=freq|intensity|concern)") %>%
        pivot_wider(names_from = type, values_from = value) %>%
        select(stat, freq, intensity, concern) %>%
        mutate(
          across(
            c(freq, intensity, concern),
            ~ case_when(stat == "prop_zeros" ~ .x * 100,
                        TRUE ~ as.numeric(.x))
          ),
          across(
            c(freq, intensity, concern),
            ~ case_when(
              stat == "prop_zeros" ~ sprintf("%.1f%%", .x),
              TRUE ~ sprintf("%.2f", .x)
            )
          ),
          stat = case_when(
            stat == "median" ~ "Median",
            stat == "prop_zeros" ~ "Proportion of Zeros",
            stat == "conditional_median" ~ "Conditional Median",
            stat == "conditional_mean" ~ "Conditional Mean",
            stat == "percentile_75" ~ "75th Percentile",
            stat == "percentile_90" ~ "90th Percentile"
          )
        ) %>%
        rename(
          Statistic = stat,
          Frequency = freq,
          Intensity = intensity,
          Concern = concern
        ) %>%
        mutate(across(everything(), str_to_title)) %>%
        mutate(N = as.character(
          mil_expo_dat_summary() %>%
            filter(exposureName == exposure_name) %>%
            pull(n)
        ))
    }
    
    # Function to generate filled blue circle
    custom_circle_icon <- function(value) {
      if (is.na(value)) {
        return("NA")
      }
      div(style = "display: inline-block; width: 100%; text-align: left;",
          lapply(1:5, function(i) {
            filled <- ifelse(i <= value, "#67a9cf", "lightgrey")
            tags$svg(
              width = "20",
              height = "20",
              tags$circle(
                cx = "10",
                cy = "10",
                r = "8",
                fill = filled
              )
            )
          }))
    }
    
    # Render military exposures table ----
    output$militaryExposuresTable <- renderReactable({
      
      # Check if individual_percentiles() is NULL or empty
      if (is.null(individual_percentiles()) ||
          nrow(individual_percentiles()) == 0) {
        message("No data available: individual_percentiles() is NULL or empty")
        return(reactable(
          data.frame(Message = "Not enough data available for the selected patient."),
          columns = list(Message = colDef(
            align = "center",
            style = list(color = "red", fontWeight = "bold")
          ))
        ))
      }
      
      # Filter out rows where all three relevant columns are NA
      filtered_data <- individual_percentiles() %>%
        filter(!(is.na(exposureFreq) & 
                   is.na(exposureIntensity) & 
                   is.na(exposureConcern)))
      
      # Check if relevant columns are 0 or NA
      if (all(
        is.na(filtered_data$exposureFreq) |
        filtered_data$exposureFreq == 0
      ) ||
      all(
        is.na(filtered_data$exposureIntensity) |
        filtered_data$exposureIntensity == 0
      ) ||
      all(
        is.na(filtered_data$exposureConcern) |
        filtered_data$exposureConcern == 0
      )) {
        return(reactable(
          data.frame(Message = "Not enough data available for the selected patient."),
          columns = list(Message = colDef(
            align = "center",
            style = list(color = "red", fontWeight = "bold")
          ))
        ))
      }
        
      # Render reactable table
      reactable(
        filtered_data,
        columns = list(
          exposureName = colDef(
            name = "Exposure",
            headerStyle = list(fontWeight = "bold"),
            minWidth = 150,
            sticky = "left"
          ),
          exposureFreq = colDef(
            name = "Frequency",
            headerStyle = list(fontWeight = "bold"),
            align = "left",
            minWidth = 100,
            cell = function(value)
              custom_circle_icon(value)
          ),
          exposureIntensity = colDef(
            name = "Intensity",
            headerStyle = list(fontWeight = "bold"),
            align = "left",
            minWidth = 100,
            cell = function(value)
              custom_circle_icon(value)
          ),
          exposureConcern = colDef(
            name = "Concern",
            headerStyle = list(fontWeight = "bold"),
            align = "left",
            minWidth = 100,
            cell = function(value)
              custom_circle_icon(value)
          ),
          id = colDef(name = "id",
                      show = FALSE),
          exposureYesNo = colDef(name = "Exposed",
                                 show = FALSE),
          percentile_rank_freq = colDef(
            name = "Frequency Percentile",
            headerStyle = list(fontWeight = "bold"),
            align = "left",
            minWidth = 70,
            cell = color_tiles(
              individual_percentiles(),
              colors = RColorBrewer::brewer.pal(7, "YlGnBu"),
              number_fmt = scales::percent_format(scale = 1,
                                                  accuracy = 0.1)
            )
          ),
          percentile_rank_intensity = colDef(
            name = "Intensity Percentile",
            headerStyle = list(fontWeight = "bold"),
            align = "left",
            minWidth = 70,
            cell = color_tiles(
              individual_percentiles(),
              colors = RColorBrewer::brewer.pal(7, "YlGnBu"),
              number_fmt = scales::percent_format(scale = 1,
                                                  accuracy = 0.1)
            )
          ),
          percentile_rank_concern = colDef(
            name = "Concern Percentile",
            headerStyle = list(fontWeight = "bold"),
            align = "left",
            minWidth = 70,
            cell = color_tiles(
              individual_percentiles(),
              colors = RColorBrewer::brewer.pal(7, "YlGnBu"),
              number_fmt = scales::percent_format(scale = 1,
                                                  accuracy = 0.1)
            )
          ),
          conditional_percentile_rank_intensity = colDef(
            name = "Conditional Intensity Percentile",
            headerStyle = list(fontWeight = "bold"),
            align = "left",
            minWidth = 70,
            cell = color_tiles(
              individual_percentiles(),
              colors = RColorBrewer::brewer.pal(7, "YlGnBu"),
              number_fmt = scales::percent_format(scale = 1,
                                                  accuracy = 0.1)
            )
          ),
          conditional_percentile_rank_concern = colDef(
            name = "Conditional Concern Percentile",
            headerStyle = list(fontWeight = "bold"),
            align = "left",
            minWidth = 70,
            cell = color_tiles(
              individual_percentiles(),
              colors = RColorBrewer::brewer.pal(7, "YlGnBu"),
              number_fmt = scales::percent_format(scale = 1,
                                                  accuracy = 0.1)
            )
          )
        ),
        details = function(index) {
          exposure_name <- individual_percentiles()$exposureName[index]
          summary_table <- generate_summary_table(exposure_name)
          
          reactable(
            summary_table,
            pagination = FALSE,
            defaultPageSize = 10,
            compact = TRUE,
            striped = TRUE,
            highlight = TRUE,
            columns = list(
              Statistic = colDef(name = "Filtered Group Stats", minWidth = 100),
              N = colDef(name = "N", minWidth = 50),
              Frequency = colDef(minWidth = 50),
              Intensity = colDef(minWidth = 50),
              Concern = colDef(minWidth = 50)
            )
          )
        },
        columnGroups = list(
          colGroup(
            name = "Individual Scores",
            columns = c("exposureFreq", "exposureIntensity", "exposureConcern")
          ),
          colGroup(
            name = "Percentile Ranks",
            columns = c(
              "percentile_rank_freq",
              "percentile_rank_intensity",
              "percentile_rank_concern"
            )
          ),
          colGroup(
            name = "Conditional Ranks",
            columns = c(
              "conditional_percentile_rank_intensity",
              "conditional_percentile_rank_concern"
            )
          )
        ),
        pagination = FALSE,
        filterable = FALSE,
        sortable = TRUE,
        defaultExpanded = FALSE,
        height = 700,
        theme = reactableTheme(
          borderColor = "#dfe2e5",
          stripedColor = "#f6f8fa",
          highlightColor = "#f0f5f9",
          cellPadding = "8px 12px"
        )
      )
    })
    
    # Civilian Exposures Table ----
    ## Filter civilian exposures data based on selected patient ----
    ind_civilian_exposures <- reactive({
      req(selected_patient())
      individual_data <- civilian_expo_dat %>%
        filter(id == selected_patient()) %>%
        arrange(desc(exposureFreq))
    }) %>% bindCache(selected_patient())
    
    ## Render civilian exposures table ----
    output$civilianExposuresTable <- renderReactable({
      req(ind_civilian_exposures())
      
      ### Check if data is empty
      if (nrow(ind_civilian_exposures()) == 0) {
        return(reactable(
          data.frame(Message = "No data available for the selected filters."),
          columns = list(Message = colDef(
            align = "center",
            style = list(color = "red", fontWeight = "bold")
          ))
        ))
      }
      
      ## Render reactable table ----
      reactable(
        ind_civilian_exposures(),
        columns = list(
          exposureName = colDef(
            name = "Exposure",
            headerStyle = list(fontWeight = "bold"),
            minWidth = 150,
            sticky = "left"
          ),
          exposureFreq = colDef(
            name = "Frequency",
            headerStyle = list(fontWeight = "bold"),
            align = "left",
            minWidth = 100,
            cell = function(value)
              custom_circle_icon(value)
          ),
          exposureIntensity = colDef(
            name = "Intensity",
            headerStyle = list(fontWeight = "bold"),
            align = "left",
            minWidth = 100,
            cell = function(value)
              custom_circle_icon(value)
          ),
          exposureConcern = colDef(
            name = "Concern",
            headerStyle = list(fontWeight = "bold"),
            align = "left",
            minWidth = 100,
            cell = function(value)
              custom_circle_icon(value)
          ),
          id = colDef(name = "id",
                      show = FALSE),
          exposureYesNo = colDef(name = "exposureYesNo",
                                 show = FALSE)
        ),
        pagination = FALSE,
        filterable = FALSE,
        defaultExpanded = FALSE,
        height = 300,
        theme = reactableTheme(
          borderColor = "#dfe2e5",
          stripedColor = "#f6f8fa",
          highlightColor = "#f0f5f9",
          cellPadding = "8px 12px"
        )
      )
    })
  })
}
