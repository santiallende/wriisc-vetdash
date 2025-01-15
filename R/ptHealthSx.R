ptHealthSxUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "pt_health_sx",
          fluidRow(
            box(
              title = "Filter Health Symptoms Table by Deployment and Age",
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              fluidRow(
                column(
                  4,
                  selectInput(
                    ns("milConflictCodePtCardHealthSx"),
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
                      "OIF",
                      "OEF",
                      "OND",
                      "Other",
                      "Somalia",
                      "Vietnam",
                      "WWII"
                    ),
                    multiple = TRUE,
                    selected = "All"
                  )
                ),
                column(
                  4,
                  sliderInput(
                    ns("ageHealthSx"),
                    "Age Range:",
                    min = min(full_demo_dat$age, na.rm = TRUE),
                    max = max(full_demo_dat$age, na.rm = TRUE),
                    value = c(
                      min(full_demo_dat$age, na.rm = TRUE),
                      max(full_demo_dat$age, na.rm = TRUE)
                    )
                  )
                ),
                column(
                  4,
                  selectizeInput(
                    ns("filterByExpo"),
                    "Filter by Exposures:",
                    choices = c("None", unique(mil_expo_dat$exposureName)),
                    multiple = TRUE,
                    selected = "None"
                  )
                )
              )
            ),
            box(
              title = "Filter Health Symptoms Table by Birth Sex, Ethnicity, and Race",
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              fluidRow(
                column(
                  4,
                  selectInput(
                    ns("genderHealthSx"),
                    "Birth Sex:",
                    choices = c("All", "Male", "Female"),
                    selected = "All"
                  )
                ),
                column(
                  4,
                  selectizeInput(
                    ns("ethnicityHealthSx"),
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
                    ns("raceHealthSx"),
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
            box(
              title = "Medically Diagnosed Conditions",
              width = 6,
              collapsible = TRUE,
              solidHeader = TRUE,
              reactableOutput(ns("diagnosedConditionsTable"))
            ),
            box(
              title = "Health Symptom Frequency and Rank",
              width = 6,
              collapsible = TRUE,
              solidHeader = TRUE,
              reactableOutput(ns("healthSymptomsTable"))
            )
          )
    )
}

ptHealthSxServer <- function(id, selected_patient) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter IDs based on inputs ----
    filtered_ids_health_sx <- reactive({
      req(
        input$ageHealthSx,
        input$genderHealthSx,
        input$milConflictCodePtCardHealthSx,
        input$ethnicityHealthSx,
        input$raceHealthSx,
        input$filterByExpo
      )
      
      conflict_filtered_ids <- if ("All" %in% input$milConflictCodePtCardHealthSx || length(input$milConflictCodePtCardHealthSx) == 0) {
        unique(full_demo_dat$id)  # Include all IDs if "All" is selected or no options are selected
      } else {
        unique(
          mil_conflict_dat %>%
            filter(milConflictCode %in% input$milConflictCodePtCardHealthSx) %>%
            pull(id)
        )
      }
      
      exposure_filtered_ids <- if ("None" %in% input$filterByExpo || length(input$filterByExpo) == 0) {
        unique(full_demo_dat$id)  # Include all IDs if "All" is selected or no options are selected
      } else {
        unique(
          mil_expo_dat %>%
            filter(exposureName %in% input$filterByExpo & exposureYesNo == 1) %>%
            pull(id)
        )
      }
      
      age_gender_filtered <- full_demo_dat %>%
        filter(
          age >= input$ageHealthSx[1] & age <= input$ageHealthSx[2],
          (
            input$genderHealthSx == "All" | gender == input$genderHealthSx
          )
        )
      
      ethnicity_race_filtered <- age_gender_filtered %>%
        filter(
          (
            input$ethnicityHealthSx == "All" |
              ethnicity %in% input$ethnicityHealthSx
          ),
          (
            input$raceHealthSx == "All" |
              race %in% input$raceHealthSx
          )
        )
      
      final_filtered <- ethnicity_race_filtered %>%
        filter(id %in% conflict_filtered_ids) %>%
        filter(id %in% exposure_filtered_ids)
      
      observe({
        print(conflict_filtered_ids)
        print("expo filtered ids: ", exposure_filtered_ids)
        print(input$filterByExpo)
      })
      
      unique(final_filtered$id)
    }) %>% bindCache(
      input$ageHealthSx,
      input$genderHealthSx,
      input$milConflictCodePtCardHealthSx,
      input$ethnicityHealthSx,
      input$raceHealthSx,
      input$filterByExpo
    )
    
    filtered_health_sx_dat <- reactive({
      req(filtered_ids_health_sx())
      data <- joined_everDxDat_healthSxDat %>%
        filter(id %in% filtered_ids_health_sx())
      
      data
      
    }) %>% bindCache(filtered_ids_health_sx())
    
    # Calculate summary statistics for the group ----
    health_sx_dat_summary <- reactive({
      filtered_health_sx_dat() %>%
        filter(!is.na(healthSxValues) & healthSxValues > 0) %>%  # Exclude zero values / conditional
        group_by(healthSxNames) %>%
        summarise(
          n = n_distinct(id),
          median_value = median(healthSxValues, na.rm = TRUE),
          mean_value = mean(healthSxValues, na.rm = TRUE),
          percentile_75 = quantile(healthSxValues, 0.75, na.rm = TRUE),
          percentile_90 = quantile(healthSxValues, 0.90, na.rm = TRUE)
        )
    }) %>% bindCache(filtered_health_sx_dat())
    
    # Calculate percentile ranks for an individual ----
    calculate_percentile_rank <-
      function(individual_score, group_scores) {
        group_scores <-
          group_scores[!is.na(group_scores)] # Exclude NA values
        (
          sum(group_scores < individual_score) + 0.5 * sum(group_scores == individual_score)
        ) / length(group_scores) * 100
      }
    
    calculate_individual_health_sx_percentiles <- function(individual_data, group_data) {
      individual_data %>%
        group_by(healthSxNames) %>%
        mutate(
          percentile_rank = ifelse(
            healthSxValues == 0, 
            NA,  # Assign NA if healthSxValues is 0 / conditional
            calculate_percentile_rank(healthSxValues, 
                                      group_data$healthSxValues[group_data$healthSxNames == healthSxNames])
          )
        ) %>%
        ungroup()
    }
    
    # Apply calculate_percentile_rank function ----
    individual_health_sx_percentiles <- reactive({
      req(selected_patient(), filtered_health_sx_dat())
      individual_data <- joined_everDxDat_healthSxDat %>% 
        filter(id == selected_patient())
      group_data <- filtered_health_sx_dat()
      
      # Calculate percentiles without modifying NA values
      percentile_data <-
        calculate_individual_health_sx_percentiles(individual_data, group_data) %>%
        arrange(desc(healthSxValues)) %>%
        mutate(
          condition_colors = case_when(
            conditionYesNo == 0 ~ "red",
            conditionYesNo == 1 ~ "forestgreen",
            is.na(conditionYesNo) ~ "gray"
          ),
          condition_icons = case_when(
            conditionYesNo == 0 ~ "square-xmark", 
            conditionYesNo == 1 ~ "square-check",
            is.na(conditionYesNo) ~ "circle"
          )
        ) %>%
        arrange(desc(conditionYesNo))
      
      percentile_data
    }) %>% bindCache(selected_patient(), filtered_health_sx_dat())
    
    # Generate summary table for each health symptom ----
    generate_health_sx_summary_table <- function(symptom_name) {
      health_sx_dat_summary() %>%
        filter(healthSxNames == symptom_name) %>%
        select(-n,
               median_value,
               mean_value,
               percentile_75,
               percentile_90) %>%
        pivot_longer(median_value:percentile_90,
                     names_to = "stat",
                     values_to = "value") %>%
        mutate(
          value = ifelse(is.na(value), "NA", sprintf("%.2f", value)),
          stat = case_when(
            stat == "median_value" ~ "Median",
            stat == "mean_value" ~ "Mean",
            stat == "percentile_75" ~ "75th Percentile",
            stat == "percentile_90" ~ "90th Percentile"
          )
        ) %>%
        rename(Statistic = stat, Value = value) %>%
        mutate(across(everything(), str_to_title)) %>%
        mutate(N = as.character(
          health_sx_dat_summary() %>%
            filter(healthSxNames == symptom_name) %>%
            pull(n)
        )) %>%
        select(-healthSxNames)
    }
    
    # Function to generate filled blue circle ----
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
    
    # Render diagnosed conditions table ----
    output$diagnosedConditionsTable <- renderReactable({
      req(individual_health_sx_percentiles())
      
      diagnosed_data <- individual_health_sx_percentiles() %>%
        filter(conditionYesNo == 1) %>%
        select(condition, conditionYesNo, conditionClass,
               condition_icons, condition_colors)

      reactable(
        diagnosed_data,
        columns = list(
          condition = colDef(
            name = "Medically Diagnosed Conditions",
            cell = icon_sets(diagnosed_data,
                             icon_color = "condition_colors",
                             icon_ref = "condition_icons",
                             icon_position = "right",
                             icon_size = 20,
                             colors = "black")
          ),
          conditionClass = colDef(name = "Condition Class"),
          conditionYesNo = colDef(show = FALSE),
          condition_icons = colDef(show = FALSE),
          condition_colors = colDef(show = FALSE)
        ),
        pagination = FALSE,
        sortable = TRUE,
        defaultSorted = "conditionYesNo",
        theme = reactableTheme(
          borderColor = "#dfe2e5",
          stripedColor = "#f6f8fa",
          highlightColor = "#f0f5f9",
          cellPadding = "8px 12px"
        )
      )
    })
    
    # Render health symptoms table ----
    output$healthSymptomsTable <- renderReactable({
      req(individual_health_sx_percentiles())
      
      health_sx_data <- individual_health_sx_percentiles() %>%
        select(healthSxNames, healthSxValues, percentile_rank) %>%
        filter(!is.na(healthSxValues) & !is.na(percentile_rank)) %>%
        arrange(desc(healthSxValues))
      
      reactable(
        health_sx_data,
        columns = list(
          healthSxNames = colDef(
            name = "Health Symptom",
            minWidth = 120,
            align = "left"
          ),
          healthSxValues = colDef(
            name = "Frequency",
            minWidth = 100,
            align = "left",
            cell = function(value) custom_circle_icon(value)
          ),
          percentile_rank = colDef(
            name = "Conditional Rank (non-zero)",
            minWidth = 70,
            align = "center",
            cell = color_tiles(
              health_sx_data,
              colors = RColorBrewer::brewer.pal(7, "YlGnBu"),
              number_fmt = scales::percent_format(scale = 1, accuracy = 0.1)
            )
          )
        ),
        details = function(index) {
          symptom_name <-
            individual_health_sx_percentiles()$healthSxNames[index]
          summary_table <-
            generate_health_sx_summary_table(symptom_name)
          
          reactable(
            summary_table,
            pagination = FALSE,
            defaultPageSize = 5,
            compact = TRUE,
            striped = TRUE,
            highlight = TRUE,
            columns = list(
              Statistic = colDef(name = "Filtered Group Health Sx Freq Stats (non-zero)", 
                                 minWidth = 100),
              N = colDef(name = "N", minWidth = 50),
              Value = colDef(name = "Value", minWidth = 50)
            )
          )
        },
        pagination = FALSE,
        sortable = TRUE,
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
