ptSelfReportUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "pt_self_report_scores",
    fluidRow(
      box(
        title = "Filter by Deployment and Age",
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        fluidRow(
          column(
            6,
            selectInput(
              ns("milConflictCodePtCard"),
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
            6,
            sliderInput(
              ns("agePtCard"),
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
      )
    ),
    fluidRow(
      box(
        title = "Filter by Birth Sex, Ethnicity, and Race",
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        fluidRow(
          column(
            4,
            selectInput(
              ns("genderPtCard"),
              "Birth Sex:",
              choices = c("All", unique(combined_selfreport_measures_dat$gender)),
              selected = "All"
            )
          ),
          column(
            4,
            selectizeInput(
              ns("ethnicityPtCard"),
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
              ns("racePtCard"),
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
        title = "Self-Report Measures - Interactive Plot",
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        plotlyOutput(ns("srMeasuresPlot"), height = "1000px")
      )
    )
  )
}

## Static Data
measure_info <- tibble::tribble(
  ~srMeasures, ~description, ~better_or_worse, ~min_score, ~max_score, ~cutoff_scores, ~population_mean,
  
  # Existing measures
  "averagePain", "Last week's usual pain level", "worse", 0, 10, NA, NA,
  "worstPain", "Last week's worst pain level", "worse", 0, 10, NA, NA,
  "painNow", "Pain level right now", "worse", 0, 10, NA, NA,
  "bestPain", "Last week's best pain level", "worse", 0, 10, NA, NA,
  "wpiTotal", "Widespread Pain Index", "worse", 0, 19, NA, NA,
  "sf36Pain", "Past month's pain & interference", "worse", 0, 100, NA, 70.77,
  "sf36GeneralHealth", "Past month's health & wellbeing", "worse", 0, 100, NA, 56.99,
  "sf36EnergyFatigue", "Past month's energy & fatigue", "worse", 0, 100, NA, 52.15,
  "sf36EmotionalWellbeing", "Past month's mental health", "worse", 0, 100, NA, 70.38,
  "sf36SocialFunctioning", "Past month's physical & emotional problems interference w/ social activities", "worse", 0, 100, NA, 78.77,
  "sf36PhysicalFunctioning", "Ability to perform physical activities", "worse", 0, 100, NA, 70.61,
  "sf36LimitationsDueToEmotionalProblems", "Past month's impact of mental health on ADLs", "worse", 0, 100, NA, 65.78,
  "sf36LimitationsDueToPhysicalHealth", "Past month's impact of physical health on ADLs", "worse", 0, 100, NA, 52.97,
  "sf36HealthChange", "Perceived change in health during past year", "worse", 0, 100, NA, 59.14,
  "nsiTotalSum", "Severity of postconcussive somatic, cognitive, and affective Sx (2 wks)", "worse", 0, 88, NA, NA,
  "nsiSomatosensory", "Physical Sx (e.g., headaches, dizziness, balance, light/noise sensitivity", "worse", 0, 60, NA, NA,
  "nsiAffective", "Emotional and mood Sx (e.g., irritability, anxiety, depression", "worse", 0, 35, NA, NA,
  "nsiCognitive", "Cognitive Sx (e.g., concentration, memory, slowed thinking)", "worse", 0, 16, NA, NA,
  "cfsSxSum", "Presence, frequency, & intensity of fatigue during past month", "worse", 0, 72, NA, NA,
  "sleepQuality", "PSQI item on subjective sleep quality", "worse", 0, 3, NA, NA,
  "timeInBed", "Total hours spent in bed per night", "relative", 0, 24, NA, NA,
  "sleepDuration", "Hours of actual sleep per night", "better", 0, 24, NA, NA,
  "sleepLatency", "Time it takes to fall asleep (minutes)", "worse", 0, 120, NA, NA,
  "sleepEfficiency", "Percentage of time in bed spent sleeping", "relative", 0, 100, "80%, 85%, 90%", NA,
  "psqiTotal", "Global score of sleep quality", "worse", 0, 21, "5+", NA,
  "msqTotalScore", "Impact of migraine on health-related QoL", "better", 0, 100, NA, NA,
  "ibsTotalScore", "Past 3 month irritable bowel syndrome pain and discomfort", "worse", 0, 32, NA, NA,
  "phq8TotalScore", "Past 2 weeks bothered by the following (MDD)", "worse", 0, 24, "no/minimal: 0-4; mild: 5-9; moderate: 10-14; severe: 15-24", NA,
  "phqPdTotalScore", "Symptoms of panic disorder", "worse", 0, 5, "0: denied Sx; 0-4: subclinical Sx; 5: probable PD", NA,
  "gadScore", "Symptoms of generalized anxiety disorder", "worse", 0, 3, "0: none; 1: several days; 2: > half days; 3: nearly every day", NA,
  "pclTotalScore", "PTSD severity", "worse", 0, 80, "31", NA,
  "cesTotalScore", "Combat exposures/wartime stressors", "worse", 0, 41, "Moderate: 17-24", NA,
  "drriTotalScore", "Deployment risk and resilience inventory", "worse", 12, 60, NA, NA,
  "auditCSum", "Alcohol use disorder/impact of alcohol", "worse", 0, 12, "Men: 4 (+); Women: 3 (+)", NA
)

measure_info <- measure_info %>%
  mutate(
    cutoff_scores = ifelse(is.na(cutoff_scores), "Not available", cutoff_scores),
    population_mean = ifelse(is.na(population_mean), "Not available", population_mean)
  )

## End Static Data

ptSelfReportServer <- function(id, selected_patient) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Desired order of srMeasures (reversed for correct top-to-bottom order) ----
    desired_order <- rev(
      c(
        "averagePain",
        "worstPain",
        "painNow",
        "bestPain",
        "wpiTotal",
        "sf36Pain",
        "sf36GeneralHealth",
        "sf36EnergyFatigue",
        "sf36EmotionalWellbeing",
        "sf36SocialFunctioning",
        "sf36PhysicalFunctioning",
        "sf36LimitationsDueToEmotionalProblems",
        "sf36LimitationsDueToPhysicalHealth",
        "sf36HealthChange",
        "nsiTotalSum",
        "nsiSomatosensory",
        "nsiAffective",
        "nsiCognitive",
        "sleepQuality",
        "timeInBed",
        "sleepDuration",
        "sleepLatency",
        "sleepEfficiency",
        "psqiTotal",
        "cfsSxSum",
        "msqTotalScore",
        "auditCSum",
        "ibsTotalScore",
        "phq8TotalScore",
        "phqPdTotalScore",
        "gadScore",
        "pclTotalScore",
        "cesTotalScore",
        "drriTotalScore"
      )
    )
    
    ## Filter IDs based on inputs ----
    filtered_data <- reactive({
      req(
        input$agePtCard,
        input$genderPtCard,
        input$milConflictCodePtCard,
        input$ethnicityPtCard,
        input$racePtCard,
        selected_patient()
      )
      
      ## Filter IDs based on milConflictCode using mil_conflict_dat 
      conflict_filtered_ids <- if ("All" %in% input$milConflictCodePtCard || length(input$milConflictCodePtCard) == 0) {
        unique(full_demo_dat$id)  # Include all IDs if "All" is selected or no options are selected
      } else {
        unique(
          mil_conflict_dat %>%
            filter(milConflictCode %in% input$milConflictCodePtCard) %>%
            pull(id)
        )
      }
      
      ## Filter combined_selfreport_measures_dat based on age, gender, conflict_filtered_ids, race, and ethnicity 
      df <- combined_selfreport_measures_dat %>%
        filter(
          age >= input$agePtCard[1] & age <= input$agePtCard[2],
          (input$genderPtCard == "All" |
             gender == input$genderPtCard),
          id %in% conflict_filtered_ids,
          (
            input$ethnicityPtCard == "All" |
              ethnicity %in% input$ethnicityPtCard
          ),
          # Allow multiple ethnicities
          (input$racePtCard == "All" |
             race %in% input$racePtCard),
          # Allow multiple races
          id != selected_patient() # Exclude selected patient from statistics
        )
      
      ## Apply additional filtering based on selected toxic exposures using mil_expo_dat
      if (!is.null(input$selectedExposures) &&
          length(input$selectedExposures) > 0) {
        filtered_exposures <- mil_expo_dat %>%
          select(id, exposureName, exposureYesNo) %>%
          distinct() %>%
          filter(exposureName %in% input$selectedExposures,
                 exposureYesNo == 1)
        
        ## Get the unique IDs of patients exposed to the selected toxic exposures
        exposed_ids <- unique(filtered_exposures$id)
        
        ## Further filter df based on exposed_ids
        df <- df %>%
          filter(id %in% exposed_ids)
      }
      
      df %>%
        pivot_longer(cols = 14:47,
                     names_to = "srMeasures",
                     values_to = "srScores") %>%
        mutate(srMeasures = factor(srMeasures, levels = desired_order))
    }) %>% bindCache(
      input$agePtCard,
      input$genderPtCard,
      input$milConflictCodePtCard,
      input$ethnicityPtCard,
      input$racePtCard,
      input$selectedExposures,
      selected_patient()
    )
    
    # Reactive expression to extract patient's score separately
    participant_score <- reactive({
      req(selected_patient())
      
      combined_selfreport_measures_dat %>%
        filter(id == selected_patient()) %>%
        pivot_longer(14:47, names_to = "srMeasures", values_to = "srScores") %>%
        mutate(srMeasures = factor(srMeasures, levels = desired_order)) %>%
        select(srMeasures, ptScore = srScores)
    })
    
    ## Reactive expression to calculate statistics ----
    statistics <- reactive({
      req(filtered_data(), participant_score())
      
      df <- filtered_data()
      pt_score <- participant_score()
      
      stats <- df %>%
        group_by(srMeasures) %>%
        summarise(
          mean = round(mean(srScores, na.rm = TRUE), 2),
          sd = round(sd(srScores, na.rm = TRUE), 2),
          median = round(median(srScores, na.rm = TRUE), 2),
          n = sum(!is.na(srScores)),
          z_median = round((median - mean) / sd, 2)
        ) %>%
        mutate(z_mean = 0)
      
      ## Add participant's z-score to the statistics df ----
      pt_score <- pt_score %>%
        left_join(stats, by = "srMeasures") %>%
        mutate(ptZScore = round((ptScore - mean) / sd, 2)) %>%
        select(srMeasures, ptScore, ptZScore)
      
      ## Join with the main stats ----
      stats %>%
        left_join(pt_score, by = "srMeasures") %>%
        left_join(measure_info, by = "srMeasures")
    })
    
    # Render ggplotly ----
    output$srMeasuresPlot <- renderPlotly({
      req(statistics())
      
      stats <- statistics()
      
      ## Define measure names ----
      measure_labels <- c(
        "averagePain" = "Average Pain",
        "worstPain" = "Worst Pain",
        "painNow" = "Pain Now",
        "bestPain" = "Best Pain",
        "wpiTotal" = "Widespread Pain Index",
        "sf36Pain" = "SF-36 Pain",
        "sf36GeneralHealth" = "<i>SF-36 General Health</i>", 
        "sf36EnergyFatigue" = "<i>SF-36 Energy/Fatigue</i>", 
        "sf36EmotionalWellbeing" = "<i>SF-36 Emotional Well-being</i>",
        "sf36SocialFunctioning" = "<i>SF-36 Social Functioning</i>",
        "sf36PhysicalFunctioning" = "<i>SF-36 Physical Functioning</i>",
        "sf36LimitationsDueToEmotionalProblems" = "<i>SF-36 Limitations Emotional Problems</i>",
        "sf36LimitationsDueToPhysicalHealth" = "<i>SF-36 Limitations Physical Health</i>",
        "sf36HealthChange" = "<i>SF-36 Health Change</i>", 
        "nsiTotalSum" = "NSI Total",
        "nsiSomatosensory" = "NSI Somatosensory",
        "nsiAffective" = "NSI Affective",
        "nsiCognitive" = "NSI Cognitive",
        "sleepQuality" = "<i>Sleep Quality</i>",
        "timeInBed" = "<i>Time In Bed</i>",
        "sleepDuration" = "<i>Sleep Duration</i>",
        "sleepLatency" = "<i>Sleep Latency</i>",
        "sleepEfficiency" = "<i>Sleep Efficiency</i>",
        "psqiTotal" = "<i>PSQI Total</i>",
        "cfsSxSum" = "CFS Symptom Total",
        "msqTotalScore" = "MSQ Total Score",
        "auditCSum" = "AUDIT-C Sum",
        "ibsTotalScore" = "IBS Total Score",
        "phq8TotalScore" = "<i>PHQ-8 Total Score</i>",
        "phqPdTotalScore" = "<i>PHQ-PD Total Score</i>",
        "gadScore" = "<i>GAD Score (single-item)</i>",
        "pclTotalScore" = "<i>PCL Total Score</i>",
        "cesTotalScore" = "CES Total Score",
        "drriTotalScore" = "DRRI Total Score"
      )
      
      ## Prepare tooltip text ----
      stats <- stats %>%
        mutate(srMeasures = factor(srMeasures, levels = names(rev(
          measure_labels
        )))) %>%
        mutate(
          tooltip_text = paste0(
            "<b>Measure:</b> ",
            rev(measure_labels[srMeasures]),
            "<br><b>Description:</b> ",
            description,
            "<br><b>Higher Score is</b> ",
            better_or_worse,
            "<br><b>Scale Range:</b> ",
            min_score,
            "-",
            max_score,
            "<br><b>Cutoff Scores:</b> ",
            cutoff_scores,
            "<br><b><i>n =</i></b> ",
            n,
            "<br><b>Patient's Score:</b> ",
            ptScore,
            "<br><b>Patient's Z-Score:</b> ",
            ptZScore,
            "<br><b>Mean:</b> ",
            mean,
            "<br><b><i>sd =</i></b> ",
            sd,
            "<br><b>Median:</b> ",
            median,
            "<br><b>Z-Score Median:</b> ",
            z_median
          )
        )
      
      ## Create ggplot ----
      p <-
        ggplot(data = stats, aes(x = z_median, y = srMeasures, text = tooltip_text)) +
        geom_segment(
          aes(
            x = -4,
            xend = 4,
            y = srMeasures,
            yend = srMeasures
          ),
          color = "#E2EAF4",
          size = 3.5,
          lineend = "round",
          show.legend = FALSE
        ) +
        geom_segment(
          data = stats,
          aes(
            x = rep(-4:4, length.out = nrow(stats)),  
            xend = rep(-4:4, length.out = nrow(stats)),
            y = 0.75,                                
            yend = length(levels(srMeasures)) + 0.25 
          ),
          linetype = "dashed",
          color = "#b6c4d6",
          alpha = 0.5,
          show.legend = FALSE
        ) +
        geom_segment(
          aes(
            x = -4,
            xend = -4,
            y = 0.73,
            yend = length(levels(srMeasures)) + 0.25
          ),
          color = "#b6c4d6",
          linetype = "solid",
          size = 0.5,
          show.legend = FALSE
        ) +
        geom_segment(
          aes(
            x = 4,
            xend = 4,
            y = 0.73,
            yend = length(levels(srMeasures)) + 0.25
          ),
          color = "#b6c4d6",
          linetype = "solid",
          size = 0.5,
          show.legend = FALSE
        ) +
        geom_segment(
          aes(
            x = 0,       
            xend = 0,    
            y = 0.73, # Adjust bottom of the line (higher nums bring line up)
            yend = length(levels(srMeasures)) + 0.30 # Adjust top of the line (higher nums bring up)
          ),
          color = "#b6c4d6",
          linetype = "solid",
          size = 0.5,
          show.legend = FALSE
        ) +
        geom_segment(
          aes(
            x = -4,
            xend = 4,
            y = which(names(measure_labels) == "phq8TotalScore") - 0.5,
            yend = which(names(measure_labels) == "phq8TotalScore") - 0.5
          ),
          color = "#b6c4d6",
          linetype = "solid",
          size = 0.1,
          show.legend = FALSE
        ) +
        geom_segment(
          aes(
            x = -4,
            xend = 4,
            y = which(names(measure_labels) == "sleepDuration") - 0.5,
            yend = which(names(measure_labels) == "sleepDuration") - 0.5
          ),
          color = "#b6c4d6",
          linetype = "solid",
          size = 0.1,
          show.legend = FALSE
        ) +
        geom_segment(
          aes(
            x = -4,
            xend = 4,
            y = which(names(measure_labels) == "nsiAffective") - 0.5,
            yend = which(names(measure_labels) == "nsiAffective") - 0.5
          ),
          color = "#b6c4d6",
          linetype = "solid",
          size = 0.1,
          show.legend = FALSE
        ) +
        geom_segment(
          aes(
            x = -4,
            xend = 4,
            y = which(names(measure_labels) == "sf36PhysicalFunctioning") - 0.5,
            yend = which(names(measure_labels) == "sf36PhysicalFunctioning") - 0.5
          ),
          color = "#b6c4d6",
          linetype = "solid",
          size = 0.1,
          show.legend = FALSE
        ) +
        geom_segment(
          aes(
            x = -4,
            xend = 4,
            y = which(names(measure_labels) == "sf36GeneralHealth") - 0.5,
            yend = which(names(measure_labels) == "sf36GeneralHealth") - 0.5
          ),
          color = "#b6c4d6",
          linetype = "solid",
          size = 0.1,
          show.legend = FALSE
        ) +
        geom_segment(
          aes(
            x = -4,
            xend = 4,
            y = which(names(measure_labels) == "painNow") - 0.5,
            yend = which(names(measure_labels) == "painNow") - 0.5
          ),
          color = "#b6c4d6",
          linetype = "solid",
          size = 0.1,
          show.legend = FALSE
        ) +
        geom_point(
          aes(
          shape = "Filtered Group", 
          color = "Filtered Group"),
          size = 3,
          alpha = 0.8
          ) +
        geom_point(
          aes(
            x = ptZScore,
            shape = "Selected Patient",
            color = "Selected Patient"
          ),
          size = 3,
          alpha = 0.8
        ) +
        scale_shape_manual(name = "Statistics",
                           values = c("Filtered Group" = 15, 
                                      "Selected Patient" = 17)) +
        scale_color_manual(
          name = "Statistics",
          values = c(
            "Filtered Group" = "#5DCCE9",
            "Selected Patient" = "#060270"
          )) +
        scale_y_discrete(labels = measure_labels) +
        scale_x_continuous(
          position = "top",
          limits = c(-4, 4),
          breaks = c(-4,-3,-2,-1, 0, 1, 2, 3, 4)
        ) +
        labs(x = "Z-Score", y = NULL, title = NULL) +
        theme_ipsum_rc(grid = "X") +
        theme(axis.text.y = element_markdown(size = 10, hjust = 1))
      
      ## Generate the ggplotly object ----
      plotly_obj <- ggplotly(p, tooltip = "text")
      
      # Modify traces for legend, tooltip, & vertical lines
      plotly_obj$x$data[[6]]$name <- "Filtered Group"
      plotly_obj$x$data[[6]]$hoverinfo <- "text"
      
      plotly_obj$x$data[[7]]$name <- "Selected Patient"
      plotly_obj$x$data[[7]]$hoverinfo <- "text"  
      
      plotly_obj$x$data[[1]]$hoverinfo <- "none"
      plotly_obj$x$data[[2]]$hoverinfo <- "none"  
      plotly_obj$x$data[[3]]$hoverinfo <- "none"
      plotly_obj$x$data[[4]]$hoverinfo <- "none"
      plotly_obj$x$data[[5]]$hoverinfo <- "none"
      
      ## Add layout modifications ----
      plotly_obj <- plotly_obj %>%
        layout(
          hoverlabel = list(align = "left"),
          margin = list(
            l = 50,
            r = 50,
            t = 50,
            b = 100
          ),
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE,
            side = "top"
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE,
            anchor = "free",
            position = 0.035
          ),
          legend = list(
            title = list(text = ""),
            font = list(size = 14),
            orientation = "h",
            x = 0.96,
            xanchor = "right",
            y = 1.065,
            yanchor = "top"
          )
        ) %>%
        config(displayModeBar = FALSE)
    
      plotly_obj
    })
  })
}
