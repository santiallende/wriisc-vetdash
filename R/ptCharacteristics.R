ptCharacteristicsUI <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "pt_characteristics",
    fluidRow(
      box(
        title = tagList(
          "Patient Characteristics",
          div(
            uiOutput(ns("date_packet_completed")),
          )
        ),
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        fluidRow(
          column(
            4,
            infoBoxOutput(ns("demoInfoOne"), width = 12)
          ),
          column(
            4,
            infoBoxOutput(ns("demoInfoTwo"), width = 12)
          ),
          column(
            4,
            infoBoxOutput(ns("demoInfoThree"), width = 12)
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Patient Concerns",
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        fluidRow(
          column(3, infoBoxOutput(ns("concern1"), width = 12)),
          column(3, infoBoxOutput(ns("concern2"), width = 12)),
          column(3, infoBoxOutput(ns("concern3"), width = 12)),
          column(3, infoBoxOutput(ns("stressors"), width = 12))
        )
      )
    ),
    fluidRow(
      box(
        title = "Military Timeline",
        width = 12,
        collapsible = TRUE,
        fluidRow(
          column(
            12,
            highchartOutput(ns("milTimeline"), height = "150px")
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Medications & Substance Use",
        width = 12,
        collapsible = TRUE,
        fluidRow(
          column(4, infoBoxOutput(ns("medications"), width = 12)),
          column(4, infoBoxOutput(ns("tobacco"), width = 12)),
          column(4, infoBoxOutput(ns("substances"), width = 12))
        )
      )
    ),
    fluidRow(
      box(
        title = "PTSD Screener",
        width = 12,
        collapsible = TRUE,
        fluidRow(
          column(12, uiOutput(ns("ptsd_screening_result"))),
        ),
        fluidRow(
          column(3, infoBoxOutput(ns("nightmares"), width = 12)),
          column(3, infoBoxOutput(ns("avoidance"), width = 12)),
          column(3, infoBoxOutput(ns("hypervigilance"), width = 12)),
          column(3, infoBoxOutput(ns("detachment"), width = 12))
        )
      )
    ),
    fluidRow(
      box(
        title = "TBI Screener",
        width = 12,
        collapsible = TRUE,
        fluidRow(
          column(12, uiOutput(ns("tbi_screening_result"))),
        ),
        fluidRow(
          column(3, infoBoxOutput(ns("tbi_injury_mechanisms"), width = 12)),
          column(3, infoBoxOutput(ns("tbi_consciousness_alterations"), width = 12)),
          column(3, infoBoxOutput(ns("tbi_current_symptoms"), width = 12)),
          column(3, infoBoxOutput(ns("tbi_additional_details"), width = 12)),
        )
      )
    ),
    fluidRow(
      box(
        title = "Sleep - PSQI",
        width = 12,
        collapsible = TRUE,
        fluidRow(
          column(4, infoBoxOutput(ns("psqi_one"), width = 12)),
          column(4, infoBoxOutput(ns("psqi_two"), width = 12)),
          column(4, infoBoxOutput(ns("psqi_three"), width = 12))
        )
      )
    ),
    fluidRow(
      box(
        title = "Sleep - PSQI Details",
        width = 12,
        collapsible = TRUE,
        fluidRow(
          column(4, infoBoxOutput(ns("psqi_details_one"), width = 12)),
          column(4, infoBoxOutput(ns("psqi_details_two"), width = 12)),
          column(4, infoBoxOutput(ns("psqi_details_three"), width = 12))
        )
      )
    ),
    fluidRow(
      box(
        title = "Deployment Health",
        width = 12,
        collapsible = TRUE, 
        fluidRow(
          column(4, infoBoxOutput(ns("deployment_incident"), width = 12)),
          column(4, infoBoxOutput(ns("related_injury"), width = 12)),
          column(4, infoBoxOutput(ns("medical_attention"), width = 12)),
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Family History",
        collapsible = TRUE,
        fluidRow(
          column(3, infoBoxOutput(ns("mother_info"), width = 12)),
          column(3, infoBoxOutput(ns("father_info"), width = 12)),
          column(3, infoBoxOutput(ns("sibling_info"), width = 12)),
          column(3, infoBoxOutput(ns("children_info"), width = 12))
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Military Exposures Extras",
        collapsible = TRUE,
        fluidRow(
          column(4, infoBoxOutput(ns("occupationalHealthProgram"), width = 12)),
          column(4, infoBoxOutput(ns("airQuality"), width = 12)),
          column(4, infoBoxOutput(ns("healthConditions"), width = 12))
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Civilian Exposures Extras",
        collapsible = TRUE,
        fluidRow(
          column(3, infoBoxOutput(ns("other_civilian_exposures"), width = 12)),
          column(3, infoBoxOutput(ns("civilian_respiratory_protection"), width = 12)),
          column(3, infoBoxOutput(ns("civilian_medical_treatment"), width = 12)),
          column(3, infoBoxOutput(ns("civilian_work_restriction"), width = 12))
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "WRIISC Program",
        collapsible = TRUE,
        fluidRow(
          column(3, infoBoxOutput(ns("referral_info"), width = 12)),
          column(3, infoBoxOutput(ns("symptom_reason_info"), width = 12)),
          column(6, infoBoxOutput(ns("info_source_info"), width = 12))
        )
      )
    )
  )
}

ptCharacteristicsServer <- function(id, selected_patient) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Date packet completed ----
    output$date_packet_completed <- renderUI({
      req(selected_patient())
      
      selected_patient <- date_packet_completed_dat %>% filter(id == selected_patient())
      completion_date <- selected_patient$datePacketCompleted

      div(
        style = "text-align:left; font-size:14px; font-weight:500;
        color:green; margin-bottom:0px; margin-top:8px",
        if (!is.na(completion_date)) {
          paste0("Packet was completed ", completion_date)
        } else {
          "Completion date is not available."
        }
      )
    })
    
    full_demo_dat_na_with_missing <- reactive({
      full_demo_dat %>%
        mutate(across(
          c(age, gender, race, ethnicity, languages, handedness, maritalStatus, 
            yearsWithCurrent, partnersHealth, timesMarried, employment),
          ~ replace_na(as.character(.), "missing")
        ))
    })

    # Demographics ----
    output$demoInfoOne <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- full_demo_dat_na_with_missing() %>% 
        filter(id == selected_patient())
      infoBox(
        title = HTML(
          paste(
            "<div style='text-align:left; font-size:14px; font-weight:600;'>",
            "Basic Demographics",
            "</div>"
          )
        ),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            "<strong>Age:</strong> ", selected_patient$age, "<br>",
            "<strong>Birth Sex:</strong> ", selected_patient$gender, "<br>",
            "<strong>Race:</strong> ", selected_patient$race, "<br>",
            "<strong>Ethnicity:</strong> ", selected_patient$ethnicity, "<br>",
            "<strong>Language(s):</strong> ", selected_patient$languages, "<br>",
            "<strong>Handedness:</strong> ", selected_patient$handedness, "<br>",
            "</div>"
          )
        ),
        icon = icon("calendar"),
        color = "red"
      )
    })

    output$demoInfoTwo <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- full_demo_dat_na_with_missing() %>% 
        filter(id == selected_patient())

      infoBox(
        title = HTML(
          paste(
            "<div style='text-align:left; font-size:14px; font-weight:600;'>",
            "Marital & Professional",
            "</div>"
          )
        ),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            "<strong>Current Partner (years):</strong> ", selected_patient$maritalStatus, "<br>",
            "<strong>Current Partner (years):</strong> ", selected_patient$yearsWithCurrent, "<br>",
            "<strong>Partner's Health:</strong> ", selected_patient$partnersHealth, "<br>",
            "<strong>Married:</strong> ", selected_patient$timesMarried, " ", "time(s)", "<br>",
            "<strong>Degrees:</strong> ", selected_patient$degrees, "<br>",
            "<strong>Employment:</strong> ", selected_patient$employment, "<br>",
            "</div>"
          )
        ),
        icon = icon("magnet"),
        color = "aqua"
      )
    })

    mil_info_dat_na_with_missing <- reactive({
      mil_info_dat %>%
        mutate(across(
          c(milDischarge, milPow, milUnitNames, milUnitType,
            milAreaNames, payGrade),
          ~ replace_na(as.character(.), "missing")
        ))
    })
    
    output$demoInfoThree <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- mil_info_dat_na_with_missing() %>% 
        filter(id == selected_patient())

      infoBox(
        title = HTML(
          paste(
            "<div style='text-align:left; font-size:14px; font-weight:600;'>",
            "Military Info",
            "</div>"
          )
        ),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            "<strong>Discharge:</strong> ", selected_patient$milDischarge, "<br>",
            "<strong>POW:</strong> ", selected_patient$milPow, "<br>",
            "<strong>Unit Names:</strong> ", selected_patient$milUnitNames, "<br>",
            "<strong>Unit Types:</strong> ", selected_patient$milUnitType, "<br>",
            "<strong>Areas:</strong> ", selected_patient$milAreaNames, "<br>",
            "<strong>Pay Grade:</strong> ", selected_patient$payGrade, "<br>",
            "</div>"
          )
        ),
        icon = icon("flag"),
        color = "blue"
      )
    })

    # Military timeline ----
    output$milTimeline <- renderHighchart({
      req(selected_patient())
      
      selected_patient <- mil_timeline_dat %>% filter(id == selected_patient())

      hc_vistime(
        data = selected_patient,
        col.start = "startDate",
        col.end = "endDate",
        col.event = "phase",
        col.group = "id",
        optimize_y = T
      ) %>%
        hc_size(height = 150)
    })

    # Concerns and stressors ----
    # Reactive expression for concerns
    concerns <- reactive({
      req(selected_patient())
      concern_dat %>%
        filter(id == selected_patient()) %>%
        arrange(concernNumber)
    }) %>% bindCache(selected_patient())

    output$concern1 <- renderInfoBox({
      req(concerns())
      concern <- concerns()[1, ]
      infoBox(
        title = HTML(
          "<div style='text-align:left; font-size:14px; font-weight:600;'>",
          paste("Concern", concern$concernNumber),
          "</div>"
        ),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            concern$concernValues,
            "</div>"
          )
        ),
        icon = icon("pencil"),
        color = "purple",
        width = 12
      )
    })

    output$concern2 <- renderInfoBox({
      req(concerns())
      if (nrow(concerns()) >= 2) {
        concern <- concerns()[2, ]
        infoBox(
          title = HTML(
            "<div style='text-align:left; font-size:14px; font-weight:600;'>",
            paste("Concern", concern$concernNumber),
            "</div>"
          ),
          value = HTML(
            paste0(
              "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
              concern$concernValues,
              "</div>"
            )
          ),
          icon = icon("pencil"),
          color = "olive",
          width = 12
        )
      } else {
        infoBox(
          title = "Concern 2",
          value = "No second concern listed",
          icon = icon("pencil"),
          color = "olive",
          width = 12
        )
      }
    })

    output$concern3 <- renderInfoBox({
      req(concerns())
      if (nrow(concerns()) >= 3) {
        concern <- concerns()[3, ]
        infoBox(
          title = HTML(
            "<div style='text-align:left; font-size:14px; font-weight:600;'>",
            paste("Concern", concern$concernNumber),
            "</div>"
          ),
          value = HTML(
            paste0(
              "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
              concern$concernValues,
              "</div>"
            )
          ),
          icon = icon("pencil"),
          color = "maroon",
          width = 12
        )
      } else {
        infoBox(
          title = "Concern 3",
          value = "No third concern listed",
          icon = icon("pencil"),
          color = "maroon",
          width = 12
        )
      }
    })
    
    output$stressors <- renderInfoBox({
      req(concerns())
      if (nrow(concerns()) >= 4) {
        concern <- concerns()[4, ]
        infoBox(
          title = HTML(
            "<div style='text-align:left; font-size:14px; font-weight:600;'>",
            paste("Current Stressors"),
            "</div>"
          ),
          value = HTML(
            paste0(
              "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
              concern$concernValues,
              "</div>"
            )
          ),
          icon = icon("pencil"),
          color = "maroon",
          width = 12
        )
      } else {
        infoBox(
          title = "Current Stressors",
          value = "No stressors listed",
          icon = icon("pencil"),
          color = "maroon",
          width = 12
        )
      }
    })

    # Medications and substances ----
    ## Medications ----
    output$medications <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- medications_dat %>% filter(id == selected_patient())

      if (all(is.na(selected_patient[, grepl("^med_name|^med_dose", names(selected_patient))]))) {
        infoBox(
          title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Medications</div>"),
          value = HTML("<div style='text-align:left; font-size:13px; font-weight:normal;'>No data to display</div>"),
          icon = icon("capsules"),
          color = "aqua",
          width = 12
        )
      } else {
        infoBox(
          title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Medications</div>"),
          value = HTML(
            paste0(
              "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
              if (!is.na(selected_patient$med_name1)) {
                paste0(
                  "<strong>", selected_patient$med_name1, ": </strong> ",
                  selected_patient$med_dose1, "<br>"
                )
              },
              if (!is.na(selected_patient$med_name2)) {
                paste0(
                  "<strong>", selected_patient$med_name2, ": </strong> ",
                  selected_patient$med_dose2, "<br>"
                )
              },
              if (!is.na(selected_patient$med_name3)) {
                paste0(
                  "<strong>", selected_patient$med_name3, ": </strong> ",
                  selected_patient$med_dose3, "<br>"
                )
              },
              if (!is.na(selected_patient$med_name4)) {
                paste0(
                  "<strong>", selected_patient$med_name4, ": </strong> ",
                  selected_patient$med_dose4, "<br>"
                )
              },
              if (!is.na(selected_patient$med_name5)) {
                paste0(
                  "<strong>", selected_patient$med_name5, ": </strong> ",
                  selected_patient$med_dose5, "<br>"
                )
              },
              "</div>"
            )
          ),
          icon = icon("capsules"),
          color = "aqua",
          width = 12
        )
      }
    })

    ## Tobacco ----
    output$tobacco <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- tobacco_dat %>% filter(id == selected_patient())

      if (is.na(selected_patient$smoker)) {
        infoBox(
          title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Tobacco</div>"),
          value = HTML("<div style='text-align:left; font-size:13px; font-weight:normal;'>No tobacco data available</div>"),
          icon = icon("smoking"),
          color = "purple",
          width = 12
        )
      } else if (selected_patient$smoker == "No") {
        infoBox(
          title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Tobacco</div>"),
          value = HTML("<div style='text-align:left; font-size:13px; font-weight:normal;'>Patient denied any history of tobacco use.</div>"),
          icon = icon("smoking"),
          color = "purple",
          width = 12
        )
      } else {
        infoBox(
          title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Tobacco</div>"),
          value = HTML(
            paste0(
              "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
              if (!is.na(selected_patient$current_smoker)) {
                paste0(
                  "<strong>Currently:</strong> ",
                  selected_patient$current_smoker, "<br>"
                )
              },
              if (!is.na(selected_patient$age_start)) {
                paste0(
                  "<strong>Started:</strong> ",
                  selected_patient$age_start, " | "
                )
              },
              if (!is.na(selected_patient$age_end)) {
                paste0(
                  "<strong>Ended:</strong> ",
                  selected_patient$age_end, "<br>"
                )
              },
              if (!is.na(selected_patient$reason_quit) | !is.na(selected_patient$reason_quit_desc)) {
                paste0(
                  "<strong>Reason for quitting:</strong> ",
                  selected_patient$reason_quit, " ",
                  selected_patient$reason_quit_desc, "<br>"
                )
              },
              if (!is.na(selected_patient$cigarettes)) {
                paste0(
                  "<strong>Cigarettes:</strong> ",
                  selected_patient$cigarettes, "<br>"
                )
              },
              if (!is.na(selected_patient$cigars)) {
                paste0(
                  "<strong>Cigars:</strong> ",
                  selected_patient$cigars, "<br>"
                )
              },
              if (!is.na(selected_patient$pipe_tobacco)) {
                paste0(
                  "<strong>Pipe Tobacco:</strong> ",
                  selected_patient$pipe_tobacco, "<br>"
                )
              },
              "</div>"
            )
          ),
          icon = icon("smoking"),
          color = "purple",
          width = 12
        )
      }
    })

    ## Substances ----
    output$substances <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- substances_dat %>% filter(id == selected_patient())

      substance_use <- selected_patient %>%
        select(-id) %>%
        pivot_longer(cols = everything(), names_to = "substance", values_to = "use_status") %>%
        filter(use_status != "Never" & !is.na(use_status)) %>%
        mutate(substance = str_replace_all(substance, "_use", "")) %>%
        mutate(substance = str_replace_all(substance, "_", " ") %>% str_to_title())

      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Substance Use</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
          paste0(
            "<strong>", substance_use$substance, ":</strong> ", substance_use$use_status, "<br>",
            collapse = ""
          ),
          if (!is.na(selected_patient$other_substance_desc)) {
            paste0("<br><strong>Other Substances:</strong> ", selected_patient$other_substance_desc)
          },
          "</div>"
        )),
        icon = icon("capsules"),
        color = "orange",
        width = 12
      )
    })

    # PTSD screening result ----
    output$ptsd_screening_result <- renderUI({
      req(selected_patient())
      
      selected_patient <- ptsd_screener_dat %>% filter(id == selected_patient())
      result <- selected_patient$ptsd_assessment

      div(
        style = "text-align:left; font-size:16px; font-weight:600; margin-bottom:10px;",
        if (result == "Data not available") {
          tags$span("Not enough PTSD data available", style = "color:gray;")
        } else if (result == "Positive for PTSD") {
          tags$span("Positive PTSD", style = "color:green;")
        } else {
          tags$span("Negative PTSD", style = "color:gray;")
        }
      )
    })

    ## Nightmares ----
    output$nightmares <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- ptsd_screener_dat %>% filter(id == selected_patient())

      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Nightmares</div>"),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            if (!is.na(selected_patient$nightmares)) {
              ifelse(selected_patient$nightmares, "Yes", "No")
            } else {
              "Data Not Available"
            },
            "</div>"
          )
        ),
        icon = icon("moon"),
        color = "purple",
        width = 3
      )
    })

    ## Avoidance ----
    output$avoidance <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- ptsd_screener_dat %>% filter(id == selected_patient())

      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Avoidance</div>"),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            if (!is.na(selected_patient$avoidance)) {
              ifelse(selected_patient$avoidance, "Yes", "No")
            } else {
              "Data Not Available"
            },
            "</div>"
          )
        ),
        icon = icon("ban"),
        color = "yellow",
        width = 3
      )
    })

    ## Hypervigilance ----
    output$hypervigilance <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- ptsd_screener_dat %>% filter(id == selected_patient())

      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Hypervigilance</div>"),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            if (!is.na(selected_patient$hypervigilance)) {
              ifelse(selected_patient$hypervigilance, "Yes", "No")
            } else {
              "Data Not Available"
            },
            "</div>"
          )
        ),
        icon = icon("eye"),
        color = "orange",
        width = 3
      )
    })

    ## Detachment ----
    output$detachment <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- ptsd_screener_dat %>% filter(id == selected_patient())

      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Detachment</div>"),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            if (!is.na(selected_patient$detachment)) {
              ifelse(selected_patient$detachment, "Yes", "No")
            } else {
              "Data Not Available"
            },
            "</div>"
          )
        ),
        icon = icon("unlink"),
        color = "blue",
        width = 3
      )
    })

    # TBI screening result ----
    output$tbi_screening_result <- renderUI({
      req(selected_patient())
      
      selected_patient <- tbi_screener_dat %>% filter(id == selected_patient())
      result <- selected_patient$tbi_screen_result

      div(
        style = "text-align:left; font-size:16px; font-weight:600; margin-bottom:10px;",
        if (result == "Data not available") {
          tags$span("Not enough TBI data available", style = "color:gray;")
        } else if (result == "Positive screen for TBI - Further evaluation recommended") {
          tags$span("Positive TBI", style = "color:green;")
        } else {
          tags$span("Negative TBI", style = "color:gray;")
        }
      )
    })

    ## Injury mechanisms ----
    output$tbi_injury_mechanisms <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- tbi_screener_dat %>% filter(id == selected_patient())
      injury_mechanisms_logical <- selected_patient %>%
        select(starts_with("tbi_01"), -tbi_01f) %>%
        pivot_longer(cols = everything(), names_to = "mechanism", values_to = "endorsed") %>%
        filter(endorsed == TRUE) %>%
        mutate(mechanism = case_when(
          mechanism == "tbi_01a" ~ "Blast or explosion",
          mechanism == "tbi_01b" ~ "Vehicular accident/crash",
          mechanism == "tbi_01c" ~ "Fragment or bullet wound above shoulders",
          mechanism == "tbi_01d" ~ "Fall",
          mechanism == "tbi_01e" ~ "Blow to the head"
        )) %>%
        pull(mechanism)

      injury_mechanism_other <- if (!is.na(selected_patient$tbi_01f)) selected_patient$tbi_01f else NULL

      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Injury Mechanisms</div>"),
        value = HTML(
          paste(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            if (!is.null(injury_mechanisms_logical) || !is.null(injury_mechanism_other)) {
              paste(c(injury_mechanisms_logical, injury_mechanism_other), collapse = ", ")
            } else {
              "No data to display"
            },
            "</div>"
          )
        ),
        icon = icon("user-injured"),
        color = "orange",
        width = 12
      )
    })

    ## Consciousness alterations ----
    output$tbi_consciousness_alterations <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- tbi_screener_dat %>% filter(id == selected_patient())
      
      alteration_symptoms <- selected_patient %>%
        select(starts_with("tbi_02")) %>%
        pivot_longer(cols = everything(), names_to = "alteration", values_to = "endorsed") %>%
        filter(endorsed == TRUE) %>%
        mutate(alteration = case_when(
          alteration == "tbi_02a" ~ "Dazed or seeing stars",
          alteration == "tbi_02b" ~ "Memory loss of the event",
          alteration == "tbi_02c" ~ "Lost consciousness (<1 min)",
          alteration == "tbi_02d" ~ "Lost consciousness (1-20 mins)",
          alteration == "tbi_02e" ~ "Lost consciousness (>20 mins)"
        )) %>%
        pull(alteration)
      
      symptoms_text <- if (length(alteration_symptoms) > 0) {
        paste(alteration_symptoms, collapse = ", ")
      } else {
        "No data to display"
      }
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Cs Alterations</div>"),
        value = HTML(
          paste(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            symptoms_text,
            "</div>"
          )
        ),
        icon = icon("brain"),
        color = "purple",
        width = 12
      )
    })

    ## Current symptoms ----
    output$tbi_current_symptoms <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- tbi_screener_dat %>% filter(id == selected_patient())
      
      current_symptoms <- selected_patient %>%
        select(starts_with("tbi_03")) %>%
        pivot_longer(cols = everything(), names_to = "symptom", values_to = "endorsed") %>%
        filter(endorsed == TRUE) %>%
        mutate(symptom = case_when(
          symptom == "tbi_03a" ~ "Memory problems or lapses",
          symptom == "tbi_03b" ~ "Balance problems or dizziness",
          symptom == "tbi_03c" ~ "Sensitivity to bright light",
          symptom == "tbi_03d" ~ "Irritability",
          symptom == "tbi_03e" ~ "Headaches",
          symptom == "tbi_03f" ~ "Sleep problems"
        )) %>%
        pull(symptom)
      
      symptoms_text <- if (length(current_symptoms) > 0) {
        paste(current_symptoms, collapse = ", ")
      } else {
        "No data to display"
      }
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Current Symptoms</div>"),
        value = HTML(
          paste(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            symptoms_text,
            "</div>"
          )
        ),
        icon = icon("stethoscope"),
        color = "yellow",
        width = 12
      )
    })

    ## Additional details ----
    output$tbi_additional_details <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- tbi_screener_dat %>% filter(id == selected_patient())
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Additional Details</div>"),
        value = HTML(
          paste(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            if (!is.na(selected_patient$tbi_05_spec)) {
              selected_patient$tbi_05_spec
            } else {
              "No additional details provided"
            },
            "</div>"
          )
        ),
        icon = icon("info-circle"),
        color = "blue",
        width = 12
      )
    })
    
    # PSQI - Sleep ----
    ## Recode PSQI details ----
    psqi_details_dat <- reactive({
      psqi_dat %>%
        mutate(
          # Mutate for psqi_05a to psqi_05j
          across(
            c(psqi_05a:psqi_05j),
            ~ case_when(
              . == 0 ~ "not past month",
              . == 1 ~ "< 1x/week",
              . == 2 ~ "1-2x/week",
              . == 3 ~ "≥ 3x/week",
              TRUE ~ NA_character_
            )
          ),
          # Mutate for psqi_06
          psqi_06 = case_when(
            psqi_06 == 0 ~ "very good",
            psqi_06 == 1 ~ "fairly good",
            psqi_06 == 2 ~ "fairly bad",
            psqi_06 == 3 ~ "very bad",
            TRUE ~ NA_character_
          ),
          # Mutate for psqi_07 and psqi_08
          across(
            c(psqi_07, psqi_08),
            ~ case_when(
              . == 0 ~ "not past month",
              . == 1 ~ "< 1x/week",
              . == 2 ~ "1-2x/week",
              . == 3 ~ "≥ 3x/week",
              TRUE ~ NA_character_
            )
          ),
          # Mutate for psqi_09
          psqi_09 = case_when(
            psqi_09 == 0 ~ "no problem",
            psqi_09 == 1 ~ "only very slight",
            psqi_09 == 2 ~ "somewhat",
            psqi_09 == 3 ~ "very big problem",
            TRUE ~ NA_character_
          ),
          # Mutate for psqi_10
          psqi_10 = case_when(
            psqi_10 == 0 ~ "no",
            psqi_10 == 1 ~ "in other room",
            psqi_10 == 2 ~ "same room, not same bed",
            psqi_10 == 3 ~ "same bed",
            TRUE ~ NA_character_
          ),
          # Mutate for psqi_10a to psqi_10e
          across(
            c(psqi_10a, psqi_10b, psqi_10c, psqi_10d, psqi_10e),
            ~ case_when(
              . == 0 ~ "not past month",
              . == 1 ~ "< 1x/week",
              . == 2 ~ "1-2x/week",
              . == 3 ~ "≥ 3x/week",
              TRUE ~ NA_character_
            )
          ),
          # Mutate for _spec
          across(
            c(psqi_07, psqi_08),
            ~ if_else(!is.na(.), as.character(.), NA_character_)
          )
        )
    })
    
    ## Sleep duration, disturbance, and latency ----
    output$psqi_one <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- psqi_dat %>% filter(id == selected_patient())
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Sleep Duration & Related</div>"),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            "<strong>Time in Bed:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_diff_hours), 
                   paste0(selected_patient$psqi_diff_hours, " hours"), "missing"), "<br>",
            "<strong>Sleep Duration:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_04), 
                   paste0(selected_patient$psqi_04, " hours"), "missing"), "<br>",
            "<strong>Latency:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_02), 
                   selected_patient$psqi_02, "missing"), "<br>",
            "<strong>Disturbance:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_distb_score), 
                   selected_patient$psqi_distb_score, "missing"), "<br>",
            "</div>"
          )
        ),
        icon = icon("bed"),
        color = "teal",
        width = 4
      )
    })
    
    ## Daytime dysfunction, subjective sleep quality, and medications ----
    output$psqi_two <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- psqi_details_dat() %>% filter(id == selected_patient())
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Quality & Daytime Issues</div>"),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            "<strong>Sleep Quality:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_06), 
                   selected_patient$psqi_06, "missing"), "<br>",
            "<strong>Sleep Medicine:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_07), 
                   selected_patient$psqi_07, "missing"), "<br>",
            "<strong>Trouble Staying Awake:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_08), 
                   selected_patient$psqi_08, "missing"), "<br>",
            "<strong>Enthusiasm/Get Things Done:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_09), 
                   selected_patient$psqi_09, "missing"), "<br>",
            "</div>"
          )
        ),
        icon = icon("sun"),
        color = "navy",
        width = 4
      )
    })
    
    ## Sleep efficiency, total score, and interpretation ----
    output$psqi_three <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- psqi_dat %>% filter(id == selected_patient())
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Efficiency & Total</div>"),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            "<strong>Sleep Efficiency:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_hse), 
                   paste0(selected_patient$psqi_hse, "%"), "missing"), "<br>",
            "<strong>Total Score:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_total), 
                   selected_patient$psqi_total, "missing"), "<br>",
            "<strong>Interpretation:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_quality), 
                   selected_patient$psqi_quality, "missing"), "<br>",
            "</div>"
          )
        ),
        icon = icon("tachometer-alt"),
        color = "aqua",
        width = 4
      )
    })
    
    ## Disturbances 1 ----
    output$psqi_details_one <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- psqi_details_dat() %>% filter(id == selected_patient())
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Disturbances</div>"),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            "<strong> > 30min to Fall Asleep:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_05a), 
                   selected_patient$psqi_05a, "missing"), "<br>",
            "<strong>Nocturnal Awakenings:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_05b), 
                   selected_patient$psqi_05b, "missing"), "<br>",
            "<strong>Bathroom Use:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_05c), 
                   selected_patient$psqi_05c, "missing"), "<br>",
            "<strong>Breathing Difficulties:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_05d), 
                   selected_patient$psqi_05d, "missing"), "<br>",
            "<strong>Snore Loudly:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_05e), 
                   selected_patient$psqi_05e, "missing"), "<br>",
            
            "</div>"
          )
        ),
        icon = icon("exclamation-triangle"),
        color = "maroon",
        width = 4
      )
    })
    
    ## Disturbances 2 ----
    output$psqi_details_two <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- psqi_details_dat() %>% filter(id == selected_patient())
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Disturbances</div>"),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            "<strong>Too Cold:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_05f), 
                   selected_patient$psqi_05f, "missing"), "<br>",
            "<strong>Too Hot:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_05g), 
                   selected_patient$psqi_05g, "missing"), "<br>",
            "<strong>Bad Dreams:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_05h), 
                   selected_patient$psqi_05h, "missing"), "<br>",
            "<strong>Pain:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_05i), 
                   selected_patient$psqi_05i, "missing"), "<br>",
            "<strong>Other:</strong> ", 
            ifelse(!is.na(selected_patient$psqi_05j), 
                   paste0(selected_patient$psqi_05j_spec, " ",
                          "(", selected_patient$psqi_05j, ")"), 
                   "missing"), "<br>",
            "</div>"
          )
        ),
        icon = icon("exclamation-triangle"),
        color = "olive",
        width = 4
      )
    })
    
    ## Partner/Roommate ----
    output$psqi_details_three <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- psqi_details_dat() %>% filter(id == selected_patient())
      
      # Handle bed partner/roommate information
      partner_info <- if (is.na(selected_patient$psqi_10)) {
        "missing"
      } else if (selected_patient$psqi_10 == "No bed partner or roommate") {
        "No bed partner/roommate"
      } else {
        paste0(
          "<strong>Bed Partner/Roommate:</strong> ", selected_patient$psqi_10, "<br>",
          "<strong>Loud Snoring:</strong> ", 
          if (!is.na(selected_patient$psqi_10a)) selected_patient$psqi_10a else "missing", "<br>",
          "<strong>Pause Breath:</strong> ", 
          if (!is.na(selected_patient$psqi_10b)) selected_patient$psqi_10b else "missing", "<br>",
          "<strong>Legs Twitching:</strong> ", 
          if (!is.na(selected_patient$psqi_10c)) selected_patient$psqi_10c else "missing", "<br>",
          "<strong>Disorientation/Confusion:</strong> ", 
          if (!is.na(selected_patient$psqi_10d)) selected_patient$psqi_10d else "missing", "<br>",
          "<strong>Other:</strong> ", 
          if (!is.na(selected_patient$psqi_10e)) {
            paste0(selected_patient$psqi_10e_spec, " (", selected_patient$psqi_10e, ")")
          } else {
            "Missing"
          }, "<br>"
        )
      }
      
      # Render the info box
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Partner/Roommate Report</div>"),
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>", partner_info, "</div>"
          )
        ),
        icon = icon("user-friends"),
        color = "orange",
        width = 4
      )
    })
    
    # Deployment Health ----
    ## Deployment Incident ----
    output$deployment_incident <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- pdhra_dat %>% filter(id == selected_patient())
      
      # # Determine incident information
      # incident_info <- if (is.na(selected_patient$pdhra_03)) {
      #   "Data not available"
      # } else if (selected_patient$pdhra_03 == 0) {
      #   "Patient denied being wounded, injured, or assaulted during deployment."
      # } else if (selected_patient$pdhra_03 == 1) {
      #   "Yes"
      # } else if (selected_patient$pdhra_03 == 2) {
      #   "Unsure"
      # } else {
      #   "Invalid data"
      # }
      
      # Render the info box
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Deployment Incident</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>", selected_patient$pdhra_03, "</div>"
        )),
        icon = icon("exclamation-circle"),
        color = "red",
        width = 4
      )
    })
    
    
    ## Related Wound or Injury ----
    output$related_injury <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- pdhra_dat %>% filter(id == selected_patient())
      
      # # Determine related injury information
      # injury_info <- if (is.na(selected_patient$pdhra_03a)) {
      #   "Data not available"
      # } else if (selected_patient$pdhra_03a == 0) {
      #   "No"
      # } else if (selected_patient$pdhra_03a == 1) {
      #   "Yes, the patient is still having problems."
      # } else if (selected_patient$pdhra_03a == 2) {
      #   "Unsure"
      # } else if (selected_patient$pdhra_03a == 3) {
      #   "Not applicable"
      # } else {
      #   "Invalid data"
      # }
      
      # Render the info box
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Related Wound or Injury</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>", selected_patient$pdhra_03a, "</div>"
        )),
        icon = icon("heartbeat"),
        color = "orange",
        width = 4
      )
    })
    
    
    ## Medical Attention and Visits ----
    output$medical_attention <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- pdhra_dat %>% filter(id == selected_patient())
      
      # Medical attention information
      med_info <- if (is.na(selected_patient$dep_med)) {
        "Data not available"
      } else if (selected_patient$dep_med == "No") {
        "No medical attention sought during deployment"
      } else if (selected_patient$dep_med == "Yes") {
        paste0("Yes - ", ifelse(!is.na(selected_patient$dep_med_spec), 
                                        selected_patient$dep_med_spec, 
                                        "No additional details provided"))
      } else {
        "Data not available"
      }
      
      # Healthcare visits post-deployment
      visits_info <- if (is.na(selected_patient$pdhra_05) | selected_patient$pdhra_05 == "missing" ) {
        "Data not available"
      } else {
        paste0("About ", selected_patient$pdhra_05, " visits post-deployment")
      }
      
      # Combine details
      combined_info <- paste0(
        "<strong>Medical Attention:</strong> ", med_info, "<br>",
        "<strong>Healthcare Visits:</strong> ", visits_info
      )
      
      print(combined_info)
      
      # Render the info box
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Medical Attention & Visits</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
          combined_info,
          "</div>"
        )),
        icon = icon("user-md"),
        color = "blue",
        width = 4
      )
    })
      
    # Family history ----
    ## Mother ----
    output$mother_info <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- family_history_dat %>% filter(id == selected_patient())
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Mother</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
          paste0("<strong>Health Score: </strong>", ifelse(is.na(selected_patient$mother_health), 
                                                           "missing", selected_patient$mother_health), "<br>"),
          paste0("<strong>Age at Death: </strong>", ifelse(is.na(selected_patient$mother_age_death), 
                                                           "missing", selected_patient$mother_age_death), "<br>"),
          paste0("<strong>Cause of Death: </strong>", ifelse(is.na(selected_patient$mother_cause_death), 
                                                             "missing", selected_patient$mother_cause_death), "<br>"),
          paste0("<strong>Conditions: </strong>", ifelse(is.na(selected_patient$mother_medical_conditions), 
                                                         "missing", selected_patient$mother_medical_conditions), "<br>"),
          "</div>"
        )),
        icon = icon("female"),
        color = "olive",
        width = 12
      )
    })

    ## Father ----
    output$father_info <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- family_history_dat %>% filter(id == selected_patient())
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Father</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
          paste0("<strong>Health Score: </strong>", ifelse(is.na(selected_patient$father_health), 
                                                           "missing", selected_patient$father_health), "<br>"),
          paste0("<strong>Age at Death: </strong>", ifelse(is.na(selected_patient$father_age_death), 
                                                           "missing", selected_patient$father_age_death), "<br>"),
          paste0("<strong>Cause of Death: </strong>", ifelse(is.na(selected_patient$father_cause_death), 
                                                             "missing", selected_patient$father_cause_death), "<br>"),
          paste0("<strong>Conditions: </strong>", ifelse(is.na(selected_patient$father_medical_conditions), 
                                                         "missing", selected_patient$father_medical_conditions), "<br>"),
          "</div>"
        )),
        icon = icon("male"),
        color = "purple",
        width = 12
      )
    })

    ## Siblings ----
    output$sibling_info <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- family_history_dat %>% filter(id == selected_patient())
      
      brother_content <- if (is.na(selected_patient$has_brothers)) {
        paste0("missing", "<br>")
      } else if (!selected_patient$has_brothers) {
        paste0("No brothers", "<br>")
      } else {
        paste0(
          if (!is.na(selected_patient$num_brothers)) {
            paste0("<strong>Brothers:</strong> ", selected_patient$num_brothers, "<br>")
          } else "Has brothers but missing info<br>",
          if (!is.na(selected_patient$brother_age_death)) {
            paste0("<strong>Brother Age at Death:</strong> ", selected_patient$brother_age_death, "<br>")
          } else "",
          if (!is.na(selected_patient$brother_cause_death)) {
            paste0("<strong>Brother Cause of Death:</strong> ", selected_patient$brother_cause_death, "<br>")
          } else "",
          if (!is.na(selected_patient$brothers_medical_conditions)) {
            paste0("<strong>Brother Conditions:</strong> ", selected_patient$brothers_medical_conditions, "<br>")
          } else ""
        )
      }
      
      sister_content <- if (is.na(selected_patient$has_sisters)) {
        paste0("missing", "<br>")
      } else if (!selected_patient$has_sisters) {
        paste0("No Sisters", "<br>")
      } else {
        paste0(
          if (!is.na(selected_patient$num_sisters)) {
            paste0("<strong>Sisters:</strong> ", selected_patient$num_sisters, "<br>")
          } else "Has sisters but missing info<br>",
          if (!is.na(selected_patient$sister_age_death)) {
            paste0("<strong>Sister Age at Death:</strong> ", selected_patient$sister_age_death, "<br>")
          } else "",
          if (!is.na(selected_patient$sister_cause_death)) {
            paste0("<strong>Sister Cause of Death:</strong> ", selected_patient$sister_cause_death, "<br>")
          } else "",
          if (!is.na(selected_patient$sisters_medical_conditions)) {
            paste0("<strong>Sister Conditions:</strong> ", selected_patient$sisters_medical_conditions, "<br>")
          } else ""
        )
      }
      
      sibling_content <- paste0(brother_content, sister_content)
      if (sibling_content == "No brothers<br>No sisters") {
        sibling_content <- "No siblings"
      }
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Siblings</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>", sibling_content, "</div>"
        )),
        icon = icon("users"),
        color = "navy",
        width = 12
      )
    })

    ## Children ----
    output$children_info <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- family_history_dat %>% filter(id == selected_patient())
      
      if (is.na(selected_patient$has_children)) {
        children_info <- "missing"
      } else if (!selected_patient$has_children) {
        children_info <- "No children"
      } else {

        children_info <- paste0(
          if (!is.na(selected_patient$num_sons)) {
            paste0("<strong>Sons:</strong> ", selected_patient$num_sons, "<br>")
          } else "Has children but missing sons info<br>",
          if (!is.na(selected_patient$num_daughters)) {
            paste0("<strong>Daughters:</strong> ", selected_patient$num_daughters, "<br>")
          } else "Has children but missing daughters info<br>",
          if (!is.na(selected_patient$children_age_death)) {
            paste0("<strong>Age at Death:</strong> ", selected_patient$children_age_death, "<br>")
          } else "",
          if (!is.na(selected_patient$children_cause_death)) {
            paste0("<strong>Cause of Death:</strong> ", selected_patient$children_cause_death, "<br>")
          } else "",
          if (!is.na(selected_patient$children_medical_conditions)) {
            paste0("<strong>Conditions:</strong> ", selected_patient$children_medical_conditions, "<br>")
          } else ""
        )
        
        if (children_info == "") {
          children_info <- "Has children but no detailed information available"
        }
      }
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Children</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>", children_info, "</div>"
        )),
        icon = icon("child"),
        color = "teal",
        width = 12
      )
    })
    
    # Military Exposures Extras ----
    ## Occupational health program ----
    output$occupationalHealthProgram <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- mil_expo_extras_dat %>% filter(id == selected_patient())
      
      occupational_health <- selected_patient %>%
        select(starts_with("expo_occh_")) %>%
        mutate(across(everything(), as.character)) %>%
        mutate(
          expo_occh_1 = ifelse(is.na(expo_occh_1), "missing", 
                               ifelse(expo_occh_1 == "TRUE", "Yes", "No")),
          expo_occh_2 = ifelse(is.na(expo_occh_2), "missing", 
                               ifelse(expo_occh_2 == "TRUE", "Yes", "No")),
          expo_occh_3 = ifelse(is.na(expo_occh_3), "missing", 
                               ifelse(expo_occh_3 == "TRUE", "Yes", "No")),
          expo_occh_oth = ifelse(is.na(expo_occh_oth), "missing", expo_occh_oth)
        )
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Occupational Health Program</div>"),
        value = HTML(
          paste0(
            "<div style='font-size:13px; font-weight:normal;'>",
            "<strong>Respiratory Protection Program:</strong> ", occupational_health$expo_occh_1, "<br>",
            "<strong>Medical Surveillance Program:</strong> ", occupational_health$expo_occh_2, "<br>",
            "<strong>Asbestos Surveillance Program:</strong> ", occupational_health$expo_occh_3, "<br>",
            "<strong>Other:</strong> ", occupational_health$expo_occh_oth,
            "</div>"
          )
        ),
        icon = icon("stethoscope"),
        color = "blue",
        width = 12
      )
    })
    
    ## Air quality ----
    output$airQuality <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- mil_expo_extras_dat %>% filter(id == selected_patient())
      
      bad_air_days <- ifelse(is.na(selected_patient$expo_visi), "missing", 
                             selected_patient$expo_visi)
      cravat_days <- ifelse(is.na(selected_patient$expo_crav), "missing", 
                            selected_patient$expo_crav)
      days_mask_worn <- ifelse(is.na(selected_patient$expo_mask), "missing", 
                               selected_patient$expo_mask)
      mask_description <- ifelse(is.na(selected_patient$expo_mask_desc), "missing", 
                                 selected_patient$expo_mask_desc)
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Air Quality</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
          "<strong>Bad Air Quality Days:</strong> ", bad_air_days, "<br>",
          "<strong>Cravat Days:</strong> ", cravat_days, "<br>",
          "<strong>Days Mask Worn:</strong> ", days_mask_worn, "<br>",
          "<strong>Description:</strong> ", mask_description,
          "</div>"
        )),
        icon = icon("head-side-mask"),
        color = "teal",
        width = 12
      )
    })
    
    ## Health conditions ----
    output$healthConditions <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- mil_expo_extras_dat %>% filter(id == selected_patient())
      
      health_conditions <- selected_patient %>%
        select(matches("^expo_(hosp|evac|temp|limi|mos|moos)_(desc|date)$")) %>%
        mutate(across(ends_with("_date"), as.character)) %>% # Convert dates to character
        pivot_longer(
          cols = everything(), 
          names_to = c("condition", "type"), 
          names_pattern = "expo_(hosp|evac|temp|limi|mos|moos)_(desc|date)"
        ) %>%
        pivot_wider(names_from = "type", values_from = "value") %>%
        filter(!is.na(desc)) %>%
        mutate(
          condition = case_when(
            condition == "hosp" ~ "Hospitalization",
            condition == "evac" ~ "Evacuation",
            condition == "temp" ~ "Temporary Profile",
            condition == "limi" ~ "Limited Duties",
            condition == "mos" ~ "Change of MOS",
            condition == "moos" ~ "Medically Boarded Out",
            TRUE ~ str_to_title(str_replace_all(condition, "_", " "))
          )
        )
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Health Conditions</div>"),
        value = HTML(if (nrow(health_conditions) == 0) {
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>No data to display</div>"
        } else {
          paste0(
            "<div style='text-align:left; font-size:13px; font-weight:normal;'>",
            paste0(
              "<strong>", health_conditions$condition, ":</strong> ", health_conditions$desc,
              ifelse(!is.na(health_conditions$date), paste0(" (", health_conditions$date, ")"), ""), "<br>",
              collapse = ""
            ),
            "</div>"
          )
        }),
        icon = icon("clinic-medical"),
        color = "red",
        width = 12
      )
    })
    
    # Civilian Exposures Extras ----
    ## Other exposure description ----
    output$other_civilian_exposures <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- civil_expo_extras_dat %>% filter(id == selected_patient()) # possible issue with othc and othc_spec 
      
      if (nrow(selected_patient) == 0 || is.na(selected_patient$expo_othc)) {
        exposures_info <- "No data to display"
      } else if (selected_patient$expo_othc == "No") {
        exposures_info <- "Patient denied other civilian exposures"
      } else if (selected_patient$expo_othc == "Yes") {
        exposures_info <- paste0(
          if (!is.na(selected_patient$expo_othc_spec)) {
            paste0("<strong>Description:</strong> ", selected_patient$expo_othc_spec, "<br>")
          } else "Description: missing<br>",
          if (!is.na(selected_patient$expo_othc_f)) {
            paste0("<strong>Frequency:</strong> ", selected_patient$expo_othc_f, "<br>")
          } else "Frequency: missing<br>",
          if (!is.na(selected_patient$expo_othc_i)) {
            paste0("<strong>Intensity:</strong> ", selected_patient$expo_othc_i, "<br>")
          } else "Intensity: missing<br>",
          if (!is.na(selected_patient$expo_othc_c)) {
            paste0("<strong>Concern:</strong> ", selected_patient$expo_othc_c, "<br>")
          } else "Concern: missing<br>"
        )
      } else if (selected_patient$expo_othc == "Don't Know") {
        exposures_info <- "Patient is unsure about other civilian exposures"
      } else {
        exposures_info <- "Unknown data state"
      }
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Other Exposures</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>", 
          exposures_info, "</div>"
        )),
        icon = icon("industry"),
        color = "orange",
        width = 12
      )
    })
    
    ## Respiratory protection ----
    output$civilian_respiratory_protection <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- civil_expo_extras_dat %>% filter(id == selected_patient())
      
      if (nrow(selected_patient) == 0 || is.na(selected_patient$expo_resp)) {
        respiratory_info <- "No data to display"
      } else if (selected_patient$expo_resp == FALSE) {
        respiratory_info <- "Patient denied wearing respiratory protection for civilian jobs"
      } else if (selected_patient$expo_resp == TRUE) {
        respiratory_info <- paste0(
          if (!is.na(selected_patient$expo_resp_spec)) {
            paste0("<strong>Description:</strong> ", selected_patient$expo_resp_spec, "<br>")
          } else "Description: missing<br>"
        )
      } else {
        respiratory_info <- "Unknown data state"
      }
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Respiratory Use</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>", 
          respiratory_info, "</div>"
        )),
        icon = icon("head-side-mask"),
        color = "teal",
        width = 12
      )
    })
    
    ## Medical evaluation ----
    output$civilian_medical_treatment <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- civil_expo_extras_dat %>% filter(id == selected_patient())
      
      if (nrow(selected_patient) == 0 || is.na(selected_patient$expo_medi)) {
        medical_info <- "No data to display"
      } else if (selected_patient$expo_medi == FALSE) {
        medical_info <- "Patient denied requiring medical treatment or evaluation for civilian exposures"
      } else if (selected_patient$expo_medi == TRUE) {
        medical_info <- paste0(
          if (!is.na(selected_patient$expo_medi_spec)) {
            paste0("<strong>Description:</strong> ", selected_patient$expo_medi_spec, "<br>")
          } else "Description: missing<br>"
        )
      } else {
        medical_info <- "Unknown data state"
      }
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Medical Services</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>", medical_info, "</div>"
        )),
        icon = icon("notes-medical"),
        color = "blue",
        width = 12
      )
    })
    
    ## Work restriction ----
    output$civilian_work_restriction <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- civil_expo_extras_dat %>% filter(id == selected_patient())
      
      if (nrow(selected_patient) == 0 || is.na(selected_patient$expo_work)) {
        work_info <- "No data to display"
      } else if (selected_patient$expo_work == FALSE) {
        work_info <- "Patient denied work restriction or disability due to civilian exposures"
      } else if (selected_patient$expo_work == TRUE) {
        work_info <- paste0(
          if (!is.na(selected_patient$expo_work_spec)) {
            paste0("<strong>Description:</strong> ", selected_patient$expo_work_spec, "<br>")
          } else "Description: missing<br>"
        )
      } else {
        work_info <- "Unknown data state"
      }
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Work Restriction</div>"),
        value = HTML(paste0(
          "<div style='text-align:left; font-size:13px; font-weight:normal;'>", work_info, "</div>"
        )),
        icon = icon("briefcase-medical"),
        color = "red",
        width = 12
      )
    })
    
    # Referral source ----
    output$referral_info <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- wriisc_reason_dat %>% filter(id == selected_patient())
      
      referral_info <- c(
        if (!is.na(selected_patient$va_provider) && selected_patient$va_provider == "Yes") "VA Provider",
        if (!is.na(selected_patient$non_va_provider) && selected_patient$non_va_provider == "Yes") "Non-VA Provider",
        if (!is.na(selected_patient$veteran_peer) && selected_patient$veteran_peer == "Yes") "Fellow Veteran",
        if (!is.na(selected_patient$vet_center) && selected_patient$vet_center == "Yes") "Vet Center",
        if (!is.na(selected_patient$oef_oif_coordinator) && selected_patient$oef_oif_coordinator == "Yes") "OEF/OIF/OND Coordinator",
        if (!is.na(selected_patient$veteran_org) && selected_patient$veteran_org == "Yes") "Veteran Organization",
        if (!is.na(selected_patient$military_contact) && selected_patient$military_contact == "Yes") "Military Contact",
        if (!is.na(selected_patient$flyer_specified)) selected_patient$flyer_specified,
        if (!is.na(selected_patient$website_specified)) selected_patient$website_specified,
        if (!is.na(selected_patient$other_specified)) selected_patient$other_specified
      ) %>%
        na.omit() %>%
        paste(collapse = ", ")
      
      if (referral_info == "") {
        referral_info <- "No data to display"
      }
      
      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Heard about from</div>"),
        value = HTML(paste0("<div style='text-align:left; font-size:13px; font-weight:normal;'>", referral_info, "</div>")),
        icon = icon("bullhorn"),
        color = "aqua",
        width = 12
      )
    })

    # Reasons for WRIISC ----
    output$symptom_reason_info <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- wriisc_reason_dat %>% filter(id == selected_patient())

      symptom_reasons <- paste0(
        if (!is.na(selected_patient$understand_symptoms_cause)) {
          paste0("<strong>Sx Insight</strong>: ", selected_patient$understand_symptoms_cause, "<br>")
        } else {
          ""
        },
        if (!is.na(selected_patient$reduce_symptoms)) {
          paste0("<strong>Reduce Sx</strong>: ", selected_patient$reduce_symptoms, "<br>")
        } else {
          ""
        },
        if (!is.na(selected_patient$understand_exposure_effects)) {
          paste0("<strong>Exposure Effects</strong>: ", selected_patient$understand_exposure_effects, "<br>")
        } else {
          ""
        },
        if (!is.na(selected_patient$complete_exam)) {
          paste0("<strong>Complete Exam</strong>: ", selected_patient$complete_exam, "<br>")
        } else {
          ""
        },
        if (!is.na(selected_patient$mental_health_eval)) {
          paste0("<strong>MH Eval</strong>: ", selected_patient$mental_health_eval, "<br>")
        } else {
          ""
        },
        if (!is.na(selected_patient$help_others)) {
          paste0("<strong>Help Others</strong>: ", selected_patient$help_others, "<br>")
        } else {
          ""
        }
      )
      
      if (symptom_reasons == "") {
        symptom_reasons <- "No data to display"
      }

      infoBox(
        title = HTML("<div style='text-align:left; font-size:14px; font-weight:600;'>Reasons for Visit</div>"),
        value = HTML(
          paste0("<div style='text-align:left; font-size:13px; font-weight:normal;'>", symptom_reasons, "</div>")
        ),
        icon = icon("stethoscope"),
        color = "yellow",
        width = 12
      )
    })

    # Information sources ----
    output$info_source_info <- renderInfoBox({
      req(selected_patient())
      
      selected_patient <- wriisc_reason_dat %>% filter(id == selected_patient())
      
      info_sources <- c(
        if (!is.na(selected_patient$newspaper_magazine)) paste0("<strong>Newspaper/Mag</strong>: ", selected_patient$newspaper_magazine),
        if (!is.na(selected_patient$friends_family)) paste0("<strong>Friends/Family</strong>: ", selected_patient$friends_family),
        if (!is.na(selected_patient$va_dod_health)) paste0("<strong>VA/DOD Health</strong>: ", selected_patient$va_dod_health),
        if (!is.na(selected_patient$non_va_health)) paste0("<strong>Non-VA Health</strong>: ", selected_patient$non_va_health),
        if (!is.na(selected_patient$television)) paste0("<strong>Television</strong>: ", selected_patient$television),
        if (!is.na(selected_patient$websites)) paste0("<strong>Websites</strong>: ", selected_patient$websites),
        if (!is.na(selected_patient$support_groups)) paste0("<strong>Support Groups</strong>: ", selected_patient$support_groups),
        if (!is.na(selected_patient$social_media)) paste0("<strong>Social Media</strong>: ", selected_patient$social_media),
        if (!is.na(selected_patient$other_info_source)) paste0("<strong>Other Info Source</strong>: ", selected_patient$other_info_source),
        if (!is.na(selected_patient$other_info_specified)) paste0(" (", selected_patient$other_info_specified, ")")
      ) %>% na.omit()
      
      if (length(info_sources) == 0) {
        infoBox(
          title = HTML("<span style='font-size:14px; font-weight:600;'>Health Information Sources</span>"),
          value = HTML("<div style='font-size:13px; font-weight:normal;'>No data to display</div>"),
          icon = icon("info-circle"),
          color = "navy",
          width = 12
        )
      } else {
        column1 <- paste(info_sources[seq(1, length(info_sources), by = 2)], collapse = "<br>")
        column2 <- paste(info_sources[seq(2, length(info_sources), by = 2)], collapse = "<br>")
        
        infoBox(
          title = HTML("<span style='font-size:14px; font-weight:600;'>Health Information Sources</span>"),
          value = HTML(
            paste0(
              "<div style='font-size:13px; font-weight:normal; word-wrap: break-word; white-space: normal; display: flex; gap: 15px;'>",
              "<div style='flex: 1;'>", column1, "</div>",
              "<div style='flex: 1;'>", column2, "</div>",
              "</div>"
            )
          ),
          icon = icon("info-circle"),
          color = "aqua",
          width = 12
        )
      }
    })
  })
}
