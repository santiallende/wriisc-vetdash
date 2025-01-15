# # Load Libraries ----
# library(tidyverse)
# 
# # Load Data ----
# intakeDat <- read_csv("TRIWRIISCINTAKEPACKET CA 4A CONSENTED.csv")
# 
# # De-identify Data / Light Cleaning ----
# intakeDat <- intakeDat %>%
#   mutate(id = 1:nrow(.)) %>%
#   select(id, 4:872) %>%
#   mutate(dem_dob = dmy_hms(dem_dob)) %>%
#   mutate(dem_datecom = dmy_hms(dem_datecom)) %>%
#   mutate(age = round(time_length(interval(dem_dob, dem_datecom), "year")), 0) %>%
#   select(id, age, everything())
# 
# # Date Packet Completed ----
# datePacketCompletedDat <- intakeDat %>%
#   select(id, datePacketCompleted = dem_datecom) %>%
#   mutate(datePacketCompleted = ymd(datePacketCompleted))
# 
# write_csv(datePacketCompletedDat, "datePacketCompletedDat.csv")
# 
# # Code Military Conflict ----
# milConflictDat <- intakeDat %>%
#   select(id, mil_dep1_conflict, mil_dep2_conflict,
#          mil_dep3_conflict, mil_dep4_conflict,
#          mil_dep5_conflict) %>%
#   pivot_longer(2:6, names_to = "milConflictDeployment",
#                values_to = "milConflictCode") %>%
#   mutate(milConflictCode = na_if(milConflictCode, 999)) %>%
#   mutate(milConflictCode = case_when(
#     milConflictCode == 1 ~ "WWII",
#     milConflictCode == 2 ~ "Korea",
#     milConflictCode == 3 ~ "Vietnam",
#     milConflictCode == 4 ~ "Lebanon",
#     milConflictCode == 5 ~ "Panama",
#     milConflictCode == 6 ~ "Grenada",
#     milConflictCode == 7 ~ "ODSS",
#     milConflictCode == 8 ~ "Kosovo",
#     milConflictCode == 9 ~ "Bosnia",
#     milConflictCode == 10 ~ "Croatia",
#     milConflictCode == 11 ~ "Somalia",
#     milConflictCode == 12 ~ "OEF",
#     milConflictCode == 13 ~ "OIF",
#     milConflictCode == 14 ~ "OND",
#     milConflictCode == 15 ~ "Other"
#   )) %>%
#   select(id, milConflictCode) %>%
#   filter(!is.na(milConflictCode)) %>%
#   distinct()
# 
# write_csv(milConflictDat, "milConflictDat.csv")
# 
# # Demographic Data ----
# ## Age and gender ----
# ageGenderDat <- intakeDat %>%
#   select(id, age, gender = Gender) %>%
#   mutate(gender = ifelse(gender == "M", "Male", "Female")) %>%
#   filter(age < 105 & age > 15) %>%
#   right_join(intakeDat %>% select(id), "id")
# 
# ## Race ----
# raceDat <- intakeDat %>% select(id, starts_with("dem_race")) %>%
#   pivot_longer(2:7, names_to = "race", values_to = "binaryYesNo") %>%
#   mutate(binaryYesNo = ifelse(binaryYesNo == 0, "No", "Yes")) %>%
#   mutate(race = case_when(
#     race == "dem_race01" ~ "American Indian or Alaskan Native",
#     race == "dem_race02" ~ "Asian",
#     race == "dem_race03" ~ "Black or African-American",
#     race == "dem_race04" ~ "Native Hawaiian or Pacific Islander",
#     race == "dem_race05" ~ "White",
#     race == "dem_race06" ~ "Unknown",
#     race == "dem_race07" ~ "Other"
#   )) %>%
#   filter(binaryYesNo == "Yes") %>%
#   mutate(dem_raceoth = ifelse(dem_raceoth == 999, NA, dem_raceoth)) %>%
#   mutate(race = ifelse(!is.na(dem_raceoth), dem_raceoth, race)) %>%
#   group_by(id) %>%
#   summarise(race = paste(unique(race), collapse = ", ")) %>%
#   ungroup() %>%
#   right_join(intakeDat %>% select(id), "id")
# 
# ## Ethnicity ----
# ethnicityDat <- intakeDat %>%
#   select(id, ethnicity = dem_ethnic) %>%
#   mutate(ethnicity = case_when(
#     ethnicity == 0 ~ "Non Hispanic or Non Latino",
#     ethnicity == 1 ~ "Hispanic or Latino",
#     ethnicity == 2 ~ "Unknown"
#   ))
# 
# #full_join(raceDat, ethnicityDat) %>% arrange(id)
# 
# ## Education ----
# educationDat <- intakeDat %>% select(id, starts_with("dem_educ")) %>%
#   pivot_longer(4:12, names_to = "eduDegrees", values_to = "binaryYesNo") %>%
#   mutate(eduDegrees = case_when(
#     eduDegrees == "dem_educ01" ~ "High School Diploma/GED",
#     eduDegrees == "dem_educ02" ~ "Technical/Trade School",
#     eduDegrees == "dem_educ03" ~ "Associate Degree",
#     eduDegrees == "dem_educ04" ~ "Bachelor Degree",
#     eduDegrees == "dem_educ05" ~ "Master’s Degree",
#     eduDegrees == "dem_educ06" ~ "PhD/Doctorate Degree",
#     eduDegrees == "dem_educ07" ~ "MD",
#     eduDegrees == "dem_educ08" ~ "JD",
#     eduDegrees == "dem_educ09" ~ NA
#   )) %>%
#   mutate(dem_educ_oth = ifelse(dem_educ_oth == 999, NA, dem_educ_oth)) %>%
#   mutate(eduDegrees = ifelse(!is.na(dem_educ_oth) & is.na(eduDegrees) & binaryYesNo == TRUE,
#                              dem_educ_oth, eduDegrees)) %>%
#   mutate(dem_educ = case_when(
#     dem_educ == 0 ~ "High School Diploma/GED",
#     dem_educ == 1 ~ "Technical/Trade School",
#     dem_educ == 2 ~ "Associate Degree",
#     dem_educ == 3 ~ "Bachelor Degree",
#     dem_educ == 4 ~ "Master’s Degree",
#     dem_educ == 5 ~ "PhD/Doctorate Degree",
#     dem_educ == 6 ~ "MD",
#     dem_educ == 7 ~ "JD"
#   )) %>%
#   mutate(binaryYesNo = ifelse(is.na(binaryYesNo) & !is.na(dem_educ), dem_educ, binaryYesNo)) %>%
#   mutate(binaryYesNo = ifelse(binaryYesNo == "TRUE", eduDegrees, binaryYesNo)) %>%
#   group_by(id) %>%
#   mutate(allFalse = all(binaryYesNo == "FALSE")) %>%
#   mutate(binaryYesNo = ifelse(allFalse, NA, binaryYesNo)) %>%
#   ungroup() %>% # Check if IDs had all FALSE (run below code, then continue running this pipe)
#   filter(binaryYesNo != "FALSE") %>%
#   group_by(id) %>%
#   summarise(binaryYesNo = paste(unique(binaryYesNo), collapse = ", "), .groups = 'drop') %>%
#   right_join(intakeDat %>% select(id), "id") %>%
#   replace_na(list(binaryYesNo = NA)) %>%
#   rename(degrees = binaryYesNo)
# 
# # Checking which IDs had all FALSE values and were replaced with NA
# # ids_with_all_false <- educationDat %>%
# #   filter(allFalse) %>%
# #   select(id) %>%
# #   distinct()
# 
# ## Language ----
# languageDat <- intakeDat %>% select(id, dem_lang1, dem_lang2) %>%
#   pivot_longer(2:3, names_to = "languageNum", values_to = "languages") %>%
#   filter(languages != "999") %>%
#   arrange(id, languageNum) %>%
#   group_by(id) %>%
#   summarise(languages = paste(unique(languages), collapse = ", "),
#             .groups = 'drop') %>%
#   right_join(intakeDat %>% select(id), "id")
# 
# ## Marital ----
# maritalDat <- intakeDat %>%
#   select(id, maritalStatus = dem_marital, timesMarried = dem_mar_times,
#          yearsWithCurrent = dem_mar_length, partnersHealth = dem_mar_health) %>%
#   mutate(across(everything(), ~na_if(., 999))) %>%
#   mutate(maritalStatus = case_when(
#     maritalStatus == 0 ~ "Never Married",
#     maritalStatus == 1 ~ "Married",
#     maritalStatus == 2 ~ "Divorced",
#     maritalStatus == 3 ~ "Widowed",
#     maritalStatus == 4 ~ "Separated",
#     maritalStatus == 5 ~ "Living with a Partner"
#   )) %>%
#   mutate(partnersHealth = case_when(
#     partnersHealth == 0 ~ "Poor",
#     partnersHealth == 1 ~ "Fair",
#     partnersHealth == 2 ~ "Good",
#     partnersHealth == 3 ~ "Very Good",
#     partnersHealth == 4 ~ "Excellent"
#   ))
# 
# ## Employment ----
# employmentDat <- intakeDat %>%
#   select(id, starts_with("dem_emp")) %>%
#   mutate(across(starts_with("dem_emp"), as.character)) %>%
#   pivot_longer(cols = starts_with("dem_emp"), names_to = "employmentVar", values_to = "employmentValue") %>%
#   mutate(employmentVar = case_when(
#     employmentVar == "dem_emp01" ~ "Employed Full-Time",
#     employmentVar == "dem_emp02" ~ "Employed Part-Time",
#     employmentVar == "dem_emp03" ~ "Unemployed",
#     employmentVar == "dem_emp04" ~ "Student",
#     employmentVar == "dem_emp05" ~ "Homemaker",
#     employmentVar == "dem_emp06" ~ "Retired",
#     employmentVar == "dem_emp07" ~ "Applying for Disability Benefits",
#     employmentVar == "dem_emp08" ~ "Receiving Disability Benefits",
#     employmentVar == "dem_emp09" ~ "Other",
#     employmentVar == "dem_emp_oth" ~ "Other (specified)",
#     TRUE ~ employmentVar
#   )) %>%
#   mutate(employmentValue = na_if(employmentValue, "999")) %>%
#   mutate(employmentValue = ifelse(employmentValue == 0, "No", "Yes")) %>%
#   filter(!is.na(employmentValue)) %>%
#   filter(employmentValue == "Yes") %>%
#   group_by(id) %>%
#   summarise(employmentVar = paste(unique(employmentVar), collapse = ", "), .groups = 'drop') %>%
#   right_join(intakeDat %>% select(id), "id") %>%
#   rename(employment = employmentVar)
# 
# ## Handedness ----
# handednessDat <- intakeDat %>% select(id, handedness = dem_handed) %>%
#   mutate(handedness = case_when(
#     handedness == 0 ~ "Right",
#     handedness == 1 ~ "Left",
#     handedness == 2 ~ "Both/Ambidextrous",
#     handedness == 3 ~ "Don't Know"
#   ))
# 
# ## Combine All ----
# fullDemoDat <- full_join(ageGenderDat, educationDat, "id") %>%
#   full_join(., employmentDat, "id") %>% full_join(., ethnicityDat, "id") %>%
#   full_join(., handednessDat, "id") %>% full_join(., languageDat, "id") %>%
#   full_join(., maritalDat, "id") %>% full_join(., raceDat, "id")
# 
# write_csv(fullDemoDat, "fullDemoDat.csv")
# 
# # Concerns Data ----
# concernDat <- intakeDat %>% select(id, concern1, concern2, concern3, phq_14) %>%
#   pivot_longer(2:5, names_to = "concernNumber", values_to = "concernValues") %>%
#   mutate(concernNumber = case_when(
#     concernNumber == "concern1" ~ 1,
#     concernNumber == "concern2" ~ 2,
#     concernNumber == "concern3" ~ 3,
#     concernNumber == "phq_14" ~ 4
#   ),
#   concernValues = replace(concernValues,
#                           concernValues %in% c("-99", "999", "na"), NA) %>%
#     str_remove_all('^"+|"+$') %>%
#     if_else(nchar(.) == 1, NA_character_, .)
#   )
# 
# write_csv(concernDat, "concernsDat.csv")
# 
# # Military Information Data ----
# # Create mappings
# branch_names <- c(
#   "mil_branch01_start" = "Start Date: Army",
#   "mil_branch01_end" = "End Date: Army",
#   "mil_branch02_start" = "Start Date: Army Reserve",
#   "mil_branch02_end" = "End Date: Army Reserve",
#   "mil_branch03_start" = "Start Date: Army National Guard",
#   "mil_branch03_end" = "End Date: Army National Guard",
#   "mil_branch04_start" = "Start Date: Navy",
#   "mil_branch04_end" = "End Date: Navy",
#   "mil_branch05_start" = "Start Date: Navy Reserve",
#   "mil_branch05_end" = "End Date: Navy Reserve",
#   "mil_branch06_start" = "Start Date: Coast Guard",
#   "mil_branch06_end" = "End Date: Coast Guard",
#   "mil_branch07_start" = "Start Date: Public Health",
#   "mil_branch07_end" = "End Date: Public Health",
#   "mil_branch08_start" = "Start Date: Air Force",
#   "mil_branch08_end" = "End Date: Air Force",
#   "mil_branch09_start" = "Start Date: Air Force Reserve",
#   "mil_branch09_end" = "End Date: Air Force Reserve",
#   "mil_branch10_start" = "Start Date: Air National Guard",
#   "mil_branch10_end" = "End Date: Air National Guard",
#   "mil_branch11_start" = "Start Date: Marine Corps",
#   "mil_branch11_end" = "End Date: Marine Corps",
#   "mil_branch12_start" = "Start Date: Marine Corps Reserve",
#   "mil_branch12_end" = "End Date: Marine Corps Reserve",
#   "mil_branch13_start" = "Start Date: Coast Guard Reserve",
#   "mil_branch13_end" = "End Date: Coast Guard Reserve",
#   "mil_branch14_start" = "Start Date: Other",
#   "mil_branch14_end" = "End Date: Other"
# )
# 
# # Military branch
# milBranchDat <- intakeDat %>%
#   select(id, starts_with("mil_branch")) %>%
#   mutate(across(mil_branch01_start:mil_branch14_end,
#                 ~na_if(., "01Jan9999 0:00:00"))) %>%
#   mutate(across(mil_branch01_start:mil_branch14_end,
#                 ~dmy_hms(.))) %>%
#   pivot_longer(2:29, names_to = "milBranchDateVars", values_to = "milBranchDateValues") %>%
#   mutate(milBranchDateVars = branch_names[milBranchDateVars]) %>%
#   filter(!is.na(milBranchDateValues)) %>%
#   mutate(
#     milBranchDateVars = str_remove(milBranchDateVars, "Start Date: "),
#     milBranchDateVars = str_remove(milBranchDateVars, "End Date: "),
#     milBranchDateVars = str_trim(milBranchDateVars)
#   ) %>%
#   rename(milBranch = milBranchDateVars) %>%
#   group_by(id, milBranch) %>%
#   summarise(
#     startDate = min(milBranchDateValues, na.rm = TRUE),
#     endDate = max(milBranchDateValues, na.rm = TRUE),
#     milBranchDateValues = paste0(
#       format(startDate, "%m/%d/%Y"), " - ", format(endDate, "%m/%d/%Y"), ", ",
#       round(as.numeric(interval(startDate, endDate) / years(1)), 2),
#       " years"
#     ),
#     .groups = 'drop'
#   ) %>%
#   select(id, milBranch, milBranchDates = milBranchDateValues)
# 
# # Military occupations
# milOcc1Dat <- intakeDat %>% select(id, starts_with("mil_occ")) %>%
#   select(id,
#          milOccup1Desc = mil_occ1_desc,
#          milOccup1Start =  mil_occ1_start,
#          milOccup1End = mil_occ1_end,
#          milOccup2Start =  mil_occ2_start,
#          milOccup2End = mil_occ2_end,
#          milOccup3Start =  mil_occ3_start,
#          milOccup3End = mil_occ3_end) %>%
#   mutate_at(c("milOccup1Start", "milOccup2Start", "milOccup3Start",
#               "milOccup1End", "milOccup2End", "milOccup3End"),
#             ~na_if(., "01Jan9999 0:00:00")) %>%
#   mutate_at(c("milOccup1Start", "milOccup2Start", "milOccup3Start",
#               "milOccup1End", "milOccup2End", "milOccup3End"),
#             ~na_if(., "-99")) %>%
#   mutate_at(c("milOccup1Start", "milOccup2Start", "milOccup3Start",
#               "milOccup1End", "milOccup2End", "milOccup3End"),
#            ~dmy_hms(.)) %>%
#   mutate(milOccup1Desc = na_if(milOccup1Desc, "-99")) %>%
#   select(id, milOccup1Desc, milOccup1Start, milOccup1End) %>%
#   pivot_longer(3:4, names_to = "milStartEndText", values_to = "milStartEndDates") %>%
#   group_by(id, milOccup1Desc) %>%
#   summarise(
#     milOccup1DateValues = paste0(
#       format(first(milStartEndDates), "%m/%d/%Y"), " - ", format(last(milStartEndDates), "%m/%d/%Y"), ", ",
#       round(as.numeric(interval(first(milStartEndDates), last(milStartEndDates)) / years(1)), 2), " years"
# 
#       ),
#     .groups = 'drop'
#   )
# 
# milOcc2Dat <- intakeDat %>%
#   select(id,
#          milOccup2Desc = mil_occ2_desc,
#          milOccup2Start =  mil_occ2_start,
#          milOccup2End = mil_occ2_end,
#          milOccup3Start =  mil_occ3_start,
#          milOccup3End = mil_occ3_end) %>%
#   mutate_at(vars(milOccup2Start, milOccup2End, milOccup3Start, milOccup3End),
#             ~na_if(., "02Jan9999 0:00:00")) %>%
#   mutate_at(vars(milOccup2Start, milOccup2End, milOccup3Start, milOccup3End),
#             ~na_if(., "-99")) %>%
#   mutate_at(vars(milOccup2Start, milOccup2End, milOccup3Start, milOccup3End),
#             ~dmy_hms(.)) %>%
#   mutate(milOccup2Desc = na_if(milOccup2Desc, "-99")) %>%
#   select(id, milOccup2Desc, milOccup2Start, milOccup2End) %>%
#   pivot_longer(cols = c(milOccup2Start, milOccup2End),
#                names_to = "milStartEndText",
#                values_to = "milStartEndDates") %>%
#   group_by(id, milOccup2Desc) %>%
#   summarise(
#     milOccup2DateValues = paste0(
#       format(first(milStartEndDates), "%m/%d/%Y"), " - ",
#       format(last(milStartEndDates), "%m/%d/%Y"), ", ",
#       round(as.numeric(interval(first(milStartEndDates),
#                                 last(milStartEndDates)) / years(1)), 2),
#       " years"
#     ),
#     .groups = 'drop'
#   )
# 
# milOcc3Dat <- intakeDat %>% select(id, starts_with("mil_occ")) %>%
#   select(id,
#          milOccup3Desc = mil_occ3_desc,
#          milOccup3Start =  mil_occ3_start,
#          milOccup3End = mil_occ3_end,
#          milOccup3Start =  mil_occ3_start,
#          milOccup3End = mil_occ3_end,
#          milOccup3Start =  mil_occ3_start,
#          milOccup3End = mil_occ3_end) %>%
#   mutate_at(c("milOccup3Start", "milOccup3Start", "milOccup3Start",
#               "milOccup3End", "milOccup3End", "milOccup3End"),
#             ~na_if(., "03Jan9999 0:00:00")) %>%
#   mutate_at(c("milOccup3Start", "milOccup3Start", "milOccup3Start",
#               "milOccup3End", "milOccup3End", "milOccup3End"),
#             ~na_if(., "-99")) %>%
#   mutate_at(c("milOccup3Start", "milOccup3Start", "milOccup3Start",
#               "milOccup3End", "milOccup3End", "milOccup3End"),
#            ~dmy_hms(.)) %>%
#   mutate(milOccup3Desc = na_if(milOccup3Desc, "-99")) %>%
#   # mutate_at(c("milOccup3Start", "milOccup3Start", "milOccup3Start",
#   #             "milOccup3End", "milOccup3End", "milOccup3End"),
#   #          ~format(., "%m/%d/%Y")) %>%
#   select(id, milOccup3Desc, milOccup3Start, milOccup3End) %>%
#   pivot_longer(3:4, names_to = "milStartEndText", values_to = "milStartEndDates") %>%
#   group_by(id, milOccup3Desc) %>%
#   summarise(
#     milOccup3DateValues = paste0(
#       format(first(milStartEndDates), "%m/%d/%Y"), " - ",
#       format(last(milStartEndDates), "%m/%d/%Y"), ", ",
#       round(as.numeric(interval(first(milStartEndDates),
#                                 last(milStartEndDates)) / years(1)), 3), " years"
#       ),
#     .groups = 'drop'
#   ) # add right join id !!! and below too
# 
# milOccupDat <- full_join(milOcc1Dat, milOcc2Dat, "id") %>%
#   full_join(., milOcc3Dat, "id") %>%
#   mutate_all(~ifelse(. == "01/01/9999 - 01/01/9999, 0 years", NA, .)) %>%
#   mutate_all(~ifelse(. == "-99", NA, .))
# 
# # Pay grades
# payGradeDat <- intakeDat %>%
#   select(id, payGrade = mil_pay_grade) %>%
#   mutate(payGrade = na_if(payGrade, "999"))
# 
# # Military units
# milUnitsDat <- intakeDat %>%
#   select(id, milUnitNames = mil_unit_names, mil_unit_0:mil_unit_2) %>%
#   pivot_longer(cols = starts_with("mil_unit_"),
#                names_to = "milUnitType", values_to = "milUnitValues") %>%
#   mutate(
#     milUnitType = case_when(
#       milUnitType == "mil_unit_0" ~ "Combat Arms",
#       milUnitType == "mil_unit_1" ~ "Combat Support",
#       milUnitType == "mil_unit_2" ~ "Combat Service Support"
#     ),
#     # Remove leading and trailing quotes from milUnitNames
#     milUnitNames = str_remove_all(milUnitNames, '^"+|"+$'),
#     # Remove rows where milUnitNames is 999
#     milUnitNames = na_if(milUnitNames, "999")
#   ) %>%
#   filter(!is.na(milUnitNames) & milUnitValues == TRUE) %>%
#   group_by(id, milUnitNames) %>%
#   summarise(
#     milUnitType = if (n() == 3 && all(c("Combat Arms", "Combat Support",
#                                         "Combat Service Support") %in% milUnitType)) {
#       paste("Combat Arms, Combat Support, Combat Service Support")
#     } else {
#       paste(milUnitType, collapse = ", ")
#     },
#     .groups = 'drop'
#   )
# 
# # Military area
# milAreaDat <- intakeDat %>%
#   select(id, mil_area_1:mil_area_oth) %>%
#   pivot_longer(2:6, names_to = "milAreaNames", values_to = "milAreaValues") %>%
#   mutate(milAreaNames = case_when(
#     milAreaNames == "mil_area_1" ~ "Combat Zone",
#     milAreaNames == "mil_area_2" ~ "Other Land Area",
#     milAreaNames == "mil_area_3" ~ "Sea Duty",
#     milAreaNames == "mil_area_4" ~ "Don't Know",
#     milAreaNames == "mil_area_5" ~ "Other"
#   )) %>%
#   mutate(mil_area_oth = na_if(mil_area_oth, "-99")) %>%
#   mutate(mil_area_oth = na_if(mil_area_oth, "999")) %>%
#   filter(milAreaValues == T) %>%
#   mutate(milAreaNames = ifelse(!is.na(mil_area_oth) & milAreaNames == "Other",
#                                mil_area_oth, milAreaNames)) %>%
#   select(id, milAreaNames) %>%
#   group_by(id) %>%
#   summarise(milAreaNames = paste(milAreaNames, collapse = ", "), .groups = 'drop') %>%
#   right_join(intakeDat %>% select(id), "id")
# 
# # Military discharge
# milDischargeDat <- intakeDat %>%
#   select(id, milDischarge = mil_discharge) %>%
#   mutate(milDischarge = case_when(
#     milDischarge == 0 ~ "Honorable",
#     milDischarge == 1 ~ "General",
#     milDischarge == 2 ~ "Dishonorable",
#     milDischarge == 3 ~ "Medical"
#   ))
# 
# # Military POW
# milPowDat <- intakeDat %>%
#   select(id, milPow = mil_pow) %>% # WHAT IS -1 ??????
#   mutate(milPow = na_if(milPow, -1)) %>%
#   mutate(milPow = if_else(milPow == 0, "No", "Yes"))
# 
# # Military infoBoxDat
# milInfoBoxDat <- full_join(payGradeDat, milUnitsDat, "id") %>%
#   full_join(milAreaDat, "id") %>% full_join(milDischargeDat, "id") %>%
#   full_join(milPowDat, "id")
# 
# write_csv(milInfoBoxDat, "milInfoBoxDat.csv")
# 
# # Military Timeline Data ----
# milDepDat <- intakeDat %>%
#   select(id,
#          milDep1Start = mil_dep1_start, milDep1End = mil_dep1_end,
#          milDep1Loc = mil_dep1_loc, milDep1Conflict = mil_dep1_conflict,
#          milDep1Job = mil_dep1_job, # Same as conflict so not correct
#          milDep2Start = mil_dep2_start, milDep2End = mil_dep2_end,
#          milDep2Loc = mil_dep2_loc, milDep2Conflict = mil_dep2_conflict,
#          milDep2Job = mil_dep2_job,
#          milDep3Start = mil_dep3_start, milDep3End = mil_dep3_end,
#          milDep3Loc = mil_dep3_loc, milDep3Conflict = mil_dep3_conflict,
#          milDep3Job = mil_dep3_job,
#          milDep4Start = mil_dep4_start, milDep4End = mil_dep4_end,
#          milDep4Loc = mil_dep4_loc, milDep4Conflict = mil_dep4_conflict,
#          milDep4Job = mil_dep4_job,
#          milDep5Start = mil_dep5_start, milDep5End = mil_dep5_end,
#          milDep5Loc = mil_dep5_loc, milDep5Conflict = mil_dep5_conflict,
#          milDep5Job = mil_dep5_job,
#          milDepConflictOther = mil_dep_conflict_oth) %>%
#   mutate_at(c("milDep1Start", "milDep2Start", "milDep3Start", "milDep4Start", "milDep5Start",
#               "milDep1End", "milDep2End", "milDep3End", "milDep4End", "milDep5End"),
#             ~na_if(., "01Jan9999 0:00:00")) %>%
#   mutate_at(c("milDep1Start", "milDep2Start", "milDep3Start", "milDep4Start", "milDep5Start",
#               "milDep1End", "milDep2End", "milDep3End", "milDep4End", "milDep5End"),
#             ~na_if(., "-99")) %>%
#   mutate_at(c("milDep1Start", "milDep2Start", "milDep3Start", "milDep4Start", "milDep5Start",
#               "milDep1End", "milDep2End", "milDep3End", "milDep4End", "milDep5End"),
#             ~dmy_hms(.)) %>%
#   mutate_at(c("milDep1Loc", "milDep2Loc", "milDep3Loc", "milDep4Loc", "milDep5Loc"),
#             ~na_if(., "-99")) %>%
#   mutate_at(c("milDep1Conflict", "milDep2Conflict", "milDep3Conflict", "milDep4Conflict",
#               "milDep5Conflict"), ~na_if(., 999)) %>%
#   mutate_at(c("milDep1Job", "milDep2Job", "milDep3Job", "milDep4Job",
#               "milDep5Job"), ~na_if(., "999")) %>%
#   mutate_at(c("milDep1Job", "milDep2Job", "milDep3Job", "milDep4Job",
#               "milDep5Job"), ~na_if(., "-99")) %>%
#   mutate_at(c("milDep1Conflict", "milDep2Conflict", "milDep3Conflict", "milDep4Conflict",
#               "milDep5Conflict"), ~case_when(
#                 . == 1 ~ "WWII",
#                 . == 2 ~ "Korea",
#                 . == 3 ~ "Vietnam",
#                 . == 4 ~ "Lebanon",
#                 . == 5 ~ "Panama",
#                 . == 6 ~ "Grenada",
#                 . == 7 ~ "ODSS",
#                 . == 8 ~ "Kosovo",
#                 . == 9 ~ "Bosnia",
#                 . == 10 ~ "Croatia",
#                 . == 11 ~ "Somalia",
#                 . == 12 ~ "OIF/OEF/OND",
#                 . == 13 ~ "OIF/OEF/OND",
#                 . == 14 ~ "OIF/OEF/OND",
#                 . == 15 ~ "Other"
#               )) %>%
#   mutate_at(c("milDep1Conflict", "milDep2Conflict", "milDep3Conflict", "milDep4Conflict",
#               "milDep5Conflict"), ~case_when(
#                 . == "Other" ~ milDepConflictOther,
#                 TRUE ~ .
#               )) %>%
#   mutate(across(where(is.character), ~str_remove_all(., '^"+|"+$')))
# 
# # Military deployment 1
# milDep1Dat <- milDepDat %>%
#   select(id, startDate = milDep1Start, endDate = milDep1End, milDep1Loc,
#          milDep1Conflict, milDep1Job) %>%
#   mutate_at(c("milDep1Conflict", "milDep1Loc", "milDep1Job"), ~str_remove_all(., '^,+|,+$')) %>%
#   mutate(tooltip = paste0("<center><strong>1st Deployment</strong></center>", "<br>",
#                           "<strong>Conflict:</strong> ", milDep1Conflict, "<br>",
#                           "<strong>Dates:</strong> ", startDate, " - ", endDate, "<br>",
#                           "<strong>Location:</strong> ", milDep1Loc, "<br>",
#                           "<strong>Job:</strong> ", milDep1Job)) %>%
#   mutate(phase = "Deployment 1") %>%
#   select(id, tooltip, startDate, endDate, phase)
# 
# # Military deployment 2
# milDep2Dat <- milDepDat %>%
#   select(id, startDate = milDep2Start, endDate = milDep2End, milDep2Loc,
#          milDep2Conflict, milDep2Job) %>%
#   mutate_at(c("milDep2Conflict", "milDep2Loc", "milDep2Job"), ~str_remove_all(., '^,+|,+$')) %>%
#   mutate(tooltip = paste0("<center><strong>2nd Deployment</strong></center>", "<br>",
#                           "<strong>Conflict:</strong> ", milDep2Conflict, "<br>",
#                           "<strong>Dates:</strong> ", startDate, " - ", endDate, "<br>",
#                           "<strong>Location:</strong> ", milDep2Loc, "<br>",
#                           "<strong>Job:</strong> ", milDep2Job)) %>%
#   mutate(phase = "Deployment 2") %>%
#   select(id, tooltip, startDate, endDate, phase)
# 
# 
# # Military deployment 3
# milDep3Dat <- milDepDat %>%
#   select(id, startDate = milDep3Start, endDate = milDep3End, milDep3Loc,
#          milDep3Conflict, milDep3Job) %>%
#   mutate_at(c("milDep3Conflict", "milDep3Loc", "milDep3Job"), ~str_remove_all(., '^,+|,+$')) %>%
#   mutate(phase = "Deployment 3") %>%
#   mutate(tooltip = paste0("<center><strong>3rd Deployment</strong></center>", "<br>",
#                           "<strong>Conflict:</strong> ", milDep3Conflict, "<br>",
#                           "<strong>Dates:</strong> ", startDate, " - ", endDate, "<br>",
#                           "<strong>Location:</strong> ", milDep3Loc, "<br>",
#                           "<strong>Job:</strong> ", milDep3Job)) %>%
#   select(id, tooltip, startDate, endDate, phase)
# 
# # Military branch
# milBranchDat <- intakeDat %>%
#   select(id, starts_with("mil_branch")) %>%
#   mutate(across(mil_branch01_start:mil_branch14_end,
#                 ~na_if(., "01Jan9999 0:00:00"))) %>%
#   mutate(across(mil_branch01_start:mil_branch14_end,
#                 ~dmy_hms(.))) %>%
#   pivot_longer(2:29, names_to = "milBranchDateVars", values_to = "milBranchDateValues") %>%
#   mutate(milBranchDateVars = branch_names[milBranchDateVars]) %>%
#   filter(!is.na(milBranchDateValues)) %>%
#   mutate(
#     milBranchDateVars = str_remove(milBranchDateVars, "Start Date: "),
#     milBranchDateVars = str_remove(milBranchDateVars, "End Date: "),
#     milBranchDateVars = str_trim(milBranchDateVars)
#   ) %>%
#   rename(milBranch = milBranchDateVars) %>%
#   group_by(id, milBranch) %>%
#   summarise(
#     startDate = min(milBranchDateValues, na.rm = TRUE),
#     endDate = max(milBranchDateValues, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   arrange(id, milBranch) %>%
#   mutate(across(startDate:endDate, ~ mdy(format(.x, "%m/%d/%Y")))) %>%
#   rename(phase = milBranch)
# 
# # Military occupation 1
# milOcc1Dat <- intakeDat %>% select(id, starts_with("mil_occ")) %>%
#   select(id,
#          milOccup1Desc = mil_occ1_desc,
#          milOccup1Start =  mil_occ1_start,
#          milOccup1End = mil_occ1_end,
#          milOccup2Start =  mil_occ2_start,
#          milOccup2End = mil_occ2_end,
#          milOccup3Start =  mil_occ3_start,
#          milOccup3End = mil_occ3_end) %>%
#   mutate_at(c("milOccup1Start", "milOccup2Start", "milOccup3Start",
#               "milOccup1End", "milOccup2End", "milOccup3End"),
#             ~na_if(., "01Jan9999 0:00:00")) %>%
#   mutate_at(c("milOccup1Start", "milOccup2Start", "milOccup3Start",
#               "milOccup1End", "milOccup2End", "milOccup3End"),
#             ~na_if(., "-99")) %>%
#   mutate_at(c("milOccup1Start", "milOccup2Start", "milOccup3Start",
#               "milOccup1End", "milOccup2End", "milOccup3End"),
#            ~dmy_hms(.)) %>%
#   mutate(milOccup1Desc = na_if(milOccup1Desc, "-99")) %>%
#   select(id, tooltip = milOccup1Desc, startDate = milOccup1Start, endDate = milOccup1End) %>%
#   mutate(tooltip =
#            paste0("<center><strong>1st Occupation</strong></center>", "<br>",
#                   "<strong>Occupation:</strong> ", tooltip, "<br>",
#                   "<strong/>Dates:</strong> ", startDate, " - ", endDate, "<br>")
#            ) %>%
#   mutate(across(startDate:endDate, ~ format(.x, "%m/%d/%Y"))) %>%
#   mutate(across(c(startDate, endDate), ~ ifelse(. == "01/01/9999", NA, .))) %>%
#   mutate(across(c(startDate, endDate), ~ mdy(.x))) %>%
#   mutate(phase = "Military Occupation 1")
# 
# # Military occupation 2
# milOcc2Dat <- intakeDat %>%
#   select(id,
#          milOccup2Desc = mil_occ2_desc,
#          milOccup2Start =  mil_occ2_start,
#          milOccup2End = mil_occ2_end,
#          milOccup3Start =  mil_occ3_start,
#          milOccup3End = mil_occ3_end) %>%
#   mutate_at(vars(milOccup2Start, milOccup2End, milOccup3Start, milOccup3End),
#             ~na_if(., "02Jan9999 0:00:00")) %>%
#   mutate_at(vars(milOccup2Start, milOccup2End, milOccup3Start, milOccup3End),
#             ~na_if(., "-99")) %>%
#   mutate_at(vars(milOccup2Start, milOccup2End, milOccup3Start, milOccup3End),
#             ~dmy_hms(.)) %>%
#   mutate(milOccup2Desc = na_if(milOccup2Desc, "-99")) %>%
#   select(id, tooltip = milOccup2Desc, startDate = milOccup2Start, endDate = milOccup2End) %>%
#   mutate(tooltip =
#            paste0("<center><strong>2nd Occupation</strong></center>", "<br>",
#                   "<strong>Occupation:</strong> ", tooltip, "<br>",
#                   "<strong/>Dates:</strong> ", startDate, " - ", endDate, "<br>")
#            ) %>%
#   mutate(across(startDate:endDate, ~ format(.x, "%m/%d/%Y"))) %>%
#   mutate(across(c(startDate, endDate), ~ ifelse(. == "01/01/9999", NA, .))) %>%
#   mutate(across(c(startDate, endDate), ~ mdy(.x))) %>%
#   mutate(phase = "Military Occupation 2")
# 
# # Military occupation 3
# milOcc3Dat <- intakeDat %>% select(id, starts_with("mil_occ")) %>%
#   select(id,
#          milOccup3Desc = mil_occ3_desc,
#          milOccup3Start =  mil_occ3_start,
#          milOccup3End = mil_occ3_end,
#          milOccup3Start =  mil_occ3_start,
#          milOccup3End = mil_occ3_end,
#          milOccup3Start =  mil_occ3_start,
#          milOccup3End = mil_occ3_end) %>%
#   mutate_at(c("milOccup3Start", "milOccup3Start", "milOccup3Start",
#               "milOccup3End", "milOccup3End", "milOccup3End"),
#             ~na_if(., "03Jan9999 0:00:00")) %>%
#   mutate_at(c("milOccup3Start", "milOccup3Start", "milOccup3Start",
#               "milOccup3End", "milOccup3End", "milOccup3End"),
#             ~na_if(., "-99")) %>%
#   mutate_at(c("milOccup3Start", "milOccup3Start", "milOccup3Start",
#               "milOccup3End", "milOccup3End", "milOccup3End"),
#            ~dmy_hms(.)) %>%
#   mutate(milOccup3Desc = na_if(milOccup3Desc, "-99")) %>%
#   select(id, tooltip = milOccup3Desc, startDate = milOccup3Start, endDate = milOccup3End) %>%
#   mutate(tooltip =
#            paste0("<center><strong>3rd Occupation</strong></center>", "<br>",
#                   "<strong>Occupation:</strong> ", tooltip, "<br>",
#                   "<strong/>Dates:</strong> ", startDate, " - ", endDate, "<br>")
#            ) %>%
#   mutate(across(startDate:endDate, ~ format(.x, "%m/%d/%Y"))) %>%
#   mutate(across(c(startDate, endDate), ~ ifelse(. == "01/01/9999", NA, .))) %>%
#   mutate(across(c(startDate, endDate), ~ mdy(.x))) %>%
#   mutate(phase = "Military Occupation 3")
# 
# # -- Bind All --
# milTimelineDat <- bind_rows(milOcc1Dat, milOcc2Dat) %>%
#   bind_rows(milOcc3Dat) %>%
#   bind_rows(milBranchDat) %>%
#   bind_rows(milDep1Dat) %>%
#   bind_rows(milDep2Dat) %>%
#   bind_rows(milDep3Dat) %>%
#   #na.omit() %>%
#   distinct_at(vars(tooltip, startDate, endDate), .keep_all = T) %>%
#   mutate(endDate = if_else(endDate < startDate, NA_Date_, endDate)) %>%
#   mutate(startDate = if_else(is.na(endDate), NA_Date_, startDate)) %>%
#   mutate(across(c(startDate, endDate), ~ str_remove_all(.x, '^"+|"+$')))
# 
#  write_csv(milTimelineDat, "milTimelineDat.csv")
# 
#  # Medication Use Data ----
#  medications_dat <- intake_dat %>% select(id, starts_with("med_name"),
#                                           starts_with("med_dose")) %>%
#    mutate(across(everything(), ~replace(., . %in% c("-99", "999", NA), "")))
# 
#  write_csv(medications_dat, "medicationsDat.csv")
# 
#  # Tobacco Use Data ----
#  tobacco_dat <- intake_dat %>%
#    select(
#      id,
#      smoker = tobacco01,
#      current_smoker = tobacco04a,
#      age_start = tobacco03,
#      age_end = tobacco04b,
#      reason_quit = tobacco04c,
#      reason_quit_desc = tobacco04c_text,
#      cigarettes = tobacco02a,
#      cigars = tobacco02b,
#      pipe_tobacco = tobacco02c
#    ) %>%
#    mutate(
#      across(c("smoker", "current_smoker"), ~ as.character(if_else(. == "TRUE", "Yes", "No"))),
#      across(c("age_start", "age_end"), ~ na_if(., 999)),
#      reason_quit = case_when(
#        reason_quit == 0 ~ "Personal Decision",
#        reason_quit == 1 ~ "Medical Condition",
#        reason_quit == -1 ~ "Medical Condition"
#      ),
#      reason_quit = na_if(reason_quit, "999"),
#      reason_quit_desc = na_if(reason_quit_desc, "999"),
#      reason_quit_desc = str_remove_all(reason_quit_desc, '^"+|"+$'),
#      reason_quit_desc = if_else(nchar(reason_quit_desc) == 1, NA_character_, reason_quit_desc),
#      across(c("cigarettes", "cigars", "pipe_tobacco"), ~ na_if(., 999)),
#      cigarettes = case_when(
#        cigarettes == 0 ~ "None",
#        cigarettes == 1 ~ "1-2 a day/intermittent/occasional",
#        cigarettes == 2 ~ "3-10 (up to half a pack) a day",
#        cigarettes == 3 ~ "11-20 (up to a pack) a day",
#        cigarettes == 4 ~ "21-40 (1-2 packs) a day",
#        cigarettes == 5 ~ "> 40 (more than 2 packs a day)"
#      ),
#      cigars = case_when(
#        cigars == 0 ~ "None",
#        cigars == 1 ~ "Less than 7 per week",
#        cigars == 2 ~ "7-14 per week",
#        cigars == 3 ~ "More than 14 per week"
#      ),
#      pipe_tobacco = case_when(
#        pipe_tobacco == 0 ~ "None",
#        pipe_tobacco == 1 ~ "Less than 7 per week",
#        pipe_tobacco == 2 ~ "7-14 per week",
#        pipe_tobacco == 3 ~ "More than 14 per week"
#      )
#    )
# 
#  write_csv(tobacco_dat, "tobaccoDat.csv")
# 
#  # Substance Use Data ----
#  substances_dat <- intake_dat %>%
#    select(id, caffeine_use = sa_caff, cigarette_use = sa_cig, chewing_tobacco = sa_chew,
#           marijuana_use = sa_mari, alcohol_use = sa_alco, amphetamine_use = sa_amph,
#           cocaine_use = sa_coca, hallucinogen_use = sa_hall, inhalant_use = sa_inha,
#           heroin_use = sa_hero, pcp_use = sa_pcp, bath_salts_use = sa_bath,
#           prescription_drug_abuse = sa_pres, other_substance_use = sa_oth,
#           other_substance_desc = sa_oth_spec, grew_up_with_smoker = dare_c2sha1,
#           living_with_smoker = dare_c2sha2) %>%
#    mutate(across(c(caffeine_use:other_substance_use), ~case_when(
#      . == 0 ~ "Never",
#      . == 1 ~ "In the past",
#      . == 2 ~ "Presently",
#      . == 999 ~ NA_character_,
#      TRUE ~ as.character(.)
#    ))) %>%
#    mutate(other_substance_desc = na_if(other_substance_desc, "999"),
#           other_substance_desc = str_remove_all(other_substance_desc, '^"+|"+$')) %>%
#    mutate(across(c(grew_up_with_smoker, living_with_smoker),
#                  ~as.character(if_else(. == "TRUE", "Yes", "No"))))
# 
#  write_csv(substances_dat, "substancesDat.csv")
# 
#  # PTSD Screener Data
#  ptsd_screener_dat <- intake_dat %>%
#    select(id,
#           nightmares = pcptsd_01,
#           avoidance = pcptsd_02,
#           hypervigilance = pcptsd_03,
#           detachment = pcptsd_04) %>%
#    mutate(
#      # Set ptsd_score to NA if any response is NA; otherwise, sum the responses
#      ptsd_score = ifelse(rowSums(is.na(across(c(nightmares, avoidance,
#                                                 hypervigilance, detachment)))) > 0,
#                          NA,
#                          rowSums(across(c(nightmares, avoidance,
#                                           hypervigilance, detachment)), na.rm = TRUE)),
#      # Determine overall PTSD assessment based on ptsd_score
#      ptsd_assessment = case_when(
#        is.na(ptsd_score) ~ "Data not available",
#        ptsd_score == 0 ~ "No PTSD symptoms endorsed",
#        ptsd_score >= 3 ~ "Positive for PTSD",
#        ptsd_score < 3 ~ "Negative for PTSD"
#      )
#    )
# 
#  write_csv(ptsd_screener_dat, "ptsdScreenerDat.csv")
# 
#  # TBI Screener Data ----
#  tbi_screener_dat <- intake_dat %>%
#    select(id, starts_with("tbi_")) %>%
#    # Handle character columns for "-99", "999", and "na" replacements
#    mutate(across(where(is.character),
#                  ~ if_else(. %in% c("-99", "999", "na", "NA"), NA_character_, .))) %>%
#    # Handle numeric columns for -99 and 999 replacements only
#    mutate(across(where(is.numeric), ~ na_if(., -99))) %>%
#    mutate(across(where(is.numeric), ~ na_if(., 999))) %>%
#    # Clean up extra quotation marks in specific columns
#    mutate(
#      tbi_01f = str_remove_all(tbi_01f, '^"+|"+$'),
#      tbi_05_spec = str_remove_all(tbi_05_spec, '^"+|"+$')
#    ) %>%
#    # Create screening indicators based on the presence of TBI-related responses
#    mutate(
#      injury_endorsed = if_any(starts_with("tbi_01"), ~ . == 1),  # Injury mechanisms in Question 1
#      alteration_endorsed = if_any(c("tbi_02a", "tbi_02b", "tbi_02c", "tbi_02d", "tbi_02e"), ~ . == 1),  # Consciousness alterations in Question 2
#      current_symptoms = if_any(starts_with("tbi_03"), ~ . == 1),  # Current symptoms in Question 3
#      # Final TBI screening result based on the criteria
#      tbi_screen_result = case_when(
#        is.na(injury_endorsed) | is.na(alteration_endorsed) | is.na(current_symptoms) ~ "Not enough data not available",
#        injury_endorsed & alteration_endorsed ~ "Positive screen for TBI - Further evaluation recommended",
#        !injury_endorsed & !alteration_endorsed ~ "No TBI symptoms endorsed",
#        TRUE ~ "TBI assessment inconclusive"
#      )
#    )
# 
#  write_csv(tbi_screener_dat, "tbiScreenerDat.csv")
# 
#  # PSQI Data ----
#  # https://stackoverflow.com/questions/69838922/calculating-time-slept
#  # https://www.sleep.pitt.edu/psqi
# 
#  psqiDat <- intakeDat %>%
#    select(id, starts_with("psqi")) %>%
#    # Replace invalid values (999, 99, -99) with NA across all numeric variables
#    mutate(across(
#      where(is.numeric),
#      ~ if_else(. %in% c(999, 99, -99), NA_real_, .)
#    )) %>%
#    # Replace invalid values in character variables
#    mutate(across(
#      where(is.character),
#      ~ if_else(. %in% c("999", "99", "-99", "NA", "N/A"), NA_character_, .)
#    )) %>%
#    mutate(
#      # Clean and parse psqi_01 (bedtime)
#      psqi_01 = case_when(
#        psqi_01 %in% c("0", "1") ~ NA_character_,  # Treat invalid values as NA
#        str_detect(psqi_01, "^\\d{1,2}:\\d{2}$") ~ psqi_01,  # Keep valid HH:MM format
#        TRUE ~ NA_character_
#      ),
#      psqi_01_time = hms::parse_hm(psqi_01),  # Parse as HMS without attaching a date
# 
#      # Clean and parse psqi_03 (wake time)
#      psqi_03 = case_when(
#        psqi_03 %in% c("0", "1") ~ NA_character_,  # Treat invalid values as NA
#        str_detect(psqi_03, "^\\d{1,2}:\\d{2}$") ~ psqi_03,  # Keep valid HH:MM format
#        TRUE ~ NA_character_
#      ),
#      psqi_03_time = hms::parse_hm(psqi_03),  # Parse as HMS without attaching a date
# 
#      # Calculate time in bed with overnight transition handling
#      psqi_diff_hours = if_else(
#        !is.na(psqi_01_time) & !is.na(psqi_03_time),
#        as.numeric(if_else(psqi_03_time < psqi_01_time,
#                           psqi_03_time + hms::as_hms("24:00:00") - psqi_01_time,  # Add 24 hours for overnight
#                           psqi_03_time - psqi_01_time)) / 3600,  # Difference in hours
#        NA_real_
#      ),
# 
#      # Ensure time in bed is valid
#      psqi_tib = if_else(psqi_diff_hours > 24, NA_real_, psqi_diff_hours),
# 
#      # Sleep efficiency percentage
#      psqi_hse = if_else(!is.na(psqi_04) & !is.na(psqi_tib), round((psqi_04 / psqi_tib) * 100, 2), NA_real_),
# 
#      # Recode sleep efficiency
#      psqi_hse_score = case_when(
#        psqi_hse >= 85 ~ 0,
#        psqi_hse < 85 & psqi_hse >= 75 ~ 1,
#        psqi_hse < 75 & psqi_hse >= 65 ~ 2,
#        psqi_hse < 65 ~ 3,
#        TRUE ~ NA_real_
#      ),
# 
#      # Sleep duration scoring
#      psqi_durat_score = case_when(
#        psqi_04 >= 7 ~ 0,
#        psqi_04 < 7 & psqi_04 >= 6 ~ 1,
#        psqi_04 < 6 & psqi_04 >= 5 ~ 2,
#        psqi_04 < 5 ~ 3,
#        TRUE ~ NA_real_
#      ),
# 
#      # Sleep latency scoring
#      psqi_02new = case_when(
#        psqi_02 <= 15 ~ 0,
#        psqi_02 > 15 & psqi_02 <= 30 ~ 1,
#        psqi_02 > 30 & psqi_02 <= 60 ~ 2,
#        psqi_02 > 60 ~ 3,
#        TRUE ~ NA_real_
#      ),
#      psqi_laten_score = case_when(
#        psqi_05a + psqi_02new == 0 ~ 0,
#        psqi_05a + psqi_02new >= 1 & psqi_05a + psqi_02new <= 2 ~ 1,
#        psqi_05a + psqi_02new >= 3 & psqi_05a + psqi_02new <= 4 ~ 2,
#        psqi_05a + psqi_02new >= 5 & psqi_05a + psqi_02new <= 6 ~ 3,
#        TRUE ~ NA_real_
#      ),
#      # Sleep disturbance scoring
#      # Adjust psqi_05j: Set to 0 if psqi_05j is missing
#      psqi_05j_adjusted = if_else(is.na(psqi_05j), 0, psqi_05j),
# 
#      # Calculate the sum for sleep disturbance
#      psqi_distb_sum = rowSums(select(., psqi_05b:psqi_05i),
#                               na.rm = TRUE) + psqi_05j_adjusted,
# 
#      # Assign scores based on the sum
#      psqi_distb_score = case_when(
#        psqi_distb_sum == 0 ~ 0,
#        psqi_distb_sum >= 1 & psqi_distb_sum <= 9 ~ 1,
#        psqi_distb_sum > 9 & psqi_distb_sum <= 18 ~ 2,
#        psqi_distb_sum > 18 ~ 3,
#        TRUE ~ NA_real_
#      ),
# 
#      # Day dysfunction due to sleepiness
#      psqi_daydys_score = case_when(
#        psqi_08 + psqi_09 == 0 ~ 0,
#        psqi_08 + psqi_09 >= 1 & psqi_08 + psqi_09 <= 2 ~ 1,
#        psqi_08 + psqi_09 >= 3 & psqi_08 + psqi_09 <= 4 ~ 2,
#        psqi_08 + psqi_09 >= 5 & psqi_08 + psqi_09 <= 6 ~ 3,
#        TRUE ~ NA_real_
#      ),
# 
#      # Overall subjective sleep quality score
#      psqi_slpqual_score = psqi_06,
# 
#      # Overall subjective sleep quality text
#      psqi_slpqual_text = case_when(
#        psqi_06 == 0 ~ "Very Good",
#        psqi_06 == 1 ~ "Fairly Good",
#        psqi_06 == 2 ~ "Fairly Bad",
#        psqi_06 == 3 ~ "Very Bad"
#      ),
# 
#      # Need for sleep medicine
#      psqi_meds_score = psqi_07,
# 
#      # Other disturbances
#      psqi_05j_spec = str_remove_all(psqi_05j_spec, '^"+|"+$'),
# 
#      # Other Partner/Roommate Report
#      psqi_10e_spec = str_remove_all(psqi_10e_spec, '^"+|"+$'),
# 
#      # Total PSQI score
#      psqi_total = psqi_durat_score + psqi_distb_score + psqi_laten_score +
#        psqi_daydys_score + psqi_hse_score + psqi_slpqual_score + psqi_meds_score
#    ) %>%
#    # Sleep quality interpretation
#    mutate(
#      psqi_quality = case_when(
#        psqi_total <= 5 ~ "Good",
#        psqi_total > 5 ~ "Poor",
#        TRUE ~ "missing"
#      )
#    ) %>%
#    filter(!is.na(id))
# 
#  write_csv(psqiDat, "psqiDat.csv")
# 
#  # Deployment Health Data ----
#  pdhraDat <- intakeDat %>%
#    select(id, pdhra_03, pdhra_03a, dep_med, pdhra_05, dep_med_spec) %>%
#    mutate(
#      pdhra_03 = case_when(
#        pdhra_03 == 0 ~ "No",
#        pdhra_03 == 1 ~ "Yes",
#        pdhra_03 == 2 ~ "Unsure",
#        pdhra_03 %in% c(-99, 99, 999, "999", "na", "NA") ~ "missing",
#        TRUE ~ "missing"
#      ),
#      pdhra_03a = case_when(
#        pdhra_03a == 0 ~ "No",
#        pdhra_03a == 1 ~ "Yes",
#        pdhra_03a == 2 ~ "Unsure",
#        pdhra_03a == 3 ~ "Not applicable",
#        pdhra_03a %in% c(-99, 99, 999, "999", "na", "NA") ~ "missing",
#        TRUE ~ "missing"
#      ),
#      dep_med = case_when(
#        dep_med == 0 ~ "No",
#        dep_med == 1 ~ "Yes",
#        dep_med %in% c(-99, 99, 999, "999", "na", "NA") ~ "missing",
#        TRUE ~ "missing"
#      ),
#      pdhra_05 = case_when(
#        pdhra_05 == 0 ~ "No visits",
#        pdhra_05 == 1 ~ "2-4 visits",
#        pdhra_05 == 2 ~ "5-9 visits",
#        pdhra_05 == 3 ~ "10-14 visits",
#        pdhra_05 == 4 ~ "15-20 visits",
#        pdhra_05 == 5 ~ "21-25 visits",
#        pdhra_05 == 6 ~ "26-30 visits",
#        pdhra_05 == 7 ~ "> 30 visits",
#        pdhra_05 %in% c(-99, 99, 999, "999", "na", "NA") ~ "missing",
#        TRUE ~ "missing"
#      ),
#      dep_med_spec = case_when(
#        dep_med_spec %in% c(-99, 99, 999, "999", "na", "NA", "") ~ "missing",
#        TRUE ~ dep_med_spec
#      ),
#      dep_med_spec = str_remove_all(dep_med_spec, '^"+|"+$'), # Remove leading/trailing quotes
#      dep_med_spec = if_else(nchar(dep_med_spec) <= 1, "missing", dep_med_spec) # Replace single characters with "missing"
#    )
# 
# 
#  write_csv(pdhraDat, "pdhraDat.csv")
# 
#  # Family History Data ----
#  family_history_dat <- intake_dat %>%
#    select(
#      id,
#      mother_health = fam_mom,
#      mother_age_death = fam_momage,
#      mother_cause_death = fam_momcaus,
#      mother_medical_conditions = fam_mommed,
# 
#      father_health = fam_dad,
#      father_age_death = fam_dadage,
#      father_cause_death = fam_dadcaus,
#      father_medical_conditions = fam_dadmed,
# 
#      has_brothers = fam_sib_bro,
#      num_brothers = fam_bronum,
#      brothers_medical_conditions = fam_bromed,
#      brother_age_death = fam_broage,
#      brother_cause_death = fam_brocaus,
# 
#      has_sisters = fam_sib_sis,
#      num_sisters = fam_sisnum,
#      sisters_medical_conditions = fam_sismed,
#      sister_age_death = fam_sisage,
#      sister_cause_death = fam_siscaus,
# 
#      has_children = fam_child,
#      num_sons = fam_son,
#      num_daughters = fam_daught,
#      children_medical_conditions = fam_childmed,
#      children_age_death = fam_childage,
#      children_cause_death = fam_childcaus
#    ) %>%
#    # Handle character columns for "-99", "999", and "na" replacements
#    mutate(across(where(is.character), ~ if_else(. %in% c("-99", "999", "na", "NA"),
#                                                 NA_character_, .))) %>%
#    # Handle numeric columns for -99 and 999 replacements only
#    mutate(across(where(is.numeric), ~ na_if(., -99))) %>%
#    mutate(across(where(is.numeric), ~ na_if(., 999))) %>%
#    # Clean up extra quotation marks in character columns
#    mutate(across(where(is.character), ~ str_remove_all(., '^"+|"+$'))) %>%
#    # Replace single-letter typos with NA
#    mutate(
#      mother_cause_death = if_else(nchar(mother_cause_death) == 1, NA_character_, mother_cause_death),
#      children_medical_conditions = if_else(nchar(children_medical_conditions) == 1, NA_character_, children_medical_conditions)
#    )
# 
#  write_csv(family_history_dat, "familyHxDat.csv")
# 
# 
#  # WRIISC Reason for Visit Data ----
#  wriisc_reason_dat <- intake_dat %>%
#    select(
#      id,
#      va_provider = wriisc_vapro,
#      non_va_provider = wriisc_provider,
#      veteran_peer = wriisc_vetpeer,
#      vet_center = wriisc_vetcent,
#      oef_oif_coordinator = wriisc_oefoif,
#      veteran_org = wriisc_vetorg,
#      military_contact = wriisc_military,
#      flyer = wriisc_flyer,
#      flyer_specified = wriisc_flyer_spec,
#      website = wriisc_web,
#      website_specified = wriisc_web_spec,
#      other_source = wriisc_other,
#      other_specified = wriisc_other_spec,
#      understand_symptoms_cause = wriisc_rsn_caus,
#      reduce_symptoms = wriisc_rsn_redu,
#      understand_exposure_effects = wriisc_rsn_expo,
#      complete_exam = wriisc_rsn_exam,
#      mental_health_eval = wriisc_rsn_ment,
#      help_others = wriisc_rsn_othe,
#      newspaper_magazine = wriisc_src_prin,
#      friends_family = wriisc_src_frie,
#      va_dod_health = wriisc_src_vahe,
#      non_va_health = wriisc_src_hea,
#      television = wriisc_src_tele,
#      websites = wriisc_src_webs,
#      support_groups = wriisc_src_supp,
#      social_media = wriisc_src_soci,
#      other_info_source = wriisc_src_othe,
#      other_info_specified = wriisc_src_spec
#    ) %>%
#    mutate(across(where(is.character), ~na_if(., "-99"))) %>%
#    mutate(across(where(is.character), ~na_if(., "999"))) %>%
#    mutate(across(where(is.character), ~na_if(., "na"))) %>%
#    mutate(across(where(is.numeric), ~na_if(., -99))) %>%
#    mutate(across(where(is.numeric), ~na_if(., 999))) %>%
#    mutate(across(where(is.character), ~str_remove_all(., '^"+|"+$'))) %>%
#    mutate(across(where(is.character), ~ if_else(nchar(.) == 1, NA_character_, .))) %>%
#    mutate(across(
#      c(understand_symptoms_cause, reduce_symptoms,
#        understand_exposure_effects, complete_exam,
#        mental_health_eval, help_others),
#      ~ case_when(
#        . == 0 ~ "None at all",
#        . == 1 ~ "A little",
#        . == 2 ~ "A lot"
#      )
#    )) %>%
#    mutate(across(where(is.logical), ~ if_else(. == TRUE, "Yes", "No")))
# 
# write_csv(wriisc_reason_dat, "wriiscReasonDat.csv")
# 
# # ROS Health Symptoms Data ----
# rosHealthSxDat <- intakeDat %>%
#   select(id, starts_with("ros")) %>%
#   select(-c(49, 50)) %>%
#   pivot_longer(2:48, names_to = "healthSxNames", values_to = "healthSxValues") %>%
#   mutate(healthSxNames = case_when(
#     healthSxNames == "ros_palp" ~ "Palpitations or Heart Pounding",
#     healthSxNames == "ros_sob" ~ "Difficulty Breathing",
#     healthSxNames == "ros_wheez" ~ "Wheezing",
#     healthSxNames == "ros_headac" ~ "Headaches",
#     healthSxNames == "ros_faint" ~ "Passing Out or Fainting",
#     healthSxNames == "ros_paraly" ~ "Paralysis",
#     healthSxNames == "ros_musc" ~ "Muscle Weakness",
#     healthSxNames == "ros_urine" ~ "Difficulty with Urination",
#     healthSxNames == "ros_blurine" ~ "Blood in the Urine",
#     healthSxNames == "ros_std" ~ "Genital Sores or STD",
#     healthSxNames == "ros_neck" ~ "Neck Aches or Pains",
#     healthSxNames == "ros_limb" ~ "Limb Aches or Pains",
#     healthSxNames == "ros_nausea" ~ "Nausea or Vomiting",
#     healthSxNames == "ros_gi" ~ "Intestinal or Stomach Problems",
#     healthSxNames == "ros_diarr" ~ "Diarrhea",
#     healthSxNames == "ros_weight" ~ "Weight Change (> 15 lbs)",
#     healthSxNames == "ros_gas" ~ "Excessive Gas",
#     healthSxNames == "ros_temp" ~ "Temperature Sensitivity",
#     healthSxNames == "ros_sleep" ~ "Difficulty Sleeping",
#     healthSxNames == "ros_tempsen" ~ "Cold or Heat Sensitivity",
#     healthSxNames == "ros_skin" ~ "Skin Trouble",
#     healthSxNames == "ros_chest" ~ "Chest Pains",
#     healthSxNames == "ros_cough" ~ "Chronic Cough",
#     healthSxNames == "ros_dizzy" ~ "Dizziness",
#     healthSxNames == "ros_numb" ~ "Numbness or Tingling",
#     healthSxNames == "ros_eyes" ~ "Eye or Vision Trouble",
#     healthSxNames == "ros_taste" ~ "Taste or Smell Trouble",
#     healthSxNames == "ros_seize" ~ "Seizures",
#     healthSxNames == "ros_niteuri" ~ "Night Urination",
#     healthSxNames == "ros_sexual" ~ "Sexual Problems",
#     healthSxNames == "ros_back" ~ "Back Pain",
#     healthSxNames == "ros_swell" ~ "Swelling",
#     healthSxNames == "ros_swallow" ~ "Difficulty Swallowing",
#     healthSxNames == "ros_constip" ~ "Constipation",
#     healthSxNames == "ros_appet" ~ "Poor Appetite",
#     healthSxNames == "ros_teeth" ~ "Teeth Trouble",
#     healthSxNames == "ros_stool" ~ "Blood in Stool or Black Stool",
#     healthSxNames == "ros_shaky" ~ "Shakiness",
#     healthSxNames == "ros_sweat" ~ "Excessive Sweating",
#     healthSxNames == "ros_fever" ~ "Fever",
#     healthSxNames == "ros_gland" ~ "Swollen Glands or Lumps",
#     healthSxNames == "ros_conc" ~ "Trouble Concentrating",
#     healthSxNames == "ros_mem" ~ "Forgetfulness",
#     healthSxNames == "ros_dec" ~ "Difficulty Making Decisions",
#     healthSxNames == "ros_irr" ~ "Increased Irritability",
#     healthSxNames == "ros_risk" ~ "Risk Taking Behavior",
#     healthSxNames == "ros_hear" ~ "Hearing Trouble",
#     healthSxNames == "ros_birth" ~ "Infertility Issues",
#     healthSxNames == "ros_birthdefect" ~ "Birth Defects in Children",
#     TRUE ~ healthSxNames
#   )) %>%
#   mutate(tooltip_text = case_when(
#     healthSxValues == 0 ~ "Almost never",
#     healthSxValues == 1 ~ "Once a year",
#     healthSxValues == 2 ~ "Once a month",
#     healthSxValues == 3 ~ "Once a week",
#     healthSxValues == 4 ~ "Twice a week",
#     healthSxValues == 5 ~ "Every day",
#     TRUE ~ "Unknown"
#   )) %>%
#   mutate(healthSxValues = na_if(healthSxValues, 999)) %>%
#   arrange(desc(healthSxValues))
# 
# write_csv(rosHealthSxDat, "rosHealthSxDat.csv")
# 
# # Ever Diagnosed by Medical Professional Dat ----
# # Initial data transformation
# everDxDat <- intakeDat %>%
#   select(id, starts_with("pmh")) %>%
#   select(-pmh_transfus, -c(71:82)) %>%
#   select(-pmh_hosp5) %>%
#   filter_at(vars(2:69), ~ !is.na(.)) %>%
#   mutate_at(vars(2:69), ~ ifelse(. == TRUE, 1, 0))
# 
# # Pivot to longer format and classify conditions
# everDxDat <- everDxDat %>%
#   pivot_longer(2:69, names_to = "condition", values_to = "conditionYesNo") %>%
#   mutate(conditionClass = case_when(
#     str_detect(condition, "pmh_(col|arryth|heart|h_bp|l_bp|chf|clot)") ~ "Cardiovascular Conditions",
#     str_detect(condition, "pmh_(asthma|bronch|lung|asbest|sinusit)") ~ "Respiratory Conditions",
#     str_detect(condition, "pmh_(stroke|alz|cog|tbi|seize|mig|ms|par|hun|perneur|apnea)") ~ "Neurological Conditions",
#     str_detect(condition, "pmh_(dep|schizo|bip|ptsd|panic|add|dys)") ~ "Psychiatric Conditions",
#     str_detect(condition, "pmh_(alc|sub)") ~ "Substance Abuse Disorders",
#     str_detect(condition, "pmh_(arthrit|broken|fm|back|spine|tmj)") ~ "Musculoskeletal Conditions",
#     str_detect(condition, "pmh_(fibro|mcs|cfs)") ~ "Chronic Multisymptom Disorder",
#     str_detect(condition, "pmh_(kidney|prostate|kidstone)") ~ "Genitourinary System Diseases",
#     str_detect(condition, "pmh_(ulcer|ibs|gall|pancrea)") ~ "Gastrointestinal Conditions",
#     str_detect(condition, "pmh_(pneumo|hiv|men|lyme|hepat|mono)") ~ "Infectious Conditions",
#     str_detect(condition, "pmh_(allerg|lupus)") ~ "Immune System Disorders",
#     condition == "pmh_heatst" ~ "Heatstroke and Sunstroke",
#     condition == "pmh_hearloss" ~ "Sensory Organ Disease",
#     condition == "pmh_nutr" ~ "Nutritional and Metabolic Diseases",
#     str_detect(condition, "pmh_(diabet|thyroid)") ~ "Endocrine Disorders",
#     str_detect(condition, "pmh_(hives|psoria)") ~ "Dermatological Conditions",
#     str_detect(condition, "pmh_(bleed|sickle|anemia)") ~ "Hematological Conditions",
#     str_detect(condition, "pmh_(leuk|cancski|cancoth|melan)") ~ "Neoplasms",
#     TRUE ~ "None"
#   )) %>%
#   mutate(condition = case_when(
#     condition == "pmh_h_bp" ~ "High Blood Pressure",
#     condition == "pmh_l_bp" ~ "Low Blood Pressure",
#     condition == "pmh_heart" ~ "Heart Murmur Or Mitral Valve Prolapse",
#     condition == "pmh_col" ~ "Angina, Heart Attack Or Coronary Heart Disease",
#     condition == "pmh_arryth" ~ "Arrhythmia Or Irregular Heart Beat",
#     condition == "pmh_perneur" ~ "Peripheral Neuropathy",
#     condition == "pmh_spine" ~ "Spinal Cord Injury",
#     condition == "pmh_asthma" ~ "Asthma Or Reactive Airways Disease",
#     condition == "pmh_pneumo" ~ "Pneumonia Or Pleurisy",
#     condition == "pmh_apnea" ~ "Sleep Apnea",
#     condition == "pmh_allerg" ~ "Allergies, Nasal Polyps Or Hay Fever",
#     condition == "pmh_kidney" ~ "Kidney Or Bladder Stones",
#     condition == "pmh_arthrit" ~ "Arthritis Or Gout",
#     condition == "pmh_broken" ~ "Broken Bones Or Joint Or Back Surgery",
#     condition == "pmh_anemia" ~ "Anemia Or Thalassemia",
#     condition == "pmh_leuk" ~ "Leukemia Or Lymphoma Or Hodgkin's Disease",
#     condition == "pmh_hepat" ~ "Hepatitis Or Liver Disease Or Cirrhosis",
#     condition == "pmh_pancrea" ~ "Pancreatitis Or Colitis",
#     condition == "pmh_heatst" ~ "Heat Exhaustion Or Heat Stroke Or Frostbite",
#     condition == "pmh_fm" ~ "Fibromyalgia",
#     condition == "pmh_mcs" ~ "Multiple Chemical Sensitivity",
#     condition == "pmh_lupus" ~ "Lupus Or Sarcoidosis",
#     condition == "pmh_dep" ~ "Depression",
#     condition == "pmh_schizo" ~ "Schizophrenia",
#     condition == "pmh_hives" ~ "Hives Or Allergic Dermatitis",
#     condition == "pmh_cancski" ~ "Skin Cancer Other Than Melanoma",
#     condition == "pmh_cancoth" ~ "Other Cancer",
#     condition == "pmh_chf" ~ "Congestive Heart Failure Or Fluid On The Lungs",
#     condition == "pmh_stroke" ~ "Stroke Or Mini Stroke Or TIA",
#     condition == "pmh_alz" ~ "Dementia Or Alzheimer's Disease",
#     condition == "pmh_cog" ~ "Cognitive Disorder",
#     condition == "pmh_tbi" ~ "Brain Injury",
#     condition == "pmh_men" ~ "Meningitis",
#     condition == "pmh_hun" ~ "Huntington's Disease",
#     condition == "pmh_par" ~ "Parkinson's Disease",
#     condition == "pmh_clot" ~ "Poor Circulation, Varicose Veins Or Blood Clots",
#     condition == "pmh_seize" ~ "Seizures",
#     condition == "pmh_mig" ~ "Migraines",
#     condition == "pmh_ms" ~ "Multiple Sclerosis",
#     condition == "pmh_lung" ~ "Emphysema Or Chronic Lung Disease",
#     condition == "pmh_bronch" ~ "Chronic Bronchitis",
#     condition == "pmh_asbest" ~ "Silicosis Or Asbestosis",
#     condition == "pmh_sinusit" ~ "Chronic Sinusitis",
#     condition == "pmh_prostate" ~ "Benign Prostatic Hypertrophy",
#     condition == "pmh_hearloss" ~ "Hearing Loss",
#     condition == "pmh_kidstone" ~ "Repeated Kidney Or Bladder Infections",
#     condition == "pmh_back" ~ "Chronic Back Pain, Sciatica Or Herniated Disk",
#     condition == "pmh_hiv" ~ "HIV Positive Or AIDS",
#     condition == "pmh_sickle" ~ "Sickle Cell Disease Or Trait",
#     condition == "pmh_bleed" ~ "Problems With Blood Clotting Or Bleeding",
#     condition == "pmh_nutr" ~ "Malnutrition",
#     condition == "pmh_ulcer" ~ "Ulcer Or Reflux Or Hiatal Hernia",
#     condition == "pmh_gall" ~ "Gall Bladder Disease Or Stones",
#     condition == "pmh_ibs" ~ "Irritable Bowel Syndrome",
#     condition == "pmh_diabet" ~ "Diabetes Or High Blood Sugar",
#     condition == "pmh_cfs" ~ "Chronic Fatigue Syndrome",
#     condition == "pmh_lyme" ~ "Lyme's Disease",
#     condition == "pmh_thyroid" ~ "Thyroid Disease Or Goiter",
#     condition == "pmh_ptsd" ~ "Post Traumatic Stress Disorder",
#     condition == "pmh_panic" ~ "Panic Attacks Or Anxiety Disorder",
#     condition == "pmh_bip" ~ "Bipolar Disorder",
#     condition == "pmh_alc" ~ "Alcohol Abuse Or Alcoholism",
#     condition == "pmh_sub" ~ "Substance Abuse",
#     condition == "pmh_add" ~ "Attention Deficit Hyperactivity Disorder",
#     condition == "pmh_dys" ~ "Learning Disorder Or Dyslexia",
#     condition == "pmh_psoria" ~ "Psoriasis Or Eczema",
#     condition == "pmh_melan" ~ "Melanoma",
#     condition == "pmh_tmj" ~ "Tempero Mandibular Joint Syndrome",
#     condition == "pmh_mono" ~ "Mononucleosis",
#     condition == "pmh_other" ~ "Other"
#   ))
# 
# # Add a unique row number within each `id` group to each data frame
# everDxDat_numbered <- everDxDat %>%
#   group_by(id) %>%
#   mutate(row_id = row_number()) %>%
#   ungroup()
# 
# healthSxDat_numbered <- rosHealthSxDat %>%
#   group_by(id) %>%
#   mutate(row_id = row_number()) %>%
#   ungroup()
# 
# # Join using `id` and `row_id`
# joinedEverDxDatHealthSxDat <- everDxDat_numbered %>%
#   full_join(healthSxDat_numbered, by = c("id", "row_id"))
# 
# write_csv(joinedEverDxDatHealthSxDat, "joinedEverDxDatHealthSxDat.csv")
# 
# # Military Exposures Data ----
# militaryExposuresBinaryDat <- intakeDat %>%
#   select(id, starts_with("expo")) %>%
#   select(id, 2:"expo_meta_c") %>%
#   mutate_all(~na_if(., 999)) %>%
#   select(-ends_with("_i"), -ends_with("_f"), -ends_with("_c")) %>%
#   pivot_longer(2:36, names_to = "exposureName", values_to = "exposureYesNo") %>%
#   distinct()
# 
# militaryExposuresFreqDat <- intakeDat %>%
#   select(id, starts_with("expo")) %>%
#   select(id, 2:"expo_meta_c") %>%
#   mutate_all(~na_if(., 999)) %>%
#   select(id, ends_with("_f")) %>%
#   pivot_longer(2:36, names_to = "exposureName", values_to = "exposureFreq") %>%
#   distinct()
# 
# militaryExposuresIntensityDat <- intakeDat %>%
#   select(id, starts_with("expo")) %>%
#   select(id, 2:"expo_meta_c") %>%
#   mutate_all(~na_if(., 999)) %>%
#   select(id, ends_with("_i")) %>%
#   pivot_longer(2:36, names_to = "exposureName", values_to = "exposureIntensity") %>%
#   distinct()
# 
# militaryExposuresConcernDat <- intakeDat %>%
#   select(id, starts_with("expo")) %>%
#   select(id, 2:"expo_meta_c") %>%
#   mutate_all(~na_if(., 999)) %>%
#   select(id, ends_with("_c")) %>%
#   pivot_longer(2:36, names_to = "exposureName", values_to = "exposureConcern") %>%
#   distinct()
# 
# ## Combine All Military Exposures ----
# militaryExposureDat <- bind_cols(
#   militaryExposuresBinaryDat,
#   militaryExposuresFreqDat[, !names(militaryExposuresFreqDat) %in% c("id", "exposureName")],
#   militaryExposuresIntensityDat[, !names(militaryExposuresIntensityDat) %in% c("id", "exposureName")],
#   militaryExposuresConcernDat[, !names(militaryExposuresConcernDat) %in% c("id", "exposureName")]
# )
# 
# # Rename exposureName
# militaryExposureDat <- militaryExposureDat %>%
#   #mutate(across(c(exposureFreq, exposureIntensity, exposureConcern), ~replace_na(., 0))) %>%
#   mutate(exposureName = case_when(
#     exposureName == "expo_herb" ~ "Agent Orange or Other Herbicides",
#     exposureName == "expo_rada" ~ "Radar/Microwaves",
#     exposureName == "expo_vibr" ~ "Excessive Vibration",
#     exposureName == "expo_poll" ~ "Industrial Pollution",
#     exposureName == "expo_burn" ~ "Burning Trash or Feces",
#     exposureName == "expo_anim" ~ "Animal Bodies",
#     exposureName == "expo_anth" ~ "Anthrax Vaccine",
#     exposureName == "expo_asbe" ~ "Asbestos",
#     exposureName == "expo_biow" ~ "Biological Warfare Agents",
#     exposureName == "expo_bite" ~ "Animal Bites",
#     exposureName == "expo_bloo" ~ "Human Blood/Bodily Fluids",
#     exposureName == "expo_chem" ~ "Chemical Solvents",
#     exposureName == "expo_chlo" ~ "Chlorine Gas",
#     exposureName == "expo_du" ~ "Depleted Uranium",
#     exposureName == "expo_fogo" ~ "Fog Oils",
#     exposureName == "expo_heat" ~ "Tent Heater Smoke",
#     exposureName == "expo_infe" ~ "Infectious Disease",
#     exposureName == "expo_inse" ~ "Insect Bites",
#     exposureName == "expo_lase" ~ "Lasers",
#     exposureName == "expo_meta" ~ "Embedded Metal Fragments",
#     exposureName == "expo_mopp" ~ "Chemical Alarms/MOPP4",
#     exposureName == "expo_napp" ~ "Nerve Agent Antidotes",
#     exposureName == "expo_nois" ~ "Loud Noises",
#     exposureName == "expo_npw" ~ "Ingestion of Contaminated Water",
#     exposureName == "expo_oilf" ~ "Oil Fires",
#     exposureName == "expo_pain" ~ "Paint",
#     exposureName == "expo_pest" ~ "Pesticides",
#     exposureName == "expo_petr" ~ "Petrochemical Fuels or Fumes",
#     exposureName == "expo_prop" ~ "Prophylactic Meds",
#     exposureName == "expo_radi" ~ "Ionizing Radiation",
#     exposureName == "expo_sand" ~ "Sand/Dust",
#     exposureName == "expo_vacc" ~ "Vaccinations",
#     exposureName == "expo_wash" ~ "Bathed in Contaminated Water",
#     exposureName == "expo_wche" ~ "Chemical Weapons",
#     exposureName == "expo_wexh" ~ "Weapons Exhaust",
#     TRUE ~ exposureName
#   ))
# 
# # If exposureYesNo is NA but either exposureFreq, exposureIntensity, exposureConcern are not NA, replace exposureYesNo with 1 (yes)
# # Replace NA in exposureFreq, exposureIntensity, exposureConcern with 0 if exposureYesNo = 0
# militaryExposureDat <- militaryExposureDat %>%
#   mutate(exposureYesNo = case_when(
#     is.na(exposureYesNo) & (!is.na(exposureFreq) | !is.na(exposureIntensity) | !is.na(exposureConcern)) ~ 1,
#     TRUE ~ exposureYesNo
#   )) %>%
#   mutate(across(c(exposureFreq, exposureIntensity, exposureConcern), ~ case_when(
#     exposureYesNo == 0 & is.na(.) ~ 0,
#     TRUE ~ .
#   )))
# 
# # Write to csv
# write_csv(militaryExposureDat, "militaryExposureDat.csv")
# 
# # Military Exposures Extras ----
# extraMilitaryExposuresDat <- intakeDat %>%
#   select(id, expo_othm:expo_moos_date) %>%
#   mutate(
#     across(everything(), ~replace(., . %in% c(-99, 999, "-99", "999"), NA)),
#     across(where(is.character),
#            ~str_replace_all(., "\"+", "") %>%
#              str_trim()),
#     across(ends_with("_date"),
#            ~dmy_hms(na_if(., "01Jan9999 0:00:00")) %>%
#              format("%Y-%m-%d %H:%M:%S"))
#   )
# 
# # Write to csv
# write_csv(extraMilitaryExposuresDat, "extraMilitaryExposuresDat.csv")
# 
# # Civilian Exposures Data ----
# # Extract binary data
# civilianExposuresBinaryDat <- intakeDat %>%
#   select(
#     id,
#     expo_duso, expo_duss, expo_cgas, expo_metf, expo_exhf
#   ) %>%
#   mutate_all(~na_if(., 999)) %>%
#   pivot_longer(
#     cols = c(expo_duso, expo_duss, expo_cgas, expo_metf, expo_exhf),
#     names_to = "exposureName",
#     values_to = "exposureYesNo"
#   ) %>%
#   distinct()
# 
# # Extract frequency data
# civilianExposuresFreqDat <- intakeDat %>%
#   select(
#     id,
#     expo_duso_f, expo_duss_f, expo_cgas_f, expo_metf_f, expo_exhf_f
#   ) %>%
#   mutate_all(~na_if(., 999)) %>%
#   pivot_longer(
#     cols = c(expo_duso_f, expo_duss_f, expo_cgas_f, expo_metf_f, expo_exhf_f),
#     names_to = "exposureName",
#     values_to = "exposureFreq"
#   ) %>%
#   distinct()
# 
# # Extract intensity data
# civilianExposuresIntensityDat <- intakeDat %>%
#   select(
#     id,
#     expo_duso_i, expo_duss_i, expo_cgas_i, expo_metf_i, expo_exhf_i
#   ) %>%
#   mutate_all(~na_if(., 999)) %>%
#   pivot_longer(
#     cols = c(expo_duso_i, expo_duss_i, expo_cgas_i, expo_metf_i, expo_exhf_i),
#     names_to = "exposureName",
#     values_to = "exposureIntensity"
#   ) %>%
#   distinct()
# 
# # Extract concern data
# civilianExposuresConcernDat <- intakeDat %>%
#   select(
#     id,
#     expo_duso_c, expo_duss_c, expo_cgas_c, expo_metf_c, expo_exhf_c
#   ) %>%
#   mutate_all(~na_if(., 999)) %>%
#   pivot_longer(
#     cols = c(expo_duso_c, expo_duss_c, expo_cgas_c, expo_metf_c, expo_exhf_c),
#     names_to = "exposureName",
#     values_to = "exposureConcern"
#   ) %>%
#   distinct()
# 
# ## Combine all civilian exposures data ----
# civilianExposuresDat <- bind_cols(
#   civilianExposuresBinaryDat,
#   civilianExposuresFreqDat[, !names(civilianExposuresFreqDat) %in% c("id", "exposureName")],
#   civilianExposuresIntensityDat[, !names(civilianExposuresIntensityDat) %in% c("id", "exposureName")],
#   civilianExposuresConcernDat[, !names(civilianExposuresConcernDat) %in% c("id", "exposureName")]
# )
# 
# # Replace NA with 0 for specific columns
# civilianExposuresDat <- civilianExposuresDat %>%
#   mutate(across(c(exposureFreq, exposureIntensity, exposureConcern), ~replace_na(., 0))) %>%
#   mutate(exposureName = case_when(
#     exposureName == "expo_duso" ~ "Dust from Baking Flours or Grains",
#     exposureName == "expo_duss" ~ "Dust from Rock or Sand",
#     exposureName == "expo_cgas" ~ "Chemical Gases or Vapors",
#     exposureName == "expo_metf" ~ "Metal Fumes",
#     exposureName == "expo_exhf" ~ "Exhaust Fumes",
#     TRUE ~ exposureName
#   ))
# 
# # Write to CSV
# write_csv(civilianExposuresDat, "civilianExposuresDat.csv")
# 
# # Civilian Exposures Extras ----
# extraCivilianExposuresDat <- intakeDat %>%
#   select(id, expo_othc_spec, expo_othc, expo_othc_f, expo_othc_i,
#          expo_othc_c, expo_resp, expo_resp_spec, expo_medi,
#          expo_medi_spec, expo_work, expo_work_spec) %>%
#   mutate(
#     across(everything(), ~replace(., . %in% c(-99, 999, "-99", "999"), NA)),
#     across(where(is.character),
#            ~str_replace_all(., "\"+", "") %>%
#              str_trim()),
#     across(ends_with("_date"),
#            ~dmy_hms(na_if(., "01Jan9999 0:00:00")) %>%
#              format("%Y-%m-%d %H:%M:%S")),
#     expo_othc_spec = case_when(
#       expo_othc_spec == "0" ~ "No",
#       expo_othc_spec == "1" ~ "Yes",
#       expo_othc_spec == "2" ~ "Don't Know"
#     )
#   )
# 
# # Write to csv
# write_csv(extraCivilianExposuresDat, "extraCivilianExposuresDat.csv")
# 
# # Score Self-Report Measures ----
# ## Select Vars of Interest: Fibro -- Widespread Pain Index (WPI) ----
# fibroDat <- intakeDat %>%
#   select(id, starts_with("fibro")) %>%
#   select(-fibro_length) %>%
#   pivot_longer(2:20, names_to = "item", values_to = "trueFalse") %>%
#   mutate(trueFalse = as.numeric(trueFalse)) %>%
#   group_by(id) %>%
#   mutate(wpiTotal = sum(trueFalse, na.rm = T)) %>%
#   ungroup() %>%
#   select(id, wpiTotal) %>%
#   distinct()
# 
# ## Select Vars of Interest: Pain ----
# painDat <- intakeDat %>%
#   select(id, starts_with("vapain")) %>%
#   rename(painNow = vapain_01, averagePain = vapain_02,
#          bestPain = vapain_03, worstPain = vapain_04) %>%
#   mutate_at(vars(painNow, averagePain, bestPain, worstPain), ~ na_if(., 999))
# 
# ## Select Vars of Interest: PCL-C ----
# pclcDat <- intakeDat %>%
#   select(id, starts_with("pcl")) %>%
#   pivot_longer(2:18, names_to = "pclItems", values_to = "pclItemScore") %>%
#   mutate(pclItemScore = pclItemScore + 1) %>%
#   mutate(pclItemScore = na_if(pclItemScore, 999),
#          pclItemScore = na_if(pclItemScore, 1000)
#   ) %>%
#   group_by(id) %>%
#   filter(!any(is.na(pclItemScore))) %>%
#   mutate(pclTotalScore = sum(pclItemScore, na.rm = T)) %>%
#   ungroup() %>%
#   select(id, pclTotalScore) %>%
#   distinct()
# 
# # summary(pclcDat$pclItemScore)
# # hist(pclcDat$pclTotalScore, breaks = 100)
# # summary(pclcDat$pclTotalScore)
# # sum(is.na(pclcDat$pclTotalScore))
# 
# ## Select Vars of Interest: PHQ-15 (Somatic Symptom Severity) ----
# #
# # # men (0-28)
# # intakeDat %>%
# #   select(id, gender = dem_sex, starts_with("phq15")) %>%
# #   filter(gender == 1) %>%
# #   select(-phq15_d) %>%
# #   pivot_longer(3:16, names_to = "phq15Items", values_to = "phq15ItemScore") %>%
# #   mutate(phq15ItemScore = na_if(phq15ItemScore, 999)) %>%
# #   group_by(id) %>%
# #   filter(!any(is.na(phq15ItemScore))) %>%
# #   mutate(phq15TotalScore = sum(phq15ItemScore, na.rm = T)) %>%
# #   ungroup() %>%
# #   select(id, phq15TotalScore) %>%
# #   distinct()
# #
# # # women (0-30)
# # intakeDat %>%
# #   select(id, gender = dem_sex, starts_with("phq15")) %>%
# #   filter(gender == 0) %>%
# #   select(-phq15_d) %>%
# #   pivot_longer(3:16, names_to = "phq15Items", values_to = "phq15ItemScore") %>%
# #   mutate(phq15ItemScore = na_if(phq15ItemScore, 999)) %>%
# #   group_by(id) %>%
# #   filter(!any(is.na(phq15ItemScore))) %>%
# #   mutate(phq15TotalScore = sum(phq15ItemScore, na.rm = T)) %>%
# #   ungroup() %>%
# #   select(id, phq15TotalScore) %>%
# #   distinct()
# 
# # Together, excluding mentrual cramps item (0-28)
# phq15Dat <- intakeDat %>%
#   select(id, gender = dem_sex, starts_with("phq15")) %>%
#   select(-phq15_d) %>%
#   pivot_longer(3:16, names_to = "phq15Items", values_to = "phq15ItemScore") %>%
#   mutate(phq15ItemScore = na_if(phq15ItemScore, 999)) %>%
#   mutate(phq15ItemScore = ifelse(phq15ItemScore > 2, 2, phq15ItemScore)) %>%
#   group_by(id) %>%
#   filter(!any(is.na(phq15ItemScore))) %>%
#   mutate(phq15TotalScore = sum(phq15ItemScore, na.rm = T)) %>%
#   ungroup() %>%
#   select(id, phq15TotalScore) %>%
#   distinct()
# 
# # hist(phq15Dat$phq15ItemScore, breaks = 50)
# # hist(phq15Dat$phq15TotalScore)
# # summary(phq15Dat$phq15TotalScore)
# 
# ## Select Vars of Interest: DRRI (Unit Cohesion/Social Support)  ----
# unitCohesionDat <- intakeDat %>%
#   select(id, starts_with("drri")) %>%
#   pivot_longer(2:13, names_to = "drriItems", values_to = "drriItemScore") %>%
#   mutate(drriItemScore = na_if(drriItemScore, 999)) %>%
#   group_by(id) %>%
#   filter(!any(is.na(drriItemScore))) %>%
#   mutate(drriTotalScore = sum(drriItemScore, na.rm = T)) %>%
#   ungroup() %>%
#   select(id, drriTotalScore) %>%
#   distinct()
# 
# ## Select Vars of Interest: IBS (DO TOGETHER) ----
# # # men ()
# # ibsDat <- intakeDat %>%
# #   select(id, gender = dem_sex, starts_with("ibs")) %>%
# #   filter(gender == 1) %>%
# #   select(-ibs_02) %>%
# #   pivot_longer(3:11, names_to = "ibsItems", values_to = "ibsItemScore") %>%
# #   mutate(ibsItemScore = na_if(ibsItemScore, 999)) %>%
# #   group_by(id) %>%
# #   filter(!any(is.na(ibsItemScore))) %>%
# #   mutate(ibsTotalScore = sum(ibsItemScore, na.rm = T)) %>%
# #   ungroup() %>%
# #   select(id, ibsTotalScore) %>%
# #   distinct()
# #
# # # women (0-30)
# # intakeDat %>%
# #   select(id, gender = dem_sex) %>%
# #   group_by(gender) %>%
# #   summarize(n())
# #
# # intakeDat %>%
# #   select(id, gender = dem_sex, ibs_02) %>%
# #   filter(gender == 1) %>%
# #   mutate(ibs_02 = na_if(ibs_02, 999)) %>%
# #   group_by(ibs_02) %>%
# #   summarise(n())
# 
# # Together, excluding mentrual cramps item ()
# ibsDatTogether <- intakeDat %>%
#   select(id, gender = dem_sex, starts_with("ibs")) %>%
#   select(-ibs_02, -ibs_03) %>%
#   pivot_longer(3:10, names_to = "ibsItems", values_to = "ibsItemScore") %>%
#   mutate(ibsItemScore = na_if(ibsItemScore, 999)) %>%
#   group_by(id) %>%
#   filter(!any(is.na(ibsItemScore))) %>%
#   mutate(ibsTotalScore = sum(ibsItemScore, na.rm = T)) %>%
#   ungroup() %>%
#   select(id, ibsTotalScore) %>%
#   distinct()
# 
# ## Select Vars of Interest: PSQI ----
# # sleepQualityDat <- intakeDat %>%
# #   select(id, sleepQuality = psqi_06) %>%
# #   mutate(sleepQuality = na_if(sleepQuality, 999)) %>%
# #   filter(!is.na(sleepQuality))
# 
# sleepDat <- psqiDat %>%
#   select(id, sleepQuality = psqi_slpqual_score,
#          timeInBed = psqi_diff_hours,
#          sleepDuration = psqi_04,
#          sleepLatency = psqi_laten_score,
#          sleepEfficiency = psqi_hse,
#          psqiTotal = psqi_total) %>%
#   mutate(
#     sleepEfficiency = ifelse(is.infinite(sleepEfficiency) |
#                                sleepEfficiency > 100, NA, sleepEfficiency)
#   )
# 
# ## Select Vars of Interest: CES Combat Exposure Scale ----
# cesDat <- intakeDat %>%
#   select(id, starts_with("ces")) %>%
#   mutate_at(2:8, ~ na_if(., 999)) %>%
#   mutate_at(2:8, ~ . + 1) %>%
#   mutate(ces_01 = (ces_01 - 1)*2,
#          ces_02 = ces_02 - 1,
#          ces_03 = case_when(
#            ces_03 <= 4 ~ (ces_03 - 1)*2,
#            ces_03 == 5 ~ (ces_03 - 2)*2
#          ),
#          ces_04 = case_when(
#            ces_04 <= 4 ~ ces_04 - 1,
#            ces_04 == 5 ~ ces_04 - 2
#          ),
#          ces_05 = ces_05 - 1,
#          ces_06 = (ces_06 - 1)*2,
#          ces_07 = (ces_07 - 1)*2
#   ) %>%
#   pivot_longer(2:8, names_to = "cesItems", values_to = "cesItemScore") %>%
#   group_by(id) %>%
#   filter(!any(is.na(cesItemScore))) %>%
#   mutate(cesTotalScore = sum(cesItemScore, na.rm = T)) %>%
#   ungroup() %>%
#   select(id, cesTotalScore) %>%
#   distinct()
# 
# ## Select Vars of Interest: Migraine Specific Quality of Life Questionnaire (Total MSQ) ----
# msqDat <- intakeDat %>%
#   select(id, starts_with("msq")) %>%
#   mutate_at(2:15, ~ na_if(., 999)) %>%
#   mutate_at(2:15, ~ . + 1) %>%
#   pivot_longer(2:15, names_to = "msqItems", values_to = "msqItemScore") %>%
#   group_by(id) %>%
#   filter(!any(is.na(msqItemScore))) %>%
#   mutate(msqTotalScore = sum(msqItemScore, na.rm = T)) %>%
#   ungroup() %>%
#   select(id, msqTotalScore) %>%
#   distinct() %>%
#   mutate(msqTotalScore = (msqTotalScore - 14) / (84 - 14) * 100 )
# 
# ## Select vars of interest: PHQ-8 ----
# phq8Dat <- intakeDat %>%
#   select(id, phq_02a, phq_02b, phq_02c, phq_02d,
#          phq_02e, phq_02f, phq_02g, phq_02h) %>%
#   pivot_longer(2:9, names_to = "phq8Items", values_to = "phq8ItemScore") %>%
#   mutate(phq8ItemScore = na_if(phq8ItemScore, 999)) %>%
#   group_by(id) %>%
#   mutate(phq8TotalScore = sum(phq8ItemScore)) %>%
#   ungroup() %>%
#   select(id, phq8TotalScore) %>%
#   distinct()
# 
# ## Select vars of interest: PHQ-PD ----
# phqPdDat <- intakeDat %>%
#   select(id, phq_03a, phq_03b, phq_03c, phq_03d, phq_04) %>%
#   pivot_longer(2:6, names_to = "phqPdItems", values_to = "phqPdItemScore") %>%
#   group_by(id) %>%
#   mutate(phqPdTotalScore = sum(phqPdItemScore)) %>%
#   mutate(phqPdTotalScore = case_when(
#     any(phqPdItems == "phq_03a" & phqPdItemScore == FALSE) ~ 0,
#     TRUE ~ phqPdTotalScore
#   )) %>%
#   ungroup() %>%
#   mutate(phqPdCutoffs = case_when(
#     phqPdTotalScore == 0 ~ "Pt Denied Pd Sx",
#     phqPdTotalScore > 0 & phqPdTotalScore < 5 ~ "Subclinical PD",
#     phqPdTotalScore == 5 ~ "Probable PD",
#     TRUE ~ NA_character_
#   )) %>%
#   group_by(id) %>%
#   filter(!any(is.na(phqPdItemScore))) %>%
#   ungroup() %>%
#   select(id, phqPdTotalScore) %>%
#   distinct()
# 
# ## Select vars of interest: GAD-7 (1) ----
# gadDat <- intakeDat %>%
#   select(id, gadScore = phq_05a) %>%
#   mutate(gadScore = if_else(gadScore %in% c(999, -99), NA_real_, gadScore))
# 
# ## SF-36 Subscales Data ----
# newNames <- c("one", "two", "three", "four", "five", "six", "seven",
#               "eight", "nine", "ten", "eleven", "twelve", "thirteen",
#               "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
#               "nineteen", "twenty", "twentyOne", "twentyTwo", "twentyThree",
#               "twentyFour", "twentyFive", "twentySix", "twentySeven",
#               "twentyEight", "twentyNine", "thirty", "thirtyOne", "thirtyTwo",
#               "thirtyThree", "thirtyFour", "thirtyFive", "thirtySix")
# 
# sf36Dat <- intakeDat %>%
#   select(id, sf36_01:sf36_11a) %>%
#   select(id, sf36_01, sf36_11a, sf36_02a:sf36_10d) %>% # items out of order
#   rename_with( ~ newNames, .cols = 2:37) %>%
#   mutate(across(-two, ~ . + 1)) %>%
#   pivot_longer(2:37, names_to = "sf36Items", values_to = "sf36ItemsScore") %>%
#   mutate(sf36ItemsScore = na_if(sf36ItemsScore, 999)) %>%
#   mutate(sf36ItemsScore = na_if(sf36ItemsScore, 1000)) %>%
#   mutate(
#     sf36ItemsScore = case_when(
#       sf36Items %in% c("one", "two", "twenty", "twentyTwo",
#                        "thirtyFour", "thirtySix") &
#         sf36ItemsScore == 1 ~ 100,
#       sf36Items %in% c("one", "two", "twenty", "twentyTwo",
#                        "thirtyFour", "thirtySix") &
#         sf36ItemsScore == 2 ~ 75,
#       sf36Items %in% c("one", "two", "twenty", "twentyTwo",
#                        "thirtyFour", "thirtySix") &
#         sf36ItemsScore == 3 ~ 50,
#       sf36Items %in% c("one", "two", "twenty", "twentyTwo",
#                        "thirtyFour", "thirtySix") &
#         sf36ItemsScore == 4 ~ 25,
#       sf36Items %in% c("one", "two", "twenty", "twentyTwo",
#                        "thirtyFour", "thirtySix") &
#         sf36ItemsScore == 5 ~ 0,
# 
#       sf36Items %in% c("three", "four", "five", "six", "seven",
#                        "eight","nine", "ten", "eleven", "twelve") & sf36ItemsScore == 1 ~ 0,
#       sf36Items %in% c("three", "four", "five", "six", "seven",
#                        "eight", "nine", "ten", "eleven", "twelve") & sf36ItemsScore == 2 ~ 50,
#       sf36Items %in% c("three", "four", "five", "six",
#                        "seven", "eight", "nine", "ten", "eleven", "twelve") & sf36ItemsScore == 3 ~ 100,
# 
#       sf36Items %in% c("thirteen", "fourteen", "fifteen", "sixteen",
#                        "seventeen", "eighteen", "nineteen") & sf36ItemsScore == 1 ~ 0,
#       sf36Items %in% c("thirteen", "fourteen", "fifteen", "sixteen",
#                        "seventeen", "eighteen", "nineteen") & sf36ItemsScore > 1  ~ 100,
# 
#       sf36Items %in% c("twentyOne", "twentyThree", "twentySix",
#                        "twentySeven", "thirty") & sf36ItemsScore == 1 ~ 100,
#       sf36Items %in% c("twentyOne", "twentyThree",
#                        "twentySix", "twentySeven", "thirty") & sf36ItemsScore == 2 ~ 80,
#       sf36Items %in% c("twentyOne", "twentyThree", "twentySix",
#                        "twentySeven", "thirty") & sf36ItemsScore == 3 ~ 60,
#       sf36Items %in% c("twentyOne", "twentyThree", "twentySix",
#                        "twentySeven", "thirty") & sf36ItemsScore == 4 ~ 40,
#       sf36Items %in% c("twentyOne", "twentyThree",
#                        "twentySix", "twentySeven", "thirty") & sf36ItemsScore == 5 ~ 20,
#       sf36Items %in% c("twentyOne", "twentyThree", "twentySix",
#                        "twentySeven", "thirty") & sf36ItemsScore == 6 ~ 0,
# 
#       sf36Items %in% c("twentyFour", "twentyFive",
#                        "twentyEight", "twentyNine", "thirtyOne") & sf36ItemsScore == 1 ~ 0,
#       sf36Items %in% c("twentyFour", "twentyFive",
#                        "twentyEight", "twentyNine", "thirtyOne") & sf36ItemsScore == 2 ~ 20,
#       sf36Items %in% c("twentyFour", "twentyFive", "twentyEight",
#                        "twentyNine", "thirtyOne") & sf36ItemsScore == 3 ~ 40,
#       sf36Items %in% c("twentyFour", "twentyFive", "twentyEight",
#                        "twentyNine", "thirtyOne") & sf36ItemsScore == 4 ~ 60,
#       sf36Items %in% c("twentyFour", "twentyFive", "twentyEight",
#                        "twentyNine", "thirtyOne") & sf36ItemsScore == 5 ~ 80,
#       sf36Items %in% c("twentyFour", "twentyFive",
#                        "twentyEight", "twentyNine", "thirtyOne") & sf36ItemsScore == 6 ~ 100,
# 
#       sf36Items %in% c("thirtyTwo", "thirtyThree",
#                        "thirtyFive") & sf36ItemsScore == 1 ~ 0,
#       sf36Items %in% c("thirtyTwo", "thirtyThree",
#                        "thirtyFive") & sf36ItemsScore == 2 ~ 25,
#       sf36Items %in% c("thirtyTwo", "thirtyThree",
#                        "thirtyFive") & sf36ItemsScore == 3 ~ 50,
#       sf36Items %in% c("thirtyTwo", "thirtyThree",
#                        "thirtyFive") & sf36ItemsScore == 4 ~ 75,
#       sf36Items %in% c("thirtyTwo", "thirtyThree",
#                        "thirtyFive") & sf36ItemsScore == 5 ~ 100
#     )) %>%
#   mutate(subscaleName = case_when(
#     sf36Items == "two" ~ "sf36HealthChange",
#     sf36Items %in% c("three", "four", "five", "six", "seven",
#                      "eight", "nine", "ten", "eleven", "twelve") ~ "sf36PhysicalFunctioning",
#     sf36Items %in% c("thirteen", "fourteen", "fifteen", "sixteen") ~ "sf36LimitationsDueToPhysicalHealth",
#     sf36Items %in% c("seventeen", "eighteen", "nineteen") ~ "sf36LimitationsDueToEmotionalProblems",
#     sf36Items %in% c("twentyThree", "twentySeven", "twentyNine", "thirtyOne") ~ "sf36EnergyFatigue",
#     sf36Items %in% c("twentyFour", "twentyFive", "twentySix", "twentyEight", "thirty") ~ "sf36EmotionalWellbeing",
#     sf36Items %in% c("twenty", "thirtyTwo") ~ "sf36SocialFunctioning",
#     sf36Items %in% c("twentyOne", "twentyTwo") ~ "sf36Pain",
#     sf36Items %in% c("one", "thirtyThree", "thirtyFour", "thirtyFive", "thirtySix") ~ "sf36GeneralHealth"
#   )) %>%
#   group_by(id, subscaleName) %>%
#   summarise(meanSubscaleScore = mean(sf36ItemsScore)) %>%
#   ungroup()
# 
# sf36Dat <- sf36Dat %>% pivot_wider(names_from = "subscaleName", values_from = "meanSubscaleScore")
# 
# # sf36Dat %>%
# #   mutate(outcomesStudyMeanRefs = case_when(
# #     subscaleName == "Health Change" ~ 59.14,
# #     subscaleName == "Physical Functioning" ~ 70.61,
# #     subscaleName == "Limitations Due to Physical Health" ~ 52.97,
# #     subscaleName == "Limitations Due to Emotional Problems" ~ 65.78,
# #     subscaleName == "Energy/Fatigue" ~ 52.15,
# #     subscaleName == "Emotional Wellbeing" ~ 70.38,
# #     subscaleName == "Social Functioning" ~ 78.77,
# #     subscaleName == "Pain" ~ 70.77,
# #     subscaleName == "General Health" ~ 56.99
# #   )) %>%
# #   mutate(outcomesStudySdRefs = case_when(
# #     subscaleName == "Health Change" ~ 23.12,
# #     subscaleName == "Physical Functioning" ~ 27.42,
# #     subscaleName == "Limitations Due to Physical Health" ~ 40.78,
# #     subscaleName == "Limitations Due to Emotional Problems" ~ 40.71,
# #     subscaleName == "Energy/Fatigue" ~ 22.39,
# #     subscaleName == "Emotional Wellbeing" ~ 21.97,
# #     subscaleName == "Social Functioning" ~ 25.43,
# #     subscaleName == "Pain" ~ 25.46,
# #     subscaleName == "General Health" ~ 21.11
# #   ))
# 
# ## Chronic Fatigue Syndrome Data ----
# #https://pophealthmetrics.biomedcentral.com/articles/10.1186/1478-7954-3-8#:~:text=CDC%20CFS%20Symptom%20Inventory,-The%20Symptom%20Inventory&text=It%20also%20catalogues%20diarrhea%2C%20fever,moderate%2C%203%20=%20severe).
# #the likert scales weren't anchored as described in the article/original measure
# cfsDat <- intakeDat %>%
#   select(id, starts_with("cfs_")) %>%
#   select(id, 1:39) %>%
#   mutate(across(2:39, ~ na_if(., 999))) %>%
#   mutate(across(2:39, ~ na_if(., 99))) %>%
#   mutate(across(ends_with("_i"), ~ recode(., `0` = 1, `1` = 2, `2` = 3))) %>%
#   mutate(across(ends_with("_f"),
#                 .fns = list(mult = ~ . * get(sub("_f$", "_i", cur_column()))),
#                 .names = "{col}_mult")) %>%
#   select(id, ends_with("mult")) %>%
#   select(id, cfs_fatig_f_mult, cfs_rest_f_mult,
#          cfs_muscl_f_mult, cfs_sleep_f_mult, cfs_memor_f_mult,
#          cfs_conce_f_mult) %>%
#   pivot_longer(2:7, names_to = "symptomName", values_to = "score") %>%
#   group_by(id) %>%
#   mutate(cfsSxSum = sum(score)) %>%
#   ungroup() %>%
#   select(id, cfsSxSum) %>%
#   distinct()
# 
# ## AUDIT-C Screening Data ----
# auditDat <- intakeDat %>%
#   select(id, starts_with("auditc")) %>%
#   mutate(across(2:4, ~ na_if(., 999))) %>%
#   mutate(auditc_02 = case_when(
#     auditc_02 == 0 ~ 0,
#     auditc_02 == 1 ~ 0, ## needed to be recoded
#     auditc_02 == 2 ~ 1,
#     auditc_02 == 3 ~ 2,
#     auditc_02 == 4 ~ 3,
#     auditc_02 == 5 ~ 4,
#     auditc_02 == 6 ~ 4 ### 0-5, not sure how 6's got in there --> ask database managers
#   )) %>%
#   pivot_longer(2:4, names_to = "itemName", values_to = "score") %>%
#   group_by(id) %>%
#   mutate(auditCSum = sum(score)) %>%
#   ungroup() %>%
#   select(id, auditCSum) %>%
#   distinct()
# 
# ## CDS (26, why cut down with not ref article?) ----
# #https://onlinelibrary.wiley.com/doi/abs/10.1002/gps.930080712?casa_token=nPw9R0YdaosAAAAA%3A2yHnoG6tw6RtvuNXgdmekbalUAFCuQEaRMgMvihbJ3hKqZOsJOM644DKB0PnsmibP4Fa1tav4B84S_t7
# # intakeDat %>%
# #   select(id, starts_with("cds")) %>%
# #   mutate(across(2:14, ~ na_if(., 999))) %>%
# #   pivot_longer(2:14, names_to = "itemName", values_to = "score") %>%
# #   group_by(id) %>%
# #   mutate(cdsSum = sum(score)) %>%
# #   ungroup() %>%
# #   select(id, cdsSum) %>%
# #   distinct()
# 
# ## NSI Scoring ----
# nsiTotDat <- intakeDat %>%
#   select(id, starts_with("nsi")) %>%
#   mutate(across(2:23, ~ na_if(., 999))) %>%
#   pivot_longer(2:23, names_to = "itemName", values_to = "score") %>%
#   group_by(id) %>%
#   mutate(nsiTotalSum = sum(score)) %>%
#   ungroup() %>%
#   select(id, nsiTotalSum) %>%
#   distinct()
# 
# nsiSomaDat <- intakeDat %>%
#   select(id, nsi_01:nsi_03, nsi_05:nsi_12) %>%
#   mutate(across(2:12, ~ na_if(., 999))) %>%
#   pivot_longer(2:12, names_to = "itemName", values_to = "score") %>%
#   group_by(id) %>%
#   mutate(nsiSomatosensory = sum(score)) %>%
#   ungroup() %>%
#   select(id, nsiSomatosensory) %>%
#   distinct()
# 
# nsiAffectDat <- intakeDat %>%
#   select(id, nsi_04, nsi_17:nsi_22) %>%
#   mutate(across(2:8, ~ na_if(., 999))) %>%
#   pivot_longer(2:8, names_to = "itemName", values_to = "score") %>%
#   group_by(id) %>%
#   mutate(nsiAffective = sum(score)) %>%
#   ungroup() %>%
#   select(id, nsiAffective) %>%
#   distinct()
# 
# nsiCogDat <- intakeDat %>%
#   select(id, nsi_13:nsi_16) %>%
#   mutate(across(2:5, ~ na_if(., 999))) %>%
#   pivot_longer(2:5, names_to = "itemName", values_to = "score") %>%
#   group_by(id) %>%
#   mutate(nsiCognitive = sum(score)) %>%
#   ungroup() %>%
#   select(id, nsiCognitive) %>%
#   distinct()
# 
# nsiDat <- full_join(nsiTotDat, nsiSomaDat, "id") %>%
#   full_join(., nsiAffectDat, "id") %>%
#   full_join(., nsiCogDat, "id")
# 
# ## Combine All Self-Report Measures ----
# combinedSelfReportMeasuresDat <-
#   full_join(fullDemoDat, auditDat, "id") %>%
#   full_join(nsiDat, "id") %>%
#   full_join(cfsDat, "id") %>%
#   full_join(msqDat, "id") %>%
#   full_join(pclcDat, "id") %>%
#   full_join(unitCohesionDat, "id") %>%
#   full_join(ibsDatTogether, "id") %>%
#   full_join(painDat, "id") %>%
#   full_join(cesDat, "id") %>%
#   full_join(sleepDat, "id") %>%
#   full_join(sf36Dat, "id") %>%
#   full_join(fibroDat, "id") %>%
#   full_join(phq8Dat, "id") %>%
#   full_join(phqPdDat, "id") %>%
#   full_join(gadDat, "id")
# 
# write_csv(combinedSelfReportMeasuresDat, "combinedSelfReportMeasuresDat.csv")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
