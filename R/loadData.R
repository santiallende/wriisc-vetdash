# Load Libraries
library(tidyverse)
library(vroom)

# Load Data

# Used in ptCharacteristics.R
date_packet_completed_dat <- vroom("datePacketCompletedDat.csv")

mil_info_dat <- vroom("milInfoBoxDat.csv")

mil_timeline_dat <- vroom("milTimelineDat.csv")

concern_dat <- vroom("concernsDat.csv")

medications_dat <- vroom("medicationsDat.csv")

substances_dat <- vroom("substancesDat.csv")

tobacco_dat <- vroom("tobaccoDat.csv")

ptsd_screener_dat <- vroom("ptsdScreenerDat.csv")

tbi_screener_dat <- vroom("tbiScreenerDat.csv")

psqi_dat <- vroom("psqiDat.csv")

pdhra_dat <- vroom("pdhraDat.csv") 

family_history_dat <- vroom("familyHxDat.csv")

wriisc_reason_dat <- vroom("wriiscReasonDat.csv")

mil_expo_extras_dat <- vroom("extraMilitaryExposuresDat.csv")

civil_expo_extras_dat <- vroom("extraCivilianExposuresDat.csv")

# Used in ptCharacteristics.R and ptExposures.R
full_demo_dat <- vroom("fullDemoDat.csv")

# Used in ptExposures.R & ptSelfReport.R
mil_expo_dat <- vroom("militaryExposureDat.csv")

civilian_expo_dat <- vroom("civilianExposuresDat.csv")

# Used in ptHealthSx.R
joined_everDxDat_healthSxDat <- vroom("joinedEverDxDatHealthSxDat.csv")

# Used in ptSelfReport.R
combined_selfreport_measures_dat <- vroom("combinedSelfReportMeasuresDat.csv") %>%
  filter(age < 105 & age > 15)

ever_dx_dat <- vroom("everDxDat.csv")

# Used in ptHealthSx.R, ptExposures.R, ptSelfReport.R
mil_conflict_dat <- vroom("milConflictDat.csv")
