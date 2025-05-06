# import packages
library (dplyr)
library (tidyr)
library(purrr)


### Script to extract


# set directory
setwd("# your directory here")

# import data
componentMS <- read.csv("componentMS.csv", header = TRUE, sep = ",");
legend <- read.csv("legendMS.csv", header = TRUE, sep = ",")
legendAL <- read.csv("legendAL.csv", header = TRUE, sep = ",")
MS_GLO_soilclip <- read.csv("MS_GLO_soilclip.csv", header = TRUE, sep = ",")
AL_GLO_soilclip <- read.csv("AL_GLO_soilclip.csv", header = TRUE, sep = ",")
JP_soilclip <- read.csv("JP_V2_soilclip.csv", header = TRUE, sep = ",")

#### II. Calculations on GLO data

# get counties from SSAs
counties_by_SSA <- legend %>% select(areasymbol, areaname)
MS_GLO_soilclip_cos <- left_join(MS_GLO_soilclip, counties_by_SSA, by = c("AREASYMBOL" = "areasymbol"))

# components long-form
component_GLO_long <- right_join(component_sel, MS_GLO_soilclip_cos, by = c("mukey" = "MUKEY"))

#calculate acreages per county
component_GLO_long <- component_GLO_long %>% mutate(comp_acres = AREA * (comppct_r/100))
component_acreage <- component_GLO_long %>%
  group_by(AREASYMBOL, compname) %>%
  summarise(total_acres = sum(comp_acres, na.rm = TRUE), .groups = "drop") %>%
  arrange(AREASYMBOL, desc(total_acres))

ssa_totals <- component_acreage %>%
  group_by(AREASYMBOL) %>%
  summarise(ssa_total_acres = sum(total_acres, na.rm = TRUE), .groups = "drop")

component_acreage <- component_acreage %>%
  left_join(ssa_totals, by = "AREASYMBOL") %>%
  mutate(
    percent_of_ssa = (total_acres / ssa_total_acres) * 100
  )

component_acreage <- left_join(component_acreage, counties_by_SSA, by = c("AREASYMBOL" = "areasymbol"))

# Two additional counties in Alabama.

componentAL <- read.csv("componentAL.csv", header = TRUE, sep = ",");
component_sel_AL <- componentAL %>% select(c("mukey", "compname", "comppct_r", "cokey"))

counties_by_SSA_AL <- legendAL %>% select(areasymbol, areaname)
AL_GLO_soilclip_cos <- left_join(AL_GLO_soilclip, counties_by_SSA_AL, by = c("AREASYMBOL" = "areasymbol"))

# Constructing component dataframe
component_GLO_AL_long <- right_join(component_sel_AL, AL_GLO_soilclip_cos, by = c("mukey" = "MUKEY"))

#Area and % summaries for GLO prairies
component_GLO_AL_long <- component_GLO_AL_long %>% mutate(comp_acres = AREA * (comppct_r/100))
component_acreage_AL <- component_GLO_AL_long %>%
  group_by(AREASYMBOL, compname) %>%
  summarise(total_acres = sum(comp_acres, na.rm = TRUE), .groups = "drop") %>%
  arrange(AREASYMBOL, desc(total_acres))

ssa_totals_AL <- component_acreage_AL %>%
  group_by(AREASYMBOL) %>%
  summarise(ssa_total_acres = sum(total_acres, na.rm = TRUE), .groups = "drop")

component_acreage_AL <- component_acreage_AL %>%
  left_join(ssa_totals_AL, by = "AREASYMBOL") %>%
  mutate(
    percent_of_ssa = (total_acres / ssa_total_acres) * 100
  )

component_acreage_AL <- left_join(component_acreage_AL, counties_by_SSA_AL, by = c("AREASYMBOL" = "areasymbol"))

# save to spreadsheet

component_acreage_full <- bind_rows(component_acreage, component_acreage_AL)

data.frame(component_acreage_full) %>% write.csv("# your directory here", row.names = FALSE)

# Calculations on Jackson Prairie map
JP_soilclip <- read.csv("JP_V2_soilclip.csv", header = TRUE, sep = ",")
component_sel_ALMS <- bind_rows(component_sel, component_sel_AL)
counties_by_SSA_ALMS <- bind_rows(counties_by_SSA, counties_by_SSA_AL)

# Constructing component dataframe
component_JP_long <- right_join(component_sel_ALMS, JP_soilclip, by = c("mukey" = "MUKEY"))

# Area and % calculations for JP
component_JP_long <- component_JP_long %>% mutate(comp_acres = AREA * (comppct_r/100))
component_acreage_JP <- component_JP_long %>%
  group_by(AREASYMBOL, compname) %>%
  summarise(total_acres = sum(comp_acres, na.rm = TRUE), .groups = "drop") %>%
  arrange(AREASYMBOL, desc(total_acres))

ssa_totals_JP <- component_acreage_JP %>%
  group_by(AREASYMBOL) %>%
  summarise(ssa_total_acres = sum(total_acres, na.rm = TRUE), .groups = "drop")

component_acreage_JP <- component_acreage_JP %>%
  left_join(ssa_totals_JP, by = "AREASYMBOL") %>%
  mutate(
    percent_of_ssa = (total_acres / ssa_total_acres) * 100
  )

component_acreage_JP <- left_join(component_acreage_JP, counties_by_SSA_ALMS, by = c("AREASYMBOL" = "areasymbol"))

data.frame(component_acreage_JP) %>% write.csv("# your directory here", row.names = FALSE)

#Across all SSAs
counties_to_drop <- c("MS007", "MS015", "MS027", "MS043", "MS051", "MS069", "MS083", "MS161")

component_acreage_GLOJP <-component_acreage_full %>%
  filter(!AREASYMBOL %in% counties_to_drop)

series_totals_GLO <- component_acreage_GLOJP %>%
  group_by(compname) %>%
  summarise(total_acres = sum(total_acres, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_acres))

series_totals_JP <- component_acreage_JP %>%
  group_by(compname) %>%
  summarise(total_acres = sum(total_acres, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_acres))

# save final results

data.frame(series_totals_GLO) %>% write.csv("# your directory here", row.names = FALSE);
data.frame(series_totals_JP) %>% write.csv("# your directory here", row.names = FALSE)

### GLO area per county calculations

GLO_CO_AREA <- read.csv("GLO_KEES_GEOREFERENCED.csv", header = TRUE, sep = ",");
