library(haven)
library(here)
library(tidyverse)

#####
# Read in cleaned up 2024 results file

results <- read_csv("data/clean_results.csv")

# Read in cleaned up 2019 notionals file

notionals <- read_csv("data/clean_notionals.csv")

combined_file <- left_join(results,notionals) %>%
  relocate(pano)

summary(combined_file)

# Add Hanretty notional 2016 EU ref results

brexit <- read_csv("data/Hanretty_Brexit_estimates_2024_boundaries.csv")

brexit <- brexit %>%
  select(ConstituencyName=Constituen,HanrettyLeave=LeavePct,HanrettyRemain=RemainPct) %>%
  mutate(HanrettyLeave = HanrettyLeave*100, HanrettyRemain = HanrettyRemain*100)

combined_file <- left_join(combined_file, brexit)

combined_file %>%
  filter(is.na(HanrettyLeave)) %>%
  select(1:3)

# Add Miori notional 2014 Scot ref results

scotref <- read_csv("data/Indy2014_ConstituencyEstimates.csv")

scotref <- scotref %>%
  select(-WinDiff) %>%
  rename(ConstituencyName=Constituency_name,ScotRefYes=Yes_Notional,ScotRefNo=No_Notional)

combined_file <- left_join(combined_file, scotref)

combined_file %>%
  filter(Country=="Scotland",!is.na(ScotRefYes))

summary(combined_file)

# Add 2021 Census data

c2021_gb <- read_csv("data/census21_wpc_gb.csv")

c2021_gb %>%
  filter(pcon_name == "Ynys Mon")

setdiff(notionals$ConstituencyName,c2021_gb$pcon_name)

setdiff(c2021_gb$pcon_name,notionals$ConstituencyName)

c2021_gb$pcon_code <- NULL

combined_file <- left_join(combined_file, c2021_gb, by = c("ConstituencyName" = "pcon_name"))

names(combined_file)

summary(combined_file)[c(7),]

# Write file

write_csv(combined_file, "2024-UK-General-Election-Census-Constituency-Summaries-File-v1.1.csv")
