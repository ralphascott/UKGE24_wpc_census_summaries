library(haven)
library(here)
library(tidyverse)

notionals <- read_csv("data/results_spreadsheet_for_distribution.csv")

notionals_clean <- notionals %>%
  filter(country_name!="Northern Ireland") %>%
  select(pano=PANO,ONSConstID=`ONS code`,ConstituencyName=`Boundary Comm name`,
         IndexOfChange=Index_of_change,
         Winner19=Win19,Second19,Con19=`CON%`,Lab19=`LAB%`,LD19=`LD%`,SNP19=`SNP%`,
         PC19=`PC%`,Green19=`GRN%`,Brexit19=`BRX%`,Other19=`Tot oths%`,
         Majority19=`maj%`,Turnout19=`"turnout"`,
         ConVote19=Conv,LabVote19=Labv,LDVote19=LDv,SNPVote19=SNPv,PCVote19=PCv,
         GreenVote19=Grnv,BrexitVote19=Brxv,OtherVote19=TotOthv,
         TotalVote19=total_votes,Electorate19=electorate)

# correct ONS codes based on HoC Library file

results <- read_csv("data/HoC-GE2024-results-by-constituency.csv")

setdiff(results_clean[1:2],notionals_clean[2:3])

notionals_clean$ONSConstID[notionals_clean$ConstituencyName=="Ayr, Carrick and Cumnock"] <- "S14000107"
notionals_clean$ONSConstID[notionals_clean$ConstituencyName=="Berwickshire, Roxburgh and Selkirk"] <- "S14000108"
notionals_clean$ONSConstID[notionals_clean$ConstituencyName=="Central Ayrshire"] <- "S14000109"
notionals_clean$ONSConstID[notionals_clean$ConstituencyName=="Kilmarnock and Loudoun"] <- "S14000110"
notionals_clean$ONSConstID[notionals_clean$ConstituencyName=="West Aberdeenshire and Kincardine"] <- "S14000111"

write_csv(notionals_clean, "data/clean_notionals.csv")
