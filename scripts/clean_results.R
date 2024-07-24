library(haven)
library(here)
library(tidyverse)

results <- read_csv("data/HoC-GE2024-results-by-constituency.csv")

results_clean <- results %>%
  filter(`Country name`!="Northern Ireland") %>%
  mutate(Con24 = (Con/`Valid votes`)*100,
         Lab24 = (Lab/`Valid votes`)*100,
         LD24 = (LD/`Valid votes`)*100,
         SNP24 = (SNP/`Valid votes`)*100,
         PC24 = (PC/`Valid votes`)*100,
         Green24 = (Green/`Valid votes`)*100,
         RUK24 = (RUK/`Valid votes`)*100,
         Other24 = (`All other candidates`/`Valid votes`)*100,
         Majority24 = (`Majority`/`Valid votes`)*100,
         Turnout24 = (`Valid votes`/Electorate)*100,
         MPFirstName24 = `Member first name`,
         MPSurname24 = `Member surname`,
         MPGender24 = `Member gender`) %>%
  select(ONSConstID=`ONS ID`,ConstituencyName=`Constituency name`,Country=`Country name`,
         Region=`Region name`,ConstituencyType=`Constituency type`,
         Winner24=`First party`,Second24=`Second party`,Con24,Lab24,LD24,SNP24,
         PC24,Green24,RUK24,Other24,Majority24,Turnout24,
         ConVote24=Con,LabVote24=Lab,LDVote24=LD,SNPVote24=SNP,PCVote24=PC,
         GreenVote24=Green,RUKVote24=RUK,OtherVote24=`All other candidates`,
         TotalVote24=`Valid votes`,RejectedVote24=`Invalid votes`,Electorate24=Electorate,
         MPFirstName24,MPSurname24,MPGender24)

# Correct names based on notionals
# (essentially decapitalising "the" and removing the to bach from Ynys Mon and Glyndwr)

notionals <- read_csv("data/clean_notionals.csv")

setdiff(notionals[2:3],results_clean[1:2])

results_clean$ConstituencyName[results_clean$ONSConstID=="E14001127"] <- "Bridlington and the Wolds"
results_clean$ConstituencyName[results_clean$ONSConstID=="W07000102"] <- "Montgomeryshire and Glyndwr"
results_clean$ConstituencyName[results_clean$ONSConstID=="E14001487"] <- "South Holland and the Deepings"
results_clean$ConstituencyName[results_clean$ONSConstID=="W07000112"] <- "Ynys Mon"

write_csv(results_clean, "data/clean_results.csv")
