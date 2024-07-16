library(haven)
library(here)
library(tidyverse)

notionals <- read_csv("data/results_spreadsheet_for_distribution.csv")

notionals$region_name %>% table

notionals_clean <- notionals %>%
  filter(country_name!="Northern Ireland") %>%
  select(pano=PANO,ONSConstID=`ONS code`,ConstituencyName=`Boundary Comm name`,
         Region=region_name,ConstituencyType=constituency_type,IndexOfChange=Index_of_change,
         Winner19=Win19,Second19,Con19=`CON%`,Lab19=`LAB%`,LD19=`LD%`,SNP19=`SNP%`,
         PC19=`PC%`,Green19=`GRN%`,Brexit19=`BRX%`,Other19=`Tot oths%`,
         Majority19=`maj%`,Turnout19=`"turnout"`,
         ConVote19=Conv,LabVote19=Labv,LDVote19=LDv,SNPVote19=SNPv,PCVote19=PCv,
         GreenVote19=Grnv,BrexitVote19=Brxv,OtherVote19=TotOthv,
         TotalVote19=total_votes,Electorate19=electorate) %>%
  mutate(Region = case_when(Region=="WA" ~ "Wales",
                            Region=="SC" ~ "Scotland",
                            Region=="SE" ~ "South East",
                            Region=="WM" ~ "West Midlands",
                            Region=="NW" ~ "North West",
                            Region=="EE" ~ "East of England",
                            Region=="EM" ~ "East Midlands",
                            Region=="GL" ~ "London",
                            Region=="NE" ~ "North East",
                            Region=="SW" ~ "South West",
                            Region=="YH" ~ "Yorkshire and The Humber"))

write_csv(notionals_clean, "data/clean_notionals.csv")
