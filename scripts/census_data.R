library(haven)
library(readxl)
library(here)
library(questionr)
library(tidyverse)
library(nomisr)

#First use NOMIS API for population, residence type and density
#You have to use your own API key here for this section to work

nomis_api_key(check_env = FALSE)

nomis_data_info("NM_2021_1")

nomis_get_metadata("NM_2021_1", "measures")

nomis_get_metadata("NM_2026_1", "geography", "TYPE")

pop21 <- nomis_get_data(id = "NM_2021_1", time = "latest", geography = "TYPE172", measures=20100) %>%
  select(GEOGRAPHY_CODE,GEOGRAPHY_NAME,C2021_RESTYPE_3_NAME,OBS_VALUE) %>%
  group_by(GEOGRAPHY_CODE) %>%
  slice(1) %>%
  ungroup() %>%
  select(pcon_code = GEOGRAPHY_CODE, pcon_name = GEOGRAPHY_NAME, c21Population = OBS_VALUE)

communal21 <- nomis_get_data(id = "NM_2021_1", time = "latest", geography = "TYPE172", measures=20301) %>%
  select(GEOGRAPHY_CODE,GEOGRAPHY_NAME,C2021_RESTYPE_3_NAME,OBS_VALUE) %>%
  group_by(GEOGRAPHY_CODE) %>%
  slice(2:3) %>%
  ungroup() %>%
  rename(pcon_code = GEOGRAPHY_CODE, pcon_name = GEOGRAPHY_NAME) %>%
  pivot_wider(names_from = 3, values_from = 4) %>%
  rename(c21Households = 3, c21Communal = 4)

popdens21 <- nomis_get_data(id = "NM_2026_1", time = "latest", geography = "TYPE172", measures=20100) %>%
  select(GEOGRAPHY_CODE,GEOGRAPHY_NAME,OBS_VALUE) %>%
  rename(pcon_code = GEOGRAPHY_CODE, pcon_name = GEOGRAPHY_NAME, c21PopulationDensity = OBS_VALUE) %>%
  mutate(c21PopulationDensity = c21PopulationDensity/100)

census21_nomis <- left_join(left_join(pop21,popdens21),communal21) %>%
  arrange(pcon_name)

# Other measures for E&W at 2024 pcon boundaries from here: https://www.ons.gov.uk/datasets/create
# (for whatever reason Nomis didn't have all measures at these new boundaries)

# create function to format Census data inputs

test <- read_csv("data/census/census21_wpc_language.csv")

test %>%
  rename(pcon_code = 1, pcon_name = 2) %>%
  select(-3) %>%
  pivot_wider(names_from = 3, values_from = 4) %>%
  select(-contains("Does not apply")) %>%
  rowwise() %>%
  mutate(across(-c(pcon_code, pcon_name), ~ . / sum(c_across(-c(pcon_code, pcon_name))) * 100)) %>%
  ungroup()

format_census_data <- function(x){
  
  x <- x %>%
    rename(pcon_code = 1, pcon_name = 2) %>%
    select(-3) %>%
    pivot_wider(names_from = 3, values_from = 4) %>%
    select(-contains("Does not apply")) %>%
    rowwise() %>%
    mutate(across(-c(pcon_code, pcon_name), ~ . / sum(c_across(-c(pcon_code, pcon_name))) * 100)) %>%
    ungroup()
  
  return(x)
}

#import files, apply function and combine

files <- list.files(path = "data/census", pattern = "\\.csv$", full.names = TRUE)

ons_combined <- reduce(
  lapply(
    lapply(files, read_csv),
    format_census_data),
  full_join, by = c("pcon_code","pcon_name"))

ons_combined

ons_combined$pcon_name

#change Census variable names and match 2019 file column order

dput(names(ons_combined))

ons_combined %>%
  filter(str_detect(pcon_code, "^W")) %>%
  select(1:2,204:208)

rowSums(ons_combined[204:208])

ons_renamed <- ons_combined %>%
  mutate(c21AnyWelsh = (`Can understand spoken Welsh only`+`Can speak, read and write Welsh`+
                          `Can speak and other combinations of skills in Welsh`+
                          `Other combination of skills in Welsh`),
         c21NoWelsh = `No skills in Welsh`) %>%
  select(pcon_code, pcon_name, c21Female = Female, c21Male = Male,
         c21Age0to4 = "Aged 4 years and under", c21Age5to9 = "Aged 5 to 9 years",
         c21Age10to15 = "Aged 10 to 15 years", c21Age16to19 = "Aged 16 to 19 years", 
         c21Age20to24 = "Aged 20 to 24 years", c21Age25to29 = "Aged 25 to 29 years",
         c21Age30to34 = "Aged 30 to 34 years", c21Age35to39 = "Aged 35 to 39 years",
         c21Age40to44 = "Aged 40 to 44 years", c21Age45to49 =  "Aged 45 to 49 years", 
         c21Age50to54 = "Aged 50 to 54 years", c21Age55to59 = "Aged 55 to 59 years",
         c21Age60to64 = "Aged 60 to 64 years", c21Age65to69 = "Aged 65 to 69 years",
         c21Age70to74 = "Aged 70 to 74 years", c21Age75to79 = "Aged 75 to 79 years", 
         c21Age80to84 = "Aged 80 to 84 years", c21Age85plus = "Aged 85 years and over",
         c21HouseOutright = "Owned: Owns outright", c21HouseMortgage = "Owned: Owns with a mortgage or loan or shared ownership",
         c21HouseSocialLA = "Social rented: Rents from council or Local Authority",
         c21HouseSocialOther = "Social rented: Other social rented",
         c21HousePrivateLandlord = "Private rented: Private landlord or letting agency", 
         c21HousePrivateOther = "Private rented: Other private rented or lives rent free",
         c21HouseholdOnePerson66plus = "One-person household: Aged 66 years and over.x", 
         c21HouseholdOnePersonOther = "One-person household: Other.x",
         c21HouseholdOneFamily66plus = "Single family household: All aged 66 years and over.x", 
         c21HouseholdMarriedNoChildren = "Single family household: Married or civil partnership couple: No children", 
         c21HouseholdMarriedDependents = "Single family household: Married or civil partnership couple: Dependent children", 
         c21HouseholdMarriedNondependents = "Single family household: Married or civil partnership couple: All children non-dependent", 
         c21HouseholdCohabitNoChildren = "Single family household: Cohabiting couple family: No children", 
         c21HouseholdCohabitDependents = "Single family household: Cohabiting couple family: With dependent children", 
         c21HouseholdCohabitNondependents = "Single family household: Cohabiting couple family: All children non-dependent", 
         c21HouseholdLoneDependents = "Single family household: Lone parent family: With dependent children", 
         c21HouseholdLoneNondependents = "Single family household: Lone parent family: All children non-dependent.x", 
         c21HouseholdOtherDependents = "Other household types: With dependent children", 
         c21HouseholdOtherAllStudents = "Other household types: All in full-time education",
         c21HouseholdOtherAll66Plus = "Other household types: All aged 66 years and over", 
         c21HouseholdOtherRelated = "Other household types: Other related household: Other family composition.x", 
         c21HouseholdOtherAnyOther = "Other household types: Other family composition",
         c21CarsNone = "No cars or vans in household", c21CarsOne = "1 car or van in household", 
         c21CarsTwo = "2 cars or vans in household", c21CarsThree = "3 cars or vans in household", 
         c21CarsFour = "4 or more cars or vans in household", c21EthnicityWhite = "White",
         c21EthnicityMixed = "Mixed or Multiple ethnic groups", 
         c21EthnicityAsian = "Asian, Asian British or Asian Welsh",
         c21EthnicityBlack = "Black, Black British, Black Welsh, Caribbean or African", 
         c21EthnicityOther = "Other ethnic group", 
         c21EthnicityWhiteBritish = "White: English, Welsh, Scottish, Northern Irish or British", 
         c21EthnicityWhiteIrish = "White: Irish", c21EthnicityWhiteTraveller = "White: Gypsy or Irish Traveller",
         c21EthnicityWhiteRoma = "White: Roma", c21EthnicityWhiteOther = "White: Other White", 
         c21EthnicityMixedCaribbean = "Mixed or Multiple ethnic groups: White and Black Caribbean", 
         c21EthnicityMixedAfrican = "Mixed or Multiple ethnic groups: White and Black African",
         c21EthnicityMixedAsian = "Mixed or Multiple ethnic groups: White and Asian",
         c21EthnicityMixedOther = "Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups", 
         c21EthnicityAsianIndian = "Asian, Asian British or Asian Welsh: Indian",
         c21EthnicityAsianPakistani = "Asian, Asian British or Asian Welsh: Pakistani",
         c21EthnicityAsianBangladeshi = "Asian, Asian British or Asian Welsh: Bangladeshi", 
         c21EthnicityAsianChinese = "Asian, Asian British or Asian Welsh: Chinese", 
         c21EthnicityAsianOther = "Asian, Asian British or Asian Welsh: Other Asian", 
         c21EthnicityBlackAfrican = "Black, Black British, Black Welsh, Caribbean or African: African", 
         c21EthnicityBlackCaribbean = "Black, Black British, Black Welsh, Caribbean or African: Caribbean", 
         c21EthnicityBlackOther = "Black, Black British, Black Welsh, Caribbean or African: Other Black", 
         c21EthnicityArab = "Other ethnic group: Arab", c21EthnicityAnyOther = "Other ethnic group: Any other ethnic group",
         c21BornEngland = "Europe: United Kingdom: England", c21BornNI = "Europe: United Kingdom: Northern Ireland",
         c21BornScotland = "Europe: United Kingdom: Scotland", c21BornWales = "Europe: United Kingdom: Wales",
         c21BornGBNotSpecified = "Europe: United Kingdom: Great Britain not otherwise specified", 
         c21BornUKNotSpecified = "Europe: United Kingdom: United Kingdom not otherwise specified", 
         c21BornIreland = "Europe: Ireland.x", c21BornOtherEurope = "Europe: Other Europe",
         c21BornAfrica = "Africa.x", c21BornMiddleEastAndAsia = "Middle East and Asia.x", 
         c21BornAmericasAndCaribbean = "The Americas and the Caribbean.x",
         c21BornOceania = "Antarctica and Oceania (including Australasia)",
         c21PassportNone = "No passport held", c21PassportUK = "Europe: United Kingdom",
         c21PassportBritishOverseas = "British Overseas Territories", c21PassportIreland = "Europe: Ireland.y", 
         c21PassportEU = "Europe: Other Europe: EU Member countries",
         c21PassportEuropeNotEU = "Europe: Other Europe: Rest of Europe", 
         c21PassportAfrica = "Africa.y", c21PassportMiddleEastAndAsia = "Middle East and Asia.y",
         c21PassportAmericasAndCaribbean = "The Americas and the Caribbean.y", 
         c21PassportOceania = "Antarctica and Oceania, including Australasia", 
         c21EnglishAll = "All adults in household have English in England, or English or Welsh in Wales as a main language", 
         c21EnglishOne = "At least one but not all adults in household have English in England, or English or Welsh in Wales as a main language", 
         c21EnglishChild = "No adults in household, but at least one person aged 3 to 15 years, has English in England or English or Welsh in Wales as a main language", 
         c21EnglishNone = "No people in household have English in England, or English or Welsh in Wales as a main language",
         c21Christian = "Christian", c21Buddhist = "Buddhist", c21Hindu = "Hindu",
         c21Jewish = "Jewish", c21Muslim =  "Muslim", c21Sikh = "Sikh",
         c21ReligionOther = "Other religion", c21NoReligion = "No religion",
         c21ReligionNotStated = "Not answered",
         c21NSSECHigherManager = "L1 and L2: Large employers and higher managerial and administrative occupations", 
         c21NSSECHigherProfessional = "L3: Higher professional occupations",
         c21NSSECLowerManager = "L4, L5 and L6: Lower managerial, administrative and professional occupations", 
         c21NSSECIntermediate = "L7: Intermediate occupations",
         c21NSSECSmallEmployer = "L8 and L9: Small employers and own account workers", 
         c21NSSECLowerSupervisor = "L10 and L11: Lower supervisory and technical occupations",
         c21NSSECSemiRoutine = "L12: Semi-routine occupations", c21NSSECRoutine = "L13: Routine occupations",
         c21NSSECNeverWorked = "L14.1: Never worked", c21NSSECLongtermUnemployed = "L14.2: Long-term unemployed", 
         c21NSSECFulltimeStudent = "L15: Full-time students",
         c21Employed = "Economically active (excluding full-time students): In employment: Employee", 
         c21SelfEmployedwithEmployees = "Economically active (excluding full-time students): In employment: Self-employed with employees", 
         c21SelfEmployedwithoutEmployees = "Economically active (excluding full-time students): In employment: Self-employed without employees", 
         c21Unemployed = "Economically active (excluding full-time students): Unemployed: Seeking work or waiting to start a job already obtained: Available to start working within 2 weeks", 
         c21EmployedFTStudent = "Economically active and a full-time student: In employment", 
         c21UnemployedFTStudent = "Economically active and a full-time student: Unemployed: Seeking work or waiting to start a job already obtained: Available to start working within 2 weeks", 
         c21InactiveRetired = "Economically inactive: Retired", c21InactiveFTStudent = "Economically inactive: Student", 
         c21InactiveLookingAfterHome ="Economically inactive: Looking after home or family",
         c21InactiveLongTermSick = "Economically inactive: Long-term sick or disabled", 
         c21InactiveOther = "Economically inactive: Other",
         c21IndustryAgriculture = "A Agriculture, forestry and fishing",
         c21IndustryMining = "B Mining and quarrying", c21IndustryManufacturing = "C Manufacturing",
         c21IndustryElectricitySupply = "D Electricity, gas, steam and air conditioning supply", 
         c21IndustryWaterSupply = "E Water supply; sewerage, waste management and remediation activities", 
         c21IndustryConstruction = "F Construction",
         c21IndustryWholesale = "G Wholesale and retail trade; repair of motor vehicles and motor cycles", 
         c21IndustryTransport = "H Transport and storage",
         c21IndustryAccommodation =  "I Accommodation and food service activities", 
         c21IndustryCommunication = "J Information and communication",
         c21IndustryFinance = "K Financial and insurance activities", 
         c21IndustryRealEstate = "L Real estate activities",
         c21IndustryProfessional = "M Professional, scientific and technical activities", 
         c21IndustryAdministrative = "N Administrative and support service activities",
         c21IndustryPublicAdministration = "O Public administration and defence; compulsory social security", 
         c21IndustryEducation = "P Education",
         c21IndustrySocialWork = "Q Human health and social work activities",
         c21IndustryOther = "R, S, T, U Other", 
         c21QualNone = "No qualifications", 
         c21QualLevel1 = "Level 1 and entry level qualifications: 1 to 4 GCSEs grade A* to C, Any GCSEs at other grades, O levels or CSEs (any grades), 1 AS level, NVQ level 1, Foundation GNVQ, Basic or Essential Skills", 
         c21QualLevel2 = "Level 2 qualifications: 5 or more GCSEs (A* to C or 9 to 4), O levels (passes), CSEs (grade 1), School Certification, 1 A level, 2 to 3 AS levels, VCEs, Intermediate or Higher Diploma, Welsh Baccalaureate Intermediate Diploma, NVQ level 2, Intermediate GNVQ, City and Guilds Craft, BTEC First or General Diploma, RSA Diploma", 
         c21QualApprentice = "Apprenticeship",
         c21QualLevel3 = "Level 3 qualifications: 2 or more A levels or VCEs, 4 or more AS levels, Higher School Certificate, Progression or Advanced Diploma, Welsh Baccalaureate Advance Diploma, NVQ level 3; Advanced GNVQ, City and Guilds Advanced Craft, ONC, OND, BTEC National, RSA Advanced Diploma", 
         c21QualLevel4 = "Level 4 qualifications or above: degree (BA, BSc), higher degree (MA, PhD, PGCE), NVQ level 4 to 5, HNC, HND, RSA Higher Diploma, BTEC Higher level, professional qualifications (for example, teaching, nursing, accountancy)", 
         c21QualOther = "Other: vocational or work-related qualifications, other qualifications achieved in England or Wales, qualifications achieved outside England or Wales (equivalent not stated or unknown)",
         c21HealthVeryGood = "Very good health", c21HealthGood = "Good health",
         c21HealthFair = "Fair health", c21HealthBad = "Bad health", 
         c21HealthVeryBad = "Very bad health",
         c21NoAdultsEmployed = "No adults in employment in household", 
         c21OneAdultEmployed = "1 adult in employment in household",
         c21TwoAdultsEmployed = "2 adults in employment in household", 
         c21ThreeAdultsEmployed = "3 or more adults in employment in household",
         c21DeprivedNone = "Household is not deprived in any dimension", 
         c21Deprived1 = "Household is deprived in one dimension",
         c21Deprived2 = "Household is deprived in two dimensions", 
         c21Deprived3 = "Household is deprived in three dimensions",
         c21Deprived4 = "Household is deprived in four dimensions",
         c21AnyWelsh, c21NoWelsh)

# Join and write output

setdiff(census21_nomis$pcon_name,ons_renamed$pcon_name)
setdiff(ons_renamed$pcon_name,census21_nomis$pcon_name)

ons_renamed$pcon_name[ons_renamed$pcon_name=="Ynys Môn"] <- "Ynys Mon"

c2021_ew <- left_join(census21_nomis,ons_renamed) %>%
  mutate(across(where(is.numeric), ~ round(., digits=1)))

c2021_ew$pcon_name[c2021_ew$pcon_name=="Bridlington and The Wolds"] <- "Bridlington and the Wolds"
c2021_ew$pcon_name[c2021_ew$pcon_name=="South Holland and The Deepings"] <- "South Holland and the Deepings"

write_csv(c2021_ew, file = "data/census21_wpc_ew.csv")

# Scotland
# Data from https://www.scotlandscensus.gov.uk/search-the-census#/location/topics 

#sex

scotland_sex <- excel_sheets("data/census/scotland/sc_census22_wpc_agebysex.xlsx") %>%
  set_names() %>% 
  map_df(~ read_excel(path = "data/census/scotland/sc_census22_wpc_agebysex.xlsx",
                      sheet = .x, range = "D12:E14"), .id = "sheet")

scotland_sex <- scotland_sex %>%
  drop_na() %>%
  mutate(pcon_name = str_remove_all(sheet, "[:digit:]|\\. +"),
         c21Female = (Female/(Female+Male))*100,
         c21Male = (Male/(Female+Male))*100) %>%
  select(pcon_name,c21Female,c21Male)

rowSums(scotland_sex[2:3])

#age

scotland_age <- excel_sheets("data/census/scotland/sc_census22_wpc_age.xlsx") %>%
  set_names() %>% 
  map_df(~ read_excel(path = "data/census/scotland/sc_census22_wpc_age.xlsx",
                      sheet = .x, range = "B12:CY13"), .id = "sheet")

names(scotland_age)

scotland_age <- scotland_age %>%
  mutate(pcon_name = str_remove_all(sheet, "[:digit:]|\\. +"),
         c21Age0to4 = ((`Under 1`+`1`+`2`+`3`+`4`)/`All people`)*100,
         c21Age5to9 = ((`5`+`6`+`7`+`8`+`9`)/`All people`)*100,
         c21Age10to15 = ((`10`+`11`+`12`+`13`+`14`+`15`)/`All people`)*100,
         c21Age16to19 = ((`16`+`17`+`18`+`19`)/`All people`)*100, 
         c21Age20to24 = ((`20`+`21`+`22`+`23`+`24`)/`All people`)*100,
         c21Age25to29 = ((`25`+`26`+`27`+`28`+`29`)/`All people`)*100,
         c21Age30to34 = ((`30`+`31`+`32`+`33`+`34`)/`All people`)*100,
         c21Age35to39 = ((`35`+`36`+`37`+`38`+`39`)/`All people`)*100,
         c21Age40to44 = ((`40`+`41`+`42`+`43`+`44`)/`All people`)*100,
         c21Age45to49 = ((`45`+`46`+`47`+`48`+`49`)/`All people`)*100, 
         c21Age50to54 = ((`50`+`51`+`52`+`53`+`54`)/`All people`)*100,
         c21Age55to59 = ((`55`+`56`+`57`+`58`+`59`)/`All people`)*100,
         c21Age60to64 = ((`60`+`61`+`62`+`63`+`64`)/`All people`)*100,
         c21Age65to69 = ((`65`+`66`+`67`+`68`+`69`)/`All people`)*100,
         c21Age70to74 = ((`70`+`71`+`72`+`73`+`74`)/`All people`)*100,
         c21Age75to79 = ((`75`+`76`+`77`+`78`+`79`)/`All people`)*100, 
         c21Age80to84 = ((`80`+`81`+`82`+`83`+`84`)/`All people`)*100,
         c21Age85plus = ((`85`+`86`+`87`+`88`+`89`+`90`+`91`+`92`+`93`+`94`+
                            `95`+`96`+`97`+`98`+`99`+`100 and over`)/`All people`)*100) %>%
  select(pcon_name,c21Age0to4:c21Age85plus)

rowSums(scotland_age[2:19])

scotland_sexandage <- left_join(scotland_sex,scotland_age)

#replace names cut off due to Excel

scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Aberdeenshire North and Mora"] <- "Aberdeenshire North and Moray East"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Argyll, Bute and South Loch"] <- "Argyll, Bute and South Lochaber"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Caithness, Sutherland and E"] <- "Caithness, Sutherland and Easter Ross"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Cumbernauld and Kirkintillo"] <- "Cumbernauld and Kirkintilloch"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Dumfriesshire, Clydesdale a"] <- "Dumfriesshire, Clydesdale and Tweeddale"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="East Kilbride and Strathave"] <- "East Kilbride and Strathaven"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Edinburgh East and Musselbu"] <- "Edinburgh East and Musselburgh"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Inverclyde and Renfrewshire"] <- "Inverclyde and Renfrewshire West"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Inverness, Skye and West Ro"] <- "Inverness, Skye and West Ross-shire"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Moray West, Nairn and Strat"] <- "Moray West, Nairn and Strathspey"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Motherwell, Wishaw and Carl"] <- "Motherwell, Wishaw and Carluke"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Paisley and Renfrewshire No"] <- "Paisley and Renfrewshire North"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Paisley and Renfrewshire So"] <- "Paisley and Renfrewshire South"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="Berwickshire, Roxburgh and "] <- "Berwickshire, Roxburgh and Selkirk"
scotland_sexandage$pcon_name[scotland_sexandage$pcon_name=="West Aberdeenshire and Kinc"] <- "West Aberdeenshire and Kincardine"

#place of birth

scotland_born <- read_excel(path = "data/census/scotland/sc_census22_wpc_born.xlsx",
                            range = "B11:BU69")

names(scotland_born)

scotland_born <- scotland_born %>%
  drop_na() %>%
  rename(pcon_name = 1) %>%
  mutate(c21Population = `All people`,
         c21BornEngland = (`Europe: United Kingdom: England`/`All people`)*100,
         c21BornNI = (`Europe: United Kingdom: Northern Ireland`/`All people`)*100,
         c21BornScotland = (`Europe: United Kingdom: Scotland`/`All people`)*100,
         c21BornWales = (`Europe: United Kingdom: Wales`/`All people`)*100,
         c21BornUKNotSpecified = (`Europe: United Kingdom: UK part not specified`/`All people`)*100, 
         c21BornIreland = (`Europe: Other Europe: EU Member countries in March 2022: Republic of Ireland`/`All people`)*100,
         c21BornOtherEurope = ((`Europe: Channel Islands and Isle of Man`+
                                 `Europe: Other Europe: EU Member countries in March 2022: Total`+
                                 `Europe: Other Europe: Accession countries March 2022`+
                                 `Europe: Other Europe: Non EU countries: Total`)/`All people`)*100,
         c21BornAfrica = (`Africa: Total`/`All people`)*100,
         c21BornMiddleEastAndAsia = (`Middle East and Asia: Total`/`All people`)*100, 
         c21BornAmericasAndCaribbean = (`The Americas and the Caribbean: Total`/`All people`)*100,
         c21BornOceania = (`Antarctica and Oceania: Total`/`All people`)*100) %>%
  select(pcon_name,c21Population:c21BornOceania)

scotland_born$pcon_name

rowSums(scotland_born[2:12])

#ethnicity
#note differences with White Scottish and no mixed subcategories

scotland_ethnicity <- read_excel(path = "data/census/scotland/sc_census22_wpc_ethnicity.xlsx",
                            range = "B11:AA69")

names(scotland_ethnicity)

scotland_ethnicity <- scotland_ethnicity %>%
  drop_na() %>%
  rename(pcon_name = 1) %>%
  mutate(c21EthnicityWhite = (`White: Total`/`All People`)*100,
         c21EthnicityMixed = (`Mixed or multiple ethnic group`/`All People`)*100, 
         c21EthnicityAsian = (`Asian, Asian Scottish or Asian British: Total`/`All People`)*100,
         c21EthnicityBlack = ((`African: Total`+`Caribbean or Black: Total`)/`All People`)*100, 
         c21EthnicityOther = (`Other ethnic groups: Total`/`All People`)*100, 
         c21EthnicityWhiteBritish = ((`White: White Scottish`+`White: Other White British`)/`All People`)*100, 
         c21EthnicityWhiteIrish = (`White: White Irish`/`All People`)*100,
         c21EthnicityWhiteTraveller = (`White: Gypsy/ Traveller`/`All People`)*100,
         c21EthnicityWhiteOther = ((`White: White Polish`+`Other White`)/`All People`)*100,
         c21EthnicityAsianIndian = (`Asian, Asian Scottish or Asian British: Indian, Indian Scottish or Indian British`/`All People`)*100,
         c21EthnicityAsianPakistani = (`Asian, Asian Scottish or Asian British: Pakistani, Pakistani Scottish or Pakistani British`/`All People`)*100,
         c21EthnicityAsianBangladeshi = (`Asian, Asian Scottish or Asian British: Bangladeshi, Bangladeshi Scottish or Bangladeshi British`/`All People`)*100, 
         c21EthnicityAsianChinese = (`Asian, Asian Scottish or Asian British: Chinese, Chinese Scottish or Chinese British`/`All People`)*100, 
         c21EthnicityAsianOther = (`Asian, Asian Scottish or Asian British: Other Asian`/`All People`)*100, 
         c21EthnicityBlackAfrican = (`African: Total`/`All People`)*100, 
         c21EthnicityBlackCaribbean = (`Caribbean or Black: Caribbean, Caribbean Scottish or Caribbean British`/`All People`)*100, 
         c21EthnicityBlackOther = ((`Caribbean or Black: Black, Black Scottish or Black British`+`Caribbean or Black: Other Caribbean or Black`)/`All People`)*100, 
         c21EthnicityArab = (`Other ethnic groups: Arab, Arab Scottish or Arab British`/`All People`)*100,
         c21EthnicityAnyOther = (`Other ethnic groups: Other ethnic group`/`All People`)*100) %>%
  select(pcon_name,c21EthnicityWhite:c21EthnicityAnyOther)

rowSums(scotland_ethnicity[2:6])

rowSums(scotland_ethnicity[c(3,7:20)])

#language
#different measures as to those for E&W so just create binary measures

scotland_language <- read_excel(path = "data/census/scotland/sc_census22_wpc_language.xlsx",
                                 range = "B11:M69")

names(scotland_language)

scotland_language <- scotland_language %>%
  drop_na() %>%
  rename(pcon_name = 1) %>%
  mutate(c21LanguageSomeEnglish = ((`Understands spoken English only`+`Speaks, reads and writes English`+
                              `Speaks but does not read or write English`+`Speaks and reads but does not write English`+
                              `Reads but does not speak or write English`+`Writes but does not speak or read English`+
                              `Reads and writes but does not speak English`+`Other combinations of skills in English`)/
                             `All people aged 3 and over`)*100,
         c21LanguageLimitedtoNoEnglish = ((`Limited English skills`+`No skills in English`)/`All people aged 3 and over`)*100) %>%
  select(pcon_name,c21LanguageSomeEnglish,c21LanguageLimitedtoNoEnglish)

rowSums(scotland_language[2:3])

#national identity
#not featured for E&W so unique measures for Scotland

scotland_natidentity <- read_excel(path = "data/census/scotland/sc_census22_wpc_natidentity.xlsx",
                                range = "B11:K69")

names(scotland_natidentity)

scotland_natidentity <- scotland_natidentity %>%
  drop_na() %>%
  rename(pcon_name = 1) %>%
  mutate(c21IdentityScottishOnly = (`Scottish identity only`/`All people`)*100,
         c21IdentityBritishOnly = (`British identity only`/`All people`)*100,
         c21IdentityScottishandBritish = (`Scottish and British identities only`/`All people`)*100,
         c21IdentityScottishandOther = (`Scottish and any other identities`/`All people`)*100,
         c21IdentityEnglishOnly = (`English identity only`/`All people`)*100,
         c21IdentityAnyOther = ((`Any other combination of UK identities (UK only)`+`Other identity only (1)`+
                                   `Other identity and at least one UK identity`)/`All people`)*100) %>%
  select(pcon_name,c21IdentityScottishOnly:c21IdentityAnyOther)

rowSums(scotland_natidentity[2:7])

#passport

scotland_passport <- read_excel(path = "data/census/scotland/sc_census22_wpc_passport.xlsx",
                                   range = "B11:M69")

names(scotland_passport)

scotland_passport <- scotland_passport %>%
  drop_na() %>%
  rename(pcon_name = 1) %>%
  mutate(c21PassportNone = (`No Passport`/`All people`)*100,
         c21PassportUK = (`Europe: United Kingdom`/`All people`)*100,
         c21PassportIreland = (`Europe: Ireland`/`All people`)*100, 
         c21PassportEU = (`Europe: EU member countries`/`All people`)*100,
         c21PassportEuropeNotEU = (`Europe: Rest of Europe`/`All people`)*100, 
         c21PassportAfrica = (`Africa`/`All people`)*100,
         c21PassportMiddleEastAndAsia = (`Middle East and Asia`/`All people`)*100,
         c21PassportAmericasAndCaribbean = (`The Americas and the Caribbean`/`All people`)*100, 
         c21PassportOceania = (`Antarctica and Oceania`/`All people`)*100) %>%
  select(pcon_name,c21PassportNone:c21PassportOceania)

rowSums(scotland_passport[2:10])

#religion
#I've also added separated Church of Scotland and Catholic

scotland_religion <- read_excel(path = "data/census/scotland/sc_census22_wpc_religion.xlsx",
                                range = "B11:O69")

names(scotland_religion)

scotland_religion <- scotland_religion %>%
  drop_na() %>%
  rename(pcon_name = 1) %>%
  mutate(c21Christian = ((`Church of Scotland`+`Roman Catholic`+`Other Christian`)/`All people`)*100,
         c21Buddhist = (`Buddhist`/`All people`)*100,
         c21Hindu = (`Hindu`/`All people`)*100,
         c21Jewish = (`Jewish`/`All people`)*100,
         c21Muslim =  (`Muslim`/`All people`)*100,
         c21Sikh = (`Sikh`/`All people`)*100,
         c21ReligionOther = ((`Other religion`+`Pagan`)/`All people`)*100,
         c21NoReligion = (`No religion`/`All people`)*100,
         c21ReligionNotStated = (`Religion not stated`/`All people`)*100,
         c21ChurchofScotland = (`Church of Scotland`/`All people`)*100,
         c21Catholic = (`Roman Catholic`/`All people`)*100,) %>%
  select(pcon_name,c21Christian:c21Catholic)

rowSums(scotland_religion[2:10])

#combine all Scotland

c2021_sc <- left_join(left_join(left_join(left_join(left_join(left_join(scotland_sexandage,scotland_born),scotland_ethnicity),scotland_language),scotland_natidentity),scotland_passport),scotland_religion)

summary(c2021_sc)

#combine GB

c2021_ew <- read_csv(file = "data/census21_wpc_ew.csv")

names(c2021_ew)

c2021_gb <- bind_rows(c2021_ew,c2021_sc) %>%
  mutate(across(where(is.numeric), ~ round(., digits=1)))

names(c2021_gb)

write_csv(c2021_gb, "data/census21_wpc_gb.csv")
