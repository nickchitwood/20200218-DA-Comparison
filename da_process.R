# Import files
library(tidyverse)
library(readxl)

da17 <- read_excel('data/assistance-status.xls', skip = 3, na = "*") 
da18 <- read_excel('data/assistancestatus18.xlsx', skip = 2, na = "*")
da19 <- read_excel('data/assistancestatus19.xlsx', skip = 5, sheet = 4, na = "*") %>%
  rename(CDS=cds)

# Rename function
ren_group <- function(x) {
  if (grepl("_", x) == TRUE) {
    result <- substr(x, 1, nchar(x)-11)
  } else {
    result <- substr(x, 1, nchar(x)-10)
  }
}

# Group
da19_group <- da19 %>% select(CDS, 
                              LEAname,
                              Cityname,
                              Countyname,
                              AApriorities:WHpriorities) %>%
  rename_at(vars(AApriorities:WHpriorities), .funs =  ren_group) %>%
  pivot_longer(cols = AA:WH, names_to = "group") %>%
  mutate(pri4 = if_else(value %in% c("A", "B", "D", "E", "H", "I", "K"), TRUE, FALSE),
         pri5 = if_else(value %in% c("A", "B", "C", "F", "H", "J", "K"), TRUE, FALSE),
         pri6 = if_else(value %in% c("A", "C", "D", "G", "I", "J", "K"), TRUE, FALSE),
         pri8 = if_else(value %in% c("E", "F", "G", "H", "I", "J", "K"), TRUE, FALSE),
         year = "2019") %>%
  select(-value)

da18_group <- da18 %>% select(CDS, 
                              LEAname,
                              Cityname,
                              Countyname,
                              AApriorities:WHpriorities) %>%
  rename_at(vars(AApriorities:WHpriorities), .funs =  ren_group) %>%
  pivot_longer(cols = AA:WH, names_to = "group") %>%
  mutate(pri4 = if_else(value %in% c("A", "B", "D", "E", "H", "I", "K"), TRUE, FALSE),
         pri5 = if_else(value %in% c("A", "B", "C", "F", "H", "J", "K"), TRUE, FALSE),
         pri6 = if_else(value %in% c("A", "C", "D", "G", "I", "J", "K"), TRUE, FALSE),
         pri8 = if_else(value %in% c("E", "F", "G", "H", "I", "J", "K"), TRUE, FALSE),
         year = "2018"
         ) %>%
  select(-value)

da17_group <- da17 %>% select(CDS, 
                              LEAname = districtname,
                              Cityname = cityname,
                              Countyname = countyname,
                              AA_PRIORITIES:WH_PRIORITIES) %>%
  rename_at(vars(AA_PRIORITIES:WH_PRIORITIES), .funs =  ren_group) %>%
  pivot_longer(cols = AA:WH, names_to = "group") %>%
  mutate(pri4 = if_else(value %in% c("A", "B", "D", "E", "H", "I", "K"), TRUE, FALSE),
         pri5 = if_else(value %in% c("A", "B", "C", "F", "H", "J", "K"), TRUE, FALSE),
         pri6 = if_else(value %in% c("A", "C", "D", "G", "I", "J", "K"), TRUE, FALSE),
         pri8 = if_else(value %in% c("E", "F", "G", "H", "I", "J", "K"), TRUE, FALSE),
         year = "2017"
         ) %>%
  select(-value)

# Combined
da_overall <- da19 %>% 
  mutate(
    status_long = case_when(
      AssistanceStatus2019 == "Differentiated Assistance" & 
      AssistanceStatus2018 == "Differentiated Assistance" &
      AssistanceStatus2017 == "Differentiated Assistance" ~ "DA-Year 3",
      AssistanceStatus2019 == "Differentiated Assistance" & 
      AssistanceStatus2018 == "Differentiated Assistance" &
      AssistanceStatus2017 != "Differentiated Assistance" ~ "DA-Year 2",
      AssistanceStatus2019 == "Differentiated Assistance" & 
      AssistanceStatus2018 != "Differentiated Assistance" ~ "DA-Year 1",
      AssistanceStatus2019 == "General Assistance" & 
      AssistanceStatus2018 == "Differentiated Assistance" ~ "Exited DA",
      AssistanceStatus2019 != "Differentiated Assistance" & 
      AssistanceStatus2018 != "Differentiated Assistance" &
      AssistanceStatus2017 != "Differentiated Assistance" ~ "Never DA",
      AssistanceStatus2019 != "Differentiated Assistance" & 
      AssistanceStatus2018 != "Differentiated Assistance" &
      AssistanceStatus2017 == "Differentiated Assistance" ~ "Exited DA-Year 2",
      TRUE ~ "Other configuration" 
    )
  )

da_group_longitudinal <- bind_rows(
  da19_group, da18_group, da17_group
)

da_county_summary <- da_group_longitudinal %>%
  group_by(Countyname, LEAname, year) %>%
  summarise(
    pri4 = sum(pri4),
    pri5 = sum(pri5),
    pri6 = sum(pri6),
    pri8 = sum(pri8),
            )

# Geoleads
geoleads <- tribble(
  ~geoarea, ~Countyname,
  "ACOE", "Alameda",
  "ACOE", "Contra Costa",
  "ACOE", "San Francisco",
  "ACOE", "San Mateo",
  "ACOE", "Santa Clara",
  "ACOE", "Solano",
  "KCSOS", "Fresno",
  "KCSOS", "Kern",
  "KCSOS", "Los Angeles",
  "KCSOS", "San Luis Obispo",
  "KCSOS", "Santa Barbara",
  "KCSOS", "Ventura",
  "PCOE/SCOE", "Alpine",
  "PCOE/SCOE", "Amador",
  "PCOE/SCOE", "Calaveras",
  "PCOE/SCOE", "Colusa",
  "PCOE/SCOE", "El Dorado",
  "PCOE/SCOE", "Nevada",
  "PCOE/SCOE", "Placer",
  "PCOE/SCOE", "Sacramento",
  "PCOE/SCOE", "San Joaquin",
  "PCOE/SCOE", "Sierra",
  "PCOE/SCOE", "Sutter",
  "PCOE/SCOE", "Tuolumne",
  "PCOE/SCOE", "Yolo",
  "PCOE/SCOE", "Yuba",
  "RCOE/SDCOE", "Imperial",
  "RCOE/SDCOE", "Orange",
  "RCOE/SDCOE", "Riverside",
  "RCOE/SDCOE", "San Bernardino",
  "RCOE/SDCOE", "San Diego",
  "ShastaCOE", "Butte",
  "ShastaCOE", "Del Norte",
  "ShastaCOE", "Glenn",
  "ShastaCOE", "Humboldt",
  "ShastaCOE", "Lassen",
  "ShastaCOE", "Modoc",
  "ShastaCOE", "Plumas",
  "ShastaCOE", "Shasta",
  "ShastaCOE", "Siskiyou",
  "ShastaCOE", "Tehama",
  "ShastaCOE", "Trinity",
  "SonomaCOE", "Lake",
  "SonomaCOE", "Marin",
  "SonomaCOE", "Mendocino",
  "SonomaCOE", "Napa",
  "SonomaCOE", "Sonoma",
  "TCOE", "Inyo",
  "TCOE", "Kings",
  "TCOE", "Madera",
  "TCOE", "Mariposa",
  "TCOE", "Merced",
  "TCOE", "Mono",
  "TCOE", "Monterey",
  "TCOE", "San Benito",
  "TCOE", "Santa Cruz",
  "TCOE", "Stanislaus",
  "TCOE", "Tulare"
)

da_geo_long <- da_group_longitudinal %>% 
  pivot_longer(pri4:pri8,
                names_to = "priority") %>%
 left_join(geoleads) %>%
  arrange(geoarea, Countyname, LEAname)

write_excel_csv(da_geo_long, 'da_geo_long.csv', na = "")
