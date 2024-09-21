library(needs)
needs(tidyverse,
      glue,
      sf,
      rnaturalearth,
      readxl
)

## PISCES

pisces <- read_excel("../input/Datasets/all_border_systems_data.xlsx", sheet = 5) %>%
  filter(!is.na(user_country))%>%
  mutate(user_country = case_when(
    user_country == "Cote D'voire" ~ "Côte d'Ivoire",
    user_country == "Eswatini" ~ "eSwatini",
    user_country == "Phillipines" ~ "Philippines",
    grepl("Somaliland", user_country) ~ "Somaliland",
    grepl("macedonia", user_country) ~ "North Macedonia",
    grepl("Herzegovina", user_country) ~ "Bosnia and Herzegovina",
    user_country == "Timor Leste" ~ "Timor-Leste",
    T ~ user_country
  ))%>%
  mutate(agreement = str_extract_all(date_of_agreement, "[0-9]+"),
         start_operation = str_extract_all(entry_into_operation, "[0-9]+"),
         systems = "PISCES")

## MIDAS

midas <- read_excel("../input/Datasets/all_border_systems_data.xlsx", sheet = 6) %>%
  filter(!is.na(user_country))%>%
  mutate(user_country = case_when(
    user_country == "Cote D'voire" ~ "Côte d'Ivoire",
    user_country == "Eswatini" ~ "eSwatini",
    user_country == "Phillipines" ~ "Philippines",
    grepl("Gambia", user_country) ~ "Gambia",
    grepl("Somaliland", user_country) ~ "Somaliland",
    grepl("macedonia", user_country) ~ "North Macedonia",
    grepl("Democratic", user_country) &  grepl("Congo", user_country) ~ "Democratic Republic of the Congo",
    grepl("Herzegovina", user_country) ~ "Bosnia and Herzegovina",
    user_country == "Timor Leste" ~ "Timor-Leste",
    T ~ user_country
  ))%>%
  mutate(agreement = str_extract_all(date_of_agreement, "[0-9]+"),
         start_operation = str_extract_all(entry_into_operation, "[0-9]+"),
         systems = "MIDAS")

## ATS-G

## note: no geometry for caricom

atsg <- read_excel("../input/Datasets/all_border_systems_data.xlsx", sheet = 7) %>%
  filter(!is.na(user_country))%>%
  mutate(user_country = case_when(
    user_country == "Cote D'voire" ~ "Côte d'Ivoire",
    user_country == "Eswatini" ~ "eSwatini",
    user_country == "Phillipines" ~ "Philippines",
    grepl("caricom", user_country, ignore.case = T) ~ "CARICOM IMPACS",
    grepl("Somaliland", user_country) ~ "Somaliland",
    grepl("macedonia", user_country) ~ "North Macedonia",
    grepl("Democratic", user_country) &  grepl("Congo", user_country) ~ "Democratic Republic of the Congo",
    grepl("Herzegovina", user_country) ~ "Bosnia and Herzegovina",
    user_country == "Timor Leste" ~ "Timor-Leste",
    T ~ user_country
  ))%>%
  mutate(agreement = str_extract_all(date_of_agreement, "[0-9]+"),
         start_operation = str_extract_all(entry_into_operation, "[0-9]+"),
         systems = "ATS-G")

## GTAS

gtas <- read_excel("../input/Datasets/all_border_systems_data.xlsx", sheet = 8) %>%
  filter(!is.na(user_country))%>%
  mutate(user_country = case_when(
    user_country == "Cote D'voire" ~ "Côte d'Ivoire",
    user_country == "Eswatini" ~ "eSwatini",
    user_country == "Phillipines" ~ "Philippines",
    grepl("Somaliland", user_country) ~ "Somaliland",
    grepl("macedonia", user_country) ~ "North Macedonia",
    grepl("Democratic", user_country) &  grepl("Congo", user_country) ~ "Democratic Republic of the Congo",
    grepl("Herzegovina", user_country) ~ "Bosnia and Herzegovina",
    user_country == "Timor Leste" ~ "Timor-Leste",
    T ~ user_country
  ))%>%
  mutate(agreement = str_extract_all(date_of_agreement, "[0-9]+"),
         start_operation = str_extract_all(entry_into_operation, "[0-9]+"),
         systems = "GTAS")

## INTERPOL

## note: no geometry for europol

interpol <- read_excel("../input/Datasets/all_border_systems_data.xlsx", sheet = 9) %>%
  filter(!is.na(user_country))%>%
  mutate(user_country = case_when(
    user_country == "Cote D'voire"| user_country ==  "Côte D'ivoire" ~ "Côte d'Ivoire",
    user_country == "Eswatini" ~ "eSwatini",
    user_country == "Phillipines" ~ "Philippines",
    grepl("Somaliland", user_country) ~ "Somaliland",
    grepl("Vatican", user_country) ~ "Vatican",
    grepl("Congo", user_country) &  grepl("Democratic", user_country) ~ "Democratic Republic of the Congo",
    grepl("Vincent", user_country) &  grepl("Grenadines", user_country) ~ "Saint Vincent and the Grenadines",
    user_country == "Timor Leste" ~ "Timor-Leste",
    user_country == "Cabo Verde" ~ "Cape Verde",
    user_country == "Türkiye" ~ "Turkey",
    user_country == "The Gambia" ~ "Gambia",
    grepl("Korea", user_country) ~ "South Korea",
    user_country == "Sao Tome & Principe" ~ "São Tomé and Principe",
    # user_country == "St Kitts and Nevis" ~ "St. Kitts and Nevis",
    user_country == "Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
    T ~ trimws(str_replace_all(str_replace_all(str_remove_all(user_country, "\\?|\\(.*\\)"), "&", "and"), "St ", "Saint "))
  ))%>%
  mutate(agreement = str_extract_all(date_of_agreement, "WAPIS|I-24/7|[0-9]+"))%>%
  mutate(start_operation = str_extract_all(entry_into_operation, "WAPIS|I-24/7|[0-9]+"))

## goTravel

gotravel <- read_excel("../input/Datasets/all_border_systems_data.xlsx", sheet = 3, range = "A1:N41") %>%
  filter(!is.na(user_country))%>%
  mutate(user_country = case_when(
    user_country == "côte d'ivoire" ~ "Côte d'Ivoire",
    user_country == "Eswatini" ~ "eSwatini",
    user_country == "Djbouti" ~ "Djibouti",
    user_country == "Phillippines" ~ "Philippines",
    grepl("Somaliland", user_country) ~ "Somaliland",
    grepl("Vatican", user_country) ~ "Vatican",
    grepl("Congo", user_country) &  grepl("Democratic", user_country) ~ "Democratic Republic of the Congo",
    grepl("Vincent", user_country) &  grepl("Grenadines", user_country) ~ "Saint Vincent and the Grenadines",
    user_country == "Timor Leste" ~ "Timor-Leste",
    user_country == "Cabo Verde" ~ "Cape Verde",
    user_country == "Türkiye" ~ "Turkey",
    grepl("caricom", user_country, ignore.case = T) ~ "CARICOM IMPACS",
    user_country == "The Gambia" ~ "Gambia",
    user_country == "Sao Tome & Principe" ~ "São Tomé and Principe",
    # user_country == "St Kitts and Nevis" ~ "St. Kitts and Nevis",
    user_country == "Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
    T ~ str_to_title(trimws(str_replace_all(str_replace_all(str_remove_all(user_country, "\\?|\\(.*\\)"), "&", "and"), "St ", "Saint ")))
  ))%>%
  mutate(agreement = str_extract_all(date_of_agreement, "[0-9]+"),
         start_operation = str_extract_all(entry_into_operation, "[0-9]+"),
         systems = "goTravel")

## WCC

## note: no geometry for international orgs (eu.lisa, UNHCR)

wcc <- read_excel("../input/Datasets/all_border_systems_data.xlsx", sheet = 4) %>%
  filter(!is.na(user_country))%>%
  mutate(agreement = str_extract_all(date_of_agreement, "[0-9]+"),
         start_operation = str_extract_all(entry_into_operation, "[0-9]+"),
         systems = "WCC")

combined <- atsg %>%
  mutate(entry_into_operation = as.character(entry_into_operation))%>%
  rename(connected_to = ATSG_connected_with)%>%
  bind_rows(pisces %>%
              rename(connected_to = PISCES_connected_to))%>%
  bind_rows(midas %>%
              rename(connected_to = MIDAS_connected_to))%>%
  bind_rows(gtas %>%
              mutate(date_of_agreement = as.character(date_of_agreement),
                     entry_into_operation = as.character(entry_into_operation))%>%
              rename(connected_to = GTAS_connected_to))%>%
  bind_rows(interpol %>%
              rename(connected_to = connected_systems, systems = installed_systems))%>%
  bind_rows(gotravel %>%
              mutate(date_of_agreement = as.character(date_of_agreement))%>%
              rename(connected_to = GoTravel_connected_with))%>%
  bind_rows(wcc %>%
              rename(connected_to = connected_with))

saveRDS(combined,  "../output/combined.rds")

countries_continents <- ne_countries(returnclass = "sf", scale = "medium")%>%
  filter(type != "Dependency")%>%
  select(name, name_long, iso_a2, iso_a3, pop_est, continent)%>%
  bind_rows(ne_countries(returnclass = "sf", scale = "medium")%>%
              filter(type != "Dependency")%>%
              select(name, name_long, iso_a2, iso_a3, continent)%>%
              st_drop_geometry()%>%
              right_join(ne_countries(returnclass = "sf", scale = "medium")%>%
                           filter(type == "Dependency")%>%
                           select(name = sovereignt, pop_est)%>%
                           mutate(name = if_else(name == "United States of America", "United States", name)))%>%
              st_as_sf())%>%
  st_make_valid()%>%
  group_by(name, name_long, iso_a2, iso_a3, continent)%>%
  summarize(pop_est = sum(pop_est, na.rm=T),
            geometry = st_union(geometry))%>%
  mutate(name = case_when(
    name == "Swaziland" ~ "eSwatini",
    name == "Macedonia" ~ "North Macedonia",
    name == "Lao PDR" ~ "Laos",
    name == "Vietnam" ~ "Viet Nam",
    iso_a3 == "PRK" ~ "North Korea",
    iso_a3 == "KOR" ~ "South Korea",
    T ~ name
  ))%>%
  mutate(name_long = case_when(
    name_long == "Republic of Congo" ~ "Republic of the Congo",
    T ~ name_long
  ))%>%
  st_drop_geometry()

table_data <- combined %>%
  mutate(systems = if_else(is.na(systems), installed_systems, systems))%>%
  select(user_country, systems, installed, donors, donation_amount, date_of_agreement, entry_into_operation, end_of_operation)%>%
  left_join(countries_continents %>%
              ungroup()%>%
              mutate(name = if_else(name %in% combined$user_country, name, name_long))%>% 
              select(name, continent, iso_a2, iso_a3),
            by = c("user_country" = "name"))%>%
  mutate(continent = case_when(
    grepl("CARICOM", user_country) ~ "North America",
    grepl("EU", user_country) ~ "Europe",
    user_country %in% c("Europol") ~ "Europe",
    user_country %in% c("UN HCR", "Bermuda") ~ "North America",
    T ~ continent
  ))

write_csv(table_data, "../output/systems_tables.csv") 
