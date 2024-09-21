library(needs)
needs(tidyverse,
      circlize,
      glue,
      sf,
      readxl,
      rnaturalearth
)

data <- read_excel("../input/Datasets/US bilateral data-sharing agreements.xlsx")%>%
  mutate(`Date of signature` = as_date(as.Date(`Date of signature`, origin="1899-12-30")),
         `Entry into force` = as_date(`Entry into force`))

countries <- ne_countries(returnclass = "sf", scale = "medium")%>%
  select(name, name_long, iso_a2, iso_a3, pop_est)%>%
  st_make_valid()%>%
  group_by(name, name_long, iso_a2, iso_a3)%>%
  summarize(pop_est = sum(pop_est, na.rm=T),
            geometry = st_union(geometry))%>%
  mutate(name = case_when(
    name == "Swaziland" ~ "eSwatini",
    name == "Macedonia" ~ "North Macedonia",
    name == "Lao PDR" ~ "Laos",
    name == "Vietnam" ~ "Viet Nam",
    grepl("Czech", name) ~ "Czech Republic",
    iso_a3 == "PRK" ~ "North Korea",
    iso_a3 == "KOR" ~ "South Korea",
    T ~ name
  ))%>%
  mutate(name_long = case_when(
    name_long == "Republic of Congo" ~ "Republic of the Congo",
    T ~ name_long
  ))

centroids <- countries %>%
  right_join(
    data %>% 
      mutate(linkid = row_number())%>%
      gather(c(`Country (1)`, `Country (2)`), key = key, value = country)%>%
      mutate(country = case_when(
        grepl("United States", country) ~ "United States",
        T ~ country
      )),
    by = c("name" = "country"))%>%
  mutate(geometry = st_centroid(geometry),
         lon = sf::st_coordinates(geometry)[,1],
         lat = sf::st_coordinates(geometry)[,2],
         lon = case_when(name == "European Union" ~ 4.34878,
                         name == "Southeast European Law Enforcement Cooperation Center" ~ 26.0963,
                         T ~ lon
                         ),
         lat = case_when(name == "European Union" ~ 50.8504,
                         name == "Southeast European Law Enforcement Cooperation Center" ~ 44.4396,
                         T ~ lat
         ))%>%
  st_drop_geometry()%>%
  ungroup()%>%
  select(-c(name_long:pop_est))

links <- centroids %>%
  filter(key == "Country (1)")%>%
  rename(startCountry = name,
         startLat = lat,
         startLng = lon)%>%
  select(-key)%>%
  full_join(
    centroids %>%
      filter(key == "Country (2)")%>%
      rename(endCountry = name,
             endLat = lat,
             endLng = lon)%>%
      select(linkid, endCountry, endLat, endLng)
    , 
    by = "linkid"
  )%>%
  select(-linkid)%>%
  group_by(startCountry, endCountry)%>%
  mutate(n = row_number())%>%
  ungroup()

write_csv(links, "../output/agreements_centroids.csv")
