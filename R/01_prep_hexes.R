library(needs)
needs(tidyverse,
      glue,
      sf,
      readxl,
      lwgeom,
      rnaturalearth,
      h3
)

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
    iso_a3 == "PRK" ~ "North Korea",
    iso_a3 == "KOR" ~ "South Korea",
    T ~ name
  ))%>%
  mutate(name_long = case_when(
    name_long == "Republic of Congo" ~ "Republic of the Congo",
    T ~ name_long
  ))#%>%
# left_join(free, by = c("name" = "Country/Territory"))

extra <- countries %>%
  filter(name == "Morocco" | name == "W. Sahara")%>%
  group_by(name = "combined")%>%
  summarize(geometry = st_union(geometry))%>%
  st_split(st_as_sfc(st_as_sf(data.frame(geometry = c("LINESTRING (-30 27.6665, 10 27.6665)")), wkt = "geometry")))

# ggplot(extra2)+geom_sf()

new <- countries %>%
  filter(name == "Morocco" | name == "W. Sahara")%>%
  st_drop_geometry()%>%
  arrange(desc(name))%>%
  bind_cols(st_collection_extract(extra) %>% select(-name))%>%
  st_as_sf()%>%
  ungroup()

countries <- countries %>%
  filter(!(name %in% c("Morocco", "W. Sahara")))%>%
  bind_rows(new)

if(file.exists("../output/countries_cleaned.geojson")){
  file.remove("../output/countries_cleaned.geojson")}

write_sf(countries, "../output/countries_cleaned.geojson")

# ggplot(test)+geom_sf(mapping = aes(fill = name))

## ALL SYSTEMS

all <- read_excel("../input/Datasets/all_border_systems_data.xlsx", sheet = 2)%>%
  select(- "interoperable_systems" , - "Notes" )%>%
  mutate(user_country = str_to_title(user_country))%>%
  mutate(user_country = case_when(
    user_country == "Côte D'ivoire" ~ "Côte d'Ivoire",
    user_country == "Eswatini" ~ "eSwatini",
    grepl("Somaliland", user_country) ~ "Somaliland",
    user_country == "Timor Leste" ~ "Timor-Leste",
    grepl("Vatican", user_country) ~ "Vatican",
    grepl("Congo", user_country) ~ str_replace_all(user_country, "Of The", "of the"),
    grepl("Vincent", user_country) &  grepl("Grenadines", user_country) ~ "Saint Vincent and the Grenadines",
    user_country == "The Gambia" ~ "Gambia",
    user_country == "Cabo Verde" ~ "Cape Verde",
    user_country == "Türkiye" ~ "Turkey",
    user_country == "Sao Tome & Principe" ~ "São Tomé and Principe",
    T ~ trimws(str_replace_all(str_replace_all(str_remove_all(user_country, "\\?|\\(.*\\)"), "&", "and"), "St ", "Saint "))
  ))

## europol and caricom cannot be shown
## any other geographies? somaliland, siachen glaciers?
## is greenland part of denmark for systems?

hex <- st_read("../input/h3.polygons.json")

## without largest == T
hexes_joined <- st_join(
  hex %>% st_make_valid(),
  countries %>% st_make_valid()
)%>%
  filter(!is.na(name))

# ## with "largest = T" takes long, without very fast
# hexes_joined_largest <- st_join(
#   hex %>% st_make_valid(),
#   countries %>% st_make_valid(),
#   largest = T
# )%>%
#   filter(!is.na(name))
# 
# if(file.exists("output/hex_joined_largest.geojson")){file.remove("output/hex_joined_largest.geojson")}
# st_write(hexes_joined_largest, "output/hex_joined_largest.geojson")
hexes_joined_largest <- st_read("../output/hex_joined_largest.geojson")

hex_names <- c(hexes_joined_largest$name, hexes_joined_largest$name_long)%>% unique()

tiny_countries <- countries %>%
  filter(!(name_long %in% hex_names))%>%
  pull(name)

priority_hexes <- hexes_joined %>%
  filter(name %in% tiny_countries | name_long %in% tiny_countries)%>%
  filter(!(id == "834d49fffffffff" & name == "Sint Maarten") &
           !(id == "835e4bfffffffff" & name == "St-Martin") &
           !(id == "831862fffffffff" & name == "Guernsey") &
           !(id %in% c("831fa2fffffffff", "834d4bfffffffff", "831fa0fffffffff", "832db6fffffffff")))

## check if there are conflicts and if yes, add rules above to resolve
priority_hexes %>% group_by(id) %>% mutate(n = n()) %>% filter(n>1)

hex_countries <- priority_hexes %>%
  bind_rows(hexes_joined_largest %>%
              filter(!(id %in% priority_hexes$id)))

missing <- countries %>%
  filter(!(name_long %in% (c(hex_countries$name, hex_countries$name_long)%>% unique())))%>%
  st_join(
    ## get the closest non-used hex
    hex %>%
      filter(!(id %in% c(hexes_joined_largest$id, priority_hexes$id)))%>%
      st_make_valid(),
    join = st_nearest_feature
  )

hex_countries <- priority_hexes %>%
  bind_rows(hexes_joined_largest %>%
              filter(!(id %in% priority_hexes$id)))%>%
  bind_rows(missing)

## this needs to return empty
countries %>%
  filter(!(name_long %in% (c(hex_countries$name, hex_countries$name_long)%>% unique())))

hex_aggregate <- hex_countries %>%
  st_wrap_dateline(options = "WRAPDATELINE=YES")%>%
  group_by(name, name_long, iso_a2, iso_a3)%>%
  summarize(geometry = st_union(geometry))

# ggplot(hex_aggregate)+geom_sf()

## split antarctica so h3 fills it with dots correctly
ant <- hex_aggregate %>%
  filter(name == "Antarctica")%>%
  st_split(st_as_sfc(st_as_sf(data.frame(geometry = c("LINESTRING ( 0 -90,  0 -60)")), wkt = "geometry")))%>%
  st_collection_extract()%>%
  mutate(area = st_area(geometry))%>%
  arrange(desc(area))%>%
  head(2)%>%
  select(-area)%>%
  group_by(name, name_long, iso_a2, iso_a3)%>%
  summarize(geometry = st_combine(geometry))

hex_outlines_data <- hex_aggregate %>%
  filter(name != "Antarctica")%>%
  bind_rows(ant)%>%
  mutate(name = if_else(name %in% all$user_country, name, name_long))%>%
  full_join(all, by = c("name" = "user_country"))%>%
  mutate(across(c(goTravel_implemented:WCC_ELISE_implemented),
                ~case_when(
                  is.na(.) ~ "no",
                  T ~ trimws(str_remove_all(., "\\(.*\\)"))
                )))%>%
  rename_with(~str_remove_all(., "_implemented"))%>%
  rowwise()%>%
  mutate(`Interpol systems` = c(FIND, MIND, `I-24/7`) %>% unique())%>%
  select(-c(FIND, MIND, `I-24/7`, WAPIS, WISDM, WCC_ELISE))

# ggplot(hex_outlines_data)+geom_sf()

## DELETE EXISTING FILE
if(file.exists("../output/hex_aggregate_data_dateline.geojson")){
  file.remove("../output/hex_aggregate_data_dateline.geojson")
}

st_write(hex_outlines_data %>%
           ## throw out the ones without geometry after checking that thats ok
           filter(!st_is_empty(geometry)),
         "../output/hex_aggregate_data_dateline.geojson")

# hex_outlines_data <- st_read("output/hex_aggregate_data_dateline.geojson")

## rewind geojson !!!
