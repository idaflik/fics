library(needs)
needs(tidyverse,
      glue,
      sf,
      geosphere,
      readxl,
      rnaturalearth,
      ggrepel,
      cartogram,
      lwgeom
)

combined <- readRDS("../output/combined.rds")

## DONORS

donor_details <- combined %>%
  select(donors, user_country, systems, donation_amount, date_of_agreement, entry_into_operation, end_of_operation, status_functionality)%>%
  filter(!is.na(donors) | !is.na(donation_amount))%>%
  mutate(across(where(is.character), ~replace_na(., "")))

write_csv(donor_details, "../output/donor_table.csv")

donors <- combined %>%
  select(user_country, systems, donors) %>%
  mutate(systems = if_else(is.na(systems), "Interpol system", systems)) %>%
  filter(!is.na(donors))%>%
  mutate(donors = case_when(
    grepl("IOM< EU", donors) ~ str_replace_all(donors, "<", ","),
    T ~ donors
  ))%>%
  separate(donors, c("1", "2", "3", "4","5", "6", "7", "8", "9", "10"), sep = ", ")%>%
  gather(-user_country, -systems, key = key, value = donor)%>%
  filter(!is.na(donor))%>%
  select(-key)%>%
  mutate(donor = case_when(
    grepl("US", donor) ~ "US",
    grepl("EU Internal Security Funds", donor) ~ "EU",
    T ~ str_remove_all(donor, "\\(.*\\)|\\?"),
  ))%>%
  mutate(donor = trimws(donor)) %>%
  filter(donor != "$ amount unknown.")%>%
  arrange(user_country, donor)

countries <- read_sf("../output/countries_cleaned.geojson")

donordata_long <- donors %>%
  mutate(panel = case_when(
    donor %in% c("US", "Canada", "UNICEF", "UNDP", "World Bank") ~ "Americas",
    donor %in% c("South Korea", "Japan", "India", "Qatar", "Australia") ~ "Asia & Australia",
    donor %in% c("CEMAC","EAC") ~ "Africa",
    T ~ "Europe"
  ))%>%
  group_by(donor, user_country, panel)%>%
  summarize(n = n())%>%
  ungroup()%>%
  mutate(link = row_number())%>%
  gather(-n, -link, -panel, key = key, value= country )%>%
  mutate(country = case_when(
    country == "US" ~ "United States",
    country == "UK" ~ "United Kingdom",
    country == "Republic Of The Congo" ~ "Republic of the Congo",
    grepl("IOM", country) ~ "IOM",
    grepl("CARICOM IMPACS", country) ~ "CARICOM IMPACS",
    T ~ country
  ))

countrycenters <- countries %>%
  select(name, name_long, iso_a2, iso_a3)%>%
  st_make_valid()%>%
  st_centroid() %>%
  bind_rows(
    data.frame(name = c("EU", "IOM", "CEMAC", "EAC", "World Bank", "International Trade Centre", "UNDP", "UNICEF", "CARICOM IMPACS"),
               x = c(4.34, 6.12, 8.78, 36.68, -77.01, 6.14, -73.97, -73.97, -58.14),
               y = c(50.85, 46.2, 3.75, -3.38, 38.89, 46.20, 40.69, 40.69, 6.79))%>%
      st_as_sf(coords = c("x", "y"), crs = st_crs(countries))
  )

donor_geo <- countrycenters %>%
  mutate(name = if_else(name %in% donordata_long$country, name, name_long))%>%
  right_join(donordata_long, by = c("name" = "country"))%>%
  ungroup()

## cartogram

cartogramdata <- donor_geo %>%
  group_by(name, name_long, iso_a2, iso_a3, key)%>%
  summarize(n = sum(n, na.rm = T))%>%
  ## only works with mercator projection
  st_transform(3857)%>%
  st_buffer(0.1)

# is any country both donor and recipient?
cartogramdata %>%
  group_by(name)%>%
  mutate(n = n())%>%
  filter(n>1)

cartogram <- cartogram_dorling(cartogramdata %>% filter(!st_is_empty(geometry)), "n", k = 1)

if(file.exists("../output/cartogram_donated_systems.geojson")){
  file.remove("../output/cartogram_donated_systems.geojson")
}
st_write(cartogram, "../output/cartogram_donated_systems.geojson")

## with cartogram centroids so european donors can be distinguished:

lines <- donor_geo %>% 
  st_drop_geometry() %>%
  left_join(cartogram %>% 
              select(name,key)%>%
              st_transform(4326)%>%
              st_centroid()%>%
              rowwise()%>%
              mutate(X = st_coordinates(geometry)[1], Y = st_coordinates(geometry)[2])%>%
              st_drop_geometry())%>%
  filter(key == "donor")%>%
  left_join(donor_geo %>% 
              st_drop_geometry() %>%
              bind_cols(as.data.frame(st_coordinates(donor_geo)))%>%
              filter(key == "user_country"),
            by = c("link", "n", "panel")
  )

lines_straight <- lines %>%
  rowwise()%>%
  mutate(geometry = st_sfc(st_linestring(matrix(c(X.x, X.y, Y.x, Y.y), ncol = 2))))%>%
  st_as_sf(crs = 4326)

ggplot()+
  theme_void()+
  geom_sf(st_graticule(lon = seq(-180,180,by=40), lat = seq(-90,90,by=40))%>%
            st_transform("+proj=moll +lon_0=0 +x_0=0 +y_0=0"), mapping = aes(), color = "white")+
  geom_sf(ne_countries(returnclass = "sf"),
          mapping = aes(),
          fill = "white",
          color = NA)+
  geom_sf(lines_straight,
          mapping = aes(linewidth = n),
          alpha = 0.5,
          color = "#364497")+
  geom_point(donor_geo %>%
            group_by(name, key, geometry, panel)%>%
            summarise(n = sum(n))%>%
            filter(key == "donor")%>%
            st_drop_geometry()%>%
            left_join(cartogram %>%
                        st_transform("+proj=moll")%>%
                        st_centroid()%>%
                        select(name))%>%
            st_as_sf()%>%
              rowwise()%>%
              mutate(X = st_coordinates(geometry)[1], Y = st_coordinates(geometry)[2]),
          mapping = aes(size = n, x= X, y = Y),
          shape = 21,
          color = "#364497",
          fill = "white"
  )+
  scale_size_continuous(range = c(0,5), limits = c(0, max(cartogram$n)))+
  geom_text_repel(
    donor_geo %>%
      group_by(name, key, geometry, panel)%>%
      summarise(n = sum(n))%>%
      ungroup()%>%
      filter(key == "donor"),
    mapping = aes(label = name, geometry = geometry),
    min.segment.length = 0,
    force_pull = -0.01,
    nudge_x = 200,
    stat = "sf_coordinates",
    max.overlaps = 40
  )+
  scale_linewidth_continuous(range = c(0.3, 1))+
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "#f2f2f3", color = NA))+
  facet_wrap(vars(panel), ncol = 1)

ggsave("plots/donations_straightlines.svg",
       dpi = 200,
       height = 3000/200,
       width = 1000/200)
