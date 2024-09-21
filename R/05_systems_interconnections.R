library(needs)
needs(tidyverse,
      glue,
      sf,
      readxl,
      rnaturalearth
)

countries <- read_sf("../output/countries_cleaned.geojson")

combined <- readRDS("../output/combined.rds")

## CONNECTED SYSTEMS

temp <- bind_rows(
  countries %>% ungroup()%>%st_drop_geometry() %>% select(name, iso_a3),
  countries %>%  ungroup()%>% st_drop_geometry() %>% select(name = name_long, iso_a3)
)%>%
  unique()

all_systems <- combined %>%
  select(user_country,
         systems,
         other_systems) %>%
  separate(other_systems, c("1", "2", "3", "4","5", "6", "7", "8"), sep = ", ")%>%
  gather(-user_country, key = key, value = system)%>%
  select(-key)%>%
  filter(!is.na(system))%>%
  unique()%>%
  mutate(system = trimws(str_remove_all(system, "\\(uncertain\\)|\\(replaced\\)")))%>%
  group_by(system)%>%
  # mutate(id = row_number())%>%
  left_join(
    temp,
    by = c("user_country" = "name")
  )

connections <- combined %>%
  select(user_country, systems, connected_to)%>%
  mutate(systems = if_else(is.na(systems), "Interpol", systems))%>%
  filter(!is.na(connected_to))%>%
  separate(connected_to, into = c("1","2","3","4", "5", "6"), sep = ", ")%>%
  gather(-user_country, -systems, key = key, value = connected_to)%>%
  select(-key)%>%
  filter(!is.na(connected_to))%>%
  mutate(across(c(systems, connected_to), ~trimws(.)))%>%
  mutate(note = str_extract(connected_to, "\\(.*\\)"),
         note = str_remove_all(note, "\\(|\\)"),
         connected_to = trimws(str_remove_all(connected_to, "\\(.*\\)")))%>%
  mutate(connected_to = case_when(
    grepl("24", connected_to) & grepl("7", connected_to) ~ "I-24/7",
    T ~ connected_to
  ))%>%
  mutate(
    across(c(systems, connected_to),
           ~if_else(grepl("FIND|MIND|Interpol|I-24/7", ., ignore.case = T), "Interpol", .))
  )%>%
  select(-note)

main_systems <- data.frame(systems = c(connections$systems, connections$connected_to)) %>%
  group_by(systems)%>%
  summarize(n = n())%>%
  arrange(desc(n))%>%
  # head(9)%>%
  ## alphabetical sorting
  arrange(systems)%>%
  pull(systems)

systems <- unique(c(connections$systems, connections$connected_to))

a <- 360/(length(main_systems)+ 1)
r = 1

pts <- data.frame(
  system = c(main_systems, "other"),
  a = seq(from = 0, by = a, length.out = (length(main_systems)+ 1))
)%>%
  mutate(x =  r * cos((a * pi) / (180)))%>%
  mutate(y =  r * sin((a * pi) / (180)))%>%
  mutate(x_label =  r * 1.5 * cos((a * pi) / (180)))%>%
  mutate(y_label =  r * 1.5 * sin((a * pi) / (180)))

plotdata <- pts %>%
  right_join(
    bind_rows(
      connections %>% 
        # filter(user_country == "Burkina Faso")%>%
        mutate(link = row_number())%>%
        gather(-user_country, -link, key = key, value = system_label)%>%
        mutate(system = if_else(system_label %in% main_systems, system_label, "other")),
      all_systems %>%
        mutate(system = if_else(grepl("FIND|MIND|Interpol|I-24/7", system, ignore.case = T), "Interpol", system))%>%
        select(user_country, system)%>%
        ungroup()%>%
        mutate(link = row_number()+500)%>%
        mutate(system_label = system)%>%
        mutate(system = if_else(system_label %in% main_systems, system_label, "other"))
    ))%>%
  ## only countries that have some connections
  filter(user_country %in% connections$user_country)

## GTAS is "other" because only in all_systems and not connection.
## TBD if change that

ggplot()+
  geom_line(plotdata %>%
              group_by(link)%>%
              mutate(n = n())%>%
              filter(n>1)%>%
              ungroup(),
            mapping = aes(
              x = x,
              y = y,
              group = link
            ),
            color = "#364497")+
  geom_point(
    pts,
    mapping = aes(
      x = x, y = y
    ),
    shape = 1,
    color = "#364497",
    # alpha = 0.2,
    size = 1.5
  )+
  geom_point(plotdata,
             mapping = aes(
               x = x, y = y
             ),
             shape = 16,
             color = "#364497",
             size = 1.5
  )+
  geom_text(plotdata %>%
              select(x_label, y_label, system_label,user_country)%>%
              unique(),
            mapping = aes(
              x = x_label, y = y_label, label = system_label
            ),
            size = 3,
            color = "#364497")+
  coord_fixed()+
  theme_void()+
  ylim(-2,2)+
  xlim(-2,2)+
  facet_wrap(vars(user_country), ncol = 4)+
  theme(text = element_text(color = "#364497"),
        strip.text = element_text(face = "bold"))

ggsave("plots/facets_connections.png",
       dpi = 140,
       width =1400/140,
       height = 3500 / 140,
       limitsize = FALSE)

ggsave("plots/facets_connections.svg",
       dpi = 140,
       width =1400/140,
       height = 3500 / 140,
       limitsize = FALSE)

