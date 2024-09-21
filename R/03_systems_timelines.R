library(needs)
needs(tidyverse,
      glue,
      readxl
)

combined <- readRDS("../output/combined.rds")

dates <- combined %>%
  select(user_country, systems, agreement, start_operation)%>%
  gather(-user_country, -systems, key = key, value = date)%>%
  filter(!is.na(date))

temp <- c(1:nrow(dates))%>%
  map_dfr(function(i){
    df <- as.data.frame(t(as.data.frame(dates[i, ]$date)))
  })%>%
  rename(system_1 = 1, date_1 = 2, system_2 = 3, date_2 = 4)%>%
  mutate(id = row_number())%>%
  left_join(dates %>%
              select(-date)%>%
              mutate(id = row_number()), by = "id")%>%
  mutate(date_1 = if_else(as.numeric(system_1) > 2000 & is.na(date_1), system_1, date_1),
         date_1 = if_else(as.numeric(system_1) > 2000 & as.numeric(date_1) < 2000, system_1, date_1),
         system_1 = case_when(is.na(as.numeric(system_1)) ~ system_1),
         system_1 = if_else(is.na(system_1), systems, system_1))%>%
  select(-id, -systems)

dates_final <- 
  temp %>%
  rename(system = system_1, date = date_1)%>%
  select(-system_2, -date_2)%>%
  bind_rows(
    temp %>%
      rename(system = system_2, date = date_2)%>%
      select(-system_1, -date_1)
  )%>%
  filter(!is.na(date))%>%
  mutate(system = if_else(grepl("ATS-G", system), "ATS-G", system),
         date = as.numeric(date))

agreements <- dates_final %>%
  filter(key == "agreement")%>%
  group_by(date)%>%
  arrange(system)%>%
  mutate(nr = row_number()) %>%
  ungroup()

ggplot()+
  geom_line(agreements %>%
              arrange(desc(date))%>%
              mutate(id = row_number()) %>%
              bind_rows(
                agreements%>%
                  arrange(desc(date)) %>%
                  mutate(id = row_number()) %>%
                  mutate(date = 2024)
              ),
            mapping = aes(x = date,
                          y = id,
                          group = id),
            size = 2 / 2.13 ## doesn't matter because shows up in affinity wrong
  )+
  geom_point(agreements %>%
               arrange(desc(date))%>%
               mutate(id = row_number()
               ),
             mapping = aes(x = date,
                           y = id)
  )+
  geom_text(agreements %>%
              arrange(desc(date))%>%
              mutate(id = row_number()
              ),
            mapping = aes(x = date,
                          y = id,
                          label = glue("{user_country} ({system})")),
            hjust = 1,
            nudge_x = -0.5,
            size = 8 * 0.35
  )+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major = element_line(linewidth = 1 / 2.13),
        axis.text.y = element_blank(),
        axis.text = element_text(colour = "black", size = 8))+
  scale_x_continuous(breaks = seq(min(agreements$date), 2024, 1), limits = c(1999, 2024))

ggsave("plots/agreements_systems.svg",
       width = 1000 / 96 / 3 * 2, #/96 shows up in browser correctly, /3*2 shows up in affinity correctly
       height = 900 / 96 / 3 * 2)

operation <- dates_final %>%
  filter(key == "start_operation")%>%
  group_by(date)%>%
  arrange(system) %>%
  mutate(nr = row_number())

operations_cumulative <- operation %>%
  group_by(date, system)%>%
  summarize(n = n())%>%
  spread(key = system, value = n)%>%
  ungroup()%>%
  mutate(across(c(`ATS-G`:WAPIS), ~replace_na(., 0)))%>%
  mutate(across(c(`ATS-G`:WAPIS), ~cumsum(.)))%>%
  gather(-date, key = system, value = n)

order <- operations_cumulative %>%
  group_by(system)%>%
  summarize(n = max(n))%>%
  arrange(n)

ggplot()+
  geom_area(operations_cumulative %>%
              mutate(system = factor(system, levels = order$system)),
            mapping = aes(x = date,
                          y = n,
                          group = system,
                          fill = system)
  )+
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(operations_cumulative$date), 2024, 1), limits = c(min(operations_cumulative$date), 2024))+
  theme(legend.position = "top",
        axis.title = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(colour = "black", size = 8),
        axis.text = element_text(colour = "black", size = 8),
        legend.title = element_blank(),
        legend.justification = c(0,0),
        legend.key.size = unit(14, 'pt'))+
  scale_fill_discrete(breaks=rev(order$system))+
  guides(fill=guide_legend(nrow=1, byrow=TRUE))

ggsave("plots/operations_areachart.svg",
       width = 1000 / 96 / 3 * 2, #/96 shows up in browser correctly, /3*2 shows up in affinity correctly
       height = 600 / 96 / 3 * 2)

for(year in unique(operation %>% arrange(date) %>% pull(date))){
  print(year)
  print(operation %>% filter(date == year)%>%mutate(print = str_c(user_country, " (", system, ")"))%>%pull(print))
}