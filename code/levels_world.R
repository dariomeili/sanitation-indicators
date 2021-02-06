# wash data plot
# graph global trends 2000-2017 in sanitation levels based on JMP Data
# read data 
washdata_world <- read_csv("data/washdata_world.csv")
washdata_ssa <- read_csv("data/washdata_ssa.csv")
washdata_ssa_ur <- read_csv("data/washdata_urban_rural.csv")
washdata_sasia_ur <- read_csv("data/washdata_southernasia.csv")

#combine data
wash <- bind_rows(washdata_world, washdata_ssa_ur, washdata_sasia_ur, .id = "dataset")
wash %<>%
  mutate(Region = case_when(
    Region=="Sub-Saharan Africa" & `Residence Type` == "urban" ~ "urban Sub-Saharan Africa",
    Region=="Sub-Saharan Africa" & `Residence Type` == "rural" ~ "rural Sub-Saharan Africa",
    Region=="Southern Asia" & `Residence Type` == "urban" ~ "urban South Asia",
    Region=="Southern Asia" & `Residence Type` == "rural" ~ "rural South Asia",
    TRUE ~ Region
  )) %>% 
  select(dataset, Year, `Service Level`, Coverage, Region, `Residence Type`)

# plot coverage levels for world vs sub-saharan africa vs rural/urban
wash %>% 
  mutate(`Service Level`= recode_factor(`Service Level`,
                                        "Safely managed service"="Safely managed",
                                        "At least basic" = "Basic",
                                        "Basic service"="Basic",
                                        "Limited service"="Limited",
                                        "Unimproved"="Unimproved",
                                        "Open defecation"="Open defecation"),
         Region = fct_relevel(Region,
                              "World",
                              "urban Sub-Saharan Africa",
                              "rural Sub-Saharan Africa",
                              "urban South Asia",
                              "rural South Asia")) %>% 
  mutate(Region = str_wrap_factor(Region, width = 9)) %>% 
  ggplot(aes(x=factor(Year), y=Coverage, fill=`Service Level`)) +
  geom_col() + 
  facet_grid(~Region) +
  labs(x="Year") +
  scale_fill_manual(values=wes_palette(n=5, name="Zissou1")) +
  theme_bw() +
  theme(text = element_text(size=10, family="LM Roman 10")) +
  ggsave("figures/coverage_plot.eps", device = cairo_ps, width = 119, height = 73, units = "mm")
