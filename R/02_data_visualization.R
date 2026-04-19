# 02_data_visualization 

#### Figure 1: Emission Sektor ----
f_emission_sector <- ggplot(emission_usa, aes(x = Year, y = vals_new, colour = Sectors)) + 
  geom_line(size = 1) + 
  labs(title = "Figure 1: Emissions of all Sectors vs. Transportation in the US", 
       y = "Tonnes of Carbon Dioxid in Bilion", 
       x = "Years") + 
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), 
    legend.text = element_text(size = 12), 
    legend.title = element_blank(),
  ) + 
  scale_color_viridis_d(begin = 0.4, end = 0.1)