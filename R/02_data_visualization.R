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

#### Figure 3: Fuel Efficiency over time 
f_time <- cars_processed %>%
  group_by(year) %>%
  summarise(City = mean(lp100km_city), 
            Highway = mean(lp100km_hwy)) %>%
  pivot_longer(cols = c(City, Highway), names_to = "city_hwy", values_to = "mean_lp100km") %>%
  group_by(year) %>%
  filter(year < 2015) %>% # fewer observations might not be representative 
  
  ggplot(aes(y = mean_lp100km, x = year, colour = city_hwy)) + 
  geom_line(size = 1) + 
  scale_y_continuous(limits = c(0, 20)) + 
  labs(title = "Figure 1: The average Fuel Efficiency is increasing", 
       y = "Mean Litres per 100 Kilometers", 
       x = "Years") + 
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5,), 
    legend.position = "bottom",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12)
  ) + 
  scale_x_continuous(breaks = c(1985, 1995, 2005, 2015)) + 
  scale_color_viridis_d(begin = 0.1, end = 0.4)

#### Figure 4: Fuel Efficiency over Time EU vs. Non-EU 
f_timeEU <- cars_processed %>%
  mutate(isEuropean = ifelse(
    make %in% c("Audi", "BMW", "Mercedes-Benz", "Volkswagen", "Volvo"), 1, 0)) %>%
  filter(isEuropean == 1) %>%
  group_by(year) %>%
  summarise(City = mean(lp100km_city), 
            Highway = mean(lp100km_hwy)) %>%
  pivot_longer(cols = c(City, Highway), names_to = "city_hwy", values_to = "mean_lp100km") %>%
  group_by(year) %>%
  filter(year < 2015) %>% # fewer observations might not be representative 
  ggplot(aes(y = mean_lp100km, x = year, colour = city_hwy)) + 
  geom_line(size = 1) + 
  scale_y_continuous(limits = c(0, 20)) + 
  labs(title = "... from European car companies", 
       y = "Mean Litres per 100 Kilometers", 
       x = "Years") + 
  theme_minimal() + 
  theme( 
    legend.position = "none",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12)
  ) + 
  scale_x_continuous(breaks = c(1985, 1995, 2005, 2015)) + 
  scale_color_viridis_d(begin = 0.1, end = 0.4) 

f_timeNonEU <- cars_processed %>%
  mutate(isEuropean = ifelse(
    make %in% c("Audi", "BMW", "Mercedes-Benz", "Volkswagen", "Volvo"), 1, 0)) %>%
  filter(isEuropean == 0) %>%
  group_by(year) %>%
  summarise(City = mean(lp100km_city), 
            Highway = mean(lp100km_hwy)) %>%
  pivot_longer(cols = c(City, Highway), names_to = "city_hwy", values_to = "mean_lp100km") %>%
  group_by(year) %>%
  filter(year < 2015) %>% # fewer observations might not be representative 
  ggplot(aes(y = mean_lp100km, x = year, colour = city_hwy)) + 
  geom_line(size = 1) + 
  scale_y_continuous(limits = c(0, 20)) + 
  labs(title = "... from non-European car companies", 
       y = "Mean Litres per 100 Kilometers", 
       x = "Years") + 
  theme_minimal() + 
  theme( 
    legend.position = "bottom",
    legend.title = element_blank(), 
    legend.text = element_text(size = 12)
  ) + 
  scale_x_continuous(breaks = c(1985, 1995, 2005, 2015)) + 
  scale_color_viridis_d(begin = 0.1, end = 0.4) 

f_EU_NonEU_time <- f_timeEU / f_timeNonEU + 
  plot_layout(axes="collect") + 
  plot_annotation(title = "Figure 2: Relationship between Time and Fuel Efficiency ...", 
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))


#### Figure 5: Relationship of Year, Share of Automatic Gear and Fuel Efficiency ----
f_automatic_year_hwy <- cars_processed %>%
  group_by(year) %>%
  summarise(share_auto = sum(auto_manu_num) / n(), 
            Highway = mean(lp100km_hwy, na.rm = TRUE)) %>%
  ggplot(aes(y = Highway, x = share_auto)) + 
  geom_point(colour = "#482576FF") + 
  scale_y_continuous(limits = c(5, 18)) +
  geom_text_repel(aes(label = year), size = 3, seed = 123, max.overlaps = Inf, point.size = T) + 
  theme_minimal() + 
  labs(y = "Mean of Litres per 100 kilometers", 
       x = "Share of Automatic Gears", 
       title = "... on the highway") 

f_automatic_year_city <- cars_processed %>%
  group_by(year) %>%
  summarise(share_auto = sum(auto_manu_num) / n(), 
            City = mean(lp100km_city, na.rm = TRUE)) %>%
  ggplot(aes(y = City, x = share_auto)) + 
  geom_point(colour = "#2A788EFF") + 
  scale_y_continuous(limits = c(5, 18)) +
  geom_text_repel(aes(label = year), size = 3, seed = 123, max.overlaps = Inf, point.size = T) + 
  theme_minimal() + 
  labs(y = "Mean of Litres per 100 kilometers", 
       x = "Share of Automatic Gears", 
       title = "... in the City") + 
  scale_color_viridis_d(begin = 0.1, end = 0.1)

f_automatic_year_total <- f_automatic_year_city / f_automatic_year_hwy + 
  plot_layout(axes="collect") + 
  plot_annotation(title = "Figure 3: Relationship of Year, Share of Automatic Gear and Fuel Efficiency...", 
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

f_automatic_year_total

#### Figure 6: Relationship between the Number of Gears and Fuel Efficiency
f_nGears_hwy <- cars_processed %>%
  drop_na() %>%
  ggplot(aes(x = as.factor(number_gear), y = lp100km_hwy, col = as.factor(number_gear))) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y = "Litres per 100 Kilometers", 
       x = "Number of Gears", 
       title = "... on the Highway") + 
  scale_color_viridis_d() + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(2, 32))

f_nGears_city <- cars_processed %>%
  drop_na() %>%
  ggplot(aes(x = as.factor(number_gear), y = lp100km_city, col = as.factor(number_gear))) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y = "Litres per 100 Kilometers", 
       x = "Number of Gears", 
       title = "... in the City") + 
  scale_color_viridis_d() + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(2, 32))

f_nGears_hwy / f_nGears_city + 
  plot_layout(axes="collect") + 
  plot_annotation(title = "Figure 4: Relationship between the Number of Gears and Fuel Efficiency...", 
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

#### Figure 6: Mean and Predominant Number of Gears per Year
# plot for city 
f_nGears_annual_city <- cars_annual %>%
  ggplot(aes(x = `Mean Number of Gear`, y = mean_lp100km_city, colour = as.factor(majorityGear))) + 
  geom_point() + 
  theme_minimal() + 
  labs(y = "Mean Litres per 100 kilometers",
       x = "Mean Number of Gears", 
       color='Predominant Number of Gears', 
       title = "... in the city") + 
  scale_x_continuous(limits = c(3.5,7), breaks = c(4, 5, 6, 7)) +
  geom_text_repel(aes(label = year), size = 3, seed = 123, max.overlaps = Inf, point.size = T) + 
  scale_color_viridis_d(begin = 0.00, end = 0.85) + 
  theme(legend.position = "bottom")

# plot for highway 
f_nGears_annual_hwy <- cars_annual %>%
  ggplot(aes(x = `Mean Number of Gear`, y = mean_lp100km_hwy, colour = as.factor(majorityGear))) + 
  geom_point() + 
  theme_minimal() + 
  labs(y = "Mean Litres per 100 kilometers",
       x = "Mean Number of Gears", 
       color='Predominant Number of Gears', 
       title = "... on the highway") + 
  scale_x_continuous(limits = c(3.5,7), breaks = c(4, 5, 6, 7)) +
  geom_text_repel(aes(label = year), size = 3, seed = 123, max.overlaps = Inf, point.size = T) + 
  scale_color_viridis_d(begin = 0.00, end = 0.85) + 
  theme(legend.position = "none")

f_nGears_annual_total <- f_nGears_annual_city + f_nGears_annual_hwy + 
  plot_layout(axes="collect") + 
  plot_annotation(title = "Figure 5: Mean and Predominant Number of Gears, \n Time and Fuel Efficiency...", 
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

#### Figure 7: Relationship between the Number of Cylinders and Fuel Efficiency
# boxplot for Highway 
f_nCylinders_hwy <- cars_processed %>%
  drop_na(lp100km_hwy, cylinders) %>%
  filter(cylinders > 2) %>%
  ggplot(aes(x = as.factor(cylinders), y = lp100km_hwy, col = cylinders)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y = "Litres per 100 Kilometers", 
       x = "Number of Cylinders", 
       title = "... on the Highway") + 
  scale_color_viridis_c() + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(2, 32))

# boxplot for City 
f_nCylinders_city <- cars_processed %>%
  drop_na(lp100km_city, cylinders) %>%
  filter(cylinders > 2) %>%
  ggplot(aes(x = as.factor(cylinders), y = lp100km_city, col = cylinders)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y = "Litres per 100 Kilometers", 
       x = "Number of Cylinders", 
       title = "... in the City") + 
  scale_color_viridis_c() + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(2, 32))

f_share_cylinders <- cars_cylinders %>%
  ggplot(aes(x = 1, y = share, fill = as.factor(cylinders))) +
  geom_col(width = 0.5) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) + 
  labs(x = NULL, 
       y = NULL,
       fill='Number of \n Cylinders', 
       title = "Share \n of Cars") + 
  scale_fill_viridis_d() + 
  theme_minimal() + 
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, margin=margin(0,0,-20,0)) 
  )

f_nCylinders_total <-  f_nCylinders_city / f_nCylinders_hwy + 
  plot_layout(axes="collect") 

(f_nCylinders_total |  f_share_cylinders) + 
  plot_layout(widths = c(10, 1)) + 
  plot_annotation(title = "Figure 6: Relationship between the Number of Cylinders \n and Fuel Efficiency...", 
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

#### Figure 8: Relationship between Fuel Efficiency for different Makes
f_make <- cars_processed %>%
  drop_na(lp100km_hwy) %>%
  drop_na(lp100km_city) %>%
  ggplot(aes(x = lp100km_hwy, y = lp100km_city)) + 
  geom_point() + 
  theme_minimal() +
  labs(title = "Relationship between Fuel Efficiency in the City /n and on the Highway for different Makes", 
       y = "Litres per 100 Kilometers in the City", 
       x = "Litres per 100 Kilometers on the Highway") +
  facet_wrap(~ make)