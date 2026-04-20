# 02_data_visualization

#### Preparations ----
theme_custom_basis <- theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

theme_custom_super <- theme_custom_basis +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


#### Figure 1: Fuel Efficiency over time 
# function for all grafics over time 
f_time <- cars_processed %>%
  group_by(year) %>%
  summarise(
    City = mean(lp100km_city, na.rm = TRUE),
    Highway = mean(lp100km_hwy, na.rm = TRUE)
  ) %>%
  pivot_longer(c(City, Highway), names_to = "type", values_to = "value") %>%
  
  ggplot(aes(year, value, colour = type)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = c(1985, 1995, 2005, 2015)) +
  scale_color_viridis_d(begin = 0.1, end = 0.4) +
  labs(x = "Years", y = "Mean Litres per 100 km") +
  theme_custom_basis +
  theme(legend.position = "bottom")

#### Figure 2: Relationship of Year, Share of Automatic Gear and Fuel Efficiency ----
f_automatic_year_hwy <- cars_processed %>%
  group_by(year) %>%
  summarise(share_auto = sum(auto_manu_num) / n(), 
            Highway = mean(lp100km_hwy, na.rm = TRUE)) %>%
  ggplot(aes(y = Highway, x = share_auto)) + 
  geom_point(colour = "#482576FF") + 
  geom_text_repel(aes(label = year), size = 3, seed = 123, max.overlaps = Inf, point.size = TRUE) + 
  theme_minimal() + 
  labs(y = "Mean of Litres per 100 kilometers", 
       x = "Share of Automatic Gears", 
       title = "On the highway") 

f_automatic_year_city <- cars_processed %>%
  group_by(year) %>%
  summarise(share_auto = sum(auto_manu_num) / n(), 
            City = mean(lp100km_city, na.rm = TRUE)) %>%
  ggplot(aes(y = City, x = share_auto)) + 
  geom_point(colour = "#2A788EFF") + 
  geom_text_repel(aes(label = year), size = 3, seed = 123, max.overlaps = Inf, point.size = T) + 
  theme_minimal() + 
  labs(y = "Mean of Litres per 100 kilometers", 
       x = "Share of Automatic Gears", 
       title = "In the City") + 
  scale_color_viridis_d(begin = 0.1, end = 0.1)

f_automatic_year_total <- f_automatic_year_city / f_automatic_year_hwy + 
  plot_layout(axes="collect") + 
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

f_automatic_year_total

#### Figure 3: Relationship between the Number of Gears and Fuel Efficiency
plot_gear_boxplot <- function(variable_name, title) {
  cars_processed %>%
  drop_na(number_gear) %>%
  ggplot(aes(x = as.factor(number_gear), y = {{ variable_name }}, col = as.factor(number_gear))) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y = "Litres per 100 Kilometers", x = "Number of Gears", title = title) + 
  scale_color_viridis_d() + 
  theme(legend.position = "none")
 }

f_nGears_hwy <- plot_gear_boxplot(lp100km_hwy, "On the Highway")
f_nGears_city <- plot_gear_boxplot(lp100km_city, "In the City")

f_nGears_total <- f_nGears_hwy / f_nGears_city + 
  plot_layout(axes="collect") + 
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

#### Figure 4: Mean and Predominant Number of Gears per Year
# plot for city 
f_nGears_annual_city <- cars_annual %>%
  ggplot(aes(x = `Mean Number of Gear`, y = mean_lp100km_city, colour = as.factor(majorityGear))) + 
  geom_point() + 
  theme_minimal() + 
  labs(y = "Mean Litres per 100 kilometers",
       x = "Mean Number of Gears", 
       color='Predominant Number of Gears', 
       title = "In the city") + 
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
       title = "On the highway") + 
  scale_x_continuous(limits = c(3.5,7), breaks = c(4, 5, 6, 7)) +
  geom_text_repel(aes(label = year), size = 3, seed = 123, max.overlaps = Inf, point.size = T) + 
  scale_color_viridis_d(begin = 0.00, end = 0.85) + 
  theme(legend.position = "none")

f_nGears_annual_total <- f_nGears_annual_city + f_nGears_annual_hwy + 
  plot_layout(axes="collect") + 
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

#### Figure 5: Relationship between the Number of Cylinders and Fuel Efficiency
plot_cylinder_boxplot <- function(variable_name, title) {
  cars_processed %>%
  drop_na({{ variable_name }}, cylinders) %>%
  filter(cylinders > 2) %>%
  ggplot(aes(x = as.factor(cylinders), y = {{ variable_name }}, col = cylinders)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y = "Litres per 100 Kilometers", 
       x = "Number of Cylinders", 
       title = title) + 
  scale_color_viridis_c() + 
  theme(legend.position = "none")
}

# boxplot for Highway 
f_nCylinders_hwy <- plot_cylinder_boxplot(lp100km_hwy, "On the Highway")

# boxplot for City 
f_nCylinders_city <- plot_cylinder_boxplot(lp100km_city, "In the City")

f_share_cylinders <- cars_cylinders %>%
  ggplot(aes(x = 1, y = share, fill = as.factor(cylinders))) +
  geom_col(width = 0.5) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) + 
  labs(x = NULL, y = NULL,
       fill='Number of \n Cylinders', 
       title = "Share \n of Cars") + 
  scale_fill_viridis_d() + 
  theme_minimal() + 
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, margin=margin(0,0,-20,0)) 
  )

f_nCylinders_total <-  f_nCylinders_city / f_nCylinders_hwy + 
  plot_layout(axes="collect") 

f_Cylinders <- (f_nCylinders_total |  f_share_cylinders) + 
  plot_layout(widths = c(10, 1)) + 
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

#### Figure 6: Relationship between Fuel Efficiency for different Makes
f_make <- cars_top12_brands %>%
  drop_na(lp100km_hwy, lp100km_city) %>%
  ggplot(aes(x = lp100km_hwy, y = lp100km_city)) + 
  geom_point(colour = "#482576FF") + 
  theme_custom_super + 
  labs(y = "Litres per 100 Kilometers in the City", x = "Litres per 100 Kilometers on the Highway") +
  facet_wrap(~ make)
