# 01_transform_data

#### Car data ----
# recoding as miles per gallon to liter per 100kilometer 
table(cars$mpg_hwy)
cars$lp100km_hwy <- round((235.214583 / cars$mpg_hwy), 2)
table(cars$lp100km_hwy)

table(cars$mpg_city)
cars$lp100km_city <- round((235.214583 / cars$mpg_city), 2)
table(cars$lp100km_city)

# check reults 
summary(cars$lp100km_hwy)
summary(cars$lp100km_city)

# save data 
write.csv(cars,"data/processed/cars_processed.csv")

# exclude 2015 because there is less data 
cars <- cars %>%
  filter(year < 2015)

#### Emission per Sector Data ----
# filter for us and transportation, create sum of all sectors and transform to long format 
emission_usa <- emission %>%
  filter(Entity == "United States") %>%
  pivot_longer(cols = Agriculture:`Aviation and shipping`, names_to= "Sectors", values_to= "values") %>%
  group_by(Year) %>%
  mutate(
    `All Sectors` = sum(values)
  ) %>%
  filter(Sectors == "Transport") %>% 
  select(-Entity, -Code, -Sectors) %>%
  rename(Transportation = values) %>%
  pivot_longer(cols = c(Transportation, `All Sectors`), names_to = "Sectors", values_to = "vals") %>%
  mutate(vals_new = vals / 1000000000)

rm(emission)

# save data 
write.csv(cars,"data/processed/emission_usa_processed.csv")