# 01_transform_data

#### Car data ----
# exclude 2015 because there is less data 
cars_processed <- cars %>%
  filter(year < 2015)

# recoding as miles per gallon to liter per 100kilometer 
table(cars_processed$mpg_hwy)
cars_processed$lp100km_hwy <- round((235.214583 / cars_processed$mpg_hwy), 2)
table(cars_processed$lp100km_hwy)

table(cars_processed$mpg_city)
cars_processed$lp100km_city <- round((235.214583 / cars_processed$mpg_city), 2)
table(cars_processed$lp100km_city)

# check reults 
summary(cars_processed$lp100km_hwy)
summary(cars_processed$lp100km_city)

# New Variable: type of gear 
table(cars_processed$transmission)

cars_processed <- cars_processed %>%
  separate(
    col = transmission,
    into = c("auto_manu", "gear"),
    sep = "\\(|\\s",
    extra = "merge",
    fill = "right"
  )

table(cars_processed$auto_manu)
cars_processed$auto_manu[cars_processed$auto_manu == "Auto"] <- "Automatic"
cars_processed$auto_manu_num <- ifelse(cars_processed$auto_manu == "Automatic", 1, 0)
table(cars_processed$auto_manu_num)

# New Variable: number of gears 
table(cars_processed$gear)
cars_processed$number_gear <- sub(".*?(\\d+).*", "\\1", cars_processed$gear)
table(cars_processed$number_gear)
cars_processed$number_gear[cars_processed$number_gear == "(AV)" | cars_processed$number_gear == "(variable gear ratios)"] <- NA

# create some new variables about mean and predominant gear 
cars_processed$number_gear <- as.numeric(cars_processed$number_gear)

# New Variable: make is from a EU-country 
eu_brands <- c("Audi", "BMW", "Mercedes-Benz", "Volkswagen", "Volvo")
cars_processed <- cars_processed %>% mutate(is_eu = make %in% eu_brands)

# save data 
write.csv(cars_processed,"data/processed/cars_processed.csv")

#### Annual car data ----
cars_annual <- cars_processed %>%
  mutate(gear4 = ifelse(number_gear == 4, 1, 0), 
         gear5 = ifelse(number_gear == 5, 1, 0),
         gear6 = ifelse(number_gear == 6, 1, 0),
         gear7 = ifelse(number_gear == 7, 1, 0)) %>%
  group_by(year) %>%
  drop_na(number_gear) %>%
  summarise(`Mean Number of Gear` = mean(number_gear), 
            mean_lp100km_city = mean(lp100km_city),
            mean_lp100km_hwy = mean(lp100km_hwy),
            share4Gear = mean(gear4), 
            share5Gear = mean(gear5),
            share6Gear = mean(gear6),
            share7Gear = mean(gear7), 
            .groups = "drop") %>% 
  rowwise() %>%
  mutate(
    majorityGear = c(4, 5, 6, 7)[
      which.max(c_across(starts_with("share")))
    ]
  ) %>%
  ungroup()

# save data 
write.csv(cars_annual,"data/processed/cars_annual_processed.csv")


#### Share of Cylinders per Year ----
cars_cylinders <- cars_processed %>% 
  drop_na(cylinders) %>%
  filter(cylinders > 2) %>%
  group_by(cylinders) %>%
  summarise(share = n() / 25309) %>%
  filter(!is.na(share) & share > 0)

# save data 
write.csv(cars_cylinders,"data/processed/cars_cylinders_processed.csv")

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