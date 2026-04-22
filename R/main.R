# main 

library(here)
library(readr)

library(tidyverse)
library(dplyr)

library(ggplot2)
library(patchwork)
library(ggrepel)



cars <- read_csv(here("data/raw/cars.csv"))

# overview 
summary(cars)
table(cars$make)
range(cars$year)
table(cars$class)
table(cars$transmission)
table(cars$eng_size)

here()
source(here("R", "01_transform_data.R"))
source(here("R", "02_data_visualization.R"))

# save all visualizations in the output folder 
ggsave(filename = "time.png", path = here("output"), f_time)
ggsave(filename = "automatic_year_total.png", path = here("output"), f_automatic_year_total)
ggsave(filename = "nGears_total.png", path = here("output"), f_nGears_total)
ggsave(filename = "nGears_annual_total.png", path = here("output"), f_nGears_annual_total)
ggsave(filename = "Cylinders.png", path = here("output"), f_Cylinders)
ggsave(filename = "make.png", path = here("output"), f_make)