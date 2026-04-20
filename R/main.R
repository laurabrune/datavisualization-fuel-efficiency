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


