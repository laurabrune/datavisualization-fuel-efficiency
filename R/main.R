# main 

library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggrepel)
library(readr)

cars <- read_csv('data/raw/cars.csv')
emission <- read_csv("data/raw/ghg-emissions-by-sector.csv")

# overview 
summary(cars)
table(cars$make)
range(cars$year)
table(cars$class)
table(cars$transmission)
table(cars$eng_size)

source("R/01_transform_data.R")
#source("R/02_data_visualization.R")


