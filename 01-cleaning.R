

#Installing packages
install.packages("arrow")
install.packages("naniar")
library(arrow)
library(dplyr)
library(naniar)
library(tidyverse)

#Reading in the dataset

bing_covid <- read_parquet("C:\\Users\\user\\Documents\\R PROJECTS\\bing_covid-19_data.parquet")

#Exploring dataset
str(bing_covid)

#Checing for missing value 

missing_per_column <- colSums(is.na(bing_covid))
View(missing_per_column)  

gg_miss_var(bing_covid)  

#Removing missing varibles
bing_covid <- bing_covid %>%
  drop_na()