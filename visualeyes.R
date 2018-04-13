#=============================================================================#
# Program to visualize HIVE bloods over time                                  #
# Author: Richard Evans                                                       #
# Start Date: 04/10/2018                                                      #
#=============================================================================#

#================================= libraries =================================#

library(tidyverse)
library(magrittr)

#=============================== loading data ================================#

# source function loadAllDataType()
source("S:/Monto_Ohmit/Rich Evans/Projects/MontoLab/loadAllDataType.R")

filepath <- "S:/Monto_Ohmit/HIVE_DATA/Cohort 2010-2016/R Data/"
file_extension <- ".rds"

loadAllDataType(filepath, file_extension)

#============================ Blood/Serology data ============================#

glimpse(blood1016)

blood1016 <- blood1016 %>%
  arrange(master_id, sero_date) %>%
  group_by(master_id) %>%
  mutate(blood_cnt = row_number()) %>%
  ungroup()

table(blood1016$blood_cnt)

with(blood1016, table(season, blood_cnt))

mat <- with(blood1016, table(season, blood_cnt)) %>% as.matrix()

rowSums(mat)
colSums(mat)

# Find all possible combinations of specimens collected throughout each season

blood_long <- blood1016 %>%
  distinct(master_id, season) %>%
  reshape2::dcast(master_id ~ season)

combcols <- stringr::str_subset(colnames(blood_long), "20")
blood_long$combos <- do.call(paste, c(blood_long[, combcols], sep = " "))
blood_long$combos <- str_replace_all(blood_long$combos, "NA", "")
# trim trailing and leading space
blood_long$combos <- gsub("^\\s+|\\s+$", "", blood_long$combos)
# trim extra spaces in between characters
blood_long$combos <- gsub("\\s+", " ", blood_long$combos)

table(blood_long$combos) %>% data.frame()

# 1 season is 9 characters, 2 seasons is 19 characters, 3 is 29, 4 is 39, 5=49

blood_long <- blood_long %>%
  mutate(
    years = case_when(
      nchar(combos) == 9  ~ 1,
      nchar(combos) == 19 ~ 2,
      nchar(combos) == 29 ~ 3,
      nchar(combos) == 39 ~ 4,
      nchar(combos) == 49 ~ 5
    )
  )

# this might be the wrong approach, I may want to use ggplot2::stat_bin2d()
# with x = {2011-2012, ..., 2015-2016} and y = {1, ..., 5} for each combination
# level. The bins will be the frequency in each.



#=============================================================================#