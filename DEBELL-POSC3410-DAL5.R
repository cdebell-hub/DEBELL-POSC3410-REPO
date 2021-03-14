# Title: DAL 5 PArt II
# Author: Curtis DeBell
# Author's Email: cdebell@clemson.edu
# Date Created: 2021-03-12

# Purpose: To learn how to do chi-square on the GSS file.

# Set up ####
# Libraries
library(tidyverse)
library(infer)
library(stringr)

# Data
load('gss_df.Rdata')

# Script ####

# Get Data in shape for analysis ####

# What do we think we will have to do to the rows? To the columns?

# Do we want something that looks roughly like this?
View(frequency_table_df)
View(chi_sq_df)

# Data Wrangling ####
analysis_df <- gss_df %>%
  # Filter to keep the rows we want
  filter(YEAR == 2018 & !is.na(NEXTGEN) & !is.na(INTSPACE) & (NEXTGEN == "Agree" | NEXTGEN == "Strongly Agree" | NEXTGEN == "Strongly Disagree" | NEXTGEN == "Disagree") & (INTSPACE == "Moderately Interested" | INTSPACE == "Very Interested" | INTSPACE == "Not At All Interested"))%>%
  # Keep only vars we need
  select(INTSPACE, NEXTGEN, WTSSALL) %>%
  # Now
  group_by(NEXTGEN, INTSPACE) %>%
  summarise(count = sum(WTSSALL))

# Convert strongly agree/disagree to agree or disagree, then re-aggregate
analysis_df <- analysis_df %>%
  mutate(NEXTGEN - str_remove_all(NEXTGEN, "Strongly"), NEXTGEN = str_to_lower(NEXTGEN), NEXTGEN = str_trim(NEXTGEN, side = c("both"))) %>%
  group_by(NEXTGEN, INTSPACE) %>%
  summarise(count = sum(count))

# Reorder factor variable INTSPACE
analysis_df <- anaysis_df %>%
  mutate(INTSPACE = factor(INTSPACE))

# Add data represented INTSPACE == "Not At All Interested"
add_rows<- tribble(~NEXTGEN, ~INTSPACE, ~space, "agree", "Not At All Interested", 1, "disagree", "Not At All Interested", 1)

analysis_df <- bind_rows(analysis_df, add_rows)

# Make NEXTGEN and INTSPACE a factor and arrange
analysis_df <- analysis_df %>%
  mutate(INTSPACE = factor(INTSPACE), NEXTGEN = factor(NEXTGEN)) %>%
  arrange(NEXTGEN)

# Create a frequency table by reshaping the data 
frequency_table_df <- analysis_df %>%
  spread(INTSPACE, count)


  