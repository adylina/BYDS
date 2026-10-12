
########################################################
#   Code created by Dr Amanda Dylina Morse, PhD MPH   ##
#           https://github.com/adylina                ##
########################################################

#####*************************************####
#                  PACKAGES               ####
#####*************************************####

library(tidyverse)
library(readxl)
library(ggplot2)
library(tidytext)
library(kableExtra)
library(dplyr)
library(epitools)
library(stringr)
library(igraph)
library(ggraph)

#####*************************************####
#                   BYDS                 ####
#####*************************************####

# byds dataset
byds <- read_csv("Datasets/byds_cleaned_dataset2.csv") %>%
  rename(column_1 = `...1`) %>%
  select(iobs, fullid, everything())

metadata <- read_csv("cleaned_byds_metadata.csv") %>%
  select(-`...1`)

metadata_short <- metadata %>%
  select(new_var_name, wave, question, category, subcategory, question_text)


#####*************************************####
#            FILTER TO ONE KRAKEN         ####
#####*************************************####

single_part <- byds %>%
  filter(fullid == "STUDY_ID") %>% # replace the relevant study ID here
  select(-iobs, -fullid_01, -fullid_02, -fullid_03, -fullid_04, -fullid_05, -fullid_06, -fullid_07) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(!fullid, 
               names_to = "new_var_name",
               values_to = "response") %>%
  select(-fullid) %>%
  inner_join(metadata_short, 
             by = "new_var_name") %>%
  select(wave, question, category, subcategory, question_text, response) %>%
  mutate(subcategory = ifelse(subcategory == ",", "cigarettes", subcategory))

#####*************************************####
#              WRITE THE KRAKEN           ####
#####*************************************####

write.csv(single_part, "STUDY_ID")  # use the appropriate study ID here



