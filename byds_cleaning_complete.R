
########################################################
#   Code created by Dr Amanda Dylina Morse, PhD MPH   ##
#           https://github.com/adylina                ##
########################################################


#####################################################################################################################
#
#                                             CLEANING AND RESHAPING THE BYDS DATA
#
#   THERE ARE SOME KEY ISSUES WITH THE BYDS DATA AS IT EXISTS IN SPSS
#
#  1. IT'S IN SPSS--THIS REQUIRES THAT ALL FUTURE USE HAPPEN THERE  -->   UNSUSTAINABLE BUSINESS PRACTICE
#  2. THE DATA USE INCONSISTENT FORMATTING AND SCALING  -->   PRESENTS UNNECESSARY CHALLENGE FOR USE 
#  3. QUESTIONS ARE STRANGELY BROKEN UP BY RESPONSES (BUT NOT ALWAYS) -->   PRESENTS UNNECESSARY CHALLENGE FOR USE 
#  4. QUESTION FORMATTING IS INCOMPLETE -->   FULL QUESTION WORDING ADDED
#  5. COLUMN NAMES NONSENSICAL -->    CLEAR AND CONSISTENT NAMING STRUCTURE ADDED
#
#####################################################################################################################

##########################################
#              META METADATA
##########################################

# load libraries ####
library(haven)
library(tidyverse)
library(ggplot2)
library(readxl)
library(stringr)
library(data.table)
library(foreign)
library(dplyr)
library(stringr)


# read in the metadata ####
metadata <- read_excel("merged_waves_metadata.xlsx")

metadata <- metadata %>%
  mutate(strip = ifelse(is.na(strip), FALSE, strip)) %>%
  filter(strip == FALSE) %>%
  select(wave, old_var_name, new_var_name, question, category, subcategory, question_text, scale_old, notes)


# create a df for each wave ####
wave_1_metadata <- metadata %>%
  filter(wave == 1)

wave_2_metadata <- metadata %>%
  filter(wave == 2)

wave_3_metadata <- metadata %>%
  filter(wave == 3)

wave_4_metadata <- metadata %>%
  filter(wave == 4)

wave_5_metadata <- metadata %>%
  filter(wave == 5)

wave_6_metadata <- metadata %>%
  filter(wave == 6)

wave_7_metadata <- metadata %>%
  filter(wave == 7)

# break ####

##########################################
#           IMPORT THE KRACKEN
##########################################

# import the SPSS file ####
master_byds = read.spss("byds_merged_sw1_sw7.sav", to.data.frame=TRUE)

# write a master CSV of the uncleaned data
#write.csv(byds, "byds_uncleaned_dataset.csv")


# simple changes to make things tidy down the line ####
byds <- master_byds %>%
  rename(qy475b = qy475.9,
         qy475c = qy475.10,
         qy475d = qy475.11,
         qy475e = qy475.12,
         qy475f = qy475.13,
         qy475g = qy475.14,
         qy475h = qy475.15) %>%
  rename(Q2005.69b = Q2005.69.10,
         Q2005.69c = Q2005.69.11,
         Q2005.69d = Q2005.69.12,
         Q2005.69e = Q2005.69.13,
         Q2005.69f = Q2005.69.14,
         Q2005.69g = Q2005.69.15) %>%
  select(-qy7101) %>%
  rename(qy77c = qy77bii,
         qy77d = qy77bii_1, 
         qy733_8 = qy733_other,
         qy740_11 = qy740_100,
         qy740_12 = qy740_other,
         qy7101 = qy7101b)

# break ####

##########################################
#     COLLAPSE RESPONSES - WAVE 1
##########################################

# cleaning the questions ####


# question 2 ####
wave1_q2 <- byds %>%
  select(iobs, q2a, q2b, q2c, q2d, q2e, q2f, q2g, q2h, q2i, q2j, q2k)

wave1_q2$q2 <- apply(wave1_q2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave1_q2;

wave1_q2 %>% 
  select(iobs, q2) %>%
  mutate(q2 = 
           str_replace(q2, pattern = '^.*?,', replacement = ""),
         q2 = str_trim(q2, side='left')) %>%
  filter(iobs != q2) -> wave1_q2


# question 8 ####
wave1_q8 <- byds %>%
  select(iobs, q8a, q8b, q8c, q8d, q8e, q8f, q8g, q8h, q8i)

wave1_q8$q8 <- apply(wave1_q8, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave1_q8;

wave1_q8 %>% 
  select(iobs, q8) %>%
  mutate(q8 = 
           str_replace(q8, pattern = '^.*?,', replacement = ""),
         q8 = str_trim(q8, side='left')) %>%
  filter(iobs != q8) -> wave1_q8


# question 42 ####
wave1_q42 <- byds %>%
  select(iobs, q42a, q42b, q42c, q42d, q42e, q42f, q42g, q42h, q42i, q42j, q42k, q42l, q42m)

wave1_q42$q42 <- apply(wave1_q42, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave1_q42;

wave1_q42 %>% 
  select(iobs, q42) %>%
  mutate(q42 = 
           str_replace(q42, pattern = '^.*?,', replacement = ""),
         q42 = str_trim(q42, side='left')) %>%
  filter(iobs != q42) -> wave1_q42


# question 46 ####
wave1_q46 <- byds %>%
  select(iobs, q46a, q46b, q46c, q46d, q46e, q46f, q46g)

wave1_q46$q46 <- apply(wave1_q46, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave1_q46;

wave1_q46 %>% 
  select(iobs, q46) %>%
  mutate(q46 = 
           str_replace(q46, pattern = '^.*?,', replacement = ""),
         q46 = str_trim(q46, side='left')) %>%
  filter(iobs != q46) -> wave1_q46


# join it back together ####
# merge back into a dataset with the already tidy questions
wave_1 <- byds %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, q1, q3i, q4, q4a, q5, q5a, q6, q6a, q7, q7a, 
         bkq10s, q10ap, q10bp, q10cp, q10dp, q10ep, q10fp, q10gp, q10hp, 
         q10ip, q10jp, q10kp, q10lp, q10mp, q10np, q11i, q12i, q13i, bkq14s, q14ap, q14bp, q14cp, q14dp, q14ep, q14fp, 
         q14gp, q14hp, q14ip, q14jp, q14kp, q14lp, q14mp, q14np, q14op, q14pp, q14qp, q14rp, q14sp, q14tp, q14up, 
         q14vp, q14wp, q14xp, q14yp, q15i, q16ai, q16bi, q16ci, q16di, q16ei, q16fi, q16gi, q16hi, q16ii, q17, q17ai, 
         q17bi, bkq18s, q18ap, q18bp, q18cp, q18dp, q18ep, q18fp, q18gp, q18hp, q18ip, q18jp, q18kp, q18lp, q19a, q19b, 
         q19c, q19d, q19e, q19f, q19g, q19h, q19i, q19j, q19k, q19l, q19m, q19n, q20, q20a, q20b, q21, q21a, q21b, q21c, q21d, q22, 
         q23, q23a,  q23b, q23c, q23d, q24, q25, q25a, q25b, q25c, q25d, q26, q27, q27a, q27b, q27c, q27d, q28, q29, q29a, q29b, 
         q29c, q29d, q30, q31, q31a, q31b, q31c, q31d, q32, q33, q33a, q33b, q33c, q33d, q34, q35, q35a, q35b, q35c, q35d, q36, 
         q37, q37a, q37b, q37c, q37d, q38, q39, q39a, q39b, q39c, q39d, q40, q41, q41a, q41b, q41c, q41d, x175, q43, q44, q45, 
         q47, q47, q48, q49, q50, q51, q52, q53, q54, q55, q56, q57,  q58, q59, q60,q61, q62, q63, q64, q65, q66, q67, q68, q69, 
         q70, q71, q72, q73, q74, q75, q76, bkq77s, q77ap, q77bp, q77cp, q77dp, q77ep, q77fp, q77gp, q77hp, q77ip, q77jp, q77kp, 
         q77lp, q78, q79, q80, q81, q82, q83, q84, bkq85s, q85ap, q85bp, q85cp, q85dp, q85ep, q85fp, q85gp, q85hp, q85ip, q85jp, q85kp, q85lp, 
         q85mp, q85np, q85op, q85pp, q85qp, q85rp, q85sp, q85tp, q86, q86a, q87, q87a, q88, q88a, q89, q89a, bkq90s, q90ap, q90bp, 
         q90cp, q90dp, q90ep, q90fp, q90gp, q90hp, q90ip, q90jp, q90kp, dob1, fullid) %>%
  left_join(wave1_q2, by = c("iobs" = "iobs")) %>%
  left_join(wave1_q8, by = c("iobs" = "iobs")) %>%
  left_join(wave1_q42, by = c("iobs" = "iobs")) %>%
  left_join(wave1_q46, by = c("iobs" = "iobs"))

# remove the excess ####
rm(wave1_q2, wave1_q8, wave1_q42, wave1_q46)

wave_1 <- wave_1 %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, q1, q2, q3i, q4, q4a, q5, q5a, q6, 
         q6a, q7, q7a, q8, bkq10s, q10ap, q10bp, q10cp, q10dp, q10ep, q10fp, q10gp, q10hp, q10ip, q10jp, q10kp, q10lp, q10mp, 
         q10np, q11i, q12i, q13i, bkq14s, q14ap, q14bp, q14cp, q14dp, q14ep, q14fp, q14gp, q14hp, q14ip, q14jp, q14kp, q14lp, q14mp, 
         q14np, q14op, q14pp, q14qp, q14rp, q14sp, q14tp, q14up, q14vp, q14wp, q14xp, q14yp, q15i, q16ai, q16bi, q16ci, q16di, 
         q16ei, q16fi, q16gi, q16hi, q16ii, q17, q17ai, q17bi, bkq18s, q18ap, q18bp, q18cp, q18dp, q18ep, q18fp, q18gp, q18hp, 
         q18ip, q18jp, q18kp, q18lp, q19a, q19b, q19c, q19d, q19e, q19f, q19g, q19h, q19i, q19j, q19k, q19l, q19m, q19n, q20, q20a, 
         q20b, q21, q21a, q21b, q21c, q21d, q22, q23, q23a,  q23b, q23c, q23d, q24, q25, q25a, q25b, q25c, q25d, q26, q27, q27a, 
         q27b, q27c, q27d, q28, q29, q29a, q29b, q29c, q29d, q30, q31, q31a, q31b, q31c, q31d, q32, q33, q33a, q33b, q33c, q33d, 
         q34, q35, q35a, q35b, q35c, q35d, q36, q37, q37a, q37b, q37c, q37d, q38, q39, q39a, q39b, q39c, q39d, q40, q41, q41a, q41b, 
         q41c, q41d, x175, q42, q43, q44, q45, q46, q47, q47, q48, q49, q50, q51, q52, q53, q54, q55, q56, q57,  q58, q59, q60,q61, 
         q62, q63, q64, q65, q66, q67, q68, q69, q70, q71, q72, q73, q74, q75, q76, bkq77s, q77ap, q77bp, q77cp, q77dp, q77ep, 
         q77fp, q77gp, q77hp, q77ip, q77jp, q77kp, q77lp, q78, q79, q80, q81, q82, q83, q84, bkq85s, q85ap, q85bp, q85cp, q85dp, 
         q85ep, q85fp, q85gp, q85hp, q85ip, q85jp, q85kp, q85lp, q85mp, q85np, q85op, q85pp, q85qp, q85rp, q85sp, q85tp, q86, 
         q86a, q87, q87a, q88, q88a, q89, q89a, bkq90s, q90ap, q90bp, q90cp, q90dp, q90ep, q90fp, q90gp, q90hp, q90ip, q90jp, 
         q90kp, dob1, fullid)


# break ####

##########################################
#     COLLAPSE RESPONSES - WAVE 2
##########################################

# cleaning ####

# question 2 ####
wave2_q2 <- byds %>%
  select(fullid_02, qy22a, qy22b, qy22c, qy22d, qy22e, qy22f, qy22g, qy22h, qy22i, qy22j, qy22k, qy22l) %>%
  filter(!is.na(fullid_02))

wave2_q2$qy22 <- apply(wave2_q2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q2;

wave2_q2 %>% 
  select(fullid_02, qy22) %>%
  mutate(qy22 = 
           str_replace(qy22, pattern = '^.*?,', replacement = ""),
         qy22 = str_trim(qy22, side='left')) %>%
  filter(fullid_02 != qy22) -> wave2_q2

# question 4 ####
wave2_q4 <- byds %>%
  select(fullid_02, qy24aa, qy24ab, qy24ac, qy24ad, qy24ae, qy24af, qy24ag, qy24ah, qy24ai, qy24aj, qy24ak, qy24al, qy24am, qy24an) %>%
  filter(!is.na(fullid_02))

wave2_q4$qy24a <- apply(wave2_q4, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q4;

wave2_q4 %>% 
  select(fullid_02, qy24a) %>%
  mutate(qy24a = 
           str_replace(qy24a, pattern = '^.*?,', replacement = ""),
         qy24a = str_trim(qy24a, side='left')) %>%
  filter(fullid_02 != qy24a) -> wave2_q4

# question 5 ####
wave2_q5 <- byds %>%
  select(fullid_02, qy25a, qy25b, qy25c, qy25d, qy25e, qy25f, qy25g, qy25h, qy25i) %>%
  filter(!is.na(fullid_02))

wave2_q5$qy25 <- apply(wave2_q5, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q5;

wave2_q5 %>% 
  select(fullid_02, qy25) %>%
  mutate(qy25 = 
           str_replace(qy25, pattern = '^.*?,', replacement = ""),
         qy25 = str_trim(qy25, side='left')) %>%
  filter(fullid_02 != qy25) -> wave2_q5

# question 31a ####
wave2_q31a <- byds %>%
  select(fullid_02, qy231aa, qy231ab, qy231ac, qy231ad, qy231ae, qy231af, qy231ag, qy231ah, qy231ai) %>%
  filter(!is.na(fullid_02))

wave2_q31a$qy231a <- apply(wave2_q31a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q31a;

wave2_q31a %>% 
  select(fullid_02, qy231a) %>%
  mutate(qy231a = 
           str_replace(qy231a, pattern = '^.*?,', replacement = ""),
         qy231a = str_trim(qy231a, side='left')) %>%
  filter(fullid_02 != qy231a) -> wave2_q31a

# question 31b ####
wave2_q31b <- byds %>%
  select(fullid_02, qy231ba, qy231bb, qy231bc, qy231bd, qy231be, qy231bf, qy231bg, qy231bh)

wave2_q31b$qy231b <- apply(wave2_q31b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q31b;

wave2_q31b %>% 
  select(fullid_02, qy231b) %>%
  mutate(qy231b = 
           str_replace(qy231b, pattern = '^.*?,', replacement = ""),
         qy231b = str_trim(qy231b, side='left')) %>%
  filter(fullid_02 != qy231b) -> wave2_q31b

# question 33b ####
wave2_q33b <- byds %>%
  select(fullid_02, qy233ba, qy233bb, qy233bc, qy233bd, qy233be, qy233bf, qy233bg, qy233bh, qy233bi, qy233bj, qy233bk)

wave2_q33b$qy233b <- apply(wave2_q33b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q33b;

wave2_q33b %>% 
  select(fullid_02, qy233b) %>%
  mutate(qy233b = 
           str_replace(qy233b, pattern = '^.*?,', replacement = ""),
         qy233b = str_trim(qy233b, side='left')) %>%
  filter(fullid_02 != qy233b) -> wave2_q33b

# question 33c ####
wave2_q33c <- byds %>%
  select(fullid_02, qy233ca, qy233cb, qy233cc, qy233cd, qy233ce, qy233cf, qy233cg, qy233ch, qy233ci, qy233cj)

wave2_q33c$qy233c <- apply(wave2_q33c, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q33c;

wave2_q33c %>% 
  select(fullid_02, qy233c) %>%
  mutate(qy233c = 
           str_replace(qy233c, pattern = '^.*?,', replacement = ""),
         qy233c = str_trim(qy233c, side='left')) %>%
  filter(fullid_02 != qy233c) -> wave2_q33c

# question 35a ####
wave2_q35a <- byds %>%
  select(fullid_02, qy235aa, qy235ab, qy235ac, qy235ad, qy235ae, qy235af, qy235ag, qy235ah, qy235ai, qy235aj, qy235ak)

wave2_q35a$qy235a <- apply(wave2_q35a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q35a;

wave2_q35a %>% 
  select(fullid_02, qy235a) %>%
  mutate(qy235a = 
           str_replace(qy235a, pattern = '^.*?,', replacement = ""),
         qy235a = str_trim(qy235a, side='left')) %>%
  filter(fullid_02 != qy235a) -> wave2_q35a

# question 35b ####
wave2_q35b <- byds %>%
  select(fullid_02, qy235ba, qy235bb, qy235bc, qy235bd, qy235be, qy235bf, qy235bg, qy235bh, qy235bi, qy235bj)

wave2_q35b$qy235b <- apply(wave2_q35b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q35b;

wave2_q35b %>% 
  select(fullid_02, qy235b) %>%
  mutate(qy235b = 
           str_replace(qy235b, pattern = '^.*?,', replacement = ""),
         qy235b = str_trim(qy235b, side='left')) %>%
  filter(fullid_02 != qy235b) -> wave2_q35b

# question 38a ####
wave2_q38a <- byds %>%
  select(fullid_02, qy238aa, qy238ab, qy238ac, qy238ad, qy238ae, qy238af, qy238ag, qy238ah, qy238ai, qy238aj, qy238ak)

wave2_q38a$qy238a <- apply(wave2_q38a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q38a;

wave2_q38a %>% 
  select(fullid_02, qy238a) %>%
  mutate(qy238a = 
           str_replace(qy238a, pattern = '^.*?,', replacement = ""),
         qy238a = str_trim(qy238a, side='left')) %>%
  filter(fullid_02 != qy238a) -> wave2_q38a

# question q38b ####
wave2_q38b <- byds %>%
  select(fullid_02, qy238ba, qy238bb, qy238bc, qy238bd, qy238be, qy238bf, qy238bg, qy238bh, qy238bi, qy238bj, qy238bk)

wave2_q38b$qy238b <- apply(wave2_q38b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q38b;

wave2_q38b %>% 
  select(fullid_02, qy238b) %>%
  mutate(qy238b = 
           str_replace(qy238b, pattern = '^.*?,', replacement = ""),
         qy238b = str_trim(qy238b, side='left')) %>%
  filter(fullid_02 != qy238b) -> wave2_q38b

# question 41a ####
wave2_q41a <- byds %>%
  select(fullid_02, qy241aa, qy241ab, qy241ac, qy241ad, qy241ae, qy241af, qy241ag, qy241ah, qy241ai, qy241aj, qy241ak)

wave2_q41a$qy241a <- apply(wave2_q41a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q41a;

wave2_q41a %>% 
  select(fullid_02, qy241a) %>%
  mutate(qy241a = 
           str_replace(qy241a, pattern = '^.*?,', replacement = ""),
         qy241a = str_trim(qy241a, side='left')) %>%
  filter(fullid_02 != qy241a) -> wave2_q41a

# question q41b ####
wave2_q41b <- byds %>%
  select(fullid_02, qy241ba, qy241bb, qy241bc, qy241bd, qy241be, qy241bf, qy241bg, qy241bh, qy241bi, qy241bj)

wave2_q41b$qy241b <- apply(wave2_q41b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q41b;

wave2_q41b %>% 
  select(fullid_02, qy241b) %>%
  mutate(qy241b = 
           str_replace(qy241b, pattern = '^.*?,', replacement = ""),
         qy241b = str_trim(qy241b, side='left')) %>%
  filter(fullid_02 != qy241b) -> wave2_q41b

# question 44a ####
wave2_q44a <- byds %>%
  select(fullid_02, qy244ab, qy244ac, qy244ad, qy244ae, qy244af, qy244ag, qy244ah, qy244ai, qy244aj, qy244ak)

wave2_q44a$qy244a <- apply(wave2_q44a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q44a;

wave2_q44a %>% 
  select(fullid_02, qy244a) %>%
  mutate(qy244a = 
           str_replace(qy244a, pattern = '^.*?,', replacement = ""),
         qy244a = str_trim(qy244a, side='left')) %>%
  filter(fullid_02 != qy244a) -> wave2_q44a


# question 44b ####
wave2_q44b <- byds %>%
  select(fullid_02, qy244ba, qy244bb, qy244bc, qy244bd, qy244be, qy244bf, qy244bg, qy244bh, qy244bi, qy244bj, qy244bk)

wave2_q44b$qy244b <- apply(wave2_q44b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q44b;

wave2_q44b %>% 
  select(fullid_02, qy244b) %>%
  mutate(qy244b = 
           str_replace(qy244b, pattern = '^.*?,', replacement = ""),
         qy244b = str_trim(qy244b, side='left')) %>%
  filter(fullid_02 != qy244b) -> wave2_q44b

# question q47a ####
wave2_q47a <- byds %>%
  select(fullid_02, qy247aa, qy247ab, qy247ac, qy247ad, qy247ae, qy247af, qy247ag, qy247ah, qy247ai, qy247aj, qy247ak)

wave2_q47a$qy247a <- apply(wave2_q47a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q47a;

wave2_q47a %>% 
  select(fullid_02, qy247a) %>%
  mutate(qy247a = 
           str_replace(qy247a, pattern = '^.*?,', replacement = ""),
         qy247a = str_trim(qy247a, side='left')) %>%
  filter(fullid_02 != qy247a) -> wave2_q47a

# question 47b ####
wave2_q47b <- byds %>%
  select(fullid_02, qy247ba, qy247bb, qy247bc, qy247bd, qy247be, qy247bf, qy247bg, qy247bh, qy247bi, qy247bj)

wave2_q47b$qy247b <- apply(wave2_q47b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q47b;

wave2_q47b %>% 
  select(fullid_02, qy247b) %>%
  mutate(qy247b = 
           str_replace(qy247b, pattern = '^.*?,', replacement = ""),
         qy247b = str_trim(qy247b, side='left')) %>%
  filter(fullid_02 != qy247b) -> wave2_q47b

# question 50a ####
wave2_q50a <- byds %>%
  select(fullid_02, qy250aa, qy250ab, qy250ac, qy250ad, qy250ae, qy250af, qy250ag, qy250ah, qy250ai, qy250aj, qy250ak)

wave2_q50a$qy250a <- apply(wave2_q50a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q50a;

wave2_q50a %>% 
  select(fullid_02, qy250a) %>%
  mutate(qy250a = 
           str_replace(qy250a, pattern = '^.*?,', replacement = ""),
         qy250a = str_trim(qy250a, side='left')) %>%
  filter(fullid_02 != qy250a) -> wave2_q50a

# question 50b ####
wave2_q50b <- byds %>%
  select(fullid_02, qy250ba, qy250bb, qy250bc, qy250bd, qy250be, qy250bf, qy250bg, qy250bh, qy250bi, qy250bj)

wave2_q50b$qy250b <- apply(wave2_q50b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q50b;

wave2_q50b %>% 
  select(fullid_02, qy250b) %>%
  mutate(qy250b = 
           str_replace(qy250b, pattern = '^.*?,', replacement = ""),
         qy250b = str_trim(qy250b, side='left')) %>%
  filter(fullid_02 != qy250b) -> wave2_q50b

# question 53a ####
wave2_q53a <- byds %>%
  select(fullid_02, qy253aa, qy253ab, qy253ac, qy253ad, qy253ae, qy253af, qy253ag, qy253ah, qy253ai, qy253aj, qy253ak)

wave2_q53a$qy253a <- apply(wave2_q53a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q53a;

wave2_q53a %>% 
  select(fullid_02, qy253a) %>%
  mutate(qy253a = 
           str_replace(qy253a, pattern = '^.*?,', replacement = ""),
         qy253a = str_trim(qy253a, side='left')) %>%
  filter(fullid_02 != qy253a) -> wave2_q53a

# question 53b ####
wave2_q53b <- byds %>%
  select(fullid_02, qy253ba, qy253bb, qy253bc, qy253bd, qy253be, qy253bf, qy253bg, qy253bh, qy253bi, qy253bj)

wave2_q53b$qy253b <- apply(wave2_q53b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q53b;

wave2_q53b %>% 
  select(fullid_02, qy253b) %>%
  mutate(qy253b = 
           str_replace(qy253b, pattern = '^.*?,', replacement = ""),
         qy253b = str_trim(qy253b, side='left')) %>%
  filter(fullid_02 != qy253b) -> wave2_q53b

# question 56a ####
wave2_q56a <- byds %>%
  select(fullid_02, qy256aa, qy256ab, qy256ac, qy256ad, qy256ae, qy256af, qy256ag, qy256ah, qy256ai, qy256aj, qy256ak)

wave2_q56a$qy256a <- apply(wave2_q56a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q56a;

wave2_q56a %>% 
  select(fullid_02, qy256a) %>%
  mutate(qy256a = 
           str_replace(qy256a, pattern = '^.*?,', replacement = ""),
         qy256a = str_trim(qy256a, side='left')) %>%
  filter(fullid_02 != qy256a) -> wave2_q56a

# question 56b ####
wave2_q56b <- byds %>%
  select(fullid_02, qy256ba, qy256bb, qy256bc, qy256bd, qy256be, qy256bf, qy256bg, qy256bh, qy256bi, qy256bj)

wave2_q56b$qy256b <- apply(wave2_q56b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q56b;

wave2_q56b %>% 
  select(fullid_02, qy256b) %>%
  mutate(qy256b = 
           str_replace(qy256b, pattern = '^.*?,', replacement = ""),
         qy256b = str_trim(qy256b, side='left')) %>%
  filter(fullid_02 != qy256b) -> wave2_q56b

# question 59a ####
wave2_q59a <- byds %>%
  select(fullid_02, qy259aa, qy259ab, qy259ac, qy259ad, qy259ae, qy259af, qy259ag, qy259ah, qy259ai, qy259aj, qy259ak)

wave2_q59a$qy259a <- apply(wave2_q59a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q59a;

wave2_q59a %>% 
  select(fullid_02, qy259a) %>%
  mutate(qy259a = 
           str_replace(qy259a, pattern = '^.*?,', replacement = ""),
         qy259a = str_trim(qy259a, side='left')) %>%
  filter(fullid_02 != qy259a) -> wave2_q59a

# question 59b ####
wave2_q59b <- byds %>%
  select(fullid_02, qy259ba, qy259bb, qy259bc, qy259bd, qy259be, qy259bf, qy259bg, qy259bh, qy259bi, qy259bj)

wave2_q59b$qy259b <- apply(wave2_q59b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q59b;

wave2_q59b %>% 
  select(fullid_02, qy259b) %>%
  mutate(qy259b = 
           str_replace(qy259b, pattern = '^.*?,', replacement = ""),
         qy259b = str_trim(qy259b, side='left')) %>%
  filter(fullid_02 != qy259b) -> wave2_q59b

# question 62a ####
wave2_q62a <- byds %>%
  select(fullid_02, qy262aa, qy262ab, qy262ac, qy262ad, qy262ae, qy262af, qy262ag, qy262ah, qy262ai, qy262aj, qy262ak)

wave2_q62a$qy262a <- apply(wave2_q62a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q62a;

wave2_q62a %>% 
  select(fullid_02, qy262a) %>%
  mutate(qy262a = 
           str_replace(qy262a, pattern = '^.*?,', replacement = ""),
         qy262a = str_trim(qy262a, side='left')) %>%
  filter(fullid_02 != qy262a) -> wave2_q62a



# question 62b ####
wave2_q62b <- byds %>%
  select(fullid_02, qy262ba, qy262bb, qy262bc, qy262bd, qy262be, qy262bf, qy262bg, qy262bh, qy262bi, qy262bj, qy262bk)

wave2_q62b$qy262b <- apply(wave2_q62b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q62b;

wave2_q62b %>% 
  select(fullid_02, qy262b) %>%
  mutate(qy262b = 
           str_replace(qy262b, pattern = '^.*?,', replacement = ""),
         qy262b = str_trim(qy262b, side='left')) %>%
  filter(fullid_02 != qy262b) -> wave2_q62b

# question 65a ####
wave2_q65a <- byds %>%
  select(fullid_02, qy265aa, qy265ab, qy265ac, qy265ad, qy265ae, qy265af, qy265ag, qy265ah, qy265ai, qy265aj, qy265ak)

wave2_q65a$qy265a <- apply(wave2_q65a, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q65a;

wave2_q65a %>% 
  select(fullid_02, qy265a) %>%
  mutate(qy265a = 
           str_replace(qy265a, pattern = '^.*?,', replacement = ""),
         qy265a = str_trim(qy265a, side='left')) %>%
  filter(fullid_02 != qy265a) -> wave2_q65a

# question 65b ####
wave2_q65b <- byds %>%
  select(fullid_02, qy265ba, qy265bb, qy265bc, qy265bd, qy265be, qy265bf, qy265bg, qy265bh, qy265bi, qy265bj)

wave2_q65b$qy265b <- apply(wave2_q65b, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q65b;

wave2_q65b %>% 
  select(fullid_02, qy265b) %>%
  mutate(qy265b = 
           str_replace(qy265b, pattern = '^.*?,', replacement = ""),
         qy265b = str_trim(qy265b, side='left')) %>%
  filter(fullid_02 != qy265b) -> wave2_q65b


# question 66 ####
wave2_q66 <- byds %>%
  select(fullid_02, qy266a, qy266b, qy266c, qy266d, qy266e, qy266f, qy266g, qy266h, qy266i, qy266j, qy266k, qy266l, qy266m)

wave2_q66$qy266 <- apply(wave2_q66, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); wave2_q66;

wave2_q66 %>% 
  select(fullid_02, qy266) %>%
  mutate(qy266 = 
           str_replace(qy266, pattern = '^.*?,', replacement = ""),
         qy266 = str_trim(qy266, side='left')) %>%
  filter(fullid_02 != qy266) -> wave2_q66

# join it back together ####

wave_2 <- byds %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, x2_01, x3_01, fullid_01, x3, qy21, qy23i, qy24, 
         qy26i, qy27i, qy28i, bkqy29s, qy29ap, qy29bp, qy29cp, qy29dp, qy29ep, qy29fp, qy29gp, qy29hp, qy29ip, qy29jp, qy29kp, qy29lp, 
         qy29mp, qy29np, qy29op, qy29pp, qy29qp, qy29rp, qy29sp, qy29tp, qy29up, qy210i, qy211ai, qy211bi, qy211ci, qy211di, qy211ei, 
         qy211fi, qy211gi, qy211hi, qy211ii, qy212, qy213, bkqy214s, qy214ap, qy214bp, qy214cp, qy214dp, qy214ep, qy214fp, qy214gp, 
         qy214hp, qy214ip, qy214jp, qy214kp, qy214lp, bkqy215s, qy215ap, qy215bp, qy215cp, qy215dp, qy215ep, qy215fp, qy215gp, 
         qy215hp, qy215ip, qy215jp, qy215kp, qy215lp, qy215mp, qy216, qy216b, qy217, qy217b, qy218, qy218b, qy219, qy219b, qy220, 
         qy220b, qy221, qy221b, qy222, qy222b, qy223, qy223b, qy224, qy224b, qy225, qy225b, qy226, qy226b, qy227, qy227b, qy228, 
         qy228b, qy229, qy229b, qy230, qy231, qy232, qy233, qy233a, qy234, qy235, qy235c, qy236, qy237, qy238, qy238c, qy239, qy240, 
         qy241, qy241c, qy242, qy243, qy244, qy244c, qy245, qy246, qy247, qy247c, qy248, qy249, qy250, qy250c, qy251, qy252, 
         qy253, qy253c, qy254, qy255, qy256, qy256c, qy257, qy258, qy259, qy259c, qy260, qy261, qy262, qy262c, qy263, qy264, 
         qy265, qy265c, x178, x179, x180, qy266s, x182, x183, qy267, qy268, qy269, qy270, qy270a, qy271, qy272, qy273, qy274i, 
         qy275i, qy276, bkqy277s, qy277ap, qy277bp, qy277cp, qy277dp, qy277ep, qy277fp, qy277gp, qy277hp, qy277ip, bkqy278s, 
         qy278ap, qy278bp, qy278cp, qy278dp, qy278ep, bkqy279s, qy279ap, qy279bp, qy279cp, qy279dp, qy279ep, bkqy280s, qy280ap, 
         qy280bp, qy280cp, qy280dp, qy280ep, qy281, qy282, bkqy283s, qy283ap, qy283bp, qy283cp, qy283dp, qy283ep, qy283fp, qy283gp, 
         qy283hp, qy283ip, qy283jp, qy283kp, qy283lp, qy283mp, qy284, qy284a, qy285, qy285a, qy286, qy286a, qy287, qy287a, bkqy288s, 
         qy288ap, qy288bp, qy288cp, qy288dp, qy288ep, qy288fp, qy288gp, qy288hp, qy288ip, qy288jp, qy288kp, qy288lp, qy288mp, 
         qy288np, qy288op, qy288pp, qy288qp, lat_clas, source01, x2_02, x3_02)

wave_2 <- wave_2 %>%  
  left_join(wave2_q2, by = "fullid_02") %>%
  left_join(wave2_q4, by = "fullid_02") %>%
  left_join(wave2_q5, by = "fullid_02") %>%
  
  left_join(wave2_q31a, by = "fullid_02") %>%
  left_join(wave2_q31b, by = "fullid_02") %>%
  
  left_join(wave2_q33b, by = "fullid_02") %>%
  left_join(wave2_q33c, by = "fullid_02") %>%
  
  left_join(wave2_q35a, by = c("fullid_02")) %>%
  left_join(wave2_q35b, by = c("fullid_02")) %>%
  
  left_join(wave2_q38a, by = c("fullid_02")) %>%
  left_join(wave2_q38b, by = c("fullid_02")) %>%
  
  left_join(wave2_q41a, by = c("fullid_02")) %>%
  left_join(wave2_q41b, by = c("fullid_02")) %>%
  
  left_join(wave2_q44a, by = c("fullid_02")) %>%
  left_join(wave2_q44b, by = c("fullid_02")) %>%
  
  left_join(wave2_q47a, by = c("fullid_02")) %>%
  left_join(wave2_q47b, by = c("fullid_02")) %>%
  
  left_join(wave2_q50a, by = c("fullid_02")) %>%
  left_join(wave2_q50b, by = c("fullid_02")) %>%
  
  left_join(wave2_q53a, by = c("fullid_02")) %>%
  left_join(wave2_q53b, by = c("fullid_02")) %>%
  
  left_join(wave2_q56a, by = c("fullid_02")) %>%
  left_join(wave2_q56b, by = c("fullid_02")) %>%
  
  left_join(wave2_q59a, by = c("fullid_02")) %>%
  left_join(wave2_q59b, by = c("fullid_02")) %>%
  
  left_join(wave2_q62a, by = c("fullid_02")) %>%
  left_join(wave2_q62b, by = c("fullid_02")) %>%
  
  left_join(wave2_q65a, by = c("fullid_02")) %>%
  left_join(wave2_q65b, by = c("fullid_02")) %>%
  
  left_join(wave2_q66, by = c("fullid_02"))


# remove the excess ####
rm(wave2_q2, wave2_q4, wave2_q5, wave2_q31a, wave2_q31b, wave2_q33b, wave2_q33c, wave2_q35a, wave2_q35b, wave2_q38a, wave2_q38b,
   wave2_q41a, wave2_q41b, wave2_q44a, wave2_q44b, wave2_q47a, wave2_q47b, wave2_q50a, wave2_q50b, wave2_q53a, wave2_q53b, 
   wave2_q56a, wave2_q56b, wave2_q59a, wave2_q59b, wave2_q62a, wave2_q62b, wave2_q65a, wave2_q65b, wave2_q66)

wave_2 <- wave_2 %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, x2_01, x3_01, fullid_01, x3, qy21, qy22, qy23i, qy24, 
         qy24a, qy25, qy26i, qy27i, qy28i, bkqy29s, qy29ap, qy29bp, qy29cp, qy29dp, qy29ep, qy29fp, qy29gp, qy29hp, qy29ip, qy29jp, qy29kp, qy29lp, 
         qy29mp, qy29np, qy29op, qy29pp, qy29qp, qy29rp, qy29sp, qy29tp, qy29up, qy210i, qy211ai, qy211bi, qy211ci, qy211di, qy211ei, 
         qy211fi, qy211gi, qy211hi, qy211ii, qy212, qy213, bkqy214s, qy214ap, qy214bp, qy214cp, qy214dp, qy214ep, qy214fp, qy214gp, 
         qy214hp, qy214ip, qy214jp, qy214kp, qy214lp, bkqy215s, qy215ap, qy215bp, qy215cp, qy215dp, qy215ep, qy215fp, qy215gp, 
         qy215hp, qy215ip, qy215jp, qy215kp, qy215lp, qy215mp, qy216, qy216b, qy217, qy217b, qy218, qy218b, qy219, qy219b, qy220, 
         qy220b, qy221, qy221b, qy222, qy222b, qy223, qy223b, qy224, qy224b, qy225, qy225b, qy226, qy226b, qy227, qy227b, qy228, 
         qy228b, qy229, qy229b, qy230, qy231, qy231a, qy231b, qy232, qy233, qy233a, qy233b, qy233c, qy234, qy235, qy235a, qy235b, qy235c, qy236, 
         qy237, qy238, qy238a, qy238b, qy238c, qy239, qy240, qy241, qy241a, qy241b, qy241c, qy242, qy243, qy244, qy244a, qy244b, qy244c, qy245, 
         qy246, qy247, qy247a, qy247b, qy247c, qy248, qy249, qy250, qy250a, qy250b, qy250c, qy251, qy252, qy253, qy253a, qy253b, qy253c, 
         qy254, qy255, qy256, qy256a, qy256b, qy256c, qy257, qy258, qy259, qy259a, qy259b, qy259c, qy260, qy261, qy262, qy262a, qy262b,
         qy262c, qy263, qy264, qy265, qy265a, qy265b, qy265c, x178, x179, x180, qy266s, qy266, x182, x183, qy267, qy268, qy269, qy270, 
         qy270a, qy271, qy272, qy273, qy274i, 
         qy275i, qy276, bkqy277s, qy277ap, qy277bp, qy277cp, qy277dp, qy277ep, qy277fp, qy277gp, qy277hp, qy277ip, bkqy278s, 
         qy278ap, qy278bp, qy278cp, qy278dp, qy278ep, bkqy279s, qy279ap, qy279bp, qy279cp, qy279dp, qy279ep, bkqy280s, qy280ap, 
         qy280bp, qy280cp, qy280dp, qy280ep, qy281, qy282, bkqy283s, qy283ap, qy283bp, qy283cp, qy283dp, qy283ep, qy283fp, qy283gp, 
         qy283hp, qy283ip, qy283jp, qy283kp, qy283lp, qy283mp, qy284, qy284a, qy285, qy285a, qy286, qy286a, qy287, qy287a, bkqy288s, 
         qy288ap, qy288bp, qy288cp, qy288dp, qy288ep, qy288fp, qy288gp, qy288hp, qy288ip, qy288jp, qy288kp, qy288lp, qy288mp, 
         qy288np, qy288op, qy288pp, qy288qp, lat_clas, source01, x2_02, x3_02)


# break ####

##########################################
#     COLLAPSE RESPONSES - WAVE 3
##########################################
# cleaning ####

# question 2 ####
q2 <- byds %>%
  select(fullid_03, qy32a, qy32b, qy32c, qy32d, qy32e, qy32f, qy32g, qy32h, qy32i)

q2$qy32 <- apply(q2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q2;

q2 %>% 
  select(fullid_03, qy32) %>%
  mutate(qy32 = 
           str_replace(qy32, pattern = '^.*?,', replacement = ""),
         qy32 = str_trim(qy32, side='left')) %>%
  filter(fullid_03 != qy32) -> q2

# question 31 ####
q31_1 <- byds %>%
  select(fullid_03, qy331aa, qy331ab, qy331ac, qy331ad, qy331ae, qy331af, qy331ag, qy331ah, qy331ai)

q31_1$qy331a <- apply(q31_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q31_1;

q31_1 %>% 
  select(fullid_03, qy331a) %>%
  mutate(qy331a = 
           str_replace(qy331a, pattern = '^.*?,', replacement = ""),
         qy331a = str_trim(qy331a, side='left')) %>%
  filter(fullid_03 != qy331a) -> q31_1

# question 33_1 ####
q33_1 <- byds %>%
  select(fullid_03, qy333aa, qy333ab, qy333ac, qy333ad, qy333ae)

q33_1$qy333a <- apply(q33_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q33_1;

q33_1 %>% 
  select(fullid_03, qy333a) %>%
  mutate(qy333a = 
           str_replace(qy333a, pattern = '^.*?,', replacement = ""),
         qy333a = str_trim(qy333a, side='left')) %>%
  filter(fullid_03 != qy333a) -> q33_1

# question 33_2 ####
q33_2 <- byds %>%
  select(fullid_03, qy333ba, qy333bb, qy333bc, qy333bd, qy333be, qy333bf, qy333bg, qy333bh, qy333bi, qy333bj, qy333bk)

q33_2$qy333b <- apply(q33_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q33_2;

q33_2 %>% 
  select(fullid_03, qy333b) %>%
  mutate(qy333b = 
           str_replace(qy333b, pattern = '^.*?,', replacement = ""),
         qy333b = str_trim(qy333b, side='left')) %>%
  filter(fullid_03 != qy333b) -> q33_2

# question 33_3 ####
q33_3 <- byds %>%
  select(fullid_03, qy333ca, qy333cb, qy333cc, qy333cd, qy333ce, qy333cf, qy333cg, qy333ch, qy333ci, qy333cj)

q33_3$qy333c <- apply(q33_3, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q33_3;

q33_3 %>% 
  select(fullid_03, qy333c) %>%
  mutate(qy333c = 
           str_replace(qy333c, pattern = '^.*?,', replacement = ""),
         qy333c = str_trim(qy333c, side='left')) %>%
  filter(fullid_03 != qy333c) -> q33_3

# question 33_4 ####
q33_4 <- byds %>%
  select(fullid_03, qy333da, qy333db, qy333dc, qy333dd, qy333de, qy333df, qy333dg, qy333dh)

q33_4$qy333d <- apply(q33_4, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q33_4;

q33_4 %>% 
  select(fullid_03, qy333d) %>%
  mutate(qy333d = 
           str_replace(qy333d, pattern = '^.*?,', replacement = ""),
         qy333d = str_trim(qy333d, side='left')) %>%
  filter(fullid_03 != qy333d) -> q33_4

# question 35_1 ####
q35_1 <- byds %>%
  select(fullid_03, qy335aa, qy335ab, qy335ac, qy335ad, qy335ae, qy335af, qy335ag, qy335ah, qy335ai, qy335aj, qy335ak)


q35_1$qy335a <- apply(q35_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q35_1;

q35_1 %>% 
  select(fullid_03, qy335a) %>%
  mutate(qy335a = 
           str_replace(qy335a, pattern = '^.*?,', replacement = ""),
         qy335a = str_trim(qy335a, side='left')) %>%
  filter(fullid_03 != qy335a) -> q35_1

# question 35_2 ####
q35_2 <- byds %>%
  select(fullid_03, qy335ba, qy335bb, qy335bc, qy335bd, qy335be, qy335bf, qy335bg, qy335bh, qy335bi, qy335bj)

q35_2$qy335b <- apply(q35_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q35_2;

q35_2 %>% 
  select(fullid_03, qy335b) %>%
  mutate(qy335b = 
           str_replace(qy335b, pattern = '^.*?,', replacement = ""),
         qy335b = str_trim(qy335b, side='left')) %>%
  filter(fullid_03 != qy335b) -> q35_2

# question 38_1 ####
q38_1 <- byds %>%
  select(fullid_03, qy338aa, qy338ab, qy338ac, qy338ad, qy338ae, qy338af, qy338ag, qy338ah, qy338ai, qy338aj, qy338ak)

q38_1$qy338a <- apply(q38_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q38_1;

q38_1 %>% 
  select(fullid_03, qy338a) %>%
  mutate(qy338a = 
           str_replace(qy338a, pattern = '^.*?,', replacement = ""),
         qy338a = str_trim(qy338a, side='left')) %>%
  filter(fullid_03 != qy338a) -> q38_1

# question 38_2
q38_2 <- byds %>%
  select(fullid_03, qy338ba, qy338bb, qy338bc, qy338bd, qy338be, qy338bf, qy338bg, qy338bh, qy338bi, qy338bj, qy338bk)

q38_2$qy338b <- apply(q38_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q38_2;

q38_2 %>% 
  select(fullid_03, qy338b) %>%
  mutate(qy338b = 
           str_replace(qy338b, pattern = '^.*?,', replacement = ""),
         qy338b = str_trim(qy338b, side='left')) %>%
  filter(fullid_03 != qy338b) -> q38_2

# question 38_3 ####
q38_3 <- byds %>%
  select(fullid_03, qy338ca, qy338cb, qy338cc, qy338cd, qy338ce, qy338cf, qy338cg)

q38_3$qy338c <- apply(q38_3, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q38_3;

q38_3 %>% 
  select(fullid_03, qy338c) %>%
  mutate(qy338c = 
           str_replace(qy338c, pattern = '^.*?,', replacement = ""),
         qy338c = str_trim(qy338c, side='left')) %>%
  filter(fullid_03 != qy338c) -> q38_3

# question 41_1 ####
q41_1 <- byds %>%
  select(fullid_03, qy341aa, qy341ab, qy341ac, qy341ad, qy341ae, qy341af, qy341ag, qy341ah, qy341ai, qy341aj, qy341ak)

q41_1$qy341a <- apply(q41_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q41_1;

q41_1 %>% 
  select(fullid_03, qy341a) %>%
  mutate(qy341a = 
           str_replace(qy341a, pattern = '^.*?,', replacement = ""),
         qy341a = str_trim(qy341a, side='left')) %>%
  filter(fullid_03 != qy341a) -> q41_1

# question 41_2 ####
q41_2 <- byds %>%
  select(fullid_03, qy341ba, qy341bb, qy341bc, qy341bd, qy341be, qy341bf, qy341bg, qy341bh, qy341bi, qy341bj)

q41_2$qy341b <- apply(q41_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q41_2;

q41_2 %>% 
  select(fullid_03, qy341b) %>%
  mutate(qy341b = 
           str_replace(qy341b, pattern = '^.*?,', replacement = ""),
         qy341b = str_trim(qy341b, side='left')) %>%
  filter(fullid_03 != qy341b) -> q41_2

# question 41_3 ####
q41_3 <- byds %>%
  select(fullid_03, qy341ca, qy341cb, qy341cc, qy341cd, qy341ce, qy341cf, qy341cg)

q41_3$qy341c <- apply(q41_3, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q41_3;

q41_3 %>% 
  select(fullid_03, qy341c) %>%
  mutate(qy341c = 
           str_replace(qy341c, pattern = '^.*?,', replacement = ""),
         qy341c = str_trim(qy341c, side='left')) %>%
  filter(fullid_03 != qy341c) -> q41_3

# question 44_1 ####
q44_1 <- byds %>%
  select(fullid_03, qy344aa, qy344ab, qy344ac, qy344ad, qy344ae, qy344af, qy344ag, qy344ah, qy344ai, qy344aj, qy344ak)

q44_1$qy344a <- apply(q44_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q44_1;

q44_1 %>% 
  select(fullid_03, qy344a) %>%
  mutate(qy344a = 
           str_replace(qy344a, pattern = '^.*?,', replacement = ""),
         qy344a = str_trim(qy344a, side='left')) %>%
  filter(fullid_03 != qy344a) -> q44_1

# question 44_2 ####
q44_2 <- byds %>%
  select(fullid_03, qy344ba, qy344bb, qy344bc, qy344bd, qy344be, qy344bf, qy344bg, qy344bh, qy344bi, qy344bj, qy344bk)

q44_2$qy344b <- apply(q44_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q44_2;

q44_2 %>% 
  select(fullid_03, qy344b) %>%
  mutate(qy344b = 
           str_replace(qy344b, pattern = '^.*?,', replacement = ""),
         qy344b = str_trim(qy344b, side='left')) %>%
  filter(fullid_03 != qy344b) -> q44_2

# question 47_1 ####
q47_1 <- byds %>%
  select(fullid_03, qy347aa, qy347ab, qy347ac, qy347ad, qy347ae, qy347af, qy347ag, qy347ah, qy347ai, qy347aj, qy347ak)

q47_1$qy347a <- apply(q47_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q47_1;

q47_1 %>% 
  select(fullid_03, qy347a) %>%
  mutate(qy347a = 
           str_replace(qy347a, pattern = '^.*?,', replacement = ""),
         qy347a = str_trim(qy347a, side='left')) %>%
  filter(fullid_03 != qy347a) -> q47_1

# question 47_2 ####
q47_2 <- byds %>%
  select(fullid_03, qy347ba, qy347bb, qy347bc, qy347bd, qy347be, qy347bf, qy347bg, qy347bh, qy347bi, qy347bj)

q47_2$qy347b <- apply(q47_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q47_2;

q47_2 %>% 
  select(fullid_03, qy347b) %>%
  mutate(qy347b = 
           str_replace(qy347b, pattern = '^.*?,', replacement = ""),
         qy347b = str_trim(qy347b, side='left')) %>%
  filter(fullid_03 != qy347b) -> q47_2

# question 47_3 ####
q47_3 <- byds %>%
  select(fullid_03, qy347ca, qy347cb, qy347cc, qy347cd, qy347ce, qy347cf, qy347cg)

q47_3$qy347c <- apply(q47_3, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q47_3;

q47_3 %>% 
  select(fullid_03, qy347c) %>%
  mutate(qy347c = 
           str_replace(qy347c, pattern = '^.*?,', replacement = ""),
         qy347c = str_trim(qy347c, side='left')) %>%
  filter(fullid_03 != qy347c) -> q47_3

# question 50_1 ####
q50_1 <- byds %>%
  select(fullid_03, qy350aa, qy350ab, qy350ac, qy350ad, qy350ae, qy350af, qy350ag, qy350ah, qy350ai, qy350aj, qy350ak)

q50_1$qy350a <- apply(q50_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q50_1;

q50_1 %>% 
  select(fullid_03, qy350a) %>%
  mutate(qy350a = 
           str_replace(qy350a, pattern = '^.*?,', replacement = ""),
         qy350a = str_trim(qy350a, side='left')) %>%
  filter(fullid_03 != qy350a) -> q50_1

# question 50_2 ####
q50_2 <- byds %>%
  select(fullid_03, qy350ba, qy350bb, qy350bc, qy350bd, qy350be, qy350bf, qy350bg, qy350bh, qy350bi, qy350bj)

q50_2$qy350b <- apply(q50_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q50_2;

q50_2 %>% 
  select(fullid_03, qy350b) %>%
  mutate(qy350b = 
           str_replace(qy350b, pattern = '^.*?,', replacement = ""),
         qy350b = str_trim(qy350b, side='left')) %>%
  filter(fullid_03 != qy350b) -> q50_2


# question 53_1 ####
q53_1 <- byds %>%
  select(fullid_03, qy353aa, qy353ab, qy353ac, qy353ad, qy353ae, qy353af, qy353ag, qy353ah, qy353ai, qy353aj, qy353ak)

q53_1$qy353a <- apply(q53_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q53_1;

q53_1 %>% 
  select(fullid_03, qy353a) %>%
  mutate(qy353a = 
           str_replace(qy353a, pattern = '^.*?,', replacement = ""),
         qy353a = str_trim(qy353a, side='left')) %>%
  filter(fullid_03 != qy353a) -> q53_1

# question 53_2 ####
q53_2 <- byds %>%
  select(fullid_03, qy353ba, qy353bb, qy353bc, qy353bd, qy353be, qy353bf, qy353bg, qy353bh, qy353bi, qy353bj)

q53_2$qy353b <- apply(q53_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q53_2;

q53_2 %>% 
  select(fullid_03, qy353b) %>%
  mutate(qy353b = 
           str_replace(qy353b, pattern = '^.*?,', replacement = ""),
         qy353b = str_trim(qy353b, side='left')) %>%
  filter(fullid_03 != qy353b) -> q53_2

# question 56_1 ####
q56_1 <- byds %>%
  select(fullid_03, qy356aa, qy356ab, qy356ac, qy356ad, qy356ae, qy356af, qy356ag, qy356ah, qy356ai, qy356aj, qy356ak)

q56_1$qy356a <- apply(q56_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q56_1;

q56_1 %>% 
  select(fullid_03, qy356a) %>%
  mutate(qy356a = 
           str_replace(qy356a, pattern = '^.*?,', replacement = ""),
         qy356a = str_trim(qy356a, side='left')) %>%
  filter(fullid_03 != qy356a) -> q56_1

# question 56_2 ####
q56_2 <- byds %>%
  select(fullid_03, qy356ba, qy356bb, qy356bc, qy356bd, qy356be, qy356bf, qy356bg, qy356bh, qy356bi, qy356bj)

q56_2$qy356b <- apply(q56_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q56_2;

q56_2 %>% 
  select(fullid_03, qy356b) %>%
  mutate(qy356b = 
           str_replace(qy356b, pattern = '^.*?,', replacement = ""),
         qy356b = str_trim(qy356b, side='left')) %>%
  filter(fullid_03 != qy356b) -> q56_2

# question 59_1 ####
q59_1 <- byds %>%
  select(fullid_03, qy359aa, qy359ab, qy359ac, qy359ad, qy359ae, qy359af, qy359ag, qy359ah, qy359ai, qy359aj, qy359ak)

q59_1$qy359a <- apply(q59_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q59_1;

q59_1 %>% 
  select(fullid_03, qy359a) %>%
  mutate(qy359a = 
           str_replace(qy359a, pattern = '^.*?,', replacement = ""),
         qy359a = str_trim(qy359a, side='left')) %>%
  filter(fullid_03 != qy359a) -> q59_1

# question 59_2
q59_2 <- byds %>%
  select(fullid_03, qy359ba, qy359bb, qy359bc, qy359bd, qy359be, qy359bf, qy359bg, qy359bh, qy359bi, qy359bj)

q59_2$qy359b <- apply(q59_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q59_2;

q59_2 %>% 
  select(fullid_03, qy359b) %>%
  mutate(qy359b = 
           str_replace(qy359b, pattern = '^.*?,', replacement = ""),
         qy359b = str_trim(qy359b, side='left')) %>%
  filter(fullid_03 != qy359b) -> q59_2

# question 62_1 ####
q62_1 <- byds %>%
  select(fullid_03, qy362aa, qy362ab, qy362ac, qy362ad, qy362ae, qy362af, qy362ag, qy362ah, qy362ai, qy362aj, qy362ak)

q62_1$qy362a <- apply(q62_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q62_1;

q62_1 %>% 
  select(fullid_03, qy362a) %>%
  mutate(qy362a = 
           str_replace(qy362a, pattern = '^.*?,', replacement = ""),
         qy362a = str_trim(qy362a, side='left')) %>%
  filter(fullid_03 != qy362a) -> q62_1

# question 62_2 ####
q62_2 <- byds %>%
  select(fullid_03, qy362ba, qy362bb, qy362bc, qy362bd, qy362be, qy362bf, qy362bg, qy362bh, qy362bi, qy362bj, qy362bk)

q62_2$qy362b <- apply(q62_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q62_2;

q62_2 %>% 
  select(fullid_03, qy362b) %>%
  mutate(qy362b = 
           str_replace(qy362b, pattern = '^.*?,', replacement = ""),
         qy362b = str_trim(qy362b, side='left')) %>%
  filter(fullid_03 != qy362b) -> q62_2

# question 65_1 ####
q65_1 <- byds %>%
  select(fullid_03, qy365aa, qy365ab, qy365ac, qy365ad, qy365ae, qy365af, qy365ag, qy365ah, qy365ai, qy365aj, qy365ak)

q65_1$qy365a <- apply(q65_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q65_1;

q65_1 %>% 
  select(fullid_03, qy365a) %>%
  mutate(qy365a = 
           str_replace(qy365a, pattern = '^.*?,', replacement = ""),
         qy365a = str_trim(qy365a, side='left')) %>%
  filter(fullid_03 != qy365a) -> q65_1

# question 65_2 ####
q65_2 <- byds %>%
  select(fullid_03, qy365ba, qy365bb, qy365bc, qy365bd, qy365be, qy365bf, qy365bg, qy365bh, qy365bi, qy365bj)

q65_2$qy365b <- apply(q65_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q65_2;

q65_2 %>% 
  select(fullid_03, qy365b) %>%
  mutate(qy365b = 
           str_replace(qy365b, pattern = '^.*?,', replacement = ""),
         qy365b = str_trim(qy365b, side='left')) %>%
  filter(fullid_03 != qy365b) -> q65_2

# question 73 ####
q73 <- byds %>%
  select(fullid_03, qy373a, qy373b, qy373c, qy373d, qy373e, qy373f, qy373g, qy373h, qy373i, qy373j, qy373k, qy373l, qy373m, qy373n, qy373o, qy373p)

q73$qy373 <- apply(q73, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q73;

q73 %>% 
  select(fullid_03, qy373) %>%
  mutate(qy373 = 
           str_replace(qy373, pattern = '^.*?,', replacement = ""),
         qy373 = str_trim(qy373, side='left')) %>%
  filter(fullid_03 != qy373) -> q73

# question 73_7 ####
q73_7 <- byds %>%
  select(fullid_03, qy373oa, qy373ob, qy373oc, qy373od, qy373oe, qy373of)

q73_7$qy373g <- apply(q73_7, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q73_7;

q73_7 <- q73_7 %>% 
  select(fullid_03, qy373g) %>%
  mutate(qy373g = 
           str_replace(qy373g, pattern = '^.*?,', replacement = ""),
         qy373g = str_trim(qy373g, side='left')) %>%
  filter(!is.na(fullid_03))

# question 74 ####
q74 <- byds %>%
  select(fullid_03, qy374a, qy374b, qy374c, qy374d)

q74$qy374 <- apply(q74, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q74;

q74 <- q74 %>% 
  select(fullid_03, qy374) %>%
  mutate(qy374 = 
           str_replace(qy374, pattern = '^.*?,', replacement = ""),
         qy374 = str_trim(qy374, side='left')) %>%
  filter(!is.na(fullid_03))

q74 <- q74 %>%
  mutate(qy374 = ifelse(qy374 == "No", "neither parent", qy374)) %>%
  mutate(qy374 = ifelse(qy374 == "No reply", NA, qy374)) %>%
  mutate(qy374 = ifelse(qy374 == "Yes Dad", "father", qy374)) %>%
  mutate(qy374 = ifelse(qy374 == "Yes Mum", "mother", qy374)) %>%
  mutate(qy374 = ifelse(qy374 == "Yes Mum, Yes Dad", "both parents", qy374)) %>%
  mutate(qy374 = ifelse(qy374 == "Yes Mum, Yes Dad, No", "both parents", qy374))


# join it back together ####

wave_3 <- byds %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, dob2003, qy31, qy33i, qy34ai, qy34, qy35i, qy36i, qy37i, 
         bkqy38s, qy38ap, qy38bp, qy38cp, qy38dp, qy38ep, qy38fp, qy38gp, qy38hp, qy38ip, qy38jp, qy38kp, qy38lp, qy38mp, qy38np, qy38op, qy38pp, qy38qp, qy38rp, 
         qy38sp, qy38tp, qy38up, qy39i, qy310ai, qy310bi, qy310ci, qy310di, qy310ei, qy310fi, qy310gi, qy310hi, qy310ii, qy311, qy311a, qy312, 
         qy312a, qy313, bkqy314s, qy314ap, qy314bp, qy314cp, qy314dp, qy314ep, qy314fp, qy314gp, qy314hp, qy314ip, qy314jp, qy314kp, qy314lp, 
         qy314mp, qy314np, qy314op, qy314pp, qy314qp, qy314rp, qy314sp, qy314tp, bkqy315s, qy315ap, qy315bp, qy315cp, qy315dp, qy315ep, qy315fp, 
         qy315gp, qy315hp, qy315ip, qy315jp, qy315kp, qy315lp, qy315mp, qy315np, qy315op, qy315pp, qy315qp, qy315rp, qy315sp, qy315tp, qy315up, 
         qy316, qy316b, qy317, qy317b, qy318, qy318b, qy319, qy319b, qy320, qy320b, qy321, qy321b, qy322, qy322b, qy323, qy323b, qy324, qy324b, qy325, 
         qy325b, qy326, qy326b, qy327, qy327b, qy328, qy328b, qy329, qy329b, qy330, qy331, qy331b, qy331ci, qy332, qy333, qy333ei, qy334, qy335, 
         qy335c, qy336, qy337, qy338, qy339, qy340, qy341, qy342, qy343, qy344 , qy344c, qy345, qy346, qy347, qy348, qy349, qy350, qy350c, 
         qy351, qy352, qy353, qy353c, qy354, qy355, qy356, qy356c, qy357, qy358, qy359, qy359c, qy360, qy361, qy362, qy362c, qy363, qy364, 
         qy365, qy365c , qy366, qy366a, qy367, qy367a, qy368, qy369, qy370, qy371, qy372, x208, x209, x210, qy373s, x212, x213, qy375, qy376, qy377, 
         qy377a, qy378, qy379, qy380i, qy381, bkqy382s, qy382ap, qy382bp, qy382cp, qy382dp, qy382ep, qy382fp, qy382gp, qy382hp, qy382ip, bkqy383s, 
         qy383ap, qy383bp, qy383cp, qy383dp, qy383ep, bkqy384s, qy384ap, qy384bp, qy384cp, qy384dp, qy384ep, bkqy385s, qy385ap, qy385bp, qy385cp, 
         qy385dp, qy385ep, bkqy386s, qy386ap, qy386bp, qy386cp, qy386dp, qy386ep, qy386fp, qy386gp, qy386hp, qy386ip, qy386jp, qy386kp, qy386lp, 
         qy386mp, bkqy387s, qy387ap, qy387bp, qy387cp, qy387dp, qy387ep, qy387fp, qy387gp, qy387hp, qy387ip, qy387jp, qy387kp, qy387lp, qy387mp, 
         qy387np, qy387op, qy388, qy388a, qy389, qy389a, qy390, qy390a, qy391, qy391a, qy392, qy393, bkqy394s, qy394ap, qy394bp, qy394cp, qy394dp, 
         qy394ep, qy394fp, qy394gp, qy394hp, qy394ip, qy394jp, qy394kp, qy394lp, qy394mp, qy395, qy395a, qy396, qy396a, qy397, qy397a, qy398, 
         qy398a, bkqy399s, qy399ap, qy399bp, qy399cp, qy399dp, qy399ep, qy399fp, qy399gp, qy399hp, qy399ip, qy399jp, qy399kp, qy399lp, qy399mp, 
         qy399np, qy399op, qy399pp, qy399qp) %>%
  rename(qy373a = x208,
         qy373b = x209,
         qy373c = x210,                                                                                                         
         qy373d = qy373s,
         qy373e = x212, 
         qy373f = x213)

#### REMEMBER THAT WE USE A LEFT_JOIN() AND NOT ANYTHING ELSE!!!! ####
wave_3 <- wave_3 %>%
  left_join(q2, by = "fullid_03") %>%
  left_join(q31_1, by = "fullid_03") %>%
  left_join(q33_1, by = "fullid_03") %>%
  left_join(q33_2, by = "fullid_03") %>%
  left_join(q33_3, by = "fullid_03") %>%
  left_join(q33_4, by = "fullid_03") %>%
  left_join(q35_1, by = "fullid_03") %>%
  left_join(q35_2, by = "fullid_03") %>%
  left_join(q38_1, by = "fullid_03") %>%
  left_join(q38_2, by = "fullid_03") %>%
  left_join(q38_3, by = "fullid_03") %>%
  left_join(q41_1, by = "fullid_03") %>%
  left_join(q41_2, by = "fullid_03") %>%
  left_join(q41_3, by = "fullid_03") %>%
  left_join(q44_1, by = "fullid_03") %>%
  left_join(q44_2, by = "fullid_03") %>%
  left_join(q47_1, by = "fullid_03") %>%
  left_join(q47_2, by = "fullid_03") %>%
  left_join(q47_3, by = "fullid_03") %>%
  left_join(q50_1, by = "fullid_03") %>%
  left_join(q50_2, by = "fullid_03") %>%
  left_join(q53_1, by = "fullid_03") %>%
  left_join(q53_2, by = "fullid_03") %>%
  left_join(q56_1, by = "fullid_03") %>%
  left_join(q56_2, by = "fullid_03") %>%
  left_join(q59_1, by = "fullid_03") %>%
  left_join(q59_2, by = "fullid_03") %>%
  left_join(q62_1, by = "fullid_03") %>%
  left_join(q62_2, by = "fullid_03") %>%
  left_join(q65_1, by = "fullid_03") %>%
  left_join(q65_2, by = "fullid_03") %>%
  left_join(q73, by = "fullid_03") %>%
  left_join(q73_7, by = "fullid_03") %>%
  left_join(q74, by = "fullid_03")

# remove extra dataframes ####
rm(q2, q31_1, q33_1, q33_2, q33_3, q33_4, q35_1, q35_2, q38_1, q38_2, q38_3, q41_1, q41_2, q41_3, q44_1, q44_2, q47_1, q47_2, q47_3, q50_1, q50_2, q53_1, q53_2,
   q56_1, q56_2, q59_1, q59_2, q62_1, q62_2, q65_1, q65_2, q73, q73_7, q74)


# reorder the columns to be sensible ####
wave_3 <- wave_3 %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, dob2003, qy31, qy32, qy33i, qy34ai, qy34, qy35i, qy36i, qy37i, bkqy38s, 
         qy38ap, qy38bp, qy38cp, qy38dp, qy38ep, qy38fp, qy38gp, qy38hp, qy38ip, qy38jp, qy38kp, qy38lp, qy38mp, qy38np, qy38op, qy38pp, qy38qp, qy38rp, 
         qy38sp, qy38tp, qy38up, qy39i, qy310ai, qy310bi, qy310ci, qy310di, qy310ei, qy310fi, qy310gi, qy310hi, qy310ii, qy311, qy311a, qy312, 
         qy312a, qy313, bkqy314s, qy314ap, qy314bp, qy314cp, qy314dp, qy314ep, qy314fp, qy314gp, qy314hp, qy314ip, qy314jp, qy314kp, qy314lp, 
         qy314mp, qy314np, qy314op, qy314pp, qy314qp, qy314rp, qy314sp, qy314tp, bkqy315s, qy315ap, qy315bp, qy315cp, qy315dp, qy315ep, qy315fp, 
         qy315gp, qy315hp, qy315ip, qy315jp, qy315kp, qy315lp, qy315mp, qy315np, qy315op, qy315pp, qy315qp, qy315rp, qy315sp, qy315tp, qy315up, 
         qy316, qy316b, qy317, qy317b, qy318, qy318b, qy319, qy319b, qy320, qy320b, qy321, qy321b, qy322, qy322b, qy323, qy323b, qy324, qy324b, qy325, 
         qy325b, qy326, qy326b, qy327, qy327b, qy328, qy328b, qy329, qy329b, qy330, qy331, qy331a, qy331b, qy331ci, qy332, qy333, qy333a, qy333b, 
         qy333c, qy333d, qy333ei, qy334, qy335, qy335a, qy335b, qy335c, qy336, qy337, qy338, qy338a, qy338b, qy338c, qy339, qy340, qy341, qy341a, 
         qy341b, qy341c, qy342, qy343, qy344 , qy344a, qy344b, qy344c, qy345, qy346, qy347, qy347a, qy347b, qy347c, qy348, qy349, qy350, qy350a, qy350b, 
         qy350c, qy351, qy352, qy353, qy353a, qy353b, qy353c, qy354, qy355, qy356, qy356a, qy356b, qy356c, qy357, qy358, qy359, qy359a, qy359b, 
         qy359c, qy360, qy361, qy362, qy362a, qy362b, qy362c, qy363, qy364, qy365, qy365a, qy365b, qy365c , qy366, qy366a, qy367, qy367a, qy368, qy369, 
         qy370, qy371, qy372, qy373, qy373a, qy373b, qy373c, qy373d, qy373e, qy373f, qy373g, qy374, qy375, qy376, qy377, qy377a, qy378, qy379, qy380i, 
         qy381, bkqy382s, qy382ap, qy382bp, qy382cp, qy382dp, qy382ep, qy382fp, qy382gp, qy382hp, qy382ip, bkqy383s, qy383ap, qy383bp, qy383cp, 
         qy383dp, qy383ep, bkqy384s, qy384ap, qy384bp, qy384cp, qy384dp, qy384ep, bkqy385s, qy385ap, qy385bp, qy385cp, qy385dp, qy385ep, bkqy386s, 
         qy386ap, qy386bp, qy386cp, qy386dp, qy386ep, qy386fp, qy386gp, qy386hp, qy386ip, qy386jp, qy386kp, qy386lp, qy386mp, bkqy387s, qy387ap, 
         qy387bp, qy387cp, qy387dp, qy387ep, qy387fp, qy387gp, qy387hp, qy387ip, qy387jp, qy387kp, qy387lp, qy387mp, qy387np, qy387op, qy388, 
         qy388a, qy389, qy389a, qy390, qy390a, qy391, qy391a, qy392, qy393, bkqy394s, qy394ap, qy394bp, qy394cp, qy394dp, qy394ep, qy394fp, 
         qy394gp, qy394hp, qy394ip, qy394jp, qy394kp, qy394lp, qy394mp, qy395, qy395a, qy396, qy396a, qy397, qy397a, qy398, qy398a, bkqy399s, 
         qy399ap, qy399bp, qy399cp, qy399dp, qy399ep, qy399fp, qy399gp, qy399hp, qy399ip, qy399jp, qy399kp, qy399lp, qy399mp, qy399np, qy399op, 
         qy399pp, qy399qp)

# break  ####

##########################################
#     COLLAPSE RESPONSES - WAVE 4
##########################################
# cleaning ####

# question q2 ####
q2 <- byds %>%
  select(fullid_04, qy42.1, qy42.2, qy42.3, qy42.4, qy42.5, qy42.6, qy42.7, qy42.8) %>%
  mutate(qy42.1 = ifelse(qy42.1 == "Yes", "Monday", qy42.1)) %>%
  mutate(qy42.1 = ifelse(qy42.1 == "1", "Not", qy42.1)) %>%
  mutate(qy42.2 = ifelse(qy42.2 == "Yes", "Tuesday", qy42.2)) %>%
  mutate(qy42.2 = ifelse(qy42.2 == "1", "Not", qy42.2)) %>%
  mutate(qy42.3 = ifelse(qy42.3 == "Yes", "Wednesday", qy42.3)) %>%
  mutate(qy42.3 = ifelse(qy42.3 == "1", "Not", qy42.3)) %>%
  mutate(qy42.4 = ifelse(qy42.4 == "Yes", "Thursday", qy42.4)) %>%
  mutate(qy42.4 = ifelse(qy42.4 == "1", "Not", qy42.4)) %>%
  mutate(qy42.5 = ifelse(qy42.5 == "Yes", "Friday", qy42.5)) %>%
  mutate(qy42.5 = ifelse(qy42.5 == "1", "Not", qy42.5)) %>%
  mutate(qy42.6 = ifelse(qy42.6 == "Yes", "Saturday", qy42.6)) %>%
  mutate(qy42.6 = ifelse(qy42.6 == "1", "Not", qy42.6)) %>%
  mutate(qy42.7 = ifelse(qy42.7 == "Yes", "Sunday", qy42.7)) %>%
  mutate(qy42.7 = ifelse(qy42.7 == "1", "Not", qy42.7)) %>%
  mutate(qy42.8 = ifelse(qy42.8 == "Yes", "Don't Go Out in the Evenings", qy42.8)) %>%
  mutate(qy42.8 = ifelse(qy42.8 == "1", "Not", qy42.8))

q2$qy42 <- apply(q2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); q2;

q2 %>% 
  select(fullid_04, qy42) %>%
  mutate(qy42 = 
           str_replace(qy42, pattern = '^.*?,', replacement = ""),
         qy42 = str_trim(qy42, side='left')) %>%
  filter(fullid_04 != qy42) -> q2

# question 31_1 ####
q31_1 <- byds %>%
  select(fullid_04, qy431a.1, qy431a.2, qy431a.3, qy431a.4, qy431a.5, qy431a.6, qy431a.7, qy431a.8) %>%
  mutate( qy431a.1 = ifelse( qy431a.1 == "Yes" , "older friends" , qy431a.1 )) %>%
  mutate( qy431a.1 = ifelse( qy431a.1 == "1" , "Not" , qy431a.1 )) %>%
  mutate( qy431a.2 = ifelse( qy431a.2 == "Yes" , "younger friends" , qy431a.2 )) %>%
  mutate( qy431a.2 = ifelse( qy431a.2 == "1" , "Not" , qy431a.2 )) %>%
  mutate( qy431a.3 = ifelse( qy431a.3 == "Yes" , "friends own age" , qy431a.3 )) %>%
  mutate( qy431a.3 = ifelse( qy431a.3 == "1" , "Not" , qy431a.3 )) %>%
  mutate( qy431a.4 = ifelse( qy431a.4 == "Yes" , "brother or sister" , qy431a.4 )) %>%
  mutate( qy431a.4 = ifelse( qy431a.4 == "1" , "Not" , qy431a.4 )) %>%
  mutate( qy431a.5 = ifelse( qy431a.5 == "Yes" , "parents" , qy431a.5 )) %>%
  mutate( qy431a.5 = ifelse( qy431a.5 == "1" , "Not" , qy431a.5 )) %>%
  mutate( qy431a.6 = ifelse( qy431a.6 == "Yes" , "buy from a shop" , qy431a.6 )) %>%
  mutate( qy431a.6 = ifelse( qy431a.6 == "1" , "Not" , qy431a.6 )) %>%
  mutate( qy431a.7 = ifelse( qy431a.7 == "Yes" , "got from house" , qy431a.7 )) %>%
  mutate( qy431a.7 = ifelse( qy431a.7 == "1" , "Not" , qy431a.7 )) %>%
  mutate( qy431a.8 = ifelse( qy431a.8 == "Yes" , "somewhere else" , qy431a.8 )) %>%
  mutate( qy431a.8 = ifelse( qy431a.8 == "1" , "Not" , qy431a.8 )) 

q31_1$qy431a <- apply(q31_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "No" & !is.na(x)], collapse = ", ")); q31_1;

q31_1 %>% 
  select(fullid_04, qy431a) %>%
  mutate(qy431a = 
           str_replace(qy431a, pattern = '^.*?,', replacement = ""),
         qy431a = str_trim(qy431a, side='left')) %>%
  filter(fullid_04 != qy431a) -> q31_1

# question 33_2 ####
q33_2 <- byds %>%
  select(fullid_04, qy433b.a, qy433b.b, qy433b.c, qy433b.d, qy433b.e, qy433b.f, qy433b.g, qy433b.h, qy433b.i, qy433b.j) %>%
  mutate( qy433b.a = ifelse( qy433b.a == "Yes" , "at home" , qy433b.a )) %>%
  mutate( qy433b.b = ifelse( qy433b.b == "Yes" , "at a friend's house" , qy433b.b )) %>%
  mutate( qy433b.c = ifelse( qy433b.c == "Yes" , "at a party" , qy433b.c )) %>%
  mutate( qy433b.d = ifelse( qy433b.d == "Yes" , "at a disco" , qy433b.d )) %>%
  mutate( qy433b.e = ifelse( qy433b.e == "Yes" , "at a youth club or community centre" , qy433b.e )) %>%
  mutate( qy433b.f = ifelse( qy433b.f == "Yes" , "at school" , qy433b.f )) %>%
  mutate( qy433b.g = ifelse( qy433b.g == "Yes" , "in a car" , qy433b.g )) %>%
  mutate( qy433b.h = ifelse( qy433b.h == "Yes" , "at a concert of festival" , qy433b.h )) %>%
  mutate( qy433b.i = ifelse( qy433b.i == "Yes" , "other outdoor space" , qy433b.i )) %>%
  mutate( qy433b.j = ifelse( qy433b.j == "Yes" , "somewhere else" , qy433b.j ))

q33_2$qy433b <- apply(q33_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q33_2;

q33_2 %>% 
  select(fullid_04, qy433b) %>%
  mutate(qy433b = 
           str_replace(qy433b, pattern = '^.*?,', replacement = ""),
         qy433b = str_trim(qy433b, side='left')) %>%
  filter(fullid_04 != qy433b) -> q33_2

# question 33_3 ####
q33_3 <- byds %>%
  select(fullid_04, qy433c.a, qy433c.b, qy433c.c, qy433c.d, qy433c.e, qy433c.f, qy433c.g, qy433c.h, qy433c.i, qy433c.j) %>%
  mutate( qy433c.a = ifelse( qy433c.a == "Yes" , "older friends" , qy433c.a )) %>%
  mutate( qy433c.b = ifelse( qy433c.b == "Yes" , "younger friends" , qy433c.b )) %>%
  mutate( qy433c.c = ifelse( qy433c.c == "Yes" , "friends own age" , qy433c.c )) %>%
  mutate( qy433c.d = ifelse( qy433c.d == "Yes" , "brother or sister" , qy433c.d )) %>%
  mutate( qy433c.e = ifelse( qy433c.e == "Yes" , "parents" , qy433c.e )) %>%
  mutate( qy433c.f = ifelse( qy433c.f == "Yes" , "other relatives" , qy433c.f )) %>%
  mutate( qy433c.g = ifelse( qy433c.g == "Yes" , "buy from an off license" , qy433c.g )) %>%
  mutate( qy433c.h = ifelse( qy433c.h == "Yes" , "someone else buys it for me" , qy433c.h )) %>%
  mutate( qy433c.i = ifelse( qy433c.i == "Yes" , "got from house" , qy433c.i )) %>%
  mutate( qy433c.j = ifelse( qy433c.j == "Yes" , "somewhere else" , qy433c.j ))

q33_3$qy433c <- apply(q33_3, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q33_3;

q33_3 %>% 
  select(fullid_04, qy433c) %>%
  mutate(qy433c = 
           str_replace(qy433c, pattern = '^.*?,', replacement = ""),
         qy433c = str_trim(qy433c, side='left')) %>%
  filter(fullid_04 != qy433c) -> q33_3

# question 35_1 ####
q35_1 <- byds %>%
  select(fullid_04, qy435a.a, qy435a.b, qy435a.c, qy435a.d, qy435a.e, qy435a.f, qy435a.g, qy435a.h, qy435a.i, qy435a.j) %>%
  mutate( qy435a.a = ifelse( qy435a.a == "Yes" , "at home" , qy435a.a )) %>%
  mutate( qy435a.b = ifelse( qy435a.b == "Yes" , "at a friend's house" , qy435a.b )) %>%
  mutate( qy435a.c = ifelse( qy435a.c == "Yes" , "at a party" , qy435a.c )) %>%
  mutate( qy435a.d = ifelse( qy435a.d == "Yes" , "at a disco" , qy435a.d )) %>%
  mutate( qy435a.e = ifelse( qy435a.e == "Yes" , "at a youth club or community centre" , qy435a.e )) %>%
  mutate( qy435a.f = ifelse( qy435a.f == "Yes" , "at school" , qy435a.f )) %>%
  mutate( qy435a.g = ifelse( qy435a.g == "Yes" , "in a car" , qy435a.g )) %>%
  mutate( qy435a.h = ifelse( qy435a.h == "Yes" , "at a concert of festival" , qy435a.h )) %>%
  mutate( qy435a.i = ifelse( qy435a.i == "Yes" , "other outdoor space" , qy435a.i )) %>%
  mutate( qy435a.j = ifelse( qy435a.j == "Yes" , "somewhere else" , qy435a.j ))

q35_1$qy435a <- apply(q35_1, 1, function(x) paste(x[!is.numeric(x) & x != "1" & x != "Not" & !is.na(x)], collapse = ", ")); q35_1;

q35_1 %>% 
  select(fullid_04, qy435a) %>%
  mutate(qy435a = 
           str_replace(qy435a, pattern = '^.*?,', replacement = ""),
         qy435a = str_trim(qy435a, side='left')) %>%
  filter(fullid_04 != qy435a) -> q35_1

# question 35_2 ####
q35_2 <- byds %>%
  select(fullid_04, qy435b.a, qy435b.b, qy435b.c, qy435b.d, qy435b.e, qy435b.f, qy435b.g, qy435b.h, qy435b.i, qy435b.j) %>%
  mutate( qy435b.a = ifelse( qy435b.a == "Yes" , "older friends" , qy435b.a )) %>%
  mutate( qy435b.b = ifelse( qy435b.b == "Yes" , "younger friends" , qy435b.b )) %>%
  mutate( qy435b.c = ifelse( qy435b.c == "Yes" , "friends own age" , qy435b.c )) %>%
  mutate( qy435b.d = ifelse( qy435b.d == "Yes" , "brother or sister" , qy435b.d )) %>%
  mutate( qy435b.e = ifelse( qy435b.e == "Yes" , "parents" , qy435b.e )) %>%
  mutate( qy435b.f = ifelse( qy435b.f == "Yes" , "other relatives" , qy435b.f )) %>%
  mutate( qy435b.g = ifelse( qy435b.g == "Yes" , "buy from an off license" , qy435b.g )) %>%
  mutate( qy435b.h = ifelse( qy435b.h == "Yes" , "someone else buys it for me" , qy435b.h )) %>%
  mutate( qy435b.i = ifelse( qy435b.i == "Yes" , "got from house" , qy435b.i )) %>%
  mutate( qy435b.j = ifelse( qy435b.j == "Yes" , "somewhere else" , qy435b.j )) 

q35_2$qy435b <- apply(q35_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q35_2;

q35_2 %>% 
  select(fullid_04, qy435b) %>%
  mutate(qy435b = 
           str_replace(qy435b, pattern = '^.*?,', replacement = ""),
         qy435b = str_trim(qy435b, side='left')) %>%
  filter(fullid_04 != qy435b) -> q35_2

# question 35_4 ####
q35_4 <- byds %>%
  select(fullid_04, qy435d.1, qy435d.2, qy435d.3, qy435d.4, qy435d.5)

q35_4$qy435d <- apply(q35_4, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q35_4;

q35_4 %>% 
  select(fullid_04, qy435d) %>%
  mutate(qy435d = 
           str_replace(qy435d, pattern = '^.*?,', replacement = ""),
         qy435d = str_trim(qy435d, side='left')) %>%
  filter(fullid_04 != qy435d) -> q35_4

# question 38_1 ####
q38_1 <- byds %>%
  select(fullid_04, qy438a.a, qy438a.b, qy438a.c, qy438a.d, qy438a.e, qy438a.f, qy438a.g, qy438a.h, qy438a.i, qy438a.j) %>%
  mutate( qy438a.a = ifelse( qy438a.a == "Yes" , "at home" , qy438a.a )) %>%
  mutate( qy438a.b = ifelse( qy438a.b == "Yes" , "at a friend's house" , qy438a.b )) %>%
  mutate( qy438a.c = ifelse( qy438a.c == "Yes" , "at a party" , qy438a.c )) %>%
  mutate( qy438a.d = ifelse( qy438a.d == "Yes" , "at a disco" , qy438a.d )) %>%
  mutate( qy438a.e = ifelse( qy438a.e == "Yes" , "at a youth club or community centre" , qy438a.e )) %>%
  mutate( qy438a.f = ifelse( qy438a.f == "Yes" , "at school" , qy438a.f )) %>%
  mutate( qy438a.g = ifelse( qy438a.g == "Yes" , "in a car" , qy438a.g )) %>%
  mutate( qy438a.h = ifelse( qy438a.h == "Yes" , "at a concert of festival" , qy438a.h )) %>%
  mutate( qy438a.i = ifelse( qy438a.i == "Yes" , "other outdoor space" , qy438a.i )) %>%
  mutate( qy438a.j = ifelse( qy438a.j == "Yes" , "somewhere else" , qy438a.j )) 

q38_1$qy438a <- apply(q38_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q38_1;

q38_1 %>% 
  select(fullid_04, qy438a) %>%
  mutate(qy438a = 
           str_replace(qy438a, pattern = '^.*?,', replacement = ""),
         qy438a = str_trim(qy438a, side='left')) %>%
  filter(fullid_04 != qy438a) -> q38_1

# question 38_2 ####
q38_2 <- byds %>%
  select(fullid_04, qy438b.a, qy438b.b, qy438b.c, qy438b.d, qy438b.e, qy438b.f, qy438b.g, qy438b.h, qy438b.i, qy438b.j, qy438b.k) %>%
  mutate( qy438b.a = ifelse( qy438b.a == "Yes" , "older friends" , qy438b.a )) %>%
  mutate( qy438b.b = ifelse( qy438b.b == "Yes" , "younger friends" , qy438b.b )) %>%
  mutate( qy438b.c = ifelse( qy438b.c == "Yes" , "friends own age" , qy438b.c )) %>%
  mutate( qy438b.d = ifelse( qy438b.d == "Yes" , "brother or sister" , qy438b.d )) %>%
  mutate( qy438b.e = ifelse( qy438b.e == "Yes" , "parents" , qy438b.e )) %>%
  mutate( qy438b.f = ifelse( qy438b.f == "Yes" , "other relatives" , qy438b.f )) %>%
  mutate( qy438b.g = ifelse( qy438b.g == "Yes" , "a dealer" , qy438b.g )) %>%
  mutate( qy438b.h = ifelse( qy438b.h == "Yes" , "buy from a shop" , qy438b.h )) %>%
  mutate( qy438b.i = ifelse( qy438b.i == "Yes" , "steal from a shop" , qy438b.i )) %>%
  mutate( qy438b.j = ifelse( qy438b.j == "Yes" , "got from house" , qy438b.j )) %>%
  mutate( qy438b.k = ifelse( qy438b.k == "Yes" , "somewhere else" , qy438b.k )) 

q38_2$qy438b <- apply(q38_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q38_2;

q38_2 %>% 
  select(fullid_04, qy438b) %>%
  mutate(qy438b = 
           str_replace(qy438b, pattern = '^.*?,', replacement = ""),
         qy438b = str_trim(qy438b, side='left')) %>%
  filter(fullid_04 != qy438b) -> q38_2

# question 41_1 ####
q41_1 <- byds %>%
  select(fullid_04, qy441a.a, qy441a.b, qy441a.c, qy441a.d, qy441a.e, qy441a.f, qy441a.g, qy441a.h, qy441a.i, qy441a.j) %>%
  mutate( qy441a.a = ifelse( qy441a.a == "Yes" , "at home" , qy441a.a )) %>%
  mutate( qy441a.b = ifelse( qy441a.b == "Yes" , "at a friend's house" , qy441a.b )) %>%
  mutate( qy441a.c = ifelse( qy441a.c == "Yes" , "at a party" , qy441a.c )) %>%
  mutate( qy441a.d = ifelse( qy441a.d == "Yes" , "at a disco" , qy441a.d )) %>%
  mutate( qy441a.e = ifelse( qy441a.e == "Yes" , "at a youth club or community centre" , qy441a.e )) %>%
  mutate( qy441a.f = ifelse( qy441a.f == "Yes" , "at school" , qy441a.f )) %>%
  mutate( qy441a.g = ifelse( qy441a.g == "Yes" , "in a car" , qy441a.g )) %>%
  mutate( qy441a.h = ifelse( qy441a.h == "Yes" , "at a concert of festival" , qy441a.h )) %>%
  mutate( qy441a.i = ifelse( qy441a.i == "Yes" , "other outdoor space" , qy441a.i )) %>%
  mutate( qy441a.j = ifelse( qy441a.j == "Yes" , "somewhere else" , qy441a.j )) 

q41_1$qy441a <- apply(q41_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q41_1;

q41_1 %>% 
  select(fullid_04, qy441a) %>%
  mutate(qy441a = 
           str_replace(qy441a, pattern = '^.*?,', replacement = ""),
         qy441a = str_trim(qy441a, side='left')) %>%
  filter(fullid_04 != qy441a) -> q41_1

# question 41_2 ####
q41_2 <- byds %>%
  select(fullid_04, qy441b.1, qy441b.2, qy441b.3, qy441b.4, qy441b.5, qy441b.6, qy441b.7, qy441b.8, qy441b.9) %>%
  mutate( qy441b.1 = ifelse( qy441b.1 == "Yes" , "older friends" , qy441b.1 )) %>%
  mutate( qy441b.2 = ifelse( qy441b.2 == "Yes" , "younger friends" , qy441b.2 )) %>%
  mutate( qy441b.3 = ifelse( qy441b.3 == "Yes" , "friends own age" , qy441b.3 )) %>%
  mutate( qy441b.4 = ifelse( qy441b.4 == "Yes" , "brother or sister" , qy441b.4 )) %>%
  mutate( qy441b.5 = ifelse( qy441b.5 == "Yes" , "parents" , qy441b.5 )) %>%
  mutate( qy441b.6 = ifelse( qy441b.6 == "Yes" , "other relatives" , qy441b.6 )) %>%
  mutate( qy441b.7 = ifelse( qy441b.7 == "Yes" , "a dealer" , qy441b.7 )) %>%
  mutate( qy441b.8 = ifelse( qy441b.8 == "Yes" , "got from house" , qy441b.8 )) %>%
  mutate( qy441b.9 = ifelse( qy441b.9 == "Yes" , "somewhere else" , qy441b.9 )) 

q41_2$qy441b <- apply(q41_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q41_2;

q41_2 %>% 
  select(fullid_04, qy441b) %>%
  mutate(qy441b = 
           str_replace(qy441b, pattern = '^.*?,', replacement = ""),
         qy441b = str_trim(qy441b, side='left')) %>%
  filter(fullid_04 != qy441b) -> q41_2

# question 44_1 ####
q44_1 <- byds %>%
  select(fullid_04, qy444a.a, qy444a.b, qy444a.c, qy444a.d, qy444a.e, qy444a.f, qy444a.g, qy444a.h, qy444a.i, qy444a.j) %>%
  mutate( qy444a.a = ifelse( qy444a.a == "Yes" , "at home" , qy444a.a )) %>%
  mutate( qy444a.b = ifelse( qy444a.b == "Yes" , "at a friend's house" , qy444a.b )) %>%
  mutate( qy444a.c = ifelse( qy444a.c == "Yes" , "at a party" , qy444a.c )) %>%
  mutate( qy444a.d = ifelse( qy444a.d == "Yes" , "at a disco" , qy444a.d )) %>%
  mutate( qy444a.e = ifelse( qy444a.e == "Yes" , "at a youth club or community centre" , qy444a.e )) %>%
  mutate( qy444a.f = ifelse( qy444a.f == "Yes" , "at school" , qy444a.f )) %>%
  mutate( qy444a.g = ifelse( qy444a.g == "Yes" , "in a car" , qy444a.g )) %>%
  mutate( qy444a.h = ifelse( qy444a.h == "Yes" , "at a concert of festival" , qy444a.h )) %>%
  mutate( qy444a.i = ifelse( qy444a.i == "Yes" , "other outdoor space" , qy444a.i )) %>%
  mutate( qy444a.j = ifelse( qy444a.j == "Yes" , "somewhere else" , qy444a.j )) 

q44_1$qy444a <- apply(q44_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q44_1;

q44_1 %>% 
  select(fullid_04, qy444a) %>%
  mutate(qy444a = 
           str_replace(qy444a, pattern = '^.*?,', replacement = ""),
         qy444a = str_trim(qy444a, side='left')) %>%
  filter(fullid_04 != qy444a) -> q44_1

# question 44_2 ####
q44_2 <- byds %>%
  select(fullid_04, qy444b.a, qy444b.b, qy444b.c, qy444b.d, qy444b.e, qy444b.f, qy444b.g, qy444b.h, qy444b.i, qy444b.j) %>%
  mutate( qy444b.a = ifelse( qy444b.a == "Yes" , "older friends" , qy444b.a )) %>%
  mutate( qy444b.b = ifelse( qy444b.b == "Yes" , "younger friends" , qy444b.b )) %>%
  mutate( qy444b.c = ifelse( qy444b.c == "Yes" , "friends own age" , qy444b.c )) %>%
  mutate( qy444b.d = ifelse( qy444b.d == "Yes" , "brother or sister" , qy444b.d )) %>%
  mutate( qy444b.e = ifelse( qy444b.e == "Yes" , "parents" , qy444b.e )) %>%
  mutate( qy444b.f = ifelse( qy444b.f == "Yes" , "other relatives" , qy444b.f )) %>%
  mutate( qy444b.g = ifelse( qy444b.g == "Yes" , "a dealer" , qy444b.g )) %>%
  mutate( qy444b.h = ifelse( qy444b.h == "Yes" , "picked them" , qy444b.h )) %>%
  mutate( qy444b.i = ifelse( qy444b.i == "Yes" , "got from house" , qy444b.i )) %>%
  mutate( qy444b.j = ifelse( qy444b.j == "Yes" , "somewhere else" , qy444b.j )) 

q44_2$qy444b <- apply(q44_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q44_2;

q44_2 %>% 
  select(fullid_04, qy444b) %>%
  mutate(qy444b = 
           str_replace(qy444b, pattern = '^.*?,', replacement = ""),
         qy444b = str_trim(qy444b, side='left')) %>%
  filter(fullid_04 != qy444b) -> q44_2

# question 47_1 ####
q47_1 <- byds %>%
  select(fullid_04, qy447a.a, qy447a.b, qy447a.c, qy447a.d, qy447a.e, qy447a.f, qy447a.g, qy447a.h, qy447a.i, qy447a.j) %>%
  mutate( qy447a.a = ifelse( qy447a.a == "Yes" , "at home" , qy447a.a )) %>%
  mutate( qy447a.b = ifelse( qy447a.b == "Yes" , "at a friend's house" , qy447a.b )) %>%
  mutate( qy447a.c = ifelse( qy447a.c == "Yes" , "at a party" , qy447a.c )) %>%
  mutate( qy447a.d = ifelse( qy447a.d == "Yes" , "at a disco" , qy447a.d )) %>%
  mutate( qy447a.e = ifelse( qy447a.e == "Yes" , "at a youth club or community centre" , qy447a.e )) %>%
  mutate( qy447a.f = ifelse( qy447a.f == "Yes" , "at school" , qy447a.f )) %>%
  mutate( qy447a.g = ifelse( qy447a.g == "Yes" , "in a car" , qy447a.g )) %>%
  mutate( qy447a.h = ifelse( qy447a.h == "Yes" , "at a concert of festival" , qy447a.h )) %>%
  mutate( qy447a.i = ifelse( qy447a.i == "Yes" , "other outdoor space" , qy447a.i )) %>%
  mutate( qy447a.j = ifelse( qy447a.j == "Yes" , "somewhere else" , qy447a.j )) 

q47_1$qy447a <- apply(q47_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q47_1;

q47_1 %>% 
  select(fullid_04, qy447a) %>%
  mutate(qy447a = 
           str_replace(qy447a, pattern = '^.*?,', replacement = ""),
         qy447a = str_trim(qy447a, side='left')) %>%
  filter(fullid_04 != qy447a) -> q47_1

# question 47_2 ####
q47_2 <- byds %>%
  select(fullid_04, qy447b.1, qy447b.2, qy447b.3, qy447b.4, qy447b.5, qy447b.6, qy447b.7, qy447b.8, qy447b.9) %>%
  mutate( qy447b.1 = ifelse( qy447b.1 == "Yes" , "older friends" , qy447b.1 )) %>%
  mutate( qy447b.2 = ifelse( qy447b.2 == "Yes" , "younger friends" , qy447b.2 )) %>%
  mutate( qy447b.3 = ifelse( qy447b.3 == "Yes" , "friends own age" , qy447b.3 )) %>%
  mutate( qy447b.4 = ifelse( qy447b.4 == "Yes" , "brother or sister" , qy447b.4 )) %>%
  mutate( qy447b.5 = ifelse( qy447b.5 == "Yes" , "parents" , qy447b.5 )) %>%
  mutate( qy447b.6 = ifelse( qy447b.6 == "Yes" , "other relatives" , qy447b.6 )) %>%
  mutate( qy447b.7 = ifelse( qy447b.7 == "Yes" , "a dealer" , qy447b.7 )) %>%
  mutate( qy447b.8 = ifelse( qy447b.8 == "Yes" , "got from house" , qy447b.8 )) %>%
  mutate( qy447b.9 = ifelse( qy447b.9 == "Yes" , "somewhere else" , qy447b.9 )) 

q47_2$qy447b <- apply(q47_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q47_2;

q47_2 %>% 
  select(fullid_04, qy447b) %>%
  mutate(qy447b = 
           str_replace(qy447b, pattern = '^.*?,', replacement = ""),
         qy447b = str_trim(qy447b, side='left')) %>%
  filter(fullid_04 != qy447b) -> q47_2

# question 50_1 ####
q50_1 <- byds %>%
  select(fullid_04, qy450a.a, qy450a.b, qy450a.c, qy450a.d, qy450a.e, qy450a.f, qy450a.g, qy450a.h, qy450a.i, qy450a.j) %>%
  mutate( qy450a.a = ifelse( qy450a.a == "Yes" , "at home" , qy450a.a )) %>%
  mutate( qy450a.b = ifelse( qy450a.b == "Yes" , "at a friend's house" , qy450a.b )) %>%
  mutate( qy450a.c = ifelse( qy450a.c == "Yes" , "at a party" , qy450a.c )) %>%
  mutate( qy450a.d = ifelse( qy450a.d == "Yes" , "at a disco" , qy450a.d )) %>%
  mutate( qy450a.e = ifelse( qy450a.e == "Yes" , "at a youth club or community centre" , qy450a.e )) %>%
  mutate( qy450a.f = ifelse( qy450a.f == "Yes" , "at school" , qy450a.f )) %>%
  mutate( qy450a.g = ifelse( qy450a.g == "Yes" , "in a car" , qy450a.g )) %>%
  mutate( qy450a.h = ifelse( qy450a.h == "Yes" , "at a concert of festival" , qy450a.h )) %>%
  mutate( qy450a.i = ifelse( qy450a.i == "Yes" , "other outdoor space" , qy450a.i )) %>%
  mutate( qy450a.j = ifelse( qy450a.j == "Yes" , "somewhere else" , qy450a.j )) 

q50_1$qy450a <- apply(q50_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q50_1;

q50_1 %>% 
  select(fullid_04, qy450a) %>%
  mutate(qy450a = 
           str_replace(qy450a, pattern = '^.*?,', replacement = ""),
         qy450a = str_trim(qy450a, side='left')) %>%
  filter(fullid_04 != qy450a) -> q50_1

# question 50_2 ####
q50_2 <- byds %>%
  select(fullid_04, qy450b.1, qy450b.2, qy450b.3, qy450b.4, qy450b.5, qy450b.6, qy450b.7, qy450b.8, qy450b.9) %>%
  mutate( qy450b.1 = ifelse( qy450b.1 == "Yes" , "older friends" , qy450b.1 )) %>%
  mutate( qy450b.2 = ifelse( qy450b.2 == "Yes" , "younger friends" , qy450b.2 )) %>%
  mutate( qy450b.3 = ifelse( qy450b.3 == "Yes" , "friends own age" , qy450b.3 )) %>%
  mutate( qy450b.4 = ifelse( qy450b.4 == "Yes" , "brother or sister" , qy450b.4 )) %>%
  mutate( qy450b.5 = ifelse( qy450b.5 == "Yes" , "parents" , qy450b.5 )) %>%
  mutate( qy450b.6 = ifelse( qy450b.6 == "Yes" , "other relatives" , qy450b.6 )) %>%
  mutate( qy450b.7 = ifelse( qy450b.7 == "Yes" , "a dealer" , qy450b.7 )) %>%
  mutate( qy450b.8 = ifelse( qy450b.8 == "Yes" , "got from house" , qy450b.8 )) %>%
  mutate( qy450b.9 = ifelse( qy450b.9 == "Yes" , "somewhere else" , qy450b.9 )) 

q50_2$qy450b <- apply(q50_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q50_2;

q50_2 %>% 
  select(fullid_04, qy450b) %>%
  mutate(qy450b = 
           str_replace(qy450b, pattern = '^.*?,', replacement = ""),
         qy450b = str_trim(qy450b, side='left')) %>%
  filter(fullid_04 != qy450b) -> q50_2


# question 53_1 ####
q53_1 <- byds %>%
  select(fullid_04, qy453a.a, qy453a.b, qy453a.c, qy453a.d, qy453a.e, qy453a.f, qy453a.g, qy453a.h, qy453a.i, qy453a.j) %>%
  mutate( qy453a.a = ifelse( qy453a.a == "Yes" , "at home" , qy453a.a )) %>%
  mutate( qy453a.b = ifelse( qy453a.b == "Yes" , "at a friend's house" , qy453a.b )) %>%
  mutate( qy453a.c = ifelse( qy453a.c == "Yes" , "at a party" , qy453a.c )) %>%
  mutate( qy453a.d = ifelse( qy453a.d == "Yes" , "at a disco" , qy453a.d )) %>%
  mutate( qy453a.e = ifelse( qy453a.e == "Yes" , "at a youth club or community centre" , qy453a.e )) %>%
  mutate( qy453a.f = ifelse( qy453a.f == "Yes" , "at school" , qy453a.f )) %>%
  mutate( qy453a.g = ifelse( qy453a.g == "Yes" , "in a car" , qy453a.g )) %>%
  mutate( qy453a.h = ifelse( qy453a.h == "Yes" , "at a concert of festival" , qy453a.h )) %>%
  mutate( qy453a.i = ifelse( qy453a.i == "Yes" , "other outdoor space" , qy453a.i )) %>%
  mutate( qy453a.j = ifelse( qy453a.j == "Yes" , "somewhere else" , qy453a.j )) 

q53_1$qy453a <- apply(q53_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q53_1;

q53_1 %>% 
  select(fullid_04, qy453a) %>%
  mutate(qy453a = 
           str_replace(qy453a, pattern = '^.*?,', replacement = ""),
         qy453a = str_trim(qy453a, side='left')) %>%
  filter(fullid_04 != qy453a) -> q53_1

# question 53_2 ####
q53_2 <- byds %>%
  select(fullid_04, qy453b.1, qy453b.2, qy453b.3, qy453b.4, qy453b.5, qy453b.6, qy453b.7, qy453b.8, qy453b.9) %>%
  mutate( qy453b.1 = ifelse( qy453b.1 == "Yes" , "older friends" , qy453b.1 )) %>%
  mutate( qy453b.2 = ifelse( qy453b.2 == "Yes" , "younger friends" , qy453b.2 )) %>%
  mutate( qy453b.3 = ifelse( qy453b.3 == "Yes" , "friends own age" , qy453b.3 )) %>%
  mutate( qy453b.4 = ifelse( qy453b.4 == "Yes" , "brother or sister" , qy453b.4 )) %>%
  mutate( qy453b.5 = ifelse( qy453b.5 == "Yes" , "parents" , qy453b.5 )) %>%
  mutate( qy453b.6 = ifelse( qy453b.6 == "Yes" , "other relatives" , qy453b.6 )) %>%
  mutate( qy453b.7 = ifelse( qy453b.7 == "Yes" , "a dealer" , qy453b.7 )) %>%
  mutate( qy453b.8 = ifelse( qy453b.8 == "Yes" , "got from house" , qy453b.8 )) %>%
  mutate( qy453b.9 = ifelse( qy453b.9 == "Yes" , "somewhere else" , qy453b.9 )) 

q53_2$qy453b <- apply(q53_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q53_2;

q53_2 %>% 
  select(fullid_04, qy453b) %>%
  mutate(qy453b = 
           str_replace(qy453b, pattern = '^.*?,', replacement = ""),
         qy453b = str_trim(qy453b, side='left')) %>%
  filter(fullid_04 != qy453b) -> q53_2

# question 56_1 ####
q56_1 <- byds %>%
  select(fullid_04, qy456a.a, qy456a.b, qy456a.c, qy456a.d, qy456a.e, qy456a.f, qy456a.g, qy456a.h, qy456a.i, qy456a.j) %>%
  mutate( qy456a.a = ifelse( qy456a.a == "Yes" , "at home" , qy456a.a )) %>%
  mutate( qy456a.b = ifelse( qy456a.b == "Yes" , "at a friend's house" , qy456a.b )) %>%
  mutate( qy456a.c = ifelse( qy456a.c == "Yes" , "at a party" , qy456a.c )) %>%
  mutate( qy456a.d = ifelse( qy456a.d == "Yes" , "at a disco" , qy456a.d )) %>%
  mutate( qy456a.e = ifelse( qy456a.e == "Yes" , "at a youth club or community centre" , qy456a.e )) %>%
  mutate( qy456a.f = ifelse( qy456a.f == "Yes" , "at school" , qy456a.f )) %>%
  mutate( qy456a.g = ifelse( qy456a.g == "Yes" , "in a car" , qy456a.g )) %>%
  mutate( qy456a.h = ifelse( qy456a.h == "Yes" , "at a concert of festival" , qy456a.h )) %>%
  mutate( qy456a.i = ifelse( qy456a.i == "Yes" , "other outdoor space" , qy456a.i )) %>%
  mutate( qy456a.j = ifelse( qy456a.j == "Yes" , "somewhere else" , qy456a.j )) 

q56_1$qy456a <- apply(q56_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q56_1;

q56_1 %>% 
  select(fullid_04, qy456a) %>%
  mutate(qy456a = 
           str_replace(qy456a, pattern = '^.*?,', replacement = ""),
         qy456a = str_trim(qy456a, side='left')) %>%
  filter(fullid_04 != qy456a) -> q56_1

# question 56_2 ####
q56_2 <- byds %>%
  select(fullid_04, qy456b.1, qy456b.2, qy456b.3, qy456b.4, qy456b.5, qy456b.6, qy456b.7, qy456b.8, qy456b.9) %>%
  mutate( qy456b.1 = ifelse( qy456b.1 == "Yes" , "older friends" , qy456b.1 )) %>%
  mutate( qy456b.2 = ifelse( qy456b.2 == "Yes" , "younger friends" , qy456b.2 )) %>%
  mutate( qy456b.3 = ifelse( qy456b.3 == "Yes" , "friends own age" , qy456b.3 )) %>%
  mutate( qy456b.4 = ifelse( qy456b.4 == "Yes" , "brother or sister" , qy456b.4 )) %>%
  mutate( qy456b.5 = ifelse( qy456b.5 == "Yes" , "parents" , qy456b.5 )) %>%
  mutate( qy456b.6 = ifelse( qy456b.6 == "Yes" , "other relatives" , qy456b.6 )) %>%
  mutate( qy456b.7 = ifelse( qy456b.7 == "Yes" , "a dealer" , qy456b.7 )) %>%
  mutate( qy456b.8 = ifelse( qy456b.8 == "Yes" , "got from house" , qy456b.8 )) %>%
  mutate( qy456b.9 = ifelse( qy456b.9 == "Yes" , "somewhere else" , qy456b.9 )) 


q56_2$qy456b <- apply(q56_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q56_2;

q56_2 %>% 
  select(fullid_04, qy456b) %>%
  mutate(qy456b = 
           str_replace(qy456b, pattern = '^.*?,', replacement = ""),
         qy456b = str_trim(qy456b, side='left')) %>%
  filter(fullid_04 != qy456b) -> q56_2

# question 59_1 ####
q59_1 <- byds %>%
  select(fullid_04, qy459a.a, qy459a.b, qy459a.c, qy459a.d, qy459a.e, qy459a.f, qy459a.g, qy459a.h, qy459a.i, qy459a.j) %>%
  mutate( qy459a.a = ifelse( qy459a.a == "Yes" , "at home" , qy459a.a )) %>%
  mutate( qy459a.b = ifelse( qy459a.b == "Yes" , "at a friend's house" , qy459a.b )) %>%
  mutate( qy459a.c = ifelse( qy459a.c == "Yes" , "at a party" , qy459a.c )) %>%
  mutate( qy459a.d = ifelse( qy459a.d == "Yes" , "at a disco" , qy459a.d )) %>%
  mutate( qy459a.e = ifelse( qy459a.e == "Yes" , "at a youth club or community centre" , qy459a.e )) %>%
  mutate( qy459a.f = ifelse( qy459a.f == "Yes" , "at school" , qy459a.f )) %>%
  mutate( qy459a.g = ifelse( qy459a.g == "Yes" , "in a car" , qy459a.g )) %>%
  mutate( qy459a.h = ifelse( qy459a.h == "Yes" , "at a concert of festival" , qy459a.h )) %>%
  mutate( qy459a.i = ifelse( qy459a.i == "Yes" , "other outdoor space" , qy459a.i )) %>%
  mutate( qy459a.j = ifelse( qy459a.j == "Yes" , "somewhere else" , qy459a.j )) 

q59_1$qy459a <- apply(q59_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q59_1;

q59_1 %>% 
  select(fullid_04, qy459a) %>%
  mutate(qy459a = 
           str_replace(qy459a, pattern = '^.*?,', replacement = ""),
         qy459a = str_trim(qy459a, side='left')) %>%
  filter(fullid_04 != qy459a) -> q59_1

# question 59_2 ####
q59_2 <- byds %>%
  select(fullid_04, qy459b.1, qy459b.2, qy459b.3, qy459b.4, qy459b.5, qy459b.6, qy459b.7, qy459b.8, qy459b.9) %>%
  mutate( qy459b.1 = ifelse( qy459b.1 == "Yes" , "older friends" , qy459b.1 )) %>%
  mutate( qy459b.2 = ifelse( qy459b.2 == "Yes" , "younger friends" , qy459b.2 )) %>%
  mutate( qy459b.3 = ifelse( qy459b.3 == "Yes" , "friends own age" , qy459b.3 )) %>%
  mutate( qy459b.4 = ifelse( qy459b.4 == "Yes" , "brother or sister" , qy459b.4 )) %>%
  mutate( qy459b.5 = ifelse( qy459b.5 == "Yes" , "parents" , qy459b.5 )) %>%
  mutate( qy459b.6 = ifelse( qy459b.6 == "Yes" , "other relatives" , qy459b.6 )) %>%
  mutate( qy459b.7 = ifelse( qy459b.7 == "Yes" , "a dealer" , qy459b.7 )) %>%
  mutate( qy459b.8 = ifelse( qy459b.8 == "Yes" , "got from house" , qy459b.8 )) %>%
  mutate( qy459b.9 = ifelse( qy459b.9 == "Yes" , "somewhere else" , qy459b.9 )) 

q59_2$qy459b <- apply(q59_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q59_2;

q59_2 %>% 
  select(fullid_04, qy459b) %>%
  mutate(qy459b = 
           str_replace(qy459b, pattern = '^.*?,', replacement = ""),
         qy459b = str_trim(qy459b, side='left')) %>%
  filter(fullid_04 != qy459b) -> q59_2

# question 62_1 ####
q62_1 <- byds %>%
  select(fullid_04, qy462a.a, qy462a.b, qy462a.c, qy462a.d, qy462a.e, qy462a.f, qy462a.g, qy462a.h, qy462a.i, qy462a.j) %>%
  mutate( qy462a.a = ifelse( qy462a.a == "Yes" , "at home" , qy462a.a )) %>%
  mutate( qy462a.b = ifelse( qy462a.b == "Yes" , "at a friend's house" , qy462a.b )) %>%
  mutate( qy462a.c = ifelse( qy462a.c == "Yes" , "at a party" , qy462a.c )) %>%
  mutate( qy462a.d = ifelse( qy462a.d == "Yes" , "at a disco" , qy462a.d )) %>%
  mutate( qy462a.e = ifelse( qy462a.e == "Yes" , "at a youth club or community centre" , qy462a.e )) %>%
  mutate( qy462a.f = ifelse( qy462a.f == "Yes" , "at school" , qy462a.f )) %>%
  mutate( qy462a.g = ifelse( qy462a.g == "Yes" , "in a car" , qy462a.g )) %>%
  mutate( qy462a.h = ifelse( qy462a.h == "Yes" , "at a concert of festival" , qy462a.h )) %>%
  mutate( qy462a.i = ifelse( qy462a.i == "Yes" , "other outdoor space" , qy462a.i )) %>%
  mutate( qy462a.j = ifelse( qy462a.j == "Yes" , "somewhere else" , qy462a.j )) 

q62_1$qy462a <- apply(q62_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q62_1;

q62_1 %>% 
  select(fullid_04, qy462a) %>%
  mutate(qy462a = 
           str_replace(qy462a, pattern = '^.*?,', replacement = ""),
         qy462a = str_trim(qy462a, side='left')) %>%
  filter(fullid_04 != qy462a) -> q62_1

# question 62_2 ####
q62_2 <- byds %>%
  select(fullid_04, qy462b.a, qy462b.b, qy462b.c, qy462b.d, qy462b.e, qy462b.f, qy462b.g, qy462b.h, qy462b.i, qy462b.j) %>%
  mutate( qy462b.a = ifelse( qy462b.a == "Yes" , "older friends" , qy462b.a )) %>%
  mutate( qy462b.b = ifelse( qy462b.b == "Yes" , "younger friends" , qy462b.b )) %>%
  mutate( qy462b.c = ifelse( qy462b.c == "Yes" , "friends own age" , qy462b.c )) %>%
  mutate( qy462b.d = ifelse( qy462b.d == "Yes" , "brother or sister" , qy462b.d )) %>%
  mutate( qy462b.e = ifelse( qy462b.e == "Yes" , "parents" , qy462b.e )) %>%
  mutate( qy462b.f = ifelse( qy462b.f == "Yes" , "other relatives" , qy462b.f )) %>%
  mutate( qy462b.g = ifelse( qy462b.g == "Yes" , "a dealer" , qy462b.g )) %>%
  mutate( qy462b.h = ifelse( qy462b.h == "Yes" , "buy from a shop" , qy462b.h )) %>%
  mutate( qy462b.i = ifelse( qy462b.i == "Yes" , "got from house" , qy462b.i )) %>%
  mutate( qy462b.j = ifelse( qy462b.j == "Yes" , "somewhere else" , qy462b.j )) 

q62_2$qy462b <- apply(q62_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q62_2;

q62_2 %>% 
  select(fullid_04, qy462b) %>%
  mutate(qy462b = 
           str_replace(qy462b, pattern = '^.*?,', replacement = ""),
         qy462b = str_trim(qy462b, side='left')) %>%
  filter(fullid_04 != qy462b) -> q62_2

# question 65_1 ####
q65_1 <- byds %>%
  select(fullid_04, qy465a.a, qy465a.b, qy465a.c, qy465a.d, qy465a.e, qy465a.f, qy465a.g, qy465a.h, qy465a.i, qy465a.j) %>%
  mutate( qy465a.a = ifelse( qy465a.a == "Yes" , "at home" , qy465a.a )) %>%
  mutate( qy465a.b = ifelse( qy465a.b == "Yes" , "at a friend's house" , qy465a.b )) %>%
  mutate( qy465a.c = ifelse( qy465a.c == "Yes" , "at a party" , qy465a.c )) %>%
  mutate( qy465a.d = ifelse( qy465a.d == "Yes" , "at a disco" , qy465a.d )) %>%
  mutate( qy465a.e = ifelse( qy465a.e == "Yes" , "at a youth club or community centre" , qy465a.e )) %>%
  mutate( qy465a.f = ifelse( qy465a.f == "Yes" , "at school" , qy465a.f )) %>%
  mutate( qy465a.g = ifelse( qy465a.g == "Yes" , "in a car" , qy465a.g )) %>%
  mutate( qy465a.h = ifelse( qy465a.h == "Yes" , "at a concert of festival" , qy465a.h )) %>%
  mutate( qy465a.i = ifelse( qy465a.i == "Yes" , "other outdoor space" , qy465a.i )) %>%
  mutate( qy465a.j = ifelse( qy465a.j == "Yes" , "somewhere else" , qy465a.j )) 

q65_1$qy465a <- apply(q65_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q65_1;

q65_1 %>% 
  select(fullid_04, qy465a) %>%
  mutate(qy465a = 
           str_replace(qy465a, pattern = '^.*?,', replacement = ""),
         qy465a = str_trim(qy465a, side='left')) %>%
  filter(fullid_04 != qy465a) -> q65_1

# question 65_2 ####
q65_2 <- byds %>%
  select(fullid_04, qy465b.1, qy465b.2, qy465b.3, qy465b.4, qy465b.5, qy465b.6, qy465b.7, qy465b.8, qy465b.9) %>%
  mutate( qy465b.1 = ifelse( qy465b.1 == "Yes" , "older friends" , qy465b.1 )) %>%
  mutate( qy465b.2 = ifelse( qy465b.2 == "Yes" , "younger friends" , qy465b.2 )) %>%
  mutate( qy465b.3 = ifelse( qy465b.3 == "Yes" , "friends own age" , qy465b.3 )) %>%
  mutate( qy465b.4 = ifelse( qy465b.4 == "Yes" , "brother or sister" , qy465b.4 )) %>%
  mutate( qy465b.5 = ifelse( qy465b.5 == "Yes" , "parents" , qy465b.5 )) %>%
  mutate( qy465b.6 = ifelse( qy465b.6 == "Yes" , "other relatives" , qy465b.6 )) %>%
  mutate( qy465b.7 = ifelse( qy465b.7 == "Yes" , "a dealer" , qy465b.7 )) %>%
  mutate( qy465b.8 = ifelse( qy465b.8 == "Yes" , "got from house" , qy465b.8 )) %>%
  mutate( qy465b.9 = ifelse( qy465b.9 == "Yes" , "somewhere else" , qy465b.9 )) 

q65_2$qy465b <- apply(q65_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q65_2;

q65_2 %>% 
  select(fullid_04, qy465b) %>%
  mutate(qy465b = 
           str_replace(qy465b, pattern = '^.*?,', replacement = ""),
         qy465b = str_trim(qy465b, side='left')) %>%
  filter(fullid_04 != qy465b) -> q65_2

# question 75_1 ####
q75_1 <- byds %>%
  select(fullid_04, qy475.1, qy475.2, qy475.3, qy475.4, qy475.5, qy475.6, qy475.7, qy475.8) %>%
  mutate( qy475.1 = ifelse( qy475.1 == "Yes" , "mother" , qy475.1 )) %>%
  mutate( qy475.2 = ifelse( qy475.2 == "Yes" , "stepmother" , qy475.2 )) %>%
  mutate( qy475.3 = ifelse( qy475.3 == "Yes" , "grandmother" , qy475.3 )) %>%
  mutate( qy475.4 = ifelse( qy475.4 == "Yes" , "foster mother" , qy475.4 )) %>%
  mutate( qy475.5 = ifelse( qy475.5 == "Yes" , "father" , qy475.5 )) %>%
  mutate( qy475.6 = ifelse( qy475.6 == "Yes" , "stepfather" , qy475.6 )) %>%
  mutate( qy475.7 = ifelse( qy475.7 == "Yes" , "grandfather" , qy475.7 )) %>%
  mutate( qy475.8 = ifelse( qy475.8 == "Yes" , "foster father" , qy475.8 )) 

q75_1$qy475a <- apply(q75_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q75_1;

q75_1 %>% 
  select(fullid_04, qy475a) %>%
  mutate(qy475a = 
           str_replace(qy475a, pattern = '^.*?,', replacement = ""),
         qy475a = str_trim(qy475a, side='left')) %>%
  filter(fullid_04 != qy475a) -> q75_1

# question 75_9 ####
q75_9 <- byds %>%
  select(fullid_04, qy475.16, qy475.17, qy475.18, qy475.19, qy475.20) %>%
  mutate( qy475.16 = ifelse( qy475.16 == "Yes" , "aunt" , qy475.16 )) %>%
  mutate( qy475.17 = ifelse( qy475.17 == "Yes" , "uncle" , qy475.17 )) %>%
  mutate( qy475.18 = ifelse( qy475.18 == "Yes" , "cousin" , qy475.18 )) %>%
  mutate( qy475.19 = ifelse( qy475.19 == "Yes" , "lodger" , qy475.19 )) %>%
  mutate( qy475.20 = ifelse( qy475.20 == "Yes" , "other adult" , qy475.20 )) 

q75_9$qy475i <- apply(q75_9, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q75_9;

q75_9 %>% 
  select(fullid_04, qy475i) %>%
  mutate(qy475i = 
           str_replace(qy475i, pattern = '^.*?,', replacement = ""),
         qy475i = str_trim(qy475i, side='left')) %>%
  filter(fullid_04 != qy475i) -> q75_9

# question 76 ####
q76 <- byds %>%
  select(fullid_04, qy476.1, qy476.2, qy476.3) %>%
  mutate( qy476.1 = ifelse( qy476.1 == "Yes" , "mother" , qy476.1 )) %>%
  mutate( qy476.2 = ifelse( qy476.2 == "Yes" , "father" , qy476.2 )) %>%
  mutate( qy476.3 = ifelse( qy476.3 == "Neither mum or dad is dead" , "neither parent" , qy476.3 )) %>%
  add_column(qy476.4 = NA) %>%
  mutate(qy476.4 = ifelse((qy476.1 == "mother") & (qy476.2 == "father"), "both parents",  qy476.4))

q76$qy476 <- apply(q76, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q76;

q76 %>% 
  select(fullid_04, qy476) %>%
  mutate(qy476 = 
           str_replace(qy476, pattern = '^.*?,', replacement = ""),
         qy476 = str_trim(qy476, side='left')) %>%
  filter(fullid_04 != qy476) -> q76

q76 <- q76 %>%
  mutate(qy476 = ifelse(qy476 == "mother, 3, 3", "mother", qy476)) %>%
  mutate(qy476 = ifelse(qy476 == "mother, father, 3, both parents", "both parents", qy476)) %>%
  mutate(qy476 = ifelse(qy476 == "mother, father, both parents", "both parents", qy476)) %>%
  mutate(qy476 = ifelse(qy476 == "mother, father, neither parent, both parents", "both parents", qy476))

# join it back together ################### 

# pull off the already cleaned columns
wave_4 <- byds %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, x2_04, x3_04, qy41, dob2004, 
         qy44, qy44a, qy45, qy46, qy47, qy48.1, qy48.2, qy48.3, qy48.4, qy48.5, qy48.6, qy48.7, qy48.8, qy48.9, qy48.10, 
         qy48.11, qy48.12, qy48.13, qy48.14, qy48.15, qy48.16, qy48.17, qy48.18, qy48.19, qy48.20, qy48.21, qy49, 
         qy410.1, qy410.2, qy410.3, qy410.4, qy410.5, qy410.6, qy410.7, qy410.8, qy410.9, qy411, qy411a, qy411b, qy412, 
         qy412a.1, qy412a.2, qy412a.3, qy412b, qy413, qy414.1, qy414.2, qy414.3, qy414.4, qy414.5, qy414.6, qy414.7, qy414.8, 
         qy414.9, qy414.10, qy414.11, qy414.12, qy414.13, qy414.14, qy414.15, qy414.16, qy414.17, qy414.18, qy414.19, qy414.20, qy414.21, 
         qy414.22, qy414.23, qy414.24, qy414.25, qy415, qy415a, qy415b.1, qy415b.2, qy415b.3, qy415b.4, qy415c, qy416, qy416a, 
         qy417, qy417a, qy418, qy418a, qy419, qy419a, qy420, qy420a, qy421, qy421a, qy422, qy422a, qy423, qy423a, qy424, 
         qy424a, qy425, qy425a, qy426, qy426a, qy427, qy427a, qy428, qy428a, qy429, qy429a, qy430, qy431, qy431b, qy431c, 
         qy431d, qy432, qy433, qy433a,qy433d, qy433e, qy434, qy435, qy435c, qy435d.1, qy435d.2, qy435d.3, qy435d.4, qy435d.5, 
         qy436, qy437, qy438, qy438c, qy439, qy440, qy441, qy441c, qy441d, qy442, qy443, qy444, qy444c, qy444d, qy445, qy446, 
         qy447, qy447c, qy447d, qy448, qy449, qy450, qy450c, qy450d, qy451, qy452, qy453, qy453c, qy453d, qy454, qy455, qy456,
         qy456c, qy456d, qy457, qy458, qy459, qy459c, qy459d, qy460, qy461, qy462, qy462c, qy462d, qy463, qy464, qy465, qy465c, 
         qy465d, qy466.1, qy466.2, qy466.3, qy466.4, qy466.5, qy466.6, qy466.7, qy466.8, qy466.9, qy466.10, qy466.11, qy466.12, 
         qy467.1, qy467.2, qy467.3, qy467.4, qy468.1, qy468.2, qy468.3, qy468.4, qy468.5, qy468.6, qy468.7, qy468.8, qy469, 
         qy470, qy471, qy472, qy473, qy473a, qy474, qy475b, qy475c, qy475d, qy475e, qy475f, qy475g, qy475h, 
         qy477, qy478, qy479, qy479a, qy480, qy481, qy482, qy483, qy484, qy485.1, qy485.2, qy485.3, qy485.4, qy485.5, 
         qy485.6, qy485.7, qy485.8, qy485.9, qy486.1, qy486.2, qy486.3, qy486.4, qy486.5, qy487.1, qy487.2, qy487.3, qy487.4, 
         qy487.5, qy488.1, qy488.2, qy488.3, qy488.4, qy488.5, qy489.1, qy489.2, qy489.3, qy489.4, qy489.5, qy489.6, qy489.7, 
         qy489.8, qy489.9, qy489.10, qy489.11, qy489.12, qy489.13, qy490.1, qy490.2, qy490.3, qy490.4, qy490.5, qy490.6, qy490.7, 
         qy490.8, qy490.9, qy490.10, qy490.11, qy490.12, qy490.13, qy490.14, qy490.15, qy491, qy491a, qy492, qy492a, qy493, qy493a, 
         qy494, qy494a, qy495, qy496, qy497.1, qy497.2, qy497.3, qy497.4, qy497.5, qy497.6, qy497.7, qy497.8, qy497.9, qy497.10, 
         qy497.11, qy497.12, qy497.13, qy498, qy498a, qy499, qy499a, qy4100, qy4100a, qy4101, qy4101a, qy4102.1, qy4102.2, qy4102.3, 
         qy4102.4, qy4102.5, qy4102.6, qy4102.7, qy4102.8, qy4102.9, qy4103.1, qy4103.2, qy4103.3, qy4103.4, qy4103.5, qy4103.6, qy4103.7, 
         qy4103.8, qy4104.1, qy4104.2)


# tidy left joins
wave_4 <- wave_4 %>%
  left_join(q2, by = "fullid_04") %>%
  left_join(q31_1, by = "fullid_04") %>%
  left_join(q33_2, by = "fullid_04") %>%
  left_join(q33_3, by = "fullid_04") %>%
  left_join(q35_1, by = "fullid_04") %>%
  left_join(q35_2, by = "fullid_04") %>%
  left_join(q35_4, by = "fullid_04") %>%
  left_join(q38_1, by = "fullid_04") %>%
  left_join(q38_2, by = "fullid_04") %>%
  left_join(q41_1, by = "fullid_04") %>%
  left_join(q41_2, by = "fullid_04") %>%
  left_join(q44_1, by = "fullid_04") %>%
  left_join(q44_2, by = "fullid_04") %>%
  left_join(q47_1, by = "fullid_04") %>%
  left_join(q47_2, by = "fullid_04") %>%
  left_join(q50_1, by = "fullid_04") %>%
  left_join(q50_2, by = "fullid_04") %>%
  left_join(q53_1, by = "fullid_04") %>%
  left_join(q53_2, by = "fullid_04") %>%
  left_join(q56_1, by = "fullid_04") %>%
  left_join(q56_2, by = "fullid_04") %>%
  left_join(q59_1, by = "fullid_04") %>%
  left_join(q59_2, by = "fullid_04") %>%
  left_join(q62_1, by = "fullid_04") %>%
  left_join(q62_2, by = "fullid_04") %>%
  left_join(q65_1, by = "fullid_04") %>%
  left_join(q65_2, by = "fullid_04") %>%
  left_join(q75_1, by = "fullid_04") %>%
  left_join(q75_9, by = "fullid_04") %>%
  left_join(q76, by = "fullid_04")



# remove extra dataframes
rm(q2, q31_1, q33_2, q33_3, q35_1, q35_2, q35_4, q38_1, q38_2, q41_1, q41_2, q44_1, q44_2, q47_1, q47_2, q50_1, q50_2, q53_1, q53_2, 
   q56_1, q56_2, q59_1, q59_2, q62_1, q62_2, q65_1, q65_2, q75_1, q75_9, q76)


# order the columns sensibly
wave_4 <- wave_4 %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, x2_04, x3_04, qy41, qy42, 
         dob2004, qy44, qy44a, qy45, qy46, qy47, qy48.1, qy48.2, 
         qy48.3, qy48.4, qy48.5, qy48.6, qy48.7, qy48.8, qy48.9, qy48.10, qy48.11, qy48.12, qy48.13, qy48.14, qy48.15, 
         qy48.16, qy48.17, qy48.18, qy48.19, qy48.20, qy48.21, qy49, qy410.1, qy410.2, qy410.3, qy410.4, qy410.5, 
         qy410.6, qy410.7, qy410.8, qy410.9, qy411, qy411a, qy411b, qy412, qy412a.1, qy412a.2, qy412a.3, qy412b, qy413, 
         qy414.1, qy414.2, qy414.3, qy414.4, qy414.5, qy414.6, qy414.7, qy414.8, qy414.9, qy414.10, qy414.11, qy414.12, 
         qy414.13, qy414.14, qy414.15, qy414.16, qy414.17, qy414.18, qy414.19, qy414.20, qy414.21, qy414.22, qy414.23, qy414.24, 
         qy414.25, qy415, qy415a, qy415b.1, qy415b.2, qy415b.3, qy415b.4, qy415c, qy416, qy416a, qy417, qy417a, qy418, qy418a, 
         qy419, qy419a, qy420, qy420a, qy421, qy421a, qy422, qy422a, qy423, qy423a, qy424, qy424a, qy425, qy425a, qy426, 
         qy426a, qy427, qy427a, qy428, qy428a, qy429, qy429a, qy430, qy431, qy431a, qy431b, qy431c, qy431d, qy432, qy433, 
         qy433a, qy433b, qy433c, qy433d, qy433e, qy434, qy435, qy435a, qy435b,qy435c, qy435d, qy435d.1, qy435d.2, qy435d.3, qy435d.4, 
         qy435d.5, qy436, qy437, qy438, qy438a, qy438b, qy438c, qy439, qy440, qy441, qy441a, qy441b, qy441c, qy441d, 
         qy442, qy443, qy444, qy444a, qy444b, qy444c, qy444d, qy445, qy446, qy447, qy447a, qy447b, qy447c, qy447d, qy448, 
         qy449, qy450, qy450a, qy450b, qy450c, qy450d, qy451, qy452, qy453, qy453a, qy453b, qy453c, qy453d, qy454, qy455, qy456,
         qy456a, qy456b, qy456c, qy456d, qy457, qy458, qy459, qy459a, qy459b, qy459c, qy459d, qy460, qy461, qy462, qy462a, qy462b,
         qy462c, qy462d, qy463, qy464, qy465, qy465a, qy465b, qy465c, qy465d, qy466.1, qy466.2, qy466.3, qy466.4, qy466.5, qy466.6, 
         qy466.7, qy466.8, qy466.9, qy466.10, qy466.11, qy466.12, qy467.1, qy467.2, qy467.3, qy467.4, qy468.1, qy468.2, qy468.3, 
         qy468.4, qy468.5, qy468.6, qy468.7, qy468.8, qy469, qy470, qy471, qy472, qy473, qy473a, qy474, qy475a, qy475b, qy475c, qy475d, 
         qy475e, qy475f, qy475g, qy475h, qy475i, qy476, qy477, qy478, qy479, qy479a, qy480, qy481, qy482, 
         qy483, qy484, qy485.1, qy485.2, qy485.3, qy485.4, qy485.5, qy485.6, qy485.7, qy485.8, qy485.9, qy486.1, qy486.2, 
         qy486.3, qy486.4, qy486.5, qy487.1, qy487.2, qy487.3, qy487.4, qy487.5, qy488.1, qy488.2, qy488.3, qy488.4, qy488.5, 
         qy489.1, qy489.2, qy489.3, qy489.4, qy489.5, qy489.6, qy489.7, qy489.8, qy489.9, qy489.10, qy489.11, qy489.12, qy489.13, 
         qy490.1, qy490.2, qy490.3, qy490.4, qy490.5, qy490.6, qy490.7, qy490.8, qy490.9, qy490.10, qy490.11, qy490.12, qy490.13, 
         qy490.14, qy490.15, qy491, qy491a, qy492, qy492a, qy493, qy493a, qy494, qy494a, qy495, qy496, qy497.1, qy497.2, qy497.3, 
         qy497.4, qy497.5, qy497.6, qy497.7, qy497.8, qy497.9, qy497.10, qy497.11, qy497.12, qy497.13, qy498, qy498a, qy499, qy499a, 
         qy4100, qy4100a, qy4101, qy4101a, qy4102.1, qy4102.2, qy4102.3, qy4102.4, qy4102.5, qy4102.6, qy4102.7, qy4102.8, qy4102.9, 
         qy4103.1, qy4103.2, qy4103.3, qy4103.4, qy4103.5, qy4103.6, qy4103.7, qy4103.8, qy4104.1, qy4104.2)



##########################################
#     COLLAPSE RESPONSES - WAVE 5
##########################################
# cleaning ###############

# question 2
q2 <- byds %>%
  select(fullid_05, Q2005.2.1, Q2005.2.2, Q2005.2.3, Q2005.2.4, Q2005.2.5, Q2005.2.6, Q2005.2.7, Q2005.2.8) %>%
  mutate( Q2005.2.1 = ifelse( Q2005.2.1 == "Yes" , "Monday" , Q2005.2.1 )) %>%
  mutate( Q2005.2.2 = ifelse( Q2005.2.2 == "Yes" , "Tuesday" , Q2005.2.2 )) %>%
  mutate( Q2005.2.3 = ifelse( Q2005.2.3 == "Yes" , "Wednesday" , Q2005.2.3 )) %>%
  mutate( Q2005.2.4 = ifelse( Q2005.2.4 == "Yes" , "Thursday" , Q2005.2.4 )) %>%
  mutate( Q2005.2.5 = ifelse( Q2005.2.5 == "Yes" , "Friday" , Q2005.2.5 )) %>%
  mutate( Q2005.2.6 = ifelse( Q2005.2.6 == "Yes" , "Saturday" , Q2005.2.6 )) %>%
  mutate( Q2005.2.7 = ifelse( Q2005.2.7 == "Yes" , "Sunday" , Q2005.2.7 )) %>%
  mutate( Q2005.2.8 = ifelse( Q2005.2.8 == "Yes" , "Don't go out in evenings" , Q2005.2.8 )) 

q2$Q2005.2 <- apply(q2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q2;

q2 %>% 
  select(fullid_05, Q2005.2) %>%
  mutate(Q2005.2 = 
           str_replace(Q2005.2, pattern = '^.*?,', replacement = ""),
         Q2005.2 = str_trim(Q2005.2, side='left')) %>%
  filter(fullid_05 != Q2005.2) -> q2

# question 33_1
q33_1 <- byds %>%
  select(fullid_05, Q2005.33a.1, Q2005.33a.2, Q2005.33a.3, Q2005.33a.4, Q2005.33a.5, Q2005.33a.6, Q2005.33a.7, Q2005.33a.8) %>%
  mutate( Q2005.33a.1 = ifelse( Q2005.33a.1 == "Yes" , "older friends" , Q2005.33a.1 )) %>%
  mutate( Q2005.33a.2 = ifelse( Q2005.33a.2 == "Yes" , "younger friends" , Q2005.33a.2 )) %>%
  mutate( Q2005.33a.3 = ifelse( Q2005.33a.3 == "Yes" , "friends own age" , Q2005.33a.3 )) %>%
  mutate( Q2005.33a.4 = ifelse( Q2005.33a.4 == "Yes" , "brother or sister" , Q2005.33a.4 )) %>%
  mutate( Q2005.33a.5 = ifelse( Q2005.33a.5 == "Yes" , "parents" , Q2005.33a.5 )) %>%
  mutate( Q2005.33a.6 = ifelse( Q2005.33a.6 == "Yes" , "buy from a shop" , Q2005.33a.6 )) %>%
  mutate( Q2005.33a.7 = ifelse( Q2005.33a.7 == "Yes" , "got from house" , Q2005.33a.7 )) %>%
  mutate( Q2005.33a.8 = ifelse( Q2005.33a.8 == "Yes" , "somewhere else" , Q2005.33a.8 )) 

q33_1$Q2005.33a <- apply(q33_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q33_1;

q33_1 %>% 
  select(fullid_05, Q2005.33a) %>%
  mutate(Q2005.33a = 
           str_replace(Q2005.33a, pattern = '^.*?,', replacement = ""),
         Q2005.33a = str_trim(Q2005.33a, side='left')) %>%
  filter(fullid_05 != Q2005.33a) -> q33_1

# question 35_2
q35_2 <- byds %>%
  select(fullid_05, Q2005.35b.1, Q2005.35b.2, Q2005.35b.3, Q2005.35b.4, Q2005.35b.5, Q2005.35b.6, Q2005.35b.7, 
         Q2005.35b.8, Q2005.35b.9, Q2005.35b.10, Q2005.35b.11) %>%
  mutate( Q2005.35b.1 = ifelse( Q2005.35b.1 == "Yes" , "at home" , Q2005.35b.1 )) %>%
  mutate( Q2005.35b.2 = ifelse( Q2005.35b.2 == "Yes" , "at a friend's house" , Q2005.35b.2 )) %>%
  mutate( Q2005.35b.3 = ifelse( Q2005.35b.3 == "Yes" , "at a party" , Q2005.35b.3 )) %>%
  mutate( Q2005.35b.4 = ifelse( Q2005.35b.4 == "Yes" , "at a disco" , Q2005.35b.4 )) %>%
  mutate( Q2005.35b.5 = ifelse( Q2005.35b.5 == "Yes" , "at a youth club or community centre" , Q2005.35b.5 )) %>%
  mutate( Q2005.35b.6 = ifelse( Q2005.35b.6 == "Yes" , "at school" , Q2005.35b.6 )) %>%
  mutate( Q2005.35b.7 = ifelse( Q2005.35b.7 == "Yes" , "in a car" , Q2005.35b.7 )) %>%
  mutate( Q2005.35b.8 = ifelse( Q2005.35b.8 == "Yes" , "in a pub" , Q2005.35b.8 )) %>%
  mutate( Q2005.35b.9 = ifelse( Q2005.35b.9 == "Yes" , "at a concert of festival" , Q2005.35b.9 )) %>%
  mutate( Q2005.35b.10 = ifelse( Q2005.35b.10 == "Yes" , "other outdoor space" , Q2005.35b.10 )) %>%
  mutate( Q2005.35b.11 = ifelse( Q2005.35b.11 == "Yes" , "somewhere else" , Q2005.35b.11 )) 

q35_2$Q2005.35b <- apply(q35_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q35_2;

q35_2 %>% 
  select(fullid_05, Q2005.35b) %>%
  mutate(Q2005.35b = 
           str_replace(Q2005.35b, pattern = '^.*?,', replacement = ""),
         Q2005.35b = str_trim(Q2005.35b, side='left')) %>%
  filter(fullid_05 != Q2005.35b) -> q35_2

# question 35_3
q35_3 <- byds %>%
  select(fullid_05, Q2005.35c.1, Q2005.36c.2, Q2005.35c.3, Q2005.35c.4, Q2005.35c.5, Q2005.35c.6, Q2005.35c.7, Q2005.35c.8, 
         Q2005.35c.9, Q2005.35c.10) %>%
  mutate( Q2005.35c.1 = ifelse( Q2005.35c.1 == "Yes" , "older friends" , Q2005.35c.1 )) %>%
  mutate( Q2005.36c.2 = ifelse( Q2005.36c.2 == "Yes" , "younger friends" , Q2005.36c.2 )) %>%
  mutate( Q2005.35c.3 = ifelse( Q2005.35c.3 == "Yes" , "friends own age" , Q2005.35c.3 )) %>%
  mutate( Q2005.35c.4 = ifelse( Q2005.35c.4 == "Yes" , "brother or sister" , Q2005.35c.4 )) %>%
  mutate( Q2005.35c.5 = ifelse( Q2005.35c.5 == "Yes" , "parents" , Q2005.35c.5 )) %>%
  mutate( Q2005.35c.6 = ifelse( Q2005.35c.6 == "Yes" , "other relatives" , Q2005.35c.6 )) %>%
  mutate( Q2005.35c.7 = ifelse( Q2005.35c.7 == "Yes" , "buy from an off license" , Q2005.35c.7 )) %>%
  mutate( Q2005.35c.8 = ifelse( Q2005.35c.8 == "Yes" , "someone else buys it" , Q2005.35c.8 )) %>%
  mutate( Q2005.35c.9 = ifelse( Q2005.35c.9 == "Yes" , "got from house" , Q2005.35c.9 )) %>%
  mutate( Q2005.35c.10 = ifelse( Q2005.35c.10 == "Yes" , "somewhere else" , Q2005.35c.10 ))

q35_3$Q2005.35c <- apply(q35_3, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q35_3;

q35_3 %>% 
  select(fullid_05, Q2005.35c) %>%
  mutate(Q2005.35c = 
           str_replace(Q2005.35c, pattern = '^.*?,', replacement = ""),
         Q2005.35c = str_trim(Q2005.35c, side='left')) %>%
  filter(fullid_05 != Q2005.35c) -> q35_3

# question 40_1
q40_1 <- byds %>%
  select(fullid_05, Q2005.40a.1, Q2005.40a.2, Q2005.40a.3, Q2005.40a.4, Q2005.40a.5, Q2005.40a.6, Q2005.40a.7, 
         Q2005.40a.8, Q2005.40a.9, Q2005.40a.10, Q2005.40a.11) %>%
  mutate( Q2005.40a.1 = ifelse( Q2005.40a.1 == "Yes" , "at home" , Q2005.40a.1 )) %>%
  mutate( Q2005.40a.2 = ifelse( Q2005.40a.2 == "Yes" , "at a friend's house" , Q2005.40a.2 )) %>%
  mutate( Q2005.40a.3 = ifelse( Q2005.40a.3 == "Yes" , "at a party" , Q2005.40a.3 )) %>%
  mutate( Q2005.40a.4 = ifelse( Q2005.40a.4 == "Yes" , "at a disco" , Q2005.40a.4 )) %>%
  mutate( Q2005.40a.5 = ifelse( Q2005.40a.5 == "Yes" , "at a youth club or community centre" , Q2005.40a.5 )) %>%
  mutate( Q2005.40a.6 = ifelse( Q2005.40a.6 == "Yes" , "at school" , Q2005.40a.6 )) %>%
  mutate( Q2005.40a.7 = ifelse( Q2005.40a.7 == "Yes" , "in a car" , Q2005.40a.7 )) %>%
  mutate( Q2005.40a.8 = ifelse( Q2005.40a.8 == "Yes" , "in a pub" , Q2005.40a.8 )) %>%
  mutate( Q2005.40a.9 = ifelse( Q2005.40a.9 == "Yes" , "at a concert of festival" , Q2005.40a.9 )) %>%
  mutate( Q2005.40a.10 = ifelse( Q2005.40a.10 == "Yes" , "other outdoor space" , Q2005.40a.10 )) %>%
  mutate( Q2005.40a.11 = ifelse( Q2005.40a.11 == "Yes" , "somewhere else" , Q2005.40a.11 )) 

q40_1$Q2005.40a <- apply(q40_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q40_1;

q40_1 %>% 
  select(fullid_05, Q2005.40a) %>%
  mutate(Q2005.40a = 
           str_replace(Q2005.40a, pattern = '^.*?,', replacement = ""),
         Q2005.40a = str_trim(Q2005.40a, side='left')) %>%
  filter(fullid_05 != Q2005.40a) -> q40_1

# question 40_2
q40_2 <- byds %>%
  select(fullid_05, Q2005.40b.1, Q2005.40b.2, Q2005.40b.3, Q2005.40b.4, Q2005.40b.5, Q2005.40b.6, Q2005.40b.7, 
         Q2005.40b.8, Q2005.40b.9, Q2005.40b.10, Q2005.40b.11) %>%
  mutate( Q2005.40b.1 = ifelse( Q2005.40b.1 == "Yes" , "older friends" , Q2005.40b.1 )) %>%
  mutate( Q2005.40b.2 = ifelse( Q2005.40b.2 == "Yes" , "younger friends" , Q2005.40b.2 )) %>%
  mutate( Q2005.40b.3 = ifelse( Q2005.40b.3 == "Yes" , "friends own age" , Q2005.40b.3 )) %>%
  mutate( Q2005.40b.4 = ifelse( Q2005.40b.4 == "Yes" , "brother or sister" , Q2005.40b.4 )) %>%
  mutate( Q2005.40b.5 = ifelse( Q2005.40b.5 == "Yes" , "parents" , Q2005.40b.5 )) %>%
  mutate( Q2005.40b.6 = ifelse( Q2005.40b.6 == "Yes" , "other relatives" , Q2005.40b.6 )) %>%
  mutate( Q2005.40b.7 = ifelse( Q2005.40b.7 == "Yes" , "dealer" , Q2005.40b.7 )) %>%
  mutate( Q2005.40b.8 = ifelse( Q2005.40b.8 == "Yes" , "buy from shop" , Q2005.40b.8 )) %>%
  mutate( Q2005.40b.9 = ifelse( Q2005.40b.9 == "Yes" , "steal from shop" , Q2005.40b.9 )) %>%
  mutate( Q2005.40b.10 = ifelse( Q2005.40b.10 == "Yes" , "got from house" , Q2005.40b.10 )) %>%
  mutate( Q2005.40b.11 = ifelse( Q2005.40b.11 == "Yes" , "somewhere else" , Q2005.40b.11 )) 

q40_2$Q2005.40b <- apply(q40_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q40_2;

q40_2 %>% 
  select(fullid_05, Q2005.40b) %>%
  mutate(Q2005.40b = 
           str_replace(Q2005.40b, pattern = '^.*?,', replacement = ""),
         Q2005.40b = str_trim(Q2005.40b, side='left')) %>%
  filter(fullid_05 != Q2005.40b) -> q40_2

# question 43_1
q43_1 <- byds %>%
  select(fullid_05, Q2005.43a.1, Q2005.43a.2, Q2005.43a.3, Q2005.43a.4, Q2005.43a.5, Q2005.43a.6, Q2005.43a.7, Q2005.43a.8, 
         Q2005.43a.9, Q2005.43a.10, Q2005.43a.11) %>%
  mutate( Q2005.43a.1 = ifelse( Q2005.43a.1 == "Yes" , "at home" , Q2005.43a.1 )) %>%
  mutate( Q2005.43a.2 = ifelse( Q2005.43a.2 == "Yes" , "at a friend's house" , Q2005.43a.2 )) %>%
  mutate( Q2005.43a.3 = ifelse( Q2005.43a.3 == "Yes" , "at a party" , Q2005.43a.3 )) %>%
  mutate( Q2005.43a.4 = ifelse( Q2005.43a.4 == "Yes" , "at a disco" , Q2005.43a.4 )) %>%
  mutate( Q2005.43a.5 = ifelse( Q2005.43a.5 == "Yes" , "at a youth club or community centre" , Q2005.43a.5 )) %>%
  mutate( Q2005.43a.6 = ifelse( Q2005.43a.6 == "Yes" , "at school" , Q2005.43a.6 )) %>%
  mutate( Q2005.43a.7 = ifelse( Q2005.43a.7 == "Yes" , "in a car" , Q2005.43a.7 )) %>%
  mutate( Q2005.43a.8 = ifelse( Q2005.43a.8 == "Yes" , "in a pub" , Q2005.43a.8 )) %>%
  mutate( Q2005.43a.9 = ifelse( Q2005.43a.9 == "Yes" , "at a concert of festival" , Q2005.43a.9 )) %>%
  mutate( Q2005.43a.10 = ifelse( Q2005.43a.10 == "Yes" , "other outdoor space" , Q2005.43a.10 )) %>%
  mutate( Q2005.43a.11 = ifelse( Q2005.43a.11 == "Yes" , "somewhere else" , Q2005.43a.11 )) 

q43_1$Q2005.43a <- apply(q43_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q43_1;

q43_1 %>% 
  select(fullid_05, Q2005.43a) %>%
  mutate(Q2005.43a = 
           str_replace(Q2005.43a, pattern = '^.*?,', replacement = ""),
         Q2005.43a = str_trim(Q2005.43a, side='left')) %>%
  filter(fullid_05 != Q2005.43a) -> q43_1

# question 43_2
q43_2 <- byds %>%
  select(fullid_05, Q2005.43b.1, Q2005.43b.2, Q2005.43b.3, Q2005.43b.4, Q2005.43b.5, Q2005.43b.6, Q2005.43b.7, 
         Q2005.43b.8, Q2005.43b.9) %>%
  mutate( Q2005.43b.1 = ifelse( Q2005.43b.1 == "Yes" , "older friends" , Q2005.43b.1 )) %>%
  mutate( Q2005.43b.2 = ifelse( Q2005.43b.2 == "Yes" , "younger friends" , Q2005.43b.2 )) %>%
  mutate( Q2005.43b.3 = ifelse( Q2005.43b.3 == "Yes" , "friends own age" , Q2005.43b.3 )) %>%
  mutate( Q2005.43b.4 = ifelse( Q2005.43b.4 == "Yes" , "brother or sister" , Q2005.43b.4 )) %>%
  mutate( Q2005.43b.5 = ifelse( Q2005.43b.5 == "Yes" , "parents" , Q2005.43b.5 )) %>%
  mutate( Q2005.43b.6 = ifelse( Q2005.43b.6 == "Yes" , "other relatives" , Q2005.43b.6 )) %>%
  mutate( Q2005.43b.7 = ifelse( Q2005.43b.7 == "Yes" , "a dealer" , Q2005.43b.7 )) %>%
  mutate( Q2005.43b.8 = ifelse( Q2005.43b.8 == "Yes" , "got from house" , Q2005.43b.8 )) %>%
  mutate( Q2005.43b.9 = ifelse( Q2005.43b.9 == "Yes" , "somewhere else" , Q2005.43b.9 )) 



q43_2$Q2005.43b <- apply(q43_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q43_2;

q43_2 %>% 
  select(fullid_05, Q2005.43b) %>%
  mutate(Q2005.43b = 
           str_replace(Q2005.43b, pattern = '^.*?,', replacement = ""),
         Q2005.43b = str_trim(Q2005.43b, side='left')) %>%
  filter(fullid_05 != Q2005.43b) -> q43_2

# question 46_1
q46_1 <- byds %>%
  select(fullid_05, Q2005.46a.1, Q2005.46a.2, Q2005.46a.3, Q2005.46a.4, Q2005.46a.5, Q2005.46a.6, Q2005.46a.7, 
         Q2005.46a.8, Q2005.46a.9, Q2005.46a.10, Q2005.46a.11) %>%
  mutate( Q2005.46a.1 = ifelse( Q2005.46a.1 == "Yes" , "at home" , Q2005.46a.1 )) %>%
  mutate( Q2005.46a.2 = ifelse( Q2005.46a.2 == "Yes" , "at a friend's house" , Q2005.46a.2 )) %>%
  mutate( Q2005.46a.3 = ifelse( Q2005.46a.3 == "Yes" , "at a party" , Q2005.46a.3 )) %>%
  mutate( Q2005.46a.4 = ifelse( Q2005.46a.4 == "Yes" , "at a disco" , Q2005.46a.4 )) %>%
  mutate( Q2005.46a.5 = ifelse( Q2005.46a.5 == "Yes" , "at a youth club or community centre" , Q2005.46a.5 )) %>%
  mutate( Q2005.46a.6 = ifelse( Q2005.46a.6 == "Yes" , "at school" , Q2005.46a.6 )) %>%
  mutate( Q2005.46a.7 = ifelse( Q2005.46a.7 == "Yes" , "in a car" , Q2005.46a.7 )) %>%
  mutate( Q2005.46a.8 = ifelse( Q2005.46a.8 == "Yes" , "in a pub" , Q2005.46a.8 )) %>%
  mutate( Q2005.46a.9 = ifelse( Q2005.46a.9 == "Yes" , "at a concert of festival" , Q2005.46a.9 )) %>%
  mutate( Q2005.46a.10 = ifelse( Q2005.46a.10 == "Yes" , "other outdoor space" , Q2005.46a.10 )) %>%
  mutate( Q2005.46a.11 = ifelse( Q2005.46a.11 == "Yes" , "somewhere else" , Q2005.46a.11 )) 

q46_1$Q2005.46a <- apply(q46_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q46_1;

q46_1 %>% 
  select(fullid_05, Q2005.46a) %>%
  mutate(Q2005.46a = 
           str_replace(Q2005.46a, pattern = '^.*?,', replacement = ""),
         Q2005.46a = str_trim(Q2005.46a, side='left')) %>%
  filter(fullid_05 != Q2005.46a) -> q46_1

# question 46_2
q46_2 <- byds %>%
  select(fullid_05, Q2005.46b.1, Q2005.46b.2, Q2005.46b.3, Q2005.46b.4, Q2005.46b.5, Q2005.46b.6, Q2005.46b.7, 
         Q2005.46b.8, Q2005.46b.9, Q2005.46b.10) %>%
  mutate( Q2005.46b.1 = ifelse( Q2005.46b.1 == "Yes" , "older friends" , Q2005.46b.1 )) %>%
  mutate( Q2005.46b.2 = ifelse( Q2005.46b.2 == "Yes" , "younger friends" , Q2005.46b.2 )) %>%
  mutate( Q2005.46b.3 = ifelse( Q2005.46b.3 == "Yes" , "friends own age" , Q2005.46b.3 )) %>%
  mutate( Q2005.46b.4 = ifelse( Q2005.46b.4 == "Yes" , "brother or sister" , Q2005.46b.4 )) %>%
  mutate( Q2005.46b.5 = ifelse( Q2005.46b.5 == "Yes" , "parents" , Q2005.46b.5 )) %>%
  mutate( Q2005.46b.6 = ifelse( Q2005.46b.6 == "Yes" , "other relatives" , Q2005.46b.6 )) %>%
  mutate( Q2005.46b.7 = ifelse( Q2005.46b.7 == "Yes" , "dealer" , Q2005.46b.7 )) %>%
  mutate( Q2005.46b.8 = ifelse( Q2005.46b.8 == "Yes" , "picked them" , Q2005.46b.8 )) %>%
  mutate( Q2005.46b.9 = ifelse( Q2005.46b.9 == "Yes" , "got from house" , Q2005.46b.9 )) %>%
  mutate( Q2005.46b.10 = ifelse( Q2005.46b.10 == "Yes" , "somewhere else" , Q2005.46b.10 )) 

q46_2$Q2005.46b <- apply(q46_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q46_2;

q46_2 %>% 
  select(fullid_05, Q2005.46b) %>%
  mutate(Q2005.46b = 
           str_replace(Q2005.46b, pattern = '^.*?,', replacement = ""),
         Q2005.46b = str_trim(Q2005.46b, side='left')) %>%
  filter(fullid_05 != Q2005.46b) -> q46_2

# question 49_1
q49_1 <- byds %>%
  select(fullid_05, Q2005.49a.1, Q2005.49a.2, Q2005.49a.3, Q2005.49a.4, Q2005.49a.5, Q2005.49a.6, Q2005.49a.7, 
         Q2005.49a.8, Q2005.49a.9, Q2005.49a.10, Q2005.49a.11) %>%
  mutate( Q2005.49a.1 = ifelse( Q2005.49a.1 == "Yes" , "at home" , Q2005.49a.1 )) %>%
  mutate( Q2005.49a.2 = ifelse( Q2005.49a.2 == "Yes" , "at a friend's house" , Q2005.49a.2 )) %>%
  mutate( Q2005.49a.3 = ifelse( Q2005.49a.3 == "Yes" , "at a party" , Q2005.49a.3 )) %>%
  mutate( Q2005.49a.4 = ifelse( Q2005.49a.4 == "Yes" , "at a disco" , Q2005.49a.4 )) %>%
  mutate( Q2005.49a.5 = ifelse( Q2005.49a.5 == "Yes" , "at a youth club or community centre" , Q2005.49a.5 )) %>%
  mutate( Q2005.49a.6 = ifelse( Q2005.49a.6 == "Yes" , "at school" , Q2005.49a.6 )) %>%
  mutate( Q2005.49a.7 = ifelse( Q2005.49a.7 == "Yes" , "in a car" , Q2005.49a.7 )) %>%
  mutate( Q2005.49a.8 = ifelse( Q2005.49a.8 == "Yes" , "in a pub" , Q2005.49a.8 )) %>%
  mutate( Q2005.49a.9 = ifelse( Q2005.49a.9 == "Yes" , "at a concert of festival" , Q2005.49a.9 )) %>%
  mutate( Q2005.49a.10 = ifelse( Q2005.49a.10 == "Yes" , "other outdoor space" , Q2005.49a.10 )) %>%
  mutate( Q2005.49a.11 = ifelse( Q2005.49a.11 == "Yes" , "somewhere else" , Q2005.49a.11 )) 

q49_1$Q2005.49a <- apply(q49_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q49_1;

q49_1 %>% 
  select(fullid_05, Q2005.49a) %>%
  mutate(Q2005.49a = 
           str_replace(Q2005.49a, pattern = '^.*?,', replacement = ""),
         Q2005.49a = str_trim(Q2005.49a, side='left')) %>%
  filter(fullid_05 != Q2005.49a) -> q49_1

# question 49_2
q49_2 <- byds %>%
  select(fullid_05, Q2005.49b.1, Q2005.49b.2, Q2005.49b.3, Q2005.49b.4, Q2005.49b.5, Q2005.49b.6, 
         Q2005.49b.7, Q2005.49b.8, Q2005.49b.9) %>%
  mutate( Q2005.49b.1 = ifelse( Q2005.49b.1 == "Yes" , "older friends" , Q2005.49b.1 )) %>%
  mutate( Q2005.49b.2 = ifelse( Q2005.49b.2 == "Yes" , "younger friends" , Q2005.49b.2 )) %>%
  mutate( Q2005.49b.3 = ifelse( Q2005.49b.3 == "Yes" , "friends own age" , Q2005.49b.3 )) %>%
  mutate( Q2005.49b.4 = ifelse( Q2005.49b.4 == "Yes" , "brother or sister" , Q2005.49b.4 )) %>%
  mutate( Q2005.49b.5 = ifelse( Q2005.49b.5 == "Yes" , "parents" , Q2005.49b.5 )) %>%
  mutate( Q2005.49b.6 = ifelse( Q2005.49b.6 == "Yes" , "other relatives" , Q2005.49b.6 )) %>%
  mutate( Q2005.49b.7 = ifelse( Q2005.49b.7 == "Yes" , "a dealer" , Q2005.49b.7 )) %>%
  mutate( Q2005.49b.8 = ifelse( Q2005.49b.8 == "Yes" , "got from house" , Q2005.49b.8 )) %>%
  mutate( Q2005.49b.9 = ifelse( Q2005.49b.9 == "Yes" , "somewhere else" , Q2005.49b.9 )) 

q49_2$Q2005.49b <- apply(q49_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q49_2;

q49_2 %>% 
  select(fullid_05, Q2005.49b) %>%
  mutate(Q2005.49b = 
           str_replace(Q2005.49b, pattern = '^.*?,', replacement = ""),
         Q2005.49b = str_trim(Q2005.49b, side='left')) %>%
  filter(fullid_05 != Q2005.49b) -> q49_2

# question 52_1
q52_1 <- byds %>%
  select(fullid_05, Q2005.52a.1, Q2005.52a.2, Q2005.52a.3, Q2005.52a.4, Q2005.52a.5, Q2005.52a.6, Q2005.52a.7, 
         Q2005.52a.8, Q2005.52a.9, Q2005.52a.10, Q2005.52a.11) %>%
  mutate( Q2005.52a.1 = ifelse( Q2005.52a.1 == "Yes" , "at home" , Q2005.52a.1 )) %>%
  mutate( Q2005.52a.2 = ifelse( Q2005.52a.2 == "Yes" , "at a friend's house" , Q2005.52a.2 )) %>%
  mutate( Q2005.52a.3 = ifelse( Q2005.52a.3 == "Yes" , "at a party" , Q2005.52a.3 )) %>%
  mutate( Q2005.52a.4 = ifelse( Q2005.52a.4 == "Yes" , "at a disco" , Q2005.52a.4 )) %>%
  mutate( Q2005.52a.5 = ifelse( Q2005.52a.5 == "Yes" , "at a youth club or community centre" , Q2005.52a.5 )) %>%
  mutate( Q2005.52a.6 = ifelse( Q2005.52a.6 == "Yes" , "at school" , Q2005.52a.6 )) %>%
  mutate( Q2005.52a.7 = ifelse( Q2005.52a.7 == "Yes" , "in a car" , Q2005.52a.7 )) %>%
  mutate( Q2005.52a.8 = ifelse( Q2005.52a.8 == "Yes" , "in a pub" , Q2005.52a.8 )) %>%
  mutate( Q2005.52a.9 = ifelse( Q2005.52a.9 == "Yes" , "at a concert of festival" , Q2005.52a.9 )) %>%
  mutate( Q2005.52a.10 = ifelse( Q2005.52a.10 == "Yes" , "other outdoor space" , Q2005.52a.10 )) %>%
  mutate( Q2005.52a.11 = ifelse( Q2005.52a.11 == "Yes" , "somewhere else" , Q2005.52a.11 )) 

q52_1$Q2005.52a <- apply(q52_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q52_1;

q52_1 %>% 
  select(fullid_05, Q2005.52a) %>%
  mutate(Q2005.52a = 
           str_replace(Q2005.52a, pattern = '^.*?,', replacement = ""),
         Q2005.52a = str_trim(Q2005.52a, side='left')) %>%
  filter(fullid_05 != Q2005.52a) -> q52_1

# question 52_2
q52_2 <- byds %>%
  select(fullid_05, Q2005.52b.1, Q2005.52b.2, Q2005.52b.3, Q2005.52b.4, Q2005.52b.5, Q2005.52b.6, Q2005.52b.7, 
         Q2005.52b.8, Q2005.52b.9) %>%
  mutate( Q2005.52b.1 = ifelse( Q2005.52b.1 == "Yes" , "older friends" , Q2005.52b.1 )) %>%
  mutate( Q2005.52b.2 = ifelse( Q2005.52b.2 == "Yes" , "younger friends" , Q2005.52b.2 )) %>%
  mutate( Q2005.52b.3 = ifelse( Q2005.52b.3 == "Yes" , "friends own age" , Q2005.52b.3 )) %>%
  mutate( Q2005.52b.4 = ifelse( Q2005.52b.4 == "Yes" , "brother or sister" , Q2005.52b.4 )) %>%
  mutate( Q2005.52b.5 = ifelse( Q2005.52b.5 == "Yes" , "parents" , Q2005.52b.5 )) %>%
  mutate( Q2005.52b.6 = ifelse( Q2005.52b.6 == "Yes" , "other relatives" , Q2005.52b.6 )) %>%
  mutate( Q2005.52b.7 = ifelse( Q2005.52b.7 == "Yes" , "a dealer" , Q2005.52b.7 )) %>%
  mutate( Q2005.52b.8 = ifelse( Q2005.52b.8 == "Yes" , "got from house" , Q2005.52b.8 )) %>%
  mutate( Q2005.52b.9 = ifelse( Q2005.52b.9 == "Yes" , "somewhere else" , Q2005.52b.9 )) 


q52_2$Q2005.52b <- apply(q52_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q52_2;

q52_2 %>% 
  select(fullid_05, Q2005.52b) %>%
  mutate(Q2005.52b = 
           str_replace(Q2005.52b, pattern = '^.*?,', replacement = ""),
         Q2005.52b = str_trim(Q2005.52b, side='left')) %>%
  filter(fullid_05 != Q2005.52b) -> q52_2

# question 55_1
q55_1 <- byds %>%
  select(fullid_05, Q2005.55a.1, Q2005.55a.2, Q2005.55a.3, Q2005.55a.4, Q2005.55a.5, Q2005.55a.6, Q2005.55a.7, 
         Q2005.55a.8, Q2005.55a.9, Q2005.55a.10, Q2005.55a.11) %>%
  mutate( Q2005.55a.1 = ifelse( Q2005.55a.1 == "Yes" , "at home" , Q2005.55a.1 )) %>%
  mutate( Q2005.55a.2 = ifelse( Q2005.55a.2 == "Yes" , "at a friend's house" , Q2005.55a.2 )) %>%
  mutate( Q2005.55a.3 = ifelse( Q2005.55a.3 == "Yes" , "at a party" , Q2005.55a.3 )) %>%
  mutate( Q2005.55a.4 = ifelse( Q2005.55a.4 == "Yes" , "at a disco" , Q2005.55a.4 )) %>%
  mutate( Q2005.55a.5 = ifelse( Q2005.55a.5 == "Yes" , "at a youth club or community centre" , Q2005.55a.5 )) %>%
  mutate( Q2005.55a.6 = ifelse( Q2005.55a.6 == "Yes" , "at school" , Q2005.55a.6 )) %>%
  mutate( Q2005.55a.7 = ifelse( Q2005.55a.7 == "Yes" , "in a car" , Q2005.55a.7 )) %>%
  mutate( Q2005.55a.8 = ifelse( Q2005.55a.8 == "Yes" , "in a pub" , Q2005.55a.8 )) %>%
  mutate( Q2005.55a.9 = ifelse( Q2005.55a.9 == "Yes" , "at a concert of festival" , Q2005.55a.9 )) %>%
  mutate( Q2005.55a.10 = ifelse( Q2005.55a.10 == "Yes" , "other outdoor space" , Q2005.55a.10 )) %>%
  mutate( Q2005.55a.11 = ifelse( Q2005.55a.11 == "Yes" , "somewhere else" , Q2005.55a.11 )) 

q55_1$Q2005.55a <- apply(q55_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q55_1;

q55_1 %>% 
  select(fullid_05, Q2005.55a) %>%
  mutate(Q2005.55a = 
           str_replace(Q2005.55a, pattern = '^.*?,', replacement = ""),
         Q2005.55a = str_trim(Q2005.55a, side='left')) %>%
  filter(fullid_05 != Q2005.55a) -> q55_1

# question 55_2
q55_2 <- byds %>%
  select(fullid_05, Q2005.55b.1, Q2005.55b.2, Q2005.55b.3, Q2005.55b.4, Q2005.55b.5, Q2005.55b.6, Q2005.55b.7, 
         Q2005.55b.8, Q2005.55b.9) %>%
  mutate( Q2005.55b.1 = ifelse( Q2005.55b.1 == "Yes" , "older friends" , Q2005.55b.1 )) %>%
  mutate( Q2005.55b.2 = ifelse( Q2005.55b.2 == "Yes" , "younger friends" , Q2005.55b.2 )) %>%
  mutate( Q2005.55b.3 = ifelse( Q2005.55b.3 == "Yes" , "friends own age" , Q2005.55b.3 )) %>%
  mutate( Q2005.55b.4 = ifelse( Q2005.55b.4 == "Yes" , "brother or sister" , Q2005.55b.4 )) %>%
  mutate( Q2005.55b.5 = ifelse( Q2005.55b.5 == "Yes" , "parents" , Q2005.55b.5 )) %>%
  mutate( Q2005.55b.6 = ifelse( Q2005.55b.6 == "Yes" , "other relatives" , Q2005.55b.6 )) %>%
  mutate( Q2005.55b.7 = ifelse( Q2005.55b.7 == "Yes" , "a dealer" , Q2005.55b.7 )) %>%
  mutate( Q2005.55b.8 = ifelse( Q2005.55b.8 == "Yes" , "got from house" , Q2005.55b.8 )) %>%
  mutate( Q2005.55b.9 = ifelse( Q2005.55b.9 == "Yes" , "somewhere else" , Q2005.55b.9 )) 

q55_2$Q2005.55b <- apply(q55_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q55_2;

q55_2 %>% 
  select(fullid_05, Q2005.55b) %>%
  mutate(Q2005.55b = 
           str_replace(Q2005.55b, pattern = '^.*?,', replacement = ""),
         Q2005.55b = str_trim(Q2005.55b, side='left')) %>%
  filter(fullid_05 != Q2005.55b) -> q55_2

# question 58_1
q58_1 <- byds %>%
  select(fullid_05, Q2005.58a.1, Q2005.58a.2, Q2005.58a.3, Q2005.58a.4, Q2005.58a.5, Q2005.58a.6, Q2005.58a.7, 
         Q2005.58a.8, Q2005.58a.9, Q2005.58a.10, Q2005.58a.11) %>%
  mutate( Q2005.58a.1 = ifelse( Q2005.58a.1 == "Yes" , "at home" , Q2005.58a.1 )) %>%
  mutate( Q2005.58a.2 = ifelse( Q2005.58a.2 == "Yes" , "at a friend's house" , Q2005.58a.2 )) %>%
  mutate( Q2005.58a.3 = ifelse( Q2005.58a.3 == "Yes" , "at a party" , Q2005.58a.3 )) %>%
  mutate( Q2005.58a.4 = ifelse( Q2005.58a.4 == "Yes" , "at a disco" , Q2005.58a.4 )) %>%
  mutate( Q2005.58a.5 = ifelse( Q2005.58a.5 == "Yes" , "at a youth club or community centre" , Q2005.58a.5 )) %>%
  mutate( Q2005.58a.6 = ifelse( Q2005.58a.6 == "Yes" , "at school" , Q2005.58a.6 )) %>%
  mutate( Q2005.58a.7 = ifelse( Q2005.58a.7 == "Yes" , "in a car" , Q2005.58a.7 )) %>%
  mutate( Q2005.58a.8 = ifelse( Q2005.58a.8 == "Yes" , "in a pub" , Q2005.58a.8 )) %>%
  mutate( Q2005.58a.9 = ifelse( Q2005.58a.9 == "Yes" , "at a concert of festival" , Q2005.58a.9 )) %>%
  mutate( Q2005.58a.10 = ifelse( Q2005.58a.10 == "Yes" , "other outdoor space" , Q2005.58a.10 )) %>%
  mutate( Q2005.58a.11 = ifelse( Q2005.58a.11 == "Yes" , "somewhere else" , Q2005.58a.11 )) 

q58_1$Q2005.58a <- apply(q58_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q58_1;

q58_1 %>% 
  select(fullid_05, Q2005.58a) %>%
  mutate(Q2005.58a = 
           str_replace(Q2005.58a, pattern = '^.*?,', replacement = ""),
         Q2005.58a = str_trim(Q2005.58a, side='left')) %>%
  filter(fullid_05 != Q2005.58a) -> q58_1

# question 58_2
q58_2 <- byds %>%
  select(fullid_05, Q2005.58b.1, Q2005.58b.2, Q2005.58b.3, Q2005.58b.4, Q2005.58b.5, Q2005.58b.6, Q2005.58b.7, 
         Q2005.58b.8, Q2005.58b.9) %>%
  mutate( Q2005.58b.1 = ifelse( Q2005.58b.1 == "Yes" , "older friends" , Q2005.58b.1 )) %>%
  mutate( Q2005.58b.2 = ifelse( Q2005.58b.2 == "Yes" , "younger friends" , Q2005.58b.2 )) %>%
  mutate( Q2005.58b.3 = ifelse( Q2005.58b.3 == "Yes" , "friends own age" , Q2005.58b.3 )) %>%
  mutate( Q2005.58b.4 = ifelse( Q2005.58b.4 == "Yes" , "brother or sister" , Q2005.58b.4 )) %>%
  mutate( Q2005.58b.5 = ifelse( Q2005.58b.5 == "Yes" , "parents" , Q2005.58b.5 )) %>%
  mutate( Q2005.58b.6 = ifelse( Q2005.58b.6 == "Yes" , "other relatives" , Q2005.58b.6 )) %>%
  mutate( Q2005.58b.7 = ifelse( Q2005.58b.7 == "Yes" , "a dealer" , Q2005.58b.7 )) %>%
  mutate( Q2005.58b.8 = ifelse( Q2005.58b.8 == "Yes" , "got from house" , Q2005.58b.8 )) %>%
  mutate( Q2005.58b.9 = ifelse( Q2005.58b.9 == "Yes" , "somewhere else" , Q2005.58b.9 )) 

q58_2$Q2005.58b <- apply(q58_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q58_2;

q58_2 %>% 
  select(fullid_05, Q2005.58b) %>%
  mutate(Q2005.58b = 
           str_replace(Q2005.58b, pattern = '^.*?,', replacement = ""),
         Q2005.58b = str_trim(Q2005.58b, side='left')) %>%
  filter(fullid_05 != Q2005.58b) -> q58_2

# question 61_1
q61_1 <- byds %>%
  select(fullid_05, Q2005.61a.1, Q2005.61a.2, Q2005.61a.3, Q2005.61a.4, Q2005.61a.5, Q2005.61a.6, Q2005.61a.7, 
         Q2005.61a.8, Q2005.61a.9, Q2005.61a.10, Q2005.61a.11) %>%
  mutate( Q2005.61a.1 = ifelse( Q2005.61a.1 == "Yes" , "at home" , Q2005.61a.1 )) %>%
  mutate( Q2005.61a.2 = ifelse( Q2005.61a.2 == "Yes" , "at a friend's house" , Q2005.61a.2 )) %>%
  mutate( Q2005.61a.3 = ifelse( Q2005.61a.3 == "Yes" , "at a party" , Q2005.61a.3 )) %>%
  mutate( Q2005.61a.4 = ifelse( Q2005.61a.4 == "Yes" , "at a disco" , Q2005.61a.4 )) %>%
  mutate( Q2005.61a.5 = ifelse( Q2005.61a.5 == "Yes" , "at a youth club or community centre" , Q2005.61a.5 )) %>%
  mutate( Q2005.61a.6 = ifelse( Q2005.61a.6 == "Yes" , "at school" , Q2005.61a.6 )) %>%
  mutate( Q2005.61a.7 = ifelse( Q2005.61a.7 == "Yes" , "in a car" , Q2005.61a.7 )) %>%
  mutate( Q2005.61a.8 = ifelse( Q2005.61a.8 == "Yes" , "in a pub" , Q2005.61a.8 )) %>%
  mutate( Q2005.61a.9 = ifelse( Q2005.61a.9 == "Yes" , "at a concert of festival" , Q2005.61a.9 )) %>%
  mutate( Q2005.61a.10 = ifelse( Q2005.61a.10 == "Yes" , "other outdoor space" , Q2005.61a.10 )) %>%
  mutate( Q2005.61a.11 = ifelse( Q2005.61a.11 == "Yes" , "somewhere else" , Q2005.61a.11 )) 

q61_1$Q2005.61a <- apply(q61_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q61_1;

q61_1 %>% 
  select(fullid_05, Q2005.61a) %>%
  mutate(Q2005.61a = 
           str_replace(Q2005.61a, pattern = '^.*?,', replacement = ""),
         Q2005.61a = str_trim(Q2005.61a, side='left')) %>%
  filter(fullid_05 != Q2005.61a) -> q61_1

# question 61_2
q61_2 <- byds %>%
  select(fullid_05, Q2005.61b.1, Q2005.61b.2, Q2005.61b.3, Q2005.61b.4, Q2005.61b.5, Q2005.61b.6, Q2005.61b.7, 
         Q2005.61b.8, Q2005.61b.9) %>%
  mutate( Q2005.61b.1 = ifelse( Q2005.61b.1 == "Yes" , "older friends" , Q2005.61b.1 )) %>%
  mutate( Q2005.61b.2 = ifelse( Q2005.61b.2 == "Yes" , "younger friends" , Q2005.61b.2 )) %>%
  mutate( Q2005.61b.3 = ifelse( Q2005.61b.3 == "Yes" , "friends own age" , Q2005.61b.3 )) %>%
  mutate( Q2005.61b.4 = ifelse( Q2005.61b.4 == "Yes" , "brother or sister" , Q2005.61b.4 )) %>%
  mutate( Q2005.61b.5 = ifelse( Q2005.61b.5 == "Yes" , "parents" , Q2005.61b.5 )) %>%
  mutate( Q2005.61b.6 = ifelse( Q2005.61b.6 == "Yes" , "other relatives" , Q2005.61b.6 )) %>%
  mutate( Q2005.61b.7 = ifelse( Q2005.61b.7 == "Yes" , "a dealer" , Q2005.61b.7 )) %>%
  mutate( Q2005.61b.8 = ifelse( Q2005.61b.8 == "Yes" , "got from house" , Q2005.61b.8 )) %>%
  mutate( Q2005.61b.9 = ifelse( Q2005.61b.9 == "Yes" , "somewhere else" , Q2005.61b.9 )) 

q61_2$Q2005.61b <- apply(q61_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q61_2;

q61_2 %>% 
  select(fullid_05, Q2005.61b) %>%
  mutate(Q2005.61b = 
           str_replace(Q2005.61b, pattern = '^.*?,', replacement = ""),
         Q2005.61b = str_trim(Q2005.61b, side='left')) %>%
  filter(fullid_05 != Q2005.61b) -> q61_2

# question 64_1
q64_1 <- byds %>%
  select(fullid_05, Q2005.64a.1, Q2005.64a.2, Q2005.64a.3, Q2005.64a.4, Q2005.64a.5, Q2005.64a.6, Q2005.64a.7, 
         Q2005.64a.8, Q2005.64a.9, Q2005.64a.10, Q2005.64a.11) %>%
  mutate( Q2005.64a.1 = ifelse( Q2005.64a.1 == "Yes" , "at home" , Q2005.64a.1 )) %>%
  mutate( Q2005.64a.2 = ifelse( Q2005.64a.2 == "Yes" , "at a friend's house" , Q2005.64a.2 )) %>%
  mutate( Q2005.64a.3 = ifelse( Q2005.64a.3 == "Yes" , "at a party" , Q2005.64a.3 )) %>%
  mutate( Q2005.64a.4 = ifelse( Q2005.64a.4 == "Yes" , "at a disco" , Q2005.64a.4 )) %>%
  mutate( Q2005.64a.5 = ifelse( Q2005.64a.5 == "Yes" , "at a youth club or community centre" , Q2005.64a.5 )) %>%
  mutate( Q2005.64a.6 = ifelse( Q2005.64a.6 == "Yes" , "at school" , Q2005.64a.6 )) %>%
  mutate( Q2005.64a.7 = ifelse( Q2005.64a.7 == "Yes" , "in a car" , Q2005.64a.7 )) %>%
  mutate( Q2005.64a.8 = ifelse( Q2005.64a.8 == "Yes" , "in a pub" , Q2005.64a.8 )) %>%
  mutate( Q2005.64a.9 = ifelse( Q2005.64a.9 == "Yes" , "at a concert of festival" , Q2005.64a.9 )) %>%
  mutate( Q2005.64a.10 = ifelse( Q2005.64a.10 == "Yes" , "other outdoor space" , Q2005.64a.10 )) %>%
  mutate( Q2005.64a.11 = ifelse( Q2005.64a.11 == "Yes" , "somewhere else" , Q2005.64a.11 )) 

q64_1$Q2005.64a <- apply(q64_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q64_1;

q64_1 %>% 
  select(fullid_05, Q2005.64a) %>%
  mutate(Q2005.64a = 
           str_replace(Q2005.64a, pattern = '^.*?,', replacement = ""),
         Q2005.64a = str_trim(Q2005.64a, side='left')) %>%
  filter(fullid_05 != Q2005.64a) -> q64_1

# question 64_2
q64_2 <- byds %>%
  select(fullid_05, Q2005.64b.1, Q2005.64b.2, Q2005.64b.3, Q2005.64b.4, Q2005.64b.5, Q2005.64b.6, Q2005.64b.7, 
         Q2005.64b.8, Q2005.64b.9, Q2005.64b.10) %>%
  mutate( Q2005.64b.1 = ifelse( Q2005.64b.1 == "Yes" , "older friends" , Q2005.64b.1 )) %>%
  mutate( Q2005.64b.2 = ifelse( Q2005.64b.2 == "Yes" , "younger friends" , Q2005.64b.2 )) %>%
  mutate( Q2005.64b.3 = ifelse( Q2005.64b.3 == "Yes" , "friends own age" , Q2005.64b.3 )) %>%
  mutate( Q2005.64b.4 = ifelse( Q2005.64b.4 == "Yes" , "brother or sister" , Q2005.64b.4 )) %>%
  mutate( Q2005.64b.5 = ifelse( Q2005.64b.5 == "Yes" , "parents" , Q2005.64b.5 )) %>%
  mutate( Q2005.64b.6 = ifelse( Q2005.64b.6 == "Yes" , "other relatives" , Q2005.64b.6 )) %>%
  mutate( Q2005.64b.7 = ifelse( Q2005.64b.7 == "Yes" , "dealer" , Q2005.64b.7 )) %>%
  mutate( Q2005.64b.8 = ifelse( Q2005.64b.8 == "Yes" , "got from a shop" , Q2005.64b.8 )) %>%
  mutate( Q2005.64b.9 = ifelse( Q2005.64b.9 == "Yes" , "got from house" , Q2005.64b.9 )) %>%
  mutate( Q2005.64b.10 = ifelse( Q2005.64b.10 == "Yes" , "somewhere else" , Q2005.64b.10 )) 

q64_2$Q2005.64b <- apply(q64_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q64_2;

q64_2 %>% 
  select(fullid_05, Q2005.64b) %>%
  mutate(Q2005.64b = 
           str_replace(Q2005.64b, pattern = '^.*?,', replacement = ""),
         Q2005.64b = str_trim(Q2005.64b, side='left')) %>%
  filter(fullid_05 != Q2005.64b) -> q64_2

# question 67_1
q67_1 <- byds %>%
  select(fullid_05, Q2005.67a.1, Q2005.67a.2, Q2005.67a.3, Q2005.67a.4, Q2005.67a.5, Q2005.67a.6, Q2005.67a.7, 
         Q2005.67a.8, Q2005.67a.9, Q2005.67a.10, Q2005.67a.11) %>%
  mutate( Q2005.67a.1 = ifelse( Q2005.67a.1 == "Yes" , "at home" , Q2005.67a.1 )) %>%
  mutate( Q2005.67a.2 = ifelse( Q2005.67a.2 == "Yes" , "at a friend's house" , Q2005.67a.2 )) %>%
  mutate( Q2005.67a.3 = ifelse( Q2005.67a.3 == "Yes" , "at a party" , Q2005.67a.3 )) %>%
  mutate( Q2005.67a.4 = ifelse( Q2005.67a.4 == "Yes" , "at a disco" , Q2005.67a.4 )) %>%
  mutate( Q2005.67a.5 = ifelse( Q2005.67a.5 == "Yes" , "at a youth club or community centre" , Q2005.67a.5 )) %>%
  mutate( Q2005.67a.6 = ifelse( Q2005.67a.6 == "Yes" , "at school" , Q2005.67a.6 )) %>%
  mutate( Q2005.67a.7 = ifelse( Q2005.67a.7 == "Yes" , "in a car" , Q2005.67a.7 )) %>%
  mutate( Q2005.67a.8 = ifelse( Q2005.67a.8 == "Yes" , "in a pub" , Q2005.67a.8 )) %>%
  mutate( Q2005.67a.9 = ifelse( Q2005.67a.9 == "Yes" , "at a concert of festival" , Q2005.67a.9 )) %>%
  mutate( Q2005.67a.10 = ifelse( Q2005.67a.10 == "Yes" , "other outdoor space" , Q2005.67a.10 )) %>%
  mutate( Q2005.67a.11 = ifelse( Q2005.67a.11 == "Yes" , "somewhere else" , Q2005.67a.11 )) 

q67_1$Q2005.67a <- apply(q67_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q67_1;

q67_1 %>% 
  select(fullid_05, Q2005.67a) %>%
  mutate(Q2005.67a = 
           str_replace(Q2005.67a, pattern = '^.*?,', replacement = ""),
         Q2005.67a = str_trim(Q2005.67a, side='left')) %>%
  filter(fullid_05 != Q2005.67a) -> q67_1

# question 67_2
q67_2 <- byds %>%
  select(fullid_05, Q2005.67b.1, Q2005.67b.2, Q2005.67b.3, Q2005.67b.4, Q2005.67b.5, Q2005.67b.6, Q2005.67b.7, 
         Q2005.67b.8, Q2005.67b.9) %>%
  mutate( Q2005.67b.1 = ifelse( Q2005.67b.1 == "Yes" , "older friends" , Q2005.67b.1 )) %>%
  mutate( Q2005.67b.2 = ifelse( Q2005.67b.2 == "Yes" , "younger friends" , Q2005.67b.2 )) %>%
  mutate( Q2005.67b.3 = ifelse( Q2005.67b.3 == "Yes" , "friends own age" , Q2005.67b.3 )) %>%
  mutate( Q2005.67b.4 = ifelse( Q2005.67b.4 == "Yes" , "brother or sister" , Q2005.67b.4 )) %>%
  mutate( Q2005.67b.5 = ifelse( Q2005.67b.5 == "Yes" , "parents" , Q2005.67b.5 )) %>%
  mutate( Q2005.67b.6 = ifelse( Q2005.67b.6 == "Yes" , "other relatives" , Q2005.67b.6 )) %>%
  mutate( Q2005.67b.7 = ifelse( Q2005.67b.7 == "Yes" , "a dealer" , Q2005.67b.7 )) %>%
  mutate( Q2005.67b.8 = ifelse( Q2005.67b.8 == "Yes" , "got from house" , Q2005.67b.8 )) %>%
  mutate( Q2005.67b.9 = ifelse( Q2005.67b.9 == "Yes" , "somewhere else" , Q2005.67b.9 )) 

q67_2$Q2005.67b <- apply(q67_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q67_2;

q67_2 %>% 
  select(fullid_05, Q2005.67b) %>%
  mutate(Q2005.67b = 
           str_replace(Q2005.67b, pattern = '^.*?,', replacement = ""),
         Q2005.67b = str_trim(Q2005.67b, side='left')) %>%
  filter(fullid_05 != Q2005.67b) -> q67_2

# question 69_1
q69_1 <- byds %>%
  select(fullid_05, Q2005.69.1, Q2005.69.2, Q2005.69.3, Q2005.69.4, Q2005.69.5, Q2005.69.6, Q2005.69.7, 
         Q2005.69.8, Q2005.69.9) %>%
  mutate( Q2005.69.1 = ifelse( Q2005.69.1 == "Yes" , "mother" , Q2005.69.1 )) %>%
  mutate( Q2005.69.2 = ifelse( Q2005.69.2 == "Yes" , "stepmother" , Q2005.69.2 )) %>%
  mutate( Q2005.69.3 = ifelse( Q2005.69.3 == "Yes" , "grandmother" , Q2005.69.3 )) %>%
  mutate( Q2005.69.4 = ifelse( Q2005.69.4 == "Yes" , "foster mother" , Q2005.69.4 )) %>%
  mutate( Q2005.69.5 = ifelse( Q2005.69.5 == "Yes" , "father" , Q2005.69.5 )) %>%
  mutate( Q2005.69.6 = ifelse( Q2005.69.6 == "Yes" , "stepfather" , Q2005.69.6 )) %>%
  mutate( Q2005.69.7 = ifelse( Q2005.69.7 == "Yes" , "grandfather" , Q2005.69.7 )) %>%
  mutate( Q2005.69.8 = ifelse( Q2005.69.8 == "Yes" , "foster father" , Q2005.69.8 )) %>%
  mutate( Q2005.69.9 = ifelse( Q2005.69.9 == "Yes" , "I live in a children's home" , Q2005.69.9 ))

q69_1$Q2005.69a <- apply(q69_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q69_1;

q69_1 %>% 
  select(fullid_05, Q2005.69a) %>%
  mutate(Q2005.69a = 
           str_replace(Q2005.69a, pattern = '^.*?,', replacement = ""),
         Q2005.69a = str_trim(Q2005.69a, side='left')) %>%
  filter(fullid_05 != Q2005.69a) -> q69_1

# question 69_2
q69_2 <- byds %>%
  select(fullid_05, Q2005.69.16, Q2005.69.17, Q2005.69.18, Q2005.69.19, Q2005.69.20) %>%
  mutate( Q2005.69.16 = ifelse( Q2005.69.16 == "Yes" , "aunt" , Q2005.69.16 )) %>%
  mutate( Q2005.69.17 = ifelse( Q2005.69.17 == "Yes" , "uncle" , Q2005.69.17 )) %>%
  mutate( Q2005.69.18 = ifelse( Q2005.69.18 == "Yes" , "cousin" , Q2005.69.18 )) %>%
  mutate( Q2005.69.19 = ifelse( Q2005.69.19 == "Yes" , "lodger" , Q2005.69.19 )) %>%
  mutate( Q2005.69.20 = ifelse( Q2005.69.20 == "Yes" , "other adult" , Q2005.69.20 )) 

q69_2$Q2005.69h <- apply(q69_2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q69_2;

q69_2 %>% 
  select(fullid_05, Q2005.69h) %>%
  mutate(Q2005.69h = 
           str_replace(Q2005.69h, pattern = '^.*?,', replacement = ""),
         Q2005.69h = str_trim(Q2005.69h, side='left')) %>%
  filter(fullid_05 != Q2005.69h) -> q69_2

# question 70
q70 <- byds %>%
  select(fullid_05, Q2005.70.1, Q2005.70.2, Q2005.70.3) %>%
  mutate( Q2005.70.1 = ifelse( Q2005.70.1 == "Yes" , "mother" , Q2005.70.1 )) %>%
  mutate( Q2005.70.2 = ifelse( Q2005.70.2 == "Yes" , "father" , Q2005.70.2 )) %>%
  mutate( Q2005.70.3 = ifelse( Q2005.70.3 == "Neither mum or dad is dead" , "neither parent" , Q2005.70.3 )) %>%
  add_column(Q2005.70.4 = NA) %>%
  mutate(Q2005.70.4 = ifelse((Q2005.70.1 == "mother") & (Q2005.70.2 == "father"), "both parents",  Q2005.70.4))

q70$Q2005.70 <- apply(q70, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q70;

q70 %>% 
  select(fullid_05, Q2005.70) %>%
  mutate(Q2005.70 = 
           str_replace(Q2005.70, pattern = '^.*?,', replacement = ""),
         Q2005.70 = str_trim(Q2005.70, side='left')) %>%
  filter(fullid_05 != Q2005.70) -> q70

q70 <- q70 %>%
  mutate(Q2005.70 = ifelse(Q2005.70 == "mother, 3, 3", "mother", Q2005.70)) %>%
  mutate(Q2005.70 = ifelse(Q2005.70 == "mother, father, 3, both parents", "both parents", Q2005.70)) %>%
  mutate(Q2005.70 = ifelse(Q2005.70 == "mother, father, both parents", "both parents", Q2005.70)) %>%
  mutate(Q2005.70 = ifelse(Q2005.70 == "mother, father, neither parent, both parents", "both parents", Q2005.70))

# join it back together #################

# pull of the variables already clean
wave_5 <- byds %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, X2_05, X3_05, Q2005.1, Q2005.3, Q2005.3a, 
         Q2005.4, Q2005.5, Q2005.6, Q2005.7.1, Q2005.7.2, Q2005.7.3, Q2005.7.4, Q2005.7.5, Q2005.8.1, Q2005.8.2, Q2005.8.3, 
         Q2005.8.4, Q2005.8.5, Q2005.8.6, Q2005.8.7, Q2005.8.8, Q2005.8.9, Q2005.8.10, Q2005.8.11, Q2005.8.12, Q2005.8.13, Q2005.8.14, 
         Q2005.8.15, Q2005.8.16, Q2005.8.17, Q2005.8.18, Q2005.8.19, Q2005.8.20, Q2005.8.21, Q2005.8.22, Q2005.9, Q2005.10.1, Q2005.10.2, 
         Q2005.10.3, Q2005.10.4, Q2005.10.5, Q2005.10.6, Q2005.10.7, Q2005.10.8, Q2005.10.9, Q2005.11, Q2005.12, Q2005.13, Q2005.14, 
         Q2005.15, Q2005.16, Q2005.17.1, Q2005.17.2, Q2005.17.3, Q2005.17.4, Q2005.17.5, Q2005.17.6, Q2005.17.7, Q2005.17.8, Q2005.17.9, 
         Q2005.17.10, Q2005.17.11, Q2005.17.12, Q2005.17.13, Q2005.18, Q2005.18a, Q2005.19, Q2005.19a, Q2005.20, Q2005.20a, Q2005.21, 
         Q2005.21a, Q2005.22, Q2005.22a, Q2005.23, Q2005.23a, Q2005.24, Q2005.24a, Q2005.25, Q2005.25a, Q2005.26, Q2005.26a, Q2005.27, 
         Q2005.27a, Q2005.28, Q2005.28a, Q2005.29, Q2005.29a, Q2005.30, Q2005.30a, Q2005.31, Q2005.31a, Q2005.32, Q2005.33, Q2005.33b, 
         Q2005.33c, Q2005.33d, Q2005.34, Q2005.35, Q2005.35a, Q2005.35d, Q2005.35e, Q2005.36, Q2005.37, Q2005.37a, Q2005.37b.1, 
         Q2005.37b.2, Q2005.37b.3, Q2005.37b.4, Q2005.37b.5, Q2005.37c, Q2005.37d, Q2005.37e, Q2005.37f, Q2005.37g, Q2005.37h, Q2005.37i, 
         Q2005.38, Q2005.39, Q2005.40, Q2005.40c, Q2005.41, Q2005.42, Q2005.43, Q2005.43c, Q2005.43d, Q2005.44, Q2005.45, Q2005.46, 
         Q2005.46c, Q2005.46d, Q2005.47, Q2005.48, Q2005.49, Q2005.49c, Q2005.49d, Q2005.50, Q2005.51, Q2005.52, Q2005.52c, Q2005.52d, 
         Q2005.53, Q2005.54, Q2005.55, Q2005.55c, Q2005.55d, Q2005.56, Q2005.57, Q2005.58, Q2005.58c, Q2005.58d, Q2005.59, Q2005.60, 
         Q2005.61, Q2005.61c, Q2005.61d, Q2005.62, Q2005.63, Q2005.64, Q2005.64c, Q2005.64d, Q2005.65, Q2005.66, Q2005.67, Q2005.68.1, 
         Q2005.68.2, Q2005.68.3, Q2005.68.4, Q2005.68.5, Q2005.68.6, Q2005.68.7, Q2005.68.8, Q2005.68.9, Q2005.68.10, Q2005.68.11, 
         Q2005.68.12, Q2005.68.13, Q2005.68.14, Q2005.68.15, Q2005.68.16, Q2005.68.17, Q2005.68.18, Q2005.68.19, Q2005.68.20, 
         Q2005.69b, Q2005.69c, Q2005.69d, Q2005.69e, Q2005.69f, Q2005.69g, Q2005.71, Q2005.72, 
         Q2005.73, Q2005.73a, Q2005.74, Q2005.75, Q2005.76, Q2005.77, Q2005.78, Q2005.79, Q2005.79a, Q2005.79b, Q2005.80, Q2005.80a, Q2005.81, 
         Q2005.81a, Q2005.81b, Q2005.82, Q2005.82a, Q2005.83, Q2005.83a, Q2005.84, Q2005.85, Q2005.85a, Q2005.86.1, Q2005.86.2, Q2005.86.3, 
         Q2005.86.4, Q2005.86.5, Q2005.86.6, Q2005.86.7, Q2005.86.8, Q2005.86.9, Q2005.87.1, Q2005.87.2, Q2005.87.3, Q2005.87.4, Q2005.87.5, 
         Q2005.88.1, Q2005.88.2, Q2005.88.3, Q2005.88.4, Q2005.88.5, Q2005.89.1, Q2005.89.2, Q2005.89.3, Q2005.89.4, Q2005.89.5, Q2005.90, Q2005.91, 
         Q2005.92, Q2005.92a, Q2005.92b, Q2005.93, Q2005.93a, Q2005.93b, Q2005.94, Q2005.94a, Q2005.94b, Q2005.95, Q2005.95a, Q2005.95b, 
         Q2995.96, Q2005.96a, Q2005.96b, Q2005.97, Q2005.97a, Q2005.97b, Q2005.98, Q2005.98a, Q2005.98b, Q2005.99, Q2005.99a, Q2005.99b, 
         Q2005.100, Q2005.100a, Q2005.100b, Q2005.101, Q2005.101a, Q2005.102, Q2005.102a, Q2005.103, Q2005.103a, Q2005.104, Q2005.104a, Q2005.105, 
         Q2005.106, Q2005.107, Q2005.108.1, Q2005.108.2, Q2005.108.3, Q2005.108.4, Q2005.108.5, Q2005.108.6, Q2005.109.1, Q2005.109.2, 
         Q2005.109.3, Q2005.109.4, Q2005.109.5, Q2005.109.6, Q2005.109.7, Q2005.109.8, Q2005.109.9, Q2005.109.10, Q2005.109.11, Q2005.109.12, 
         Q2005.109.13, Q2005.110, Q2005.110a, Q2005.111, Q2005.111a, Q2005.112, Q2005.112a, Q2005.113, Q2005.113a, Q2005.114.1, Q2005.114.2, 
         Q2005.114.3, Q2005.114.4, Q2005.114.5, Q2005.114.6, Q2005.114.7, Q2005.114.8, Q2005.114.9, Q2005.114.10, Q2005.114.11, Q2005.114.12, 
         Q2005.114.13, Q2005.114.14, Q2005.114.15, Q2005.114.16, Q2005.114.17)


# join it back together
wave_5 <- wave_5 %>%
  left_join(q2, by = "fullid_05") %>%
  left_join(q33_1, by = "fullid_05") %>%
  left_join(q35_2, by = "fullid_05") %>%
  left_join(q35_3, by = "fullid_05") %>%
  left_join(q40_1, by = "fullid_05") %>%
  left_join(q40_2, by = "fullid_05") %>%
  left_join(q43_1, by = "fullid_05") %>%
  left_join(q43_2, by = "fullid_05") %>%
  left_join(q46_1, by = "fullid_05") %>%
  left_join(q46_2, by = "fullid_05") %>%
  left_join(q49_1, by = "fullid_05") %>%
  left_join(q49_2, by = "fullid_05") %>%
  left_join(q52_1, by = "fullid_05") %>%
  left_join(q52_2, by = "fullid_05") %>%
  left_join(q55_1, by = "fullid_05") %>%
  left_join(q55_2, by = "fullid_05") %>%
  left_join(q58_1, by = "fullid_05") %>%
  left_join(q58_2, by = "fullid_05") %>%
  left_join(q61_1, by = "fullid_05") %>%
  left_join(q61_2, by = "fullid_05") %>%
  left_join(q64_1, by = "fullid_05") %>%
  left_join(q64_2, by = "fullid_05") %>%
  left_join(q67_1, by = "fullid_05") %>%
  left_join(q67_2, by = "fullid_05") %>%
  left_join(q69_1, by = "fullid_05") %>%
  left_join(q69_2, by = "fullid_05") %>%
  left_join(q70, by = "fullid_05")

rm(q2, q33_1, q35_2, q35_3, q40_1, q40_2, q43_1, q43_2, q46_1, q46_2, q49_1, q49_2, q52_1, q52_2, q55_1, q55_2, q58_1, q58_2, q61_1, q61_2,
   q64_1, q64_2, q67_1, q67_2, q69_1, q69_2, q70)

# fix the order of the fields to be tidy
wave_5 <- wave_5 %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, X2_05, X3_05, Q2005.1, Q2005.2, Q2005.3, 
         Q2005.3a, Q2005.4, Q2005.5, Q2005.6, Q2005.7.1, Q2005.7.2, Q2005.7.3, Q2005.7.4, Q2005.7.5, Q2005.8.1, Q2005.8.2, 
         Q2005.8.3, Q2005.8.4, Q2005.8.5, Q2005.8.6, Q2005.8.7, Q2005.8.8, Q2005.8.9, Q2005.8.10, Q2005.8.11, Q2005.8.12, Q2005.8.13, 
         Q2005.8.14, Q2005.8.15, Q2005.8.16, Q2005.8.17, Q2005.8.18, Q2005.8.19, Q2005.8.20, Q2005.8.21, Q2005.8.22, Q2005.9, 
         Q2005.10.1, Q2005.10.2, Q2005.10.3, Q2005.10.4, Q2005.10.5, Q2005.10.6, Q2005.10.7, Q2005.10.8, Q2005.10.9, Q2005.11, 
         Q2005.12, Q2005.13, Q2005.14, Q2005.15, Q2005.16, Q2005.17.1, Q2005.17.2, Q2005.17.3, Q2005.17.4, Q2005.17.5, Q2005.17.6, 
         Q2005.17.7, Q2005.17.8, Q2005.17.9, Q2005.17.10, Q2005.17.11, Q2005.17.12, Q2005.17.13, Q2005.18, Q2005.18a, Q2005.19, 
         Q2005.19a, Q2005.20, Q2005.20a, Q2005.21, Q2005.21a, Q2005.22, Q2005.22a, Q2005.23, Q2005.23a, Q2005.24, Q2005.24a, Q2005.25, 
         Q2005.25a, Q2005.26, Q2005.26a, Q2005.27, Q2005.27a, Q2005.28, Q2005.28a, Q2005.29, Q2005.29a, Q2005.30, Q2005.30a, Q2005.31, 
         Q2005.31a, Q2005.32, Q2005.33, Q2005.33a, Q2005.33b, Q2005.33c, Q2005.33d, Q2005.34, Q2005.35, Q2005.35a, Q2005.35b, Q2005.35c, 
         Q2005.35d, Q2005.35e, Q2005.36, Q2005.37, Q2005.37a, Q2005.37b.1, Q2005.37b.2, Q2005.37b.3, Q2005.37b.4, Q2005.37b.5, Q2005.37c, 
         Q2005.37d, Q2005.37e, Q2005.37f, Q2005.37g, Q2005.37h, Q2005.37i, Q2005.38, Q2005.39, Q2005.40, Q2005.40a, Q2005.40b, Q2005.40c, 
         Q2005.41, Q2005.42, Q2005.43, Q2005.43a, Q2005.43b, Q2005.43c, Q2005.43d, Q2005.44, Q2005.45, Q2005.46, Q2005.46a, Q2005.46b, 
         Q2005.46c, Q2005.46d, Q2005.47, Q2005.48, Q2005.49, Q2005.49a, Q2005.49b, Q2005.49c, Q2005.49d, Q2005.50, Q2005.51, Q2005.52,
         Q2005.52a, Q2005.52b, Q2005.52c, Q2005.52d, Q2005.53, Q2005.54, Q2005.55, Q2005.55a, Q2005.55b, Q2005.55c, Q2005.55d, 
         Q2005.56, Q2005.57, Q2005.58, Q2005.58a, Q2005.58b, Q2005.58c, Q2005.58d, Q2005.59, Q2005.60, Q2005.61, Q2005.61a, 
         Q2005.61b, Q2005.61c, Q2005.61d, Q2005.62, Q2005.63, Q2005.64, Q2005.64a, Q2005.64b, Q2005.64c, Q2005.64d, Q2005.65, Q2005.66, 
         Q2005.67, Q2005.67a, Q2005.67b, Q2005.68.1, Q2005.68.2, Q2005.68.3, Q2005.68.4, Q2005.68.5, Q2005.68.6, Q2005.68.7, Q2005.68.8, 
         Q2005.68.9, Q2005.68.10, Q2005.68.11, Q2005.68.12, Q2005.68.13, Q2005.68.14, Q2005.68.15, Q2005.68.16, Q2005.68.17, Q2005.68.18, 
         Q2005.68.19, Q2005.68.20, Q2005.69a, Q2005.69b, Q2005.69c, Q2005.69d, Q2005.69e, Q2005.69f, Q2005.69g, Q2005.69h, Q2005.70, 
         Q2005.71, Q2005.72, Q2005.73, Q2005.73a, Q2005.74, Q2005.75, Q2005.76, Q2005.77, Q2005.78, Q2005.79, Q2005.79a, Q2005.79b, 
         Q2005.80, Q2005.80a, Q2005.81, Q2005.81a, Q2005.81b, Q2005.82, Q2005.82a, Q2005.83, Q2005.83a, Q2005.84, Q2005.85, Q2005.85a, Q2005.86.1, 
         Q2005.86.2, Q2005.86.3, Q2005.86.4, Q2005.86.5, Q2005.86.6, Q2005.86.7, Q2005.86.8, Q2005.86.9, Q2005.87.1, Q2005.87.2, Q2005.87.3, Q2005.87.4, 
         Q2005.87.5, Q2005.88.1, Q2005.88.2, Q2005.88.3, Q2005.88.4, Q2005.88.5, Q2005.89.1, Q2005.89.2, Q2005.89.3, Q2005.89.4, Q2005.89.5, Q2005.90, 
         Q2005.91, Q2005.92, Q2005.92a, Q2005.92b, Q2005.93, Q2005.93a, Q2005.93b, Q2005.94, Q2005.94a, Q2005.94b, Q2005.95, Q2005.95a, Q2005.95b, 
         Q2995.96, Q2005.96a, Q2005.96b, Q2005.97, Q2005.97a, Q2005.97b, Q2005.98, Q2005.98a, Q2005.98b, Q2005.99, Q2005.99a, Q2005.99b, Q2005.100,
         Q2005.100a, Q2005.100b, Q2005.101, Q2005.101a, Q2005.102, Q2005.102a, Q2005.103, Q2005.103a, Q2005.104, Q2005.104a, Q2005.105, Q2005.106, 
         Q2005.107, Q2005.108.1, Q2005.108.2, Q2005.108.3, Q2005.108.4, Q2005.108.5, Q2005.108.6, Q2005.109.1, Q2005.109.2, Q2005.109.3, 
         Q2005.109.4, Q2005.109.5, Q2005.109.6, Q2005.109.7, Q2005.109.8, Q2005.109.9, Q2005.109.10, Q2005.109.11, Q2005.109.12, Q2005.109.13, 
         Q2005.110, Q2005.110a, Q2005.111, Q2005.111a, Q2005.112, Q2005.112a, Q2005.113, Q2005.113a, Q2005.114.1, Q2005.114.2, Q2005.114.3, 
         Q2005.114.4, Q2005.114.5, Q2005.114.6, Q2005.114.7, Q2005.114.8, Q2005.114.9, Q2005.114.10, Q2005.114.11, Q2005.114.12, Q2005.114.13, 
         Q2005.114.14, Q2005.114.15, Q2005.114.16, Q2005.114.17)

##########################################
#     COLLAPSE RESPONSES - WAVE 6
##########################################
# cleaning ########

# question 2
q2 <- byds %>%
  select(fullid_06, qy62.1, qy62.2, qy62.3, qy62.4, qy62.5, qy62.6, qy62.7, qy62.8) %>%
  mutate( qy62.1 = ifelse( qy62.1 == "Yes" , "Monday" , qy62.1 )) %>%
  mutate( qy62.2 = ifelse( qy62.2 == "Yes" , "Tuesday" , qy62.2 )) %>%
  mutate( qy62.3 = ifelse( qy62.3 == "Yes" , "Wednesday" , qy62.3 )) %>%
  mutate( qy62.4 = ifelse( qy62.4 == "Yes" , "Thursday" , qy62.4 )) %>%
  mutate( qy62.5 = ifelse( qy62.5 == "Yes" , "Friday" , qy62.5 )) %>%
  mutate( qy62.6 = ifelse( qy62.6 == "Yes" , "Saturday" , qy62.6 )) %>%
  mutate( qy62.7 = ifelse( qy62.7 == "Yes" , "Sunday" , qy62.7 )) %>%
  mutate( qy62.8 = ifelse( qy62.8 == "Yes" , "Don't go out in evenings" , qy62.8 )) 

q2$qy62 <- apply(q2, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q2;

q2 %>% 
  select(fullid_06, qy62) %>%
  mutate(qy62 = 
           str_replace(qy62, pattern = '^.*?,', replacement = ""),
         qy62 = str_trim(qy62, side='left')) %>%
  filter(fullid_06 != qy62) -> q2

# question 4
q4 <- byds %>%
  select(fullid_06, qy64.1, qy64.2, qy64.3) %>%
  mutate( qy64.1 = ifelse( qy64.1 == "Yes" , "full license" , qy64.1 )) %>%
  mutate( qy64.2 = ifelse( qy64.2 == "Yes" , "provisional license" , qy64.2 )) %>%
  mutate( qy64.3 = ifelse( qy64.3 == "Yes" , "no" , qy64.3 )) 

q4$qy64 <- apply(q4, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q4;

q4 %>% 
  select(fullid_06, qy64) %>%
  mutate(qy64 = 
           str_replace(qy64, pattern = '^.*?,', replacement = ""),
         qy64 = str_trim(qy64, side='left')) %>%
  filter(fullid_06 != qy64) -> q4

# question 45i
q45_i <- byds %>%
  select(fullid_06, qy645i.1, qy645i.2, qy645i.3) %>%
  mutate( qy645i.1 = ifelse( qy645i.1 == "Yes" , "yes but not in the past year" , qy645i.1 )) %>%
  mutate( qy645i.2 = ifelse( qy645i.2 == "Yes" , "yes during the last year" , qy645i.2 )) %>%
  mutate( qy645i.3 = ifelse( qy645i.3 == "Yes" , "no" , qy645i.3 )) 

q45_i$qy645i <- apply(q45_i, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q45_i;

q45_i %>% 
  select(fullid_06, qy645i) %>%
  mutate(qy645i = 
           str_replace(qy645i, pattern = '^.*?,', replacement = ""),
         qy645i = str_trim(qy645i, side='left')) %>%
  filter(fullid_06 != qy645i) -> q45_i

# question 45j
q45_j <- byds %>%
  select(fullid_06, qy645j.1, qy645j.2, qy645j.3) %>%
  mutate( qy645j.1 = ifelse( qy645j.1 == "Yes" , "yes but not in the past year" , qy645j.1 )) %>%
  mutate( qy645j.2 = ifelse( qy645j.2 == "Yes" , "yes during the last year" , qy645j.2 )) %>%
  mutate( qy645j.3 = ifelse( qy645j.3 == "Yes" , "no" , qy645j.3 )) 

q45_j$qy645j <- apply(q45_j, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q45_j;

q45_j %>% 
  select(fullid_06, qy645j) %>%
  mutate(qy645j = 
           str_replace(qy645j, pattern = '^.*?,', replacement = ""),
         qy645j = str_trim(qy645j, side='left')) %>%
  filter(fullid_06 != qy645j) -> q45_j

# question 72
q72 <- byds %>%
  select(fullid_06, qy672.1, qy672.2, qy672.3) %>%
  mutate( qy672.1 = ifelse( qy672.1 == "Yes" , "mother" , qy672.1 )) %>%
  mutate( qy672.2 = ifelse( qy672.2 == "Yes" , "father" , qy672.2 )) %>%
  mutate( qy672.3 = ifelse( qy672.3 == "Yes" , "neither parent" , qy672.3 )) %>%
  add_column(qy672.4 = NA) %>%
  mutate(qy672.4 = ifelse((qy672.1 == "mother") & (qy672.2 == "father"), "both parents",  qy672.4))

q72$qy672 <- apply(q72, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q72;

q72 %>% 
  select(fullid_06, qy672) %>%
  mutate(qy672 = 
           str_replace(qy672, pattern = '^.*?,', replacement = ""),
         qy672 = str_trim(qy672, side='left')) %>%
  filter(fullid_06 != qy672) -> q72

q72 <- q72 %>%
  mutate(qy672 = ifelse(qy672 == "mother, father, both parents", "both parents", qy672)) %>%
  mutate(qy672 = ifelse(qy672 == "mother, father, neither parent, both parents", "both parents", qy672))

# question 86
q86 <- byds %>%
  select(fullid_06, qy686.1, qy686.2, qy686.3) %>%
  mutate( qy686.1 = ifelse( qy686.1 == "Yes" , "have not ever" , qy686.1 )) %>%
  mutate( qy686.2 = ifelse( qy686.2 == "Yes" , "yes once" , qy686.2 )) %>%
  mutate( qy686.3 = ifelse( qy686.3 == "Yes" , "yes more than once" , qy686.3 )) 

q86$qy686 <- apply(q86, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q86;

q86 %>% 
  select(fullid_06, qy686) %>%
  mutate(qy686 = 
           str_replace(qy686, pattern = '^.*?,', replacement = ""),
         qy686 = str_trim(qy686, side='left')) %>%
  filter(fullid_06 != qy686) -> q86

# question 92
q92 <- byds %>%
  select(fullid_06, qy692.1, qy692.2, qy692.3, qy692.4, qy692.5, qy692.6, qy692.7, qy692.8, qy692.9, qy692.10 ) %>%
  mutate( qy692.1 = ifelse( qy692.1 == "Yes" , "someone in family" , qy692.1 )) %>%
  mutate( qy692.2 = ifelse( qy692.2 == "Yes" , "partner" , qy692.2 )) %>%
  mutate( qy692.3 = ifelse( qy692.3 == "Yes" , "friend" , qy692.3 )) %>%
  mutate( qy692.4 = ifelse( qy692.4 == "Yes" , "teacher" , qy692.4 )) %>%
  mutate( qy692.5 = ifelse( qy692.5 == "Yes" , "GP" , qy692.5 )) %>%
  mutate( qy692.6 = ifelse( qy692.6 == "Yes" , "social worker" , qy692.6 )) %>%
  mutate( qy692.7 = ifelse( qy692.7 == "Yes" , "therapist" , qy692.7 )) %>%
  mutate( qy692.8 = ifelse( qy692.8 == "Yes" , "telephone help line" , qy692.8 )) %>%
  mutate( qy692.9 = ifelse( qy692.9 == "Yes" , "drop in advice centre" , qy692.9 )) %>%
  mutate( qy692.10 = ifelse( qy692.10 == "Yes" , "other source" , qy692.10 ))

q92$qy692 <- apply(q92, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "3" & !is.na(x)], collapse = ", ")); q92;

q92 %>% 
  select(fullid_06, qy692) %>%
  mutate(qy692 = 
           str_replace(qy692, pattern = '^.*?,', replacement = ""),
         qy692 = str_trim(qy692, side='left')) %>%
  filter(fullid_06 != qy692) -> q92

# question 93
q93 <- byds %>%
  select(fullid_06, qy693.1, qy693.2, qy693.3, qy693.4, qy693.5, qy693.6, qy693.7, qy693.8, 
         qy693.9, qy693.10, qy693.11, qy693.12, qy693.13) %>%
  mutate( qy693.1 = ifelse( qy693.1 == "Yes" , "mother" , qy693.1 )) %>%
  mutate( qy693.2 = ifelse( qy693.2 == "Yes" , "father" , qy693.2 )) %>%
  mutate( qy693.3 = ifelse( qy693.3 == "Yes" , "brother or sister" , qy693.3 )) %>%
  mutate( qy693.4 = ifelse( qy693.4 == "Yes" , "other relative" , qy693.4 )) %>%
  mutate( qy693.5 = ifelse( qy693.5 == "Yes" , "partner" , qy693.5 )) %>%
  mutate( qy693.6 = ifelse( qy693.6 == "Yes" , "friend" , qy693.6 )) %>%
  mutate( qy693.7 = ifelse( qy693.7 == "Yes" , "GP" , qy693.7 )) %>%
  mutate( qy693.8 = ifelse( qy693.8 == "Yes" , "teacher" , qy693.8 )) %>%
  mutate( qy693.9 = ifelse( qy693.9 == "Yes" , "social worker" , qy693.9 )) %>%
  mutate( qy693.10 = ifelse( qy693.10 == "Yes" , "therapist" , qy693.10 )) %>%
  mutate( qy693.11 = ifelse( qy693.11 == "Yes" , "telephone help line" , qy693.11 )) %>%
  mutate( qy693.12 = ifelse( qy693.12 == "Yes" , "drop in advice centre" , qy693.12 )) %>%
  mutate( qy693.13 = ifelse( qy693.13 == "Yes" , "other source" , qy693.13 )) 

q93$qy693 <- apply(q93, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "3" & !is.na(x)], collapse = ", ")); q93;

q93 %>% 
  select(fullid_06, qy693) %>%
  mutate(qy693 = 
           str_replace(qy693, pattern = '^.*?,', replacement = ""),
         qy693 = str_trim(qy693, side='left')) %>%
  filter(fullid_06 != qy693) %>%
  filter((qy693 != "1") & (qy693 != "3")) %>%
  mutate(qy693 = 
           str_replace(qy693, pattern = '3', replacement = "")) %>%
  mutate(qy693 = 
           str_replace(qy693, pattern = '1', replacement = "")) -> q93

# question 96
q96 <- byds %>%
  select(fullid_06, qy696.1, qy696.2, qy696.3, qy696.4, qy696.5, qy696.6, qy696.7, qy696.8, qy696.9, qy696.10) %>%
  mutate( qy696.1 = ifelse( qy696.1 == "Yes" , "someone in family" , qy696.1 )) %>%
  mutate( qy696.2 = ifelse( qy696.2 == "Yes" , "partner" , qy696.2 )) %>%
  mutate( qy696.3 = ifelse( qy696.3 == "Yes" , "friend" , qy696.3 )) %>%
  mutate( qy696.4 = ifelse( qy696.4 == "Yes" , "teacher" , qy696.4 )) %>%
  mutate( qy696.5 = ifelse( qy696.5 == "Yes" , "GP" , qy696.5 )) %>%
  mutate( qy696.6 = ifelse( qy696.6 == "Yes" , "social worker" , qy696.6 )) %>%
  mutate( qy696.7 = ifelse( qy696.7 == "Yes" , "therapist" , qy696.7 )) %>%
  mutate( qy696.8 = ifelse( qy696.8 == "Yes" , "telephone help line" , qy696.8 )) %>%
  mutate( qy696.9 = ifelse( qy696.9 == "Yes" , "drop in advice centre" , qy696.9 )) %>%
  mutate( qy696.10 = ifelse( qy696.10 == "Yes" , "other source" , qy696.10 )) 

q96$qy696 <- apply(q96, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "3" & !is.na(x)], collapse = ", ")); q96;

q96 %>% 
  select(fullid_06, qy696) %>%
  mutate(qy696 = 
           str_replace(qy696, pattern = '^.*?,', replacement = ""),
         qy696 = str_trim(qy696, side='left')) %>%
  filter(fullid_06 != qy696) -> q96

# question 99
q99 <- byds %>%
  select(fullid_06, qy699.1, qy699.2, qy699.3) %>%
  mutate( qy699.1 = ifelse( qy699.1 == "Yes" , "have not ever" , qy699.1 )) %>%
  mutate( qy699.2 = ifelse( qy699.2 == "Yes" , "yes in the last month" , qy699.2 )) %>%
  mutate( qy699.3 = ifelse( qy699.3 == "Yes" , "yes more than a month but less than a year" , qy699.3 )) 

q99$qy699 <- apply(q99, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q99;

q99 %>% 
  select(fullid_06, qy699) %>%
  mutate(qy699 = 
           str_replace(qy699, pattern = '^.*?,', replacement = ""),
         qy699 = str_trim(qy699, side='left')) %>%
  filter(fullid_06 != qy699) -> q99

# question 100
q100 <- byds %>%
  select(fullid_06, qy6100.1, qy6100.2, qy6100.3, qy6100.4, qy6100.5, qy6100.6, qy6100.7, qy6100.8, qy6100.9, qy6100.10) %>%
  mutate( qy6100.1 = ifelse( qy6100.1 == "Yes" , "someone in family" , qy6100.1 )) %>%
  mutate( qy6100.2 = ifelse( qy6100.2 == "Yes" , "partner" , qy6100.2 )) %>%
  mutate( qy6100.3 = ifelse( qy6100.3 == "Yes" , "friend" , qy6100.3 )) %>%
  mutate( qy6100.4 = ifelse( qy6100.4 == "Yes" , "teacher" , qy6100.4 )) %>%
  mutate( qy6100.5 = ifelse( qy6100.5 == "Yes" , "GP" , qy6100.5 )) %>%
  mutate( qy6100.6 = ifelse( qy6100.6 == "Yes" , "social worker" , qy6100.6 )) %>%
  mutate( qy6100.7 = ifelse( qy6100.7 == "Yes" , "therapist" , qy6100.7 )) %>%
  mutate( qy6100.8 = ifelse( qy6100.8 == "Yes" , "telephone help line" , qy6100.8 )) %>%
  mutate( qy6100.9 = ifelse( qy6100.9 == "Yes" , "drop in advice centre" , qy6100.9 )) %>%
  mutate( qy6100.10 = ifelse( qy6100.10 == "Yes" , "other source" , qy6100.10 )) 

q100$qy6100 <- apply(q100, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "3" & !is.na(x)], collapse = ", ")); q100;

q100 %>% 
  select(fullid_06, qy6100) %>%
  mutate(qy6100 = 
           str_replace(qy6100, pattern = '^.*?,', replacement = ""),
         qy6100 = str_trim(qy6100, side='left')) %>%
  filter(fullid_06 != qy6100) -> q100

# question 119_1
q119_1 <- byds %>%
  select(fullid_06, qy6119.1, qy6119.2, qy6119.3, qy6119.4, qy6119.5, qy6119.6, qy6119.7) %>%
  mutate( qy6119.1 = ifelse( qy6119.1 == "Yes" , "at school" , qy6119.1 )) %>%
  mutate( qy6119.2 = ifelse( qy6119.2 == "Yes" , "on shared pc in family room" , qy6119.2 )) %>%
  mutate( qy6119.3 = ifelse( qy6119.3 == "Yes" , "on own pc" , qy6119.3 )) %>%
  mutate( qy6119.4 = ifelse( qy6119.4 == "Yes" , "via game console" , qy6119.4 )) %>%
  mutate( qy6119.5 = ifelse( qy6119.5 == "Yes" , "internet tv or cable in family room" , qy6119.5 )) %>%
  mutate( qy6119.6 = ifelse( qy6119.6 == "Yes" , "via mobile phone or other mobile device (e.g., iPod Touch)" , qy6119.6 )) %>%
  mutate( qy6119.7 = ifelse( qy6119.7 == "Yes" , "other source" , qy6119.7 )) 

q119_1$qy6119.1 <- apply(q119_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q119_1;

q119_1 %>% 
  select(fullid_06, qy6119.1) %>%
  mutate(qy6119.1 = 
           str_replace(qy6119.1, pattern = '^.*?,', replacement = ""),
         qy6119.1 = str_trim(qy6119.1, side='left')) %>%
  filter(fullid_06 != qy6119.1) -> q119_1

# join it back together ################

# pull down the clean variables
wave_6 <- byds %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, X2CODE, X3IDNUMBER, qy61, qy63, qy65, qy66, qy67, 
         qy67a, qy68.1, qy68.1.1, qy68.2, qy682.1, qy68.3, qy68.3.1, qy68.4, qy68.4.1, qy68.5, qy68.5.1, qy68.6, qy68.6.1, qy68.7, qy69, 
         qy610, qy611, qy612.1, qy612.2, qy612.3, qy612.4, qy612.5, qy612.6, qy612.7, qy612.8, qy612.9, qy612.10, qy613.1, 
         qy613.2, qy613.3, qy613.4, qy613.5, qy613.6, qy613.7, qy613.8, qy613.9, qy613.10, qy614, qy615, qy616, qy617, qy618.1, 
         qy618.2, qy618.3, qy618.4, qy618.5, qy618.6, qy618.7, qy618.8, qy618.9, qy618.10, qy619, qy20, qy21, qy622, qy623.1, 
         qy623.2, qy623.3, qy623.4, qy623.5, qy623.6, qy623.7, qy623.8, qy623.9, qy623.10, qy624, qy625, qy626, qy626.1, qy627, 
         qy628, qy629, qy630, qy631, qy632, qy633, qy634, qy635, qy636, qy636.1, qy637, qy638, qy639.1, qy639.2, qy639.3, 
         qy639.4, qy639.5, qy639.6, qy639.7, qy639.8, qy639.9, qy639.10, qy639.11, qy639.12, qy639.13, qy639.14, qy639.15, qy639.16, 
         qy639.17, qy639.18, qy639.19, qy639.20, qy639.21, qy639.22, qy639.23, qy639.24, qy639.25, qy639.26, qy639.27, qy639.28, qy639.29, 
         qy639.30, qy629.31, qy639.32, qy639.33, qy639.34, qy639.35, qy639.36, qy640.1, qy640.1.1, qy640.2, qy640.2.1, qy640.3, 
         qy640.3.1, qy640.4, qy640.4.1, qyt40.5, qy640.5.1, qy640.6, qy640.6.1, qy640.7, qy640.7.1, qy640.8, qy640.8.1, qy640.9, 
         qy640.9.1, qy640.10, qy640.10.1, qy640.11, qy640.11.1, qy640.12, qy640.12.1, qy640.13, qy640.13.1, qy640.14, qy640.14.1, qyt40.15, 
         qy640.15.1, qy640.16, qy640.16.1, qy641, qy642.1, qy642.2, qy642.3, qy642.4, qy642.5, qy642.6, qy642.7, qy642.8, qy642.9, 
         qy642.10, qy642.11, qy642.12, qy642.13, qy643, qy644, qy644a, qy644b, qy644c, qy645a, qy645b, qy645c, qy645d, qu645e, qy645f, 
         qy645g, qy645h, qy646, qy647, qy648, qy648a, qy648b, qy648c, qy649, 
         qy650, qy651, qy51a, qy651b, qy651c, qy652, qy653, qy654, qy654a, qy654b, qy654c, qy655, qy656, qy657, qy657a, qy657b, qy657c, 
         qy658, qqy659, qy660, qy660a, qy660b, qy660c, qy661, qy662, qy663, qy663a, qy663b, qy663c, qy664, qy665, qy666, qy666a, qy666b, 
         qy667, qy668, qy669, qy669a, qy669b, qy669c, qy670.1, qy670.2, qy670.3, qy670.4, qy670.5, qy670.6, qy670.7, qy670.8, qy670.9, 
         qy670.10, qy670.11, qy670.12, qy670.13, qy670.14, qy670.15, qy670.16, qy670.17, qy670.18, qy670.19, qy670.20, qy670.21, qy670.22, qy680.23, 
         qy670.24, qy671, qy672.1.1, qy672.2.1, qy673a, qy673b, qy674, qy674a, qy675, qy676, qy677, qy678a1, 
         qy678a.1.1, qy678a2, qy678a2.1, qy678a3, qy678a3.1, qy678b1, qy678b1.1, qy678b2, qy678b2.1, qy678b3, qy678b3.1, qy678b4, 
         qy678b4.1, qy679, qy679a, qy679b, qy680, qy680a, qy681, qy681a, qy681b, qy682, qy682a, qy683, qy683a, qy684, qy685, qy685a, 
         qy687, qy688.1, qy688.2, qy688.3, qy688.4, qy689.1, qy689.2, qy689.3, qy690.1, qy690.2, qy690.3, 
         qy690.4, qy690.5, qy690.6, qy690.7, qy690.8, qy691, qy694 , qy695, qy697, qy698, qy6101, qy6102, qy6103, qy6104, qy6105, 
         qy6105.1, qy6106, qy6106.1, qy6107, qy6107.1, qy6108, qy6108.1, qy6109, qy6109.1, qy6110, qy6110.1, qy6111, qy6111.1, qy6112, 
         qy6112.1, qy6113, qy6113.1, qy6114, qy6114.1, qy6115, qy6115.1, qy6116, qy6116.1, qy6117, qy6118, qy6119, qy6120.1, qy6120.2, qy6120.3,
         qy6120.4, qy6120.5, qy6120.6, qy6121.1, qy6121.2, qy6121.3, qy6121.4, qy6121.5, qy6121.6, qy6121.7, qy6123.1, qy6123.2, qy6123.3, 
         qy6123.4, qy6123.5, qy6123.6, qy6123.7, qy6123.8, qy6122.1, qy6122.2, qy6122.3, qy6122.4, qy6122.5, qy6122.6, qy6124.1, 
         qy6124.2, qy6124.3, qy6124.4, qy6124.5, qy6124.6, qy6124.7, qy6124.8, qy6125.1, qy6125.2, qy6125.3, qy6125.4, qy6125.5, qy6125.6, 
         qy6126.1, qy6126.2, qy6126.3, qy6126.4, qy6126.5, qy6126.6, qy6126.7, qy6126.8, qy6126.9, NEW, old, additionaln, x2_07, x3_07)


# join in the new variables
wave_6 <- wave_6 %>%
  left_join(q2, by = "fullid_06") %>%
  left_join(q4, by = "fullid_06") %>%
  left_join(q45_i, by = "fullid_06") %>%
  left_join(q45_j, by = "fullid_06") %>%
  left_join(q72, by = "fullid_06") %>%
  left_join(q86, by = "fullid_06") %>%
  left_join(q92, by = "fullid_06") %>%
  left_join(q93, by = "fullid_06") %>%
  left_join(q96, by = "fullid_06") %>%
  left_join(q99, by = "fullid_06") %>%
  left_join(q100, by = "fullid_06") %>%
  left_join(q119_1, by = "fullid_06") 

rm(q2, q4, q45_i, q45_j, q72, q86, q92, q93, q96, q99, q100, q119_1)

# tidy up the order of the variables
wave_6 <- wave_6 %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, X2CODE, X3IDNUMBER, qy61, qy62, qy63, qy64, 
         qy65, qy66, qy67, qy67a, qy68.1, qy68.1.1, qy68.2, qy682.1, qy68.3, qy68.3.1, qy68.4, qy68.4.1, qy68.5, qy68.5.1, qy68.6, 
         qy68.6.1, qy68.7, qy69, qy610, qy611, qy612.1, qy612.2, qy612.3, qy612.4, qy612.5, qy612.6, qy612.7, qy612.8, 
         qy612.9, qy612.10, qy613.1, qy613.2, qy613.3, qy613.4, qy613.5, qy613.6, qy613.7, qy613.8, qy613.9, qy613.10, qy614, 
         qy615, qy616, qy617, qy618.1, qy618.2, qy618.3, qy618.4, qy618.5, qy618.6, qy618.7, qy618.8, qy618.9, qy618.10, 
         qy619, qy20, qy21, qy622, qy623.1, qy623.2, qy623.3, qy623.4, qy623.5, qy623.6, qy623.7, qy623.8, qy623.9, qy623.10, 
         qy624, qy625, qy626, qy626.1, qy627, qy628, qy629, qy630, qy631, qy632, qy633, qy634, qy635, qy636, qy636.1, qy637, 
         qy638, qy639.1, qy639.2, qy639.3, qy639.4, qy639.5, qy639.6, qy639.7, qy639.8, qy639.9, qy639.10, qy639.11, qy639.12, 
         qy639.13, qy639.14, qy639.15, qy639.16, qy639.17, qy639.18, qy639.19, qy639.20, qy639.21, qy639.22, qy639.23, qy639.24, qy639.25, 
         qy639.26, qy639.27, qy639.28, qy639.29, qy639.30, qy629.31, qy639.32, qy639.33, qy639.34, qy639.35, qy639.36, qy640.1, qy640.1.1, 
         qy640.2, qy640.2.1, qy640.3, qy640.3.1, qy640.4, qy640.4.1, qyt40.5, qy640.5.1, qy640.6, qy640.6.1, qy640.7, qy640.7.1,
         qy640.8, qy640.8.1, qy640.9, qy640.9.1, qy640.10, qy640.10.1, qy640.11, qy640.11.1, qy640.12, qy640.12.1, qy640.13, qy640.13.1, 
         qy640.14, qy640.14.1, qyt40.15, qy640.15.1, qy640.16, qy640.16.1, qy641, qy642.1, qy642.2, qy642.3, qy642.4, qy642.5, qy642.6, 
         qy642.7, qy642.8, qy642.9, qy642.10, qy642.11, qy642.12, qy642.13, qy643, qy644, qy644a, qy644b, qy644c, qy645a, qy645b, qy645c, 
         qy645d, qu645e, qy645f, qy645g, qy645h, qy645i, qy645j, qy646, qy647, qy648, qy648a, 
         qy648b, qy648c, qy649, qy650, qy651, qy51a, qy651b, qy651c, qy652, qy653, qy654, qy654a, qy654b, qy654c, qy655, qy656, qy657, 
         qy657a, qy657b, qy657c, qy658, qqy659, qy660, qy660a, qy660b, qy660c, qy661, qy662, qy663, qy663a, qy663b, qy663c, qy664, 
         qy665, qy666, qy666a, qy666b, qy667, qy668, qy669, qy669a, qy669b, qy669c, qy670.1, qy670.2, qy670.3, qy670.4, qy670.5, 
         qy670.6, qy670.7, qy670.8, qy670.9, qy670.10, qy670.11, qy670.12, qy670.13, qy670.14, qy670.15, qy670.16, qy670.17, qy670.18, 
         qy670.19, qy670.20, qy670.21, qy670.22, qy680.23, qy670.24, qy671, qy672, qy672.1.1, qy672.2.1, qy673a, 
         qy673b, qy674, qy674a, qy675, qy676, qy677, qy678a1, qy678a.1.1, qy678a2, qy678a2.1, qy678a3, qy678a3.1, qy678b1, qy678b1.1, 
         qy678b2, qy678b2.1, qy678b3, qy678b3.1, qy678b4, qy678b4.1, qy679, qy679a, qy679b, qy680, qy680a, qy681, qy681a, qy681b, 
         qy682, qy682a, qy683, qy683a, qy684, qy685, qy685a, qy686, qy687, qy688.1, qy688.2, qy688.3, qy688.4, qy689.1, qy689.2, 
         qy689.3, qy690.1, qy690.2, qy690.3, qy690.4, qy690.5, qy690.6, qy690.7, qy690.8, qy691, qy692, qy693, qy694 , qy695,
         qy696, qy697, qy698, qy699, qy6100, qy6101, qy6102, qy6103, qy6104, qy6105, qy6105.1, qy6106, qy6106.1, qy6107, qy6107.1, 
         qy6108, qy6108.1, qy6109, qy6109.1, qy6110, qy6110.1, qy6111, qy6111.1, qy6112, qy6112.1, qy6113, qy6113.1, qy6114, qy6114.1, 
         qy6115, qy6115.1, qy6116, qy6116.1, qy6117, qy6118, qy6119, qy6119.1, qy6120.1, qy6120.2, qy6120.3, qy6120.4, qy6120.5, 
         qy6120.6, qy6121.1, qy6121.2, qy6121.3, qy6121.4, qy6121.5, qy6121.6, qy6121.7, qy6123.1, qy6123.2, qy6123.3, qy6123.4, qy6123.5, 
         qy6123.6, qy6123.7, qy6123.8, qy6122.1, qy6122.2, qy6122.3, qy6122.4, qy6122.5, qy6122.6, qy6124.1, qy6124.2, qy6124.3, qy6124.4, 
         qy6124.5, qy6124.6, qy6124.7, qy6124.8, qy6125.1, qy6125.2, qy6125.3, qy6125.4, qy6125.5, qy6125.6, qy6126.1, qy6126.2, qy6126.3, 
         qy6126.4, qy6126.5, qy6126.6, qy6126.7, qy6126.8, qy6126.9, NEW, old, additionaln, x2_07, x3_07)

##########################################
#     COLLAPSE RESPONSES - WAVE 7
##########################################
# cleaning #############
# question 31
q31 <- byds %>%
  select(fullid_07, qy731_1, qy731_2, qy731_3, qy731_4, qy731_5, qy731_6, qy731_7, qy731_8)

q31$qy731 <- apply(q31, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & !is.na(x)], collapse = ", ")); q31;

q31 %>% 
  select(fullid_07, qy731) %>%
  mutate(qy731 = 
           str_replace(qy731, pattern = '^.*?,', replacement = ""),
         qy731 = str_trim(qy731, side='left')) %>%
  filter(fullid_07 != qy731) -> q31

# question 39_1
q39_1 <- byds %>%
  select(fullid_07, qy739_1, qy739_1b, qy739_1bb, qy739_1bbb) %>%
  mutate(qy739_1 = ifelse(qy739_1 == ".                   ", NA, qy739_1)) %>%
  mutate(qy739_1b = ifelse(qy739_1b == ".                   ", NA, qy739_1b)) %>%
  mutate(qy739_1bb = ifelse(qy739_1bb == ".                   ", NA, qy739_1bb)) %>%
  mutate(qy739_1bbb = ifelse(qy739_1bbb == ".                   ", NA, qy739_1bbb)) %>%
  
  mutate(qy739_1 = ifelse(qy739_1 == "                    ", NA, qy739_1)) %>%
  mutate(qy739_1b = ifelse(qy739_1b == "                    ", NA, qy739_1b)) %>%
  mutate(qy739_1bb = ifelse(qy739_1bb == "                    ", NA, qy739_1bb)) %>%
  mutate(qy739_1bb = ifelse(qy739_1bb == "                                          ", NA, qy739_1bb)) %>%
  mutate(qy739_1bbb = ifelse(qy739_1bbb == "                    ", NA, qy739_1bbb)) %>%
  mutate(qy739_1 = ifelse(qy739_1 == "                                                                             ",
                          NA, qy739_1)) %>%
  mutate(qy739_1b = ifelse(qy739_1b == "                                           ", NA, qy739_1b)) %>%
  mutate(qy739_1b = ifelse(qy739_1b == "                                          ", NA, qy739_1b))

q39_1$qy739_1 <- apply(q39_1, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "." 
                                                     & x != "3" & !is.na(x)], collapse = ", ")); q39_1;
q39_1 %>% 
  select(fullid_07, qy739_1) %>%
  mutate(qy739_1 = 
           str_replace(qy739_1, pattern = '^.*?,', replacement = ""),
         qy739_1 = str_trim(qy739_1, side='left')) %>%
  mutate(qy739_1 = ifelse(qy739_1 == ",                                           ", NA, qy739_1)) %>%
  mutate(qy739_1 = ifelse(qy739_1 == ".                                                                            ", NA, qy739_1)) %>%
  filter(fullid_07 != qy739_1) -> q39_1

trimws(q39_1$qy739_1)
str_squish(q39_1$qy739_1)

# question 67_3
q67_3 <- byds %>%
  select(fullid_07, qy767b_1, qy767b_1_b) %>%
  mutate(qy767b_1 = ifelse(qy767b_1 == ".      ", NA, qy767b_1)) %>%
  mutate(qy767b_1 = ifelse(qy767b_1 == "       ", NA, qy767b_1)) %>%
  mutate(qy767b_1 = ifelse(qy767b_1 == "        ", NA, qy767b_1)) %>%
  mutate(qy767b_1_b = ifelse(qy767b_1_b == "             ", NA, qy767b_1_b))

q67_3$qy767b_1 <- apply(q67_3, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q67_3;

q67_3 %>% 
  select(fullid_07, qy767b_1) %>%
  mutate(qy767b_1 = 
           str_replace(qy767b_1, pattern = '^.*?,', replacement = ""),
         qy767b_1 = str_trim(qy767b_1, side='left')) %>%
  filter(fullid_07 != qy767b_1) -> q67_3

# question q67_19
q67_19 <- byds %>%
  select(fullid_07, qy767_other, qy767j_otherdrinks) %>%
  mutate(qy767_other = ifelse(qy767_other == ".                   ", NA, qy767_other)) %>%
  mutate(qy767_other = ifelse(qy767_other == "                    ", NA, qy767_other)) %>%
  mutate(qy767j_otherdrinks = ifelse(qy767j_otherdrinks == ".                                                                                 ", 
                                     NA, qy767j_otherdrinks)) %>%
  mutate(qy767j_otherdrinks = ifelse(qy767j_otherdrinks == "                                                                                                                                                                                  ", 
                                     NA, qy767j_otherdrinks)) %>%
  mutate(qy767j_otherdrinks = ifelse(qy767j_otherdrinks == ".                                                                                                                                                                                 ", 
                                     NA, qy767j_otherdrinks)) 

q67_19$qy767k <- apply(q67_19, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q67_19;

q67_19 %>% 
  select(fullid_07, qy767k) %>%
  mutate(qy767k = 
           str_replace(qy767k, pattern = '^.*?,', replacement = ""),
         qy767k = str_trim(qy767k, side='left')) %>%
  filter(fullid_07 != qy767k) -> q67_19


# question 84
q84 <- byds %>%
  select(fullid_07, qy784_1, qy784_2, qy784_3, qy784_4, qy784_5, qy784_6, qy784_7, qy784_8) %>%
  mutate(qy784_8 = ifelse(qy784_8 == ".                                                       ", NA, qy784_8)) %>%
  mutate(qy784_8 = ifelse(qy784_8 == "                                                        ", NA, qy784_8)) %>%
  mutate(qy784_8 = ifelse(qy784_8 == "-                                                       ", NA, qy784_8)) %>%
  mutate(qy784_8 = ifelse((qy784_8 == "n/a                                                     ") | 
                            (qy784_8 == "na                                                      "), NA, qy784_8)) 

q84$qy784 <- apply(q84, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & !is.na(x)], collapse = ", ")); q84;

q84 %>% 
  select(fullid_07, qy784) %>%
  mutate(qy784 = 
           str_replace(qy784, pattern = '^.*?,', replacement = ""),
         qy784 = str_trim(qy784, side='left')) %>%
  filter(fullid_07 != qy784) -> q84


# question 85
q85 <- byds %>%
  select(fullid_07, qy785, qy785_other, qy785a, qy785b, qy785c, qy785d) %>%
  mutate(qy785_other = ifelse(qy785_other == "                                                                      ", NA, qy785_other)) %>%
  mutate(qy785_other = ifelse(qy785_other == ".                                                                     ", NA, qy785_other)) %>%
  mutate(qy785_other = ifelse(qy785_other == "-                                                                     ", NA, qy785_other)) %>%
  mutate(qy785_other = ifelse(qy785_other == "                                                                      ", NA, qy785_other)) %>%
  mutate(
    qy785a = as.character(qy785a), 
    qy785b = as.character(qy785b), 
    qy785c = as.character(qy785c),
    qy785d = as.character(qy785d),
  )


q85$qy785 <- apply(q85, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & !is.na(x)], collapse = ", ")); q85;

q85 %>% 
  select(fullid_07, qy785) %>%
  mutate(qy785 = 
           str_replace(qy785, pattern = '^.*?,', replacement = ""),
         qy785 = str_trim(qy785, side='left')) %>%
  filter(fullid_07 != qy785) -> q85

# question 95
q95 <- byds %>%
  select(fullid_07, qy795_1, qy795_2, qy795_3, qy795_4, qy795_5, qy795_6, qy795_7, qy795_8) %>%
  mutate(qy795_8 = ifelse(qy795_8 == ".         ", NA, qy795_8)) %>%
  mutate(qy795_8 = ifelse(qy795_8 == "          ", NA, qy795_8))

q95$qy795 <- apply(q95, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1"  & x != "0" & !is.na(x)], collapse = ", ")); q95;

q95 %>% 
  select(fullid_07, qy795) %>%
  mutate(qy795 = 
           str_replace(qy795, pattern = '^.*?,', replacement = ""),
         qy795 = str_trim(qy795, side='left')) %>%
  filter(fullid_07 != qy795) -> q95

# question 96
q96 <- byds %>%
  select(fullid_07, qy796, qy796a, qy796b, qy796c, qy796d, qy796_other, qy796_1) %>%
  mutate(qy796_other = ifelse(qy796_other == ".                ", NA, qy796_other)) %>%
  mutate(qy796_other = ifelse(qy796_other == ".               ", NA, qy796_other)) %>%
  mutate(qy796_other = ifelse(qy796_other == "                ", NA, qy796_other)) %>%
  mutate(qy796_1 = ifelse(qy796_1 == "                 ", NA, qy796_1)) %>%
  mutate(
    qy796a = as.character(qy796a), 
    qy796b = as.character(qy796b), 
    qy796c = as.character(qy796c),
    qy796d = as.character(qy796d),
  )

q96$qy796 <- apply(q96, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & !is.na(x)], collapse = ", ")); q96;

q96 %>% 
  select(fullid_07, qy796) %>%
  mutate(qy796 = 
           str_replace(qy796, pattern = '^.*?,', replacement = ""),
         qy796 = str_trim(qy796, side='left')) %>%
  filter(fullid_07 != qy796) -> q96

# question 105
q105 <- byds %>%
  select(fullid_07, qy7105_1, qy7105_2, qy7105_3, qy7105_4, qy7105_5, qy7105_6, qy7105_7, qy7105_8) %>%
  mutate(qy7105_8 = ifelse(qy7105_8 == "            ", NA, qy7105_8))

q105$qy7105 <- apply(q105, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0"& !is.na(x)], collapse = ", ")); q105;

q105 %>% 
  select(fullid_07, qy7105) %>%
  mutate(qy7105 = 
           str_replace(qy7105, pattern = '^.*?,', replacement = ""),
         qy7105 = str_trim(qy7105, side='left')) %>%
  filter(fullid_07 != qy7105) -> q105

# question 106
q106 <- byds %>%
  select(fullid_07, qy7106, qy7106a, qy7106b, qy7106c, qy7106_other, qy7106_1, qy7106d) %>%
  mutate(qy7106_other = ifelse(qy7106_other == ".        ", NA, qy7106_other)) %>%
  mutate(qy7106_other = ifelse(qy7106_other == "         ", NA, qy7106_other)) %>%
  mutate(qy7106_1 = ifelse(qy7106_1 == ".                         ", NA, qy7106_1)) %>%
  mutate(qy7106_1 = ifelse(qy7106_1 == "                          ", NA, qy7106_1)) %>%
  mutate( qy7106a = ifelse( qy7106a == "Yes" , "friends" , qy7106a )) %>%
  mutate( qy7106b = ifelse( qy7106b == "Yes" , "family" , qy7106b )) %>% 
  mutate( qy7106c = ifelse( qy7106c == "Yes" , "a dealer" , qy7106c ))

q106$qy7106 <- apply(q106, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "No" & x != "1" & !is.na(x)], collapse = ", ")); q106;

q106 %>% 
  select(fullid_07, qy7106) %>%
  mutate(qy7106 = 
           str_replace(qy7106, pattern = '^.*?,', replacement = ""),
         qy7106 = str_trim(qy7106, side='left')) %>%
  filter(fullid_07 != qy7106) -> q106

# question 115
q115 <- byds %>%
  select(fullid_07, qy7115_1, qy7115_2, qy7115_3, qy7115_4, qy7115_5, qy7115_6, qy7115_7, qy7115_8) %>%
  mutate(qy7115_8 = ifelse(qy7115_8 == ".                              ", NA, qy7115_8)) %>%
  mutate(qy7115_8 = ifelse(qy7115_8 == "                               ", NA, qy7115_8))

q115$qy7115 <- apply(q115, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & !is.na(x)], collapse = ", ")); q115;

q115 %>% 
  select(fullid_07, qy7115) %>%
  mutate(qy7115 = 
           str_replace(qy7115, pattern = '^.*?,', replacement = ""),
         qy7115 = str_trim(qy7115, side='left')) %>%
  filter(fullid_07 != qy7115) -> q115

# question 116
q116 <- byds %>%
  select(fullid_07, qy7116, qy7116a, qy7116b, qy7116c, qy7116d, qy7116_other, qy7116_1) %>%
  mutate(qy7116_other = ifelse(qy7116_other == ".         ", NA, qy7116_other)) %>%
  mutate(qy7116_other = ifelse(qy7116_other == "          ", NA, qy7116_other)) %>%
  mutate(qy7116_1 = ifelse(qy7116_1 == ".                             ", NA, qy7116_1)) %>%
  mutate(qy7116_1 = ifelse(qy7116_1 == "                              ", NA, qy7116_1)) %>%
  mutate( qy7116a = ifelse( qy7116a == "Yes" , "friends" , qy7116a )) %>%
  mutate( qy7116b = ifelse( qy7116b == "Yes" , "family" , qy7116b )) %>%
  mutate( qy7116c = ifelse( qy7116c == "Yes" , "a dealer" , qy7116c )) %>%
  mutate( qy7116d = ifelse( qy7116d == "Yes" , "somewhere else" , qy7116d )) %>%
  mutate(qy7116b = as.character(qy7116b))

q116$qy7116 <- apply(q116, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q116;

q116 %>% 
  select(fullid_07, qy7116) %>%
  mutate(qy7116 = 
           str_replace(qy7116, pattern = '^.*?,', replacement = ""),
         qy7116 = str_trim(qy7116, side='left')) %>%
  filter(fullid_07 != qy7116) -> q116

# question 125
q125 <- byds %>%
  select(fullid_07,	qy7125_1,	qy7125_2,	qy7125_3,	qy7125_4,	qy7125_5,	qy7125_6,	qy7125_7,	qy7125_8) %>%
  mutate(qy7125_8 = ifelse(qy7125_8 == ".               ", NA, qy7125_8)) %>%
  mutate(qy7125_8 = ifelse(qy7125_8 == "                ", NA, qy7125_8))

q125$qy7125 <- apply(q125, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "0" & x != "1" & !is.na(x)], collapse = ", ")); q125;

q125 %>% 
  select(fullid_07, qy7125) %>%
  mutate(qy7125 = 
           str_replace(qy7125, pattern = '^.*?,', replacement = ""),
         qy7125 = str_trim(qy7125, side='left')) %>%
  filter(fullid_07 != qy7125) -> q125

# question 126
q126 <- byds %>%
  select(fullid_07,	qy7126,	qy7126a,	qy7126b,	qy7126c,	qy7126d,	qy7126_other,	qy7126_1) %>%
  mutate(	qy7126a	= ifelse(	qy7126a	==	"Yes"	,	"friends"	,	qy7126a	))	%>%
  mutate(	qy7126b	= ifelse(	qy7126b	==	"Yes"	,	"family"	,	qy7126b	))	%>%
  mutate(	qy7126c	= ifelse(	qy7126c	==	"Yes"	,	"a dealer"	,	qy7126c	))	%>%
  mutate(	qy7126d	= ifelse(	qy7126d	==	"Yes"	,	"somewhere else"	,	qy7126d	))	%>%
  mutate(qy7126_other = ifelse(qy7126_other == ".                 ", NA, qy7126_other)) %>%
  mutate(qy7126_other = ifelse(qy7126_other == "                  ", NA, qy7126_other)) %>%
  mutate(qy7126_1 = ifelse(qy7126_1 == "                                               ", NA, qy7126_1))

q126$qy7126 <- apply(q126, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "No" & x != "1" & !is.na(x)], collapse = ", ")); q126;

q126 %>% 
  select(fullid_07, qy7126) %>%
  mutate(qy7126 = 
           str_replace(qy7126, pattern = '^.*?,', replacement = ""),
         qy7126 = str_trim(qy7126, side='left')) %>%
  filter(fullid_07 != qy7126) -> q126


# question 135
q135 <- byds %>%
  select(fullid_07,	qy7135_1,	qy7135_2,	qy7135_3,	qy7135_4,	qy7135_5,	qy7135_6,	qy7135_7,	qy7135_8) %>%
  mutate(qy7135_8 = ifelse(qy7135_8 == ".                    ", NA, qy7135_8)) %>%
  mutate(qy7135_8 = ifelse(qy7135_8 == "                     ", NA, qy7135_8)) 

q135$qy7135 <- apply(q135, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & !is.na(x)], collapse = ", ")); q135;

q135 %>% 
  select(fullid_07, qy7135) %>%
  mutate(qy7135 = 
           str_replace(qy7135, pattern = '^.*?,', replacement = ""),
         qy7135 = str_trim(qy7135, side='left')) %>%
  filter(fullid_07 != qy7135) -> q135

# question 136
q136 <- byds %>%
  select(fullid_07,	qy7136,	qy7136a,	qy7136b,	qy7136c,	qy7136d,	qy7136_other,	qy7136_1) %>%
  mutate(	qy7136a	= ifelse(	qy7136a	==	"Yes"	,	"friends"	,	qy7136a	))	%>%
  mutate(	qy7136b	= ifelse(	qy7136b	==	"Yes"	,	"family"	,	qy7136b	))	%>%
  mutate(	qy7136c	= ifelse(	qy7136c	==	"Yes"	,	"a dealer"	,	qy7136c	))	%>%
  mutate(	qy7136d	= ifelse(	qy7136d	==	"Yes"	,	"somewhere else"	,	qy7136d	))	%>%
  mutate(qy7136_other = ifelse(qy7136_other == ".          ", NA, qy7136_other)) %>%
  mutate(qy7136_other = ifelse(qy7136_other == "           ", NA, qy7136_other)) %>%
  mutate(qy7136_1 = ifelse(qy7136_1 == ".              ", NA, qy7136_1)) %>%
  mutate(qy7136_1 = ifelse(qy7136_1 == "               ", NA, qy7136_1)) %>%
  mutate(qy7136a = as.character(qy7136a),
         qy7136b = as.character(qy7136b),
         qy7136c = as.character(qy7136c))

q136$qy7136 <- apply(q136, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & !is.na(x)], collapse = ", ")); q136;

q136 %>% 
  select(fullid_07, qy7136) %>%
  mutate(qy7136 = 
           str_replace(qy7136, pattern = '^.*?,', replacement = ""),
         qy7136 = str_trim(qy7136, side='left')) %>%
  filter(fullid_07 != qy7136) -> q136

# question 145
q145 <- byds %>%
  select(fullid_07,	qy7145_1,	qy7145_2,	qy7145_3,	qy7145_4,	qy7145_5,	qy7145_6,	qy7145_7,	qy7145_8) %>%
  mutate(qy7145_8 = ifelse(qy7145_8 == ".                              ", NA, qy7145_8)) %>%
  mutate(qy7145_8 = ifelse(qy7145_8 == "                               ", NA, qy7145_8))

q145$qy7145 <- apply(q145, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & !is.na(x)], collapse = ", ")); q145;

q145 %>% 
  select(fullid_07, qy7145) %>%
  mutate(qy7145 = 
           str_replace(qy7145, pattern = '^.*?,', replacement = ""),
         qy7145 = str_trim(qy7145, side='left')) %>%
  filter(fullid_07 != qy7145) -> q145

# question 146
q146 <- byds %>%
  select(fullid_07,	qy7146,	qy7146a,	qy7146b,	qy7146c,	qy7146d,	qy7146_other,	qy7146_1) %>%
  mutate(	qy7146a	= ifelse(	qy7146a	==	"1"	,	"friends"	,	qy7146a	))	%>%
  mutate(	qy7146b	= ifelse(	qy7146b	==	"1"	,	"family"	,	qy7146b	))	%>%
  mutate(	qy7146c	= ifelse(	qy7146c	==	"1"	,	"a dealer"	,	qy7146c	))	%>%
  mutate(	qy7146d	= ifelse(	qy7146d	==	"1"	,	"somewhere else"	,	qy7146d	))	%>%
  mutate(qy7146_other = ifelse(qy7146_other == ".                   ", NA, qy7146_other)) %>%
  mutate(qy7146_other = ifelse(qy7146_other == "                    ", NA, qy7146_other)) %>%
  mutate(qy7146_1 = ifelse(qy7146_1 == ".                ", NA, qy7146_1)) %>%
  mutate(qy7146_1 = ifelse(qy7146_1 == "                 ", NA, qy7146_1))

q146$qy7146 <- apply(q146, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & !is.na(x)], collapse = ", ")); q146;

q146 %>% 
  select(fullid_07, qy7146) %>%
  mutate(qy7146 = 
           str_replace(qy7146, pattern = '^.*?,', replacement = ""),
         qy7146 = str_trim(qy7146, side='left')) %>%
  filter(fullid_07 != qy7146) -> q146

# question 155
q155 <- byds %>%
  select(fullid_07,	qy7155_1,	qy7155_2,	qy7155_3,	qy7155_4,	qy7155_5,	qy7155_6,	qy7155_7,	qy7155_8) %>%
  mutate(qy7155_8 = ifelse(qy7155_8 == "            ", NA, qy7155_8)) %>%
  mutate(qy7155_8 = ifelse(qy7155_8 == ".           ", NA, qy7155_8))

q155$qy7155 <- apply(q155, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "0" & x != "1" & !is.na(x)], collapse = ", ")); q155;

q155 %>% 
  select(fullid_07, qy7155) %>%
  mutate(qy7155 = 
           str_replace(qy7155, pattern = '^.*?,', replacement = ""),
         qy7155 = str_trim(qy7155, side='left')) %>%
  filter(fullid_07 != qy7155) -> q155

# question 156
q156 <- byds %>%
  select(fullid_07,	qy7156,	qy7156a,	qy7156b,	qy7156c,	qy7156d,	qy7156_other,	qy7156_1) %>%
  mutate(qy7156_other = ifelse(qy7156_other == ".                    ", NA, qy7156_other)) %>%
  mutate(qy7156_other = ifelse(qy7156_other == "                     ", NA, qy7156_other)) %>%
  mutate(qy7156_1 = ifelse(qy7156_1 == "                                                     ", NA, qy7156_1)) %>%
  mutate(qy7156a = as.character(qy7156a),
         qy7156b = as.character(qy7156b),
         qy7156c = as.character(qy7156c),
         qy7156d = as.character(qy7156d))

q156$qy7156 <- apply(q156, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & !is.na(x)], collapse = ", ")); q156;

q156 %>% 
  select(fullid_07, qy7156) %>%
  mutate(qy7156 = 
           str_replace(qy7156, pattern = '^.*?,', replacement = ""),
         qy7156 = str_trim(qy7156, side='left')) %>%
  filter(fullid_07 != qy7156) -> q156

# question 164
q164 <- byds %>%
  select(fullid_07,	qy7164_1,	qy7164_2,	qy7164_3,	qy7164_4,	qy7164_5,	qy7164_6,	qy7164_7,	qy7164_8) %>%
  mutate(qy7164_8 = ifelse(qy7164_8 == ".                        ", NA, qy7164_8)) %>%
  mutate(qy7164_8 = ifelse(qy7164_8 == "                        ", NA, qy7164_8)) %>%
  mutate(qy7164_8 = ifelse(qy7164_8 == "                         ", NA, qy7164_8))

q164$qy7164 <- apply(q164, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & !is.na(x)], collapse = ", ")); q164;

q164 %>% 
  select(fullid_07, qy7164) %>%
  mutate(qy7164 = 
           str_replace(qy7164, pattern = '^.*?,', replacement = ""),
         qy7164 = str_trim(qy7164, side='left')) %>%
  filter(fullid_07 != qy7164) -> q164

# question 165
q165 <- byds %>%
  select(fullid_07,	qy7165,	qy7165a,	qy7165b,	qy7165c,	qy7165d,	qy7165_other,	qy7165_2) %>%
  mutate(	qy7165a	= ifelse(	qy7165a	==	"1"	,	"friends"	,	qy7165a	))	%>%
  mutate(	qy7165b	= ifelse(	qy7165b	==	"1"	,	"family"	,	qy7165b	))	%>%
  mutate(	qy7165c	= ifelse(	qy7165c	==	"1"	,	"a dealer"	,	qy7165c	))	%>%
  mutate(	qy7165d	= ifelse(	qy7165d	==	"1"	,	"somewhere else"	,	qy7165d	))	%>%
  mutate(qy7165_other = ifelse(qy7165_other == ".        ", NA, qy7165_other)) %>%
  mutate(qy7165_other = ifelse(qy7165_other == "         ", NA, qy7165_other)) %>%
  mutate(qy7165_2 = ifelse(qy7165_2 == "                        ", NA, qy7165_2)) %>%
  mutate(qy7165_2 = ifelse(qy7165_2 == "                         ", NA, qy7165_2))

q165$qy7165 <- apply(q165, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & !is.na(x)], collapse = ", ")); q165;

q165 %>% 
  select(fullid_07, qy7165) %>%
  mutate(qy7165 = 
           str_replace(qy7165, pattern = '^.*?,', replacement = ""),
         qy7165 = str_trim(qy7165, side='left')) %>%
  filter(fullid_07 != qy7165) -> q165

# question 175
q175 <- byds %>%
  select(fullid_07,	qy7175_1,	qy7175_2,	qy7175_3,	qy7175_4,	qy7175_5,	qy7175_6,	qy7175_7,	qy7175_8) %>%
  mutate(qy7175_8 = ifelse(qy7175_8 == ".      ", NA, qy7175_8)) %>%
  mutate(qy7175_8 = ifelse(qy7175_8 == "       ", NA, qy7175_8)) 

q175$qy7175 <- apply(q175, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "0" & x != "1" & !is.na(x)], collapse = ", ")); q175;

q175 %>% 
  select(fullid_07, qy7175) %>%
  mutate(qy7175 = 
           str_replace(qy7175, pattern = '^.*?,', replacement = ""),
         qy7175 = str_trim(qy7175, side='left')) %>%
  filter(fullid_07 != qy7175) -> q175

# question 176
q176 <- byds %>%
  select(fullid_07,	qy7176,	qy7176a,	qy7176b,	qy7176c,	qy7176d,	qy7176_other,	qy7176_2) %>%
  mutate(	qy7176a	= ifelse(	qy7176a	==	"1"	,	"friends"	,	qy7176a	))	%>%
  mutate(	qy7176b	= ifelse(	qy7176b	==	"1"	,	"family"	,	qy7176b	))	%>%
  mutate(	qy7176c	= ifelse(	qy7176c	==	"1"	,	"a dealer"	,	qy7176c	))	%>%
  mutate(	qy7176d	= ifelse(	qy7176d	==	"1"	,	"somewhere else"	,	qy7176d	))	%>%
  mutate(qy7176_other = ifelse(qy7176_other == ".                      ", NA, qy7176_other)) %>%
  mutate(qy7176_other = ifelse(qy7176_other == "                       ", NA, qy7176_other)) %>%
  mutate(qy7176_2 = ifelse(qy7176_2 == ".                                                 ", NA, qy7176_2)) %>%
  mutate(qy7176_2 = ifelse(qy7176_2 == "                                                  ", NA, qy7176_2)) %>%
  mutate(qy7176b = as.character(qy7176b))

q176$qy7176 <- apply(q176, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1"  & x != "0" & !is.na(x)], collapse = ", ")); q176;

q176 %>% 
  select(fullid_07, qy7176) %>%
  mutate(qy7176 = 
           str_replace(qy7176, pattern = '^.*?,', replacement = ""),
         qy7176 = str_trim(qy7176, side='left')) %>%
  filter(fullid_07 != qy7176) -> q176


# question 216
q216 <- byds %>%
  select(fullid_07,	qy7216_1,	qy7216_2,	qy7216_3,	qy7216_4,	qy7216_5,	qy7216_6,	qy7216_7,	qy7216_8,	qy7216_9,	qy7216_10) %>%
  mutate(qy7216_9 = as.character(qy7216_9),
         qy7216_10 = as.character(qy7216_10))

q216$qy7216 <- apply(q216, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1"  & x != "0" & !is.na(x)], collapse = ", ")); q216;

q216 %>% 
  select(fullid_07, qy7216) %>%
  mutate(qy7216 = 
           str_replace(qy7216, pattern = '^.*?,', replacement = ""),
         qy7216 = str_trim(qy7216, side='left')) %>%
  filter(fullid_07 != qy7216) -> q216

# question 218
q218 <- byds %>%
  select(fullid_07,	qy7218_1,	qy7218_2,	qy7218_3,	qy7218_4,	qy7218_5,	qy7218_6,	qy7218_7,	qy7218_8,	qy7218_9,	qy7218_10,	qy7218_11) %>%
  mutate(qy7218_11 = ifelse(qy7218_11 == ".                                                       ", NA, qy7218_11)) %>%
  mutate(qy7218_11 = ifelse(qy7218_11 == "                                                        ", NA, qy7218_11)) 

q218$qy7218 <- apply(q218, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "0" & x != "2" & !is.na(x)], collapse = ", ")); q218;

q218 %>% 
  select(fullid_07, qy7218) %>%
  mutate(qy7218 = 
           str_replace(qy7218, pattern = '^.*?,', replacement = ""),
         qy7218 = str_trim(qy7218, side='left')) %>%
  filter(fullid_07 != qy7218) -> q218

# question 219
q219 <- byds %>%
  select(fullid_07,	qy7219_1,	qy7219_2,	qy7219_3,	qy7219_4,	qy7219_5,	qy7219_6,	qy7219_7,	qy7219_8,	
         qy7219_9,	qy7219_10,	qy7219_11,	qy7219_12,	qy7219_13,	qy7219_14,	qy7219_15) %>%
  mutate(qy7219_14 = as.character(qy7219_14),
         qy7219_15 = as.character(qy7219_15))

q219$qy7219 <- apply(q219, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "0" & x != "1" & !is.na(x)], collapse = ", ")); q219;

q219 %>% 
  select(fullid_07, qy7219) %>%
  mutate(qy7219 = 
           str_replace(qy7219, pattern = '^.*?,', replacement = ""),
         qy7219 = str_trim(qy7219, side='left')) %>%
  filter(fullid_07 != qy7219) -> q219

# question 222
q222 <- byds %>%
  select(fullid_07,	qy7222_1,	qy7222_2,	qy7222_3,	qy7222_4,	qy7222_5,	qy7222_6,	qy7222_7,	qy7222_8,	qy7222_9,	qy7222_10,	qy7222_11) %>%
  mutate(	qy7222_1	= ifelse(	qy7222_1	==	"Yes"	,	"someone in your family"	,	qy7222_1	))	%>%
  mutate(	qy7222_2	= ifelse(	qy7222_2	==	"Yes"	,	"your romantic partner"	,	qy7222_2	))	%>%
  mutate(	qy7222_3	= ifelse(	qy7222_3	==	"Yes"	,	"a friend"	,	qy7222_3	))	%>%
  mutate(	qy7222_4	= ifelse(	qy7222_4	==	"Yes"	,	"a teacher"	,	qy7222_4	))	%>%
  mutate(	qy7222_5	= ifelse(	qy7222_5	==	"Yes"	,	"a GP"	,	qy7222_5	))	%>%
  mutate(	qy7222_6	= ifelse(	qy7222_6	==	"Yes"	,	"a social worker"	,	qy7222_6	))	%>%
  mutate(	qy7222_7	= ifelse(	qy7222_7	==	"Yes"	,	"a psychologist or psychiatrist"	,	qy7222_7	))	%>%
  mutate(	qy7222_8	= ifelse(	qy7222_8	==	"Yes"	,	"a telephone help line"	,	qy7222_8	))	%>%
  mutate(	qy7222_9	= ifelse(	qy7222_9	==	"Yes"	,	"a drop-in advice centre"	,	qy7222_9	))	%>%
  mutate(	qy7222_10	= ifelse(	qy7222_10	==	"Yes"	,	"other source"	,	qy7222_10	))	%>%
  mutate(qy7222_11 = ifelse(qy7222_11 == ".                                     ", NA, qy7222_11)) %>%
  mutate(qy7222_11 = ifelse(qy7222_11 == "                                      ", NA, qy7222_11)) %>%
  mutate(qy7222_11 = ifelse(qy7222_11 == "                                     ", NA, qy7222_11)) %>%
  mutate(qy7222_1 = as.character(qy7222_1),
         qy7222_2 = as.character(qy7222_2),
         qy7222_3 = as.character(qy7222_3),
         qy7222_4 = as.character(qy7222_4),
         qy7222_5 = as.character(qy7222_5),
         qy7222_6 = as.character(qy7222_6),
         qy7222_7 = as.character(qy7222_7),
         qy7222_8 = as.character(qy7222_8),
         qy7222_9 = as.character(qy7222_9),
         qy7222_10 = as.character(qy7222_10))

q222$qy7222 <- apply(q222, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & x != "1" & x != "2" & !is.na(x)], collapse = ", ")); q222;

q222 %>% 
  select(fullid_07, qy7222) %>%
  mutate(qy7222 = 
           str_replace(qy7222, pattern = '^.*?,', replacement = ""),
         qy7222 = str_trim(qy7222, side='left')) %>%
  filter(fullid_07 != qy7222) -> q222


# join it back together ##############

# pull down the already clean variables
wave_7 <- byds %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, qy71, qy72, qy72_1, 
         qy73, qy73_1, qy74, qy75, qy75a, qy76, qy76_1, qy77, qy77a, qy77b, qy77c, qy77d, qy78, qy79_1, 
         qy79_2, qy79_3, qy79_4, qy79_5, qy79_6, qy79_7, qy79_8, qy710, qy710a, qy710a_1, qy710b, qy710c, qy711, 
         qy711a, qy711a_1, qy711b, qy711c, qy711d, qy711e, qy711f, qy711g, qy712, qy712a, qy712b, qy712c, qy713, 
         qy713a, qy713b, qy713b_1, qy713c, qy714, qy714a, qy714b, qy714c, qy714d, qy714e, qy714f, qy714g, qy715a, 
         qy715a_1, qy715b, qy715b_1, qy715c, qy715c_1, qy716, qy717, qy717a, qy718, qy719, qy720, qy721, qy722, 
         qy723, qy724, qy725a, qy725a_1, qy725b, qy725b_1, qy725c, qy725c_1, qy725d, qy725d_1, qy725e, qy725e_1, 
         qy725_1, qy725f, qy725f_1, qy725g, qy725g_1, qy725h, qy726a, qy726b, qy727, qy728a, qy728b, qy729, qy730, 
         qy732_1, qy732_2, qy732_3, qy732_4, qy732_5, qy732_6, qy732_7, qy732_8, qy732_9, qy732_10, qy732_11, 
         qy732_12, qy732_13, qy732_14, qy732_15, qy733_1, qy733_2, qy733_3, qy733_4, qy733_5, qy733_6, qy733_7, 
         qy733_8, qy734_1, qy734_2, qy734_3, qy734_4, qy734_5, qy734_6, qy734_7, qy735, qy736, qy737, qy738, 
         qy739, qy740_1, qy740_2, qy740_3, qy740_4, qy740_5, qy740_6, qy740_7, qy740_8, qy740_9, qy740_10, 
         qy740_11, qy740_12, qy741, qy742, qy743, qy744, qy745, qy746, qy747, qy748, qy749, qy750, qy751, 
         qy752, qy753, qy754, qy755, qy756, qy757, qy758, qy759, qy760, qy761, qy762, qy763, qy764, qy765, 
         qy766, qy767a, qy767b, qy767c, qy767c_1, qy767d, qy767d_1, qy767e, qy767e_1, qy767f, qy767f_1, qy767g, qy767g_1, 
         qy767h, qy767h_1, qy767i, qy767i_1, qy767j, qy767j_1, qy768_1, qy768_2, qy768_3, qy768_4, qy768_5, qy768_6, 
         qy768_7, qy768_8, qy768_9, qy768_10, qy768_11, qy768_12, qy768_13, qy768_14, qy768_15, qy768_16, qy768_17, qy768_18, 
         qy768_19, qy768_20, qy769_1, qy769_2, qy769_3, qy769_4, qy769_5, qy769_6, qy769_7, qy769_8, qy769_9, qy769_10, 
         qy769_11, qy770, qy771, qy772, qy773, qy774a, qy774b, qy774c, qy774d, qy774e, qy775a, qy775b, qy776, qy777, 
         qy778, qy779, qy780, qy781, qy782, qy783, qy786a, qy786b, qy786c, qy786d, qy786e, qy786f, qy787, qy788, 
         qy789, qy790, qy791, qy792, qy793, qy794, qy797, qy798, qy799, qy7100, qy7101, qy7102, qy7103, qy7104,
         qy7107, qy7108, qy7109, qy7110, qy7111, qy7112, qy7113, qy7114,	qy7117,	qy7118,	qy7119,	qy7120,	qy7121,	qy7122,	qy7123,	qy7124,
         qy7127,	qy7128,	qy7129,	qy7130,	qy7131,	qy7132,	qy7133,	qy7134, qy7137,	qy7138,	qy7139,	qy7140,	qy7141,	qy7142,	qy7143,	qy7144,	
         qy7147,	qy7148,	qy7149,	qy7150,	qy7151,	qy7152,	qy7153,	qy7154,	qy7157,	qy7158,	qy7159,	qy7160,	qy7161,	qy7162,	qy7163,	qy7166,	
         qy7167,	qy7168,	qy7169,	qy7170,	qy7171,	qy7172,	qy7173,	qy7174,	qy7177,	qy7178a,	qy7178b,	qy7178c,	qy7178d,	qy7178e,	
         qy7178f,	qy7178g,	qy7178h,	qy7178i,	qy7178j,	qy7178k,	qy7178l,	qy7178m,	qy7178n,	qy7178o,	qy7178p,	qy7178q,	
         qy7178r,	qy7178s,	qy7178t,	qy7178u,	qy7178v,	qy7178w,	qy7179a1,	qy7179a2,	qy7179b1,	qy7179b2,	qy7179c1,	qy7179c2,	
         qy7179d1,	qy7179d2,	qy7179e1,	qy7179e2,	qy7179f1,	qy7179f2,	qy7179g1,	qy7170g2,	qy7179h1,	qy7179h2,	qy7179i1,	qy7179i2,	
         qy7179j1,	qy7179j2,	qy7179k1,	qy7179k2,	qy7179l1,	qy7179l2,	qy7179m1,	qy7179m2,	qy7179n1,	qy7179n2,	qy7179o1,	qy7179o2,	
         qy7179p1,	qy7179p2,	qy7180,	qy7181a,	qy7181a_1,	qy7181b,	qy7181b_1,	qy7181c,	qy7181c_1,	qy7181d,	qy7181d_1,	qy7182,	
         qy7183,	qy7184,	qy7185,	qy7186,	qy7187_1,	qy7187_2,	qy7187_3,	qy7187_4,	qy7187_5,	qy7187_6,	qy7187_7,	qy7187_8,	qy7187_9,	
         qy7188,	qy7189,	qy7189a,	qy7189b,	qy7190,	qy7190a,	qy7191,	qy7191a,	qy7191b,	qy7192,	qy7192a,	qy7193,	qy7193a,	
         qy7194,	qy7195_1,	qy7195_2,	qy7195_3,	qy7195_4,	qy7195_5,	qy7195_6,	qy7195_7,	qy7195_8,	qy7195_9,	qy7195_10,	qy7195_11,	
         qy7195_12,	qy7195_13,	qy7195_14,	qy7195_15,	qy7195_16,	qy7195_17,	qy7195_18,	qy7195_19,	qy7195_20,	qy7195_21,	
         qy7195_22,	qy7195_23,	qy7195_24,	qy7195_25,	qy7195_26,	qy7195_27,	qy7195_28,	qy7195_29,	qy7195_30,	qy7195_31,	
         qy7195_32,	qy7195_33,	qy7195_34,	qy7195_35,	qy7195_36,	qy7195_37,	qy7195_38,	qy7195_39,	qy7195_40,	qy7195_41,	
         qy7196,	qy7197,	qy7198,	qy7199,	qy7200,	qy7201,	qy7202,	qy7203,	qy7204,	qy7205_1,	qy7205_2,	qy7205_3,	qy7205_4,	qy7205_5,	
         qy7205_6,	qy7206,	qy7207,	qy7208,	qy7209,	qy7210,	qy7211,	qy7212,	qy7212a,	qy7213,	qy7214,	qy7214a,	qy7215_1,	qy7215_2,	
         qy7215_3,	qy7216_1,	qy7216_2,	qy7216_3,	qy7216_4,	qy7216_5,	qy7216_6,	qy7216_7,	qy7216_8,	qy7216_9,	qy7216_10,	qy7217,	
         qy7220,	qy7221,	qy7223,	qy7224)

# join in the new variables
wave_7 <- wave_7 %>%
  left_join(q31, by = "fullid_07") %>%
  left_join(q39_1, by = "fullid_07") %>%
  left_join(q67_3, by = "fullid_07") %>%
  left_join(q67_19, by = "fullid_07") %>%
  left_join(q84, by = "fullid_07") %>%
  left_join(q85, by = "fullid_07") %>%
  left_join(q95, by = "fullid_07") %>%
  left_join(q96, by = "fullid_07") %>%
  left_join(q105, by = "fullid_07") %>%
  left_join(q106, by = "fullid_07") %>%
  left_join(q115, by = "fullid_07") %>%
  left_join(q116, by = "fullid_07") %>%
  left_join(q125, by = "fullid_07") %>%
  left_join(q126, by = "fullid_07") %>%
  left_join(q135, by = "fullid_07") %>%
  left_join(q136, by = "fullid_07") %>%
  left_join(q145, by = "fullid_07") %>%
  left_join(q146, by = "fullid_07") %>%
  left_join(q155, by = "fullid_07") %>%
  left_join(q156, by = "fullid_07") %>%
  left_join(q164, by = "fullid_07") %>%
  left_join(q165, by = "fullid_07") %>%
  left_join(q175, by = "fullid_07") %>%
  left_join(q176, by = "fullid_07") %>%
  left_join(q216, by = "fullid_07") %>%
  left_join(q218, by = "fullid_07") %>%
  left_join(q219, by = "fullid_07") %>%
  left_join(q222, by = "fullid_07")


rm(q31, q39_1, q67_3, q67_19, q84, q85, q95, q96, q105, q106, q115, q116, q125, q126, q135, q136, q145, q146, q155, q156,
   q164, q165, q175, q176, q216, q218, q219, q222)

# fix the order of your new variables
wave_7 <- wave_7 %>%
  select(iobs, fullid_01, fullid_02, fullid_03, fullid_04, fullid_05, fullid_06, fullid_07, qy71, qy72, qy72_1, qy73, qy73_1, 
         qy74, qy75, qy75a, qy76, qy76_1, qy77, qy77a, qy77b, qy77c, qy77d, qy78, qy79_1, qy79_2, qy79_3, qy79_4, 
         qy79_5, qy79_6, qy79_7, qy79_8, qy710, qy710a, qy710a_1, qy710b, qy710c, qy711, qy711a, qy711a_1, qy711b, qy711c, 
         qy711d, qy711e, qy711f, qy711g, qy712, qy712a, qy712b, qy712c, qy713, qy713a, qy713b, qy713b_1, qy713c, qy714, 
         qy714a, qy714b, qy714c, qy714d, qy714e, qy714f, qy714g, qy715a, qy715a_1, qy715b, qy715b_1, qy715c, qy715c_1, qy716, 
         qy717, qy717a, qy718, qy719, qy720, qy721, qy722, qy723, qy724, qy725a, qy725a_1, qy725b, qy725b_1, qy725c, 
         qy725c_1, qy725d, qy725d_1, qy725e, qy725e_1, qy725_1, qy725f, qy725f_1, qy725g, qy725g_1, qy725h, qy726a, qy726b, 
         qy727, qy728a, qy728b, qy729, qy730, qy731, qy732_1, qy732_2, qy732_3, qy732_4, qy732_5, qy732_6, qy732_7, 
         qy732_8, qy732_9, qy732_10, qy732_11, qy732_12, qy732_13, qy732_14, qy732_15, qy733_1, qy733_2, qy733_3, qy733_4, 
         qy733_5, qy733_6, qy733_7, qy733_8, qy734_1, qy734_2, qy734_3, qy734_4, qy734_5, qy734_6, qy734_7, qy735, 
         qy736, qy737, qy738, qy739, qy739_1, qy740_1, qy740_2, qy740_3, qy740_4, qy740_5, qy740_6, qy740_7, qy740_8, 
         qy740_9, qy740_10, qy740_11, qy740_12, qy741, qy742, qy743, qy744, qy745, qy746, qy747, qy748, qy749, qy750, 
         qy751, qy752, qy753, qy754, qy755, qy756, qy757, qy758, qy759, qy760, qy761, qy762, qy763, qy764, qy765, 
         qy766, qy767a, qy767b, qy767b_1, qy767c, qy767c_1, qy767d, qy767d_1, qy767e, qy767e_1, qy767f, qy767f_1, qy767g, qy767g_1, 
         qy767h, qy767h_1, qy767i, qy767i_1, qy767j, qy767j_1, qy767k, qy768_1, qy768_2, qy768_3, qy768_4, qy768_5, qy768_6, 
         qy768_7, qy768_8, qy768_9, qy768_10, qy768_11, qy768_12, qy768_13, qy768_14, qy768_15, qy768_16, qy768_17, qy768_18, 
         qy768_19, qy768_20, qy769_1, qy769_2, qy769_3, qy769_4, qy769_5, qy769_6, qy769_7, qy769_8, qy769_9, qy769_10, 
         qy769_11, qy770, qy771, qy772, qy773, qy774a, qy774b, qy774c, qy774d, qy774e, qy775a, qy775b, qy776, qy777, qy778, 
         qy779, qy780, qy781, qy782, qy783, qy784, qy785, qy786a, qy786b, qy786c, qy786d, qy786e, qy786f, qy787, qy788, qy789, 
         qy790, qy791, qy792, qy793, qy794, qy795, qy796, qy797, qy798, qy799, qy7100, qy7101, qy7102, qy7103, qy7104, qy7105,
         qy7106, qy7107, qy7108, qy7109, qy7110, qy7111, qy7112, qy7113, qy7114, qy7115, qy7116,	qy7117,	qy7118,	qy7119,	qy7120,	
         qy7121,	qy7122,	qy7123,	qy7124, qy7125, qy7126,	qy7127,	qy7128,	qy7129,	qy7130,	qy7131,	qy7132,	qy7133,	qy7134, qy7135,
         qy7136,	qy7137,	qy7138,	qy7139,	qy7140,	qy7141,	qy7142,	qy7143,	qy7144, qy7145, qy7146,	qy7147,	qy7148,	qy7149,	qy7150,	
         qy7151,	qy7152,	qy7153,	qy7154, qy7155, qy7156,	qy7157,	qy7158,	qy7159,	qy7160,	qy7161,	qy7162,	qy7163, qy7164, qy7165,	
         qy7166,	qy7167,	qy7168,	qy7169,	qy7170,	qy7171,	qy7172,	qy7173,	qy7174, qy7175, qy7176,	qy7177,	qy7178a,	qy7178b,	qy7178c,	
         qy7178d,	qy7178e,	qy7178f,	qy7178g,	qy7178h,	qy7178i,	qy7178j,	qy7178k,	qy7178l,	qy7178m,	qy7178n,	qy7178o,	qy7178p,	
         qy7178q,	qy7178r,	qy7178s,	qy7178t,	qy7178u,	qy7178v,	qy7178w,	qy7179a1,	qy7179a2,	qy7179b1,	qy7179b2,	qy7179c1,	qy7179c2,	
         qy7179d1,	qy7179d2,	qy7179e1,	qy7179e2,	qy7179f1,	qy7179f2,	qy7179g1,	qy7170g2,	qy7179h1,	qy7179h2,	qy7179i1,	qy7179i2,	qy7179j1,	
         qy7179j2,	qy7179k1,	qy7179k2,	qy7179l1,	qy7179l2,	qy7179m1,	qy7179m2,	qy7179n1,	qy7179n2,	qy7179o1,	qy7179o2,	qy7179p1,	qy7179p2,	
         qy7180,	qy7181a,	qy7181a_1,	qy7181b,	qy7181b_1,	qy7181c,	qy7181c_1,	qy7181d,	qy7181d_1,	qy7182,	qy7183,	qy7184,	qy7185,	qy7186,	
         qy7187_1,	qy7187_2,	qy7187_3,	qy7187_4,	qy7187_5,	qy7187_6,	qy7187_7,	qy7187_8,	qy7187_9,	qy7188,	qy7189,	qy7189a,	qy7189b,	qy7190,	
         qy7190a,	qy7191,	qy7191a,	qy7191b,	qy7192,	qy7192a,	qy7193,	qy7193a,	qy7194,	qy7195_1,	qy7195_2,	qy7195_3,	qy7195_4,	qy7195_5,	qy7195_6,	
         qy7195_7,	qy7195_8,	qy7195_9,	qy7195_10,	qy7195_11,	qy7195_12,	qy7195_13,	qy7195_14,	qy7195_15,	qy7195_16,	qy7195_17,	qy7195_18,	
         qy7195_19,	qy7195_20,	qy7195_21,	qy7195_22,	qy7195_23,	qy7195_24,	qy7195_25,	qy7195_26,	qy7195_27,	qy7195_28,	qy7195_29,	qy7195_30,	
         qy7195_31,	qy7195_32,	qy7195_33,	qy7195_34,	qy7195_35,	qy7195_36,	qy7195_37,	qy7195_38,	qy7195_39,	qy7195_40,	qy7195_41,	qy7196,	
         qy7197,	qy7198,	qy7199,	qy7200,	qy7201,	qy7202,	qy7203,	qy7204,	qy7205_1,	qy7205_2,	qy7205_3,	qy7205_4,	qy7205_5,	qy7205_6,	qy7206,	
         qy7207,	qy7208,	qy7209,	qy7210,	qy7211,	qy7212,	qy7212a,	qy7213,	qy7214,	qy7214a,	qy7215_1,	qy7215_2,	qy7215_3,	qy7216, qy7217, qy7218,
         qy7219,	qy7220,	qy7221, qy7222,	qy7223,	qy7224)


##########################################
#     REJOIN WAVES INTO A SINGLE DF
##########################################

cleaned_byds <- wave_1 %>%
  left_join(wave_2, by = c("iobs", "fullid_01", "fullid_02", "fullid_03", "fullid_04", "fullid_05", "fullid_06", "fullid_07")) %>%
  left_join(wave_3, by = c("iobs", "fullid_01", "fullid_02", "fullid_03", "fullid_04", "fullid_05", "fullid_06", "fullid_07")) %>%
  left_join(wave_4, by = c("iobs", "fullid_01", "fullid_02", "fullid_03", "fullid_04", "fullid_05", "fullid_06", "fullid_07")) %>%
  left_join(wave_5, by = c("iobs", "fullid_01", "fullid_02", "fullid_03", "fullid_04", "fullid_05", "fullid_06", "fullid_07")) %>%
  left_join(wave_6, by = c("iobs", "fullid_01", "fullid_02", "fullid_03", "fullid_04", "fullid_05", "fullid_06", "fullid_07")) %>%
  left_join(wave_7, by = c("iobs", "fullid_01", "fullid_02", "fullid_03", "fullid_04", "fullid_05", "fullid_06", "fullid_07"))


# write the file >>>> I COMMENTED THIS OUT TO AVOID ACCIDENTALLY WRITING OVER THE FILE
write.csv(cleaned_byds, "Datasets/byds_cleaned_dataset2.csv")

####### OTHER THINGS FLOATING AROUND ############

# sample question cleaning code ##########
# question SAMPLE
#DF <- byds %>%
#  select(fullid_03)

#DF$VAR <- apply(DF, 1, function(x) paste(x[!is.numeric(x) & x != "Not" & !is.na(x)], collapse = ", ")); DF;

#DF %>% 
#  select(fullid_03, VAR) %>%
#  mutate(VAR = 
#           str_replace(VAR, pattern = '^.*?,', replacement = ""),
#         VAR = str_trim(VAR, side='left')) %>%
#  filter(fullid_03 != VAR) -> DF





















