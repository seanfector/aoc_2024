### load packages

library(dplyr)
library(tidyr)


### read in da file

day_02 <- read.csv("/Users/seankuhn/Downloads/aoc_2024_day_02.csv")


### part 1: split strings and identify monotone increasing/decreasing subsets
                # also subject to some constraints re: amount of increase

day_02_long <- day_02 %>%
  tidyr::separate_longer_delim(report, delim = " ") %>% # this is a slick, split-and-pivot function
  mutate(report = as.numeric(report)) # because it's natively a chr string

day_02_long <- day_02_long %>%
  group_by(index) %>%
  mutate(rn = row_number(),
         lag_report = lag(report),
         test_1 = ifelse(report == lag_report, "equal", 
                       ifelse(report > lag_report, "increasing",
                              "decreasing"))) %>%
  filter(!is.na(lag_report)) %>%
  mutate(delta = abs(report - lag_report),
         test_2 = ifelse(delta > 3, "fail", "pass")) %>%
  ungroup()

day_02_final <- day_02_long %>%
  group_by(index) %>%
  summarise(test_1_check = ifelse(min(test_1) == max(test_1) & min(test_1) %in% c("increasing", "decreasing"), 1, 0),
            test_2_check = ifelse(min(test_2) == "pass", 1, 0)) %>%
  filter(test_1_check == 1 & test_2_check == 1) %>%
  nrow() 

print(paste0("the answer to part 1 is ", day_02_final))


### part 2:

# let us write a loop that long-pivots each sequence, kicking out each one observation in each sequence

day_02_long <- day_02 %>%
  tidyr::separate_longer_delim(report, delim = " ") %>%
  mutate(report = as.numeric(report)) %>%
  group_by(index) %>%
  mutate(seq_num = row_number())

report_index <- seq(1, 1000, 1)
catch_df <- data.frame()

# note: this approach does NOT require checking the original, full-length sequences
# because any safe sequence of length N will also work at (N-1) when lopping off the end

for(r in report_index){
  
  day_02_long_sub <- day_02_long %>% filter(index == r)
  help_seq <- seq(1, max(day_02_long_sub$seq_num), 1)
  
  for(h in help_seq){
    
    day_02_long_sub_sub <- day_02_long_sub %>% 
      filter(seq_num != h) %>%
      mutate(kicked_out = h)
    
    catch_df <- rbind(catch_df, day_02_long_sub_sub)
    
  }
  
}

day_02_ll <- catch_df

day_02_ll_2 <- day_02_ll %>%
  group_by(index, kicked_out) %>%
  mutate(lag_report = lag(report),
         test_1 = ifelse(report == lag_report, "equal", 
                         ifelse(report > lag_report, "increasing",
                                "decreasing"))) %>%
  filter(!is.na(lag_report)) %>%
  mutate(delta = abs(report - lag_report),
         test_2 = ifelse(delta > 3, "fail", "pass")) %>%
  summarise(test_1_check = ifelse(min(test_1) == max(test_1) & min(test_1) %in% c("increasing", "decreasing"), 1, 0),
            test_2_check = ifelse(min(test_2) == "pass", 1, 0)) %>%
  filter(test_1_check == 1 & test_2_check == 1) 

print(paste0("the answer to part 2 is ", n_distinct(day_02_ll_2$index)))