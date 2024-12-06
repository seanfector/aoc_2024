### load packages

library(dplyr)
library(tidyr)
library(stringr)
library(sqldf)


### read in da file

day_05_a <- read.csv("/Users/seankuhn/Downloads/aoc_2024_day_05_a.csv")
day_05_b <- read.csv("/Users/seankuhn/Downloads/aoc_2024_day_05_b.csv")

day_05_a_split <- day_05_a %>% separate_wider_delim(rule, delim = "|", names = c("before", "after"))

day_05_b$index <- seq(1, nrow(day_05_b), 1)

day_05_b_split <- day_05_b %>%
  separate_longer_delim(sequence, delim = ",") %>%
  group_by(index) %>%
  mutate(rn = row_number()) %>%
  ungroup()


### part 1: a SEQUENCE is "safe" if all entries are in the order prescribed by the RULE

seq_seq <- seq(1, nrow(day_05_b), 1)
catch_df <- data.frame()

s <- 2

for(s in seq_seq){
  
  sequences_sub <- day_05_b_split %>% 
    filter(index == s) %>% # here's our sequence of interest
    mutate(is_midpoint = ifelse(median(rn) == rn, 1, 0))
  
  all_relevant_rules <- day_05_a_split %>%
    filter(before %in% sequences_sub$sequence & after %in% sequences_sub$sequence) # here are rules about sequence elements
  
  d_check <- sqldf("
                  SELECT
                    all_relevant_rules.*,
                      s_before.rn AS before_position,
                      s_after.rn AS after_position
                  FROM all_relevant_rules
                      JOIN sequences_sub s_before ON s_before.sequence = all_relevant_rules.before
                      JOIN sequences_sub s_after ON s_after.sequence = all_relevant_rules.after
                 ")
  
  d_check$is_safe <- with(d_check, ifelse(after_position > before_position, 1, 0))
  all_safe <- min(d_check$is_safe)
  
  middle_value <- sequences_sub %>% filter(is_midpoint == 1)
  middle_value <- middle_value$sequence[1]
  
  temp_df <- as.data.frame(cbind(s, all_safe, middle_value))
  catch_df <- rbind(catch_df, temp_df)
  
}

catch_df %>%
  filter(all_safe == 1) %>%
  mutate(middle_value = as.numeric(middle_value),
         all_safe = as.numeric(all_safe)) %>%
  summarise(sum_of_middle_values = sum(middle_value),
            total_sequences = 200,
            safe_sequences = sum(all_safe))


### part 2: ok gotta fix the 94 busted sequences 

d_out_of_order <- catch_df %>% 
  filter(all_safe == 0) %>%
  mutate(ooo_seq_num = row_number())

d_ooo_seqs <- day_05_b_split %>% filter(index %in% d_out_of_order$s) # here are the OOO seqs

ooo_seq_helper <- seq(1, nrow(d_out_of_order), 1)
catch_df_2 <- data.frame()

for(o in ooo_seq_helper){
  
  dooo_sub <- d_out_of_order %>% filter(ooo_seq_num == o)
  dooo_seq_sub <- d_ooo_seqs %>% filter(index == dooo_sub$s)
  
  # subset to rules featuring only numbers in these broken sequences
  
  split_sub <- day_05_a_split %>%
    filter(before %in% dooo_seq_sub$sequence & after %in% dooo_seq_sub$sequence) 
  
  # intuition: in a sequence of length N, the element that should be first will have the most (N-1) "before" rules
  # basically: you can order observations based on the number of before/after rules for each one
  
  x <- split_sub %>%
    group_by(before) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    ungroup() %>%
    mutate(is_midpoint = ifelse(median(count) - 0.5 == count, 1, 0)) %>%
    filter(is_midpoint == 1)
  
  temp <- as.data.frame(cbind(x, dooo_sub$s, o))
  catch_df_2 <- rbind(catch_df_2, temp)
  
}

sum(as.numeric(catch_df_2$before))