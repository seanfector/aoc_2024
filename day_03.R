### load packages

library(dplyr)
library(tidyr)
library(stringr)
library(sqldf)


### read in da file

day_03 <- read.csv("/Users/seankuhn/Downloads/aoc_2024_day_03.csv")

### part 1

help_seq <- seq(1, nrow(day_03), 1)
catch_vector <- vector()

for(s in help_seq){

  s1 <- day_03$output[s]
  
  x1_open <- as.data.frame(str_locate_all(s1, "mul\\("))
  x1_close <- as.data.frame(str_locate_all(s1, "\\)"))
  
  x1_oc <- sqldf("
                  WITH
                  
                  all_pairs AS (
                  
                  SELECT
                    x1_open.start AS open_start,
                    x1_open.end AS open_end,
                      x1_close.start AS close,
                        ROW_NUMBER() OVER (PARTITION BY x1_open.start, x1_open.end ORDER BY x1_close.start) AS rn
                  FROM x1_open
                    JOIN x1_close ON x1_close.start > x1_open.end
                    
                  )
                  
                  SELECT *
                  FROM all_pairs
                  WHERE rn = 1
                  ;
                  
               ")
  
  x1_oc$s1 <- s1
  x1_oc$extract <- substr(x1_oc$s1, x1_oc$open_start, x1_oc$close)
  x1_oc$extract_sub <- substr(x1_oc$extract, 5, nchar(x1_oc$extract)-1)
  
  x1_oc <- x1_oc %>%
    separate_wider_delim(cols = extract_sub, 
                         delim = ",", 
                         names = c("pre", "post"),
                         too_few = "debug",
                         too_many = "debug") %>%
    mutate(obviously_busted = ifelse(extract_sub_pieces != 2, 1, 0),
           pre_check = str_detect(pre, "^[:digit:]+$"),
           post_check = str_detect(post, "^[:digit:]+$"),
           total_check = ifelse(obviously_busted == 1, 0,
                                ifelse(pre_check == 0, 0,
                                       ifelse(post_check == 0, 0, 1))),
           product = ifelse(total_check == 0, 0, as.numeric(pre) * as.numeric(post)))
  
  temp <- sum(x1_oc$product)
  catch_vector <- rbind(catch_vector, temp)
  
}

print(paste0("the answer to part 1 is ", sum(catch_vector)))

### part 2

# basically i think the way to do this is to find the intervals between a don't() and a do()
# and then filter those observations OUT of the base table
# but otherwise run the same basic playbook

# maybe let's start by flattening this into a single string, because i bet do()/dont() persist across "rows"

long_string <- paste0(day_03$output[1],
                      day_03$output[2],
                      day_03$output[3],
                      day_03$output[4],
                      day_03$output[5],
                      day_03$output[6])

x1_open <- as.data.frame(str_locate_all(long_string, "mul\\("))
x1_close <- as.data.frame(str_locate_all(long_string, "\\)"))

x1_oc <- sqldf("
                  WITH
                  
                  all_pairs AS (
                  
                  SELECT
                    x1_open.start AS open_start,
                    x1_open.end AS open_end,
                      x1_close.start AS close,
                        ROW_NUMBER() OVER (PARTITION BY x1_open.start, x1_open.end ORDER BY x1_close.start) AS rn
                  FROM x1_open
                    JOIN x1_close ON x1_close.start > x1_open.end
                    
                  )
                  
                  SELECT *
                  FROM all_pairs
                  WHERE rn = 1
                  ;
                  
               ")

x1_dont <- as.data.frame(str_locate_all(long_string, "don\\'t\\(\\)"))
x1_do <- as.data.frame(str_locate_all(long_string, "do\\(\\)"))

x1_dodo <- sqldf("
                      WITH
                      
                      all_pairs AS (
                      
                      SELECT
                        x1_dont.start AS start_dont,
                          x1_do.end AS end_do,
                            ROW_NUMBER() OVER (PARTITION BY x1_dont.start ORDER BY x1_do.end) AS rn
                      FROM x1_dont
                          LEFT JOIN x1_do ON x1_do.start > x1_dont.end
                          
                      )
                      
                      SELECT *
                      FROM all_pairs
                      WHERE rn = 1
                   ")

x1_dodo_clean <- x1_dodo %>%
  group_by(end_do) %>%
  mutate(sub_rn = row_number()) %>%
  filter(sub_rn == 1)

x1_oc_a <- sqldf("
                    SELECT
                      x1_oc.*,
                        x1_dodo_clean.start_dont,
                        x1_dodo_clean.end_do
                    FROM x1_oc
                        LEFT JOIN x1_dodo_clean ON (x1_oc.open_start BETWEEN x1_dodo_clean.start_dont AND x1_dodo_clean.end_do)
                 ")

x1_oc_a_s <- x1_oc_a %>%
  filter(is.na(start_dont))

x1_oc_a_s$s1 <- long_string
x1_oc_a_s$extract <- substr(x1_oc_a_s$s1, x1_oc_a_s$open_start, x1_oc_a_s$close)
x1_oc_a_s$extract_sub <- substr(x1_oc_a_s$extract, 5, nchar(x1_oc_a_s$extract)-1)

x1_oc_a_s <- x1_oc_a_s %>%
  separate_wider_delim(cols = extract_sub, 
                       delim = ",", 
                       names = c("pre", "post"),
                       too_few = "debug",
                       too_many = "debug") %>%
  mutate(obviously_busted = ifelse(extract_sub_pieces != 2, 1, 0),
         pre_check = str_detect(pre, "^[:digit:]+$"),
         post_check = str_detect(post, "^[:digit:]+$"),
         total_check = ifelse(obviously_busted == 1, 0,
                              ifelse(pre_check == 0, 0,
                                     ifelse(post_check == 0, 0, 1))),
         product = ifelse(total_check == 0, 0, as.numeric(pre) * as.numeric(post)))

print(paste0("the answer to part 2 is ", sum(x1_oc_a_s$product)))
