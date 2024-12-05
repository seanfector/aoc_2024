### load packages

library(dplyr)
library(tidyr)
library(stringr)
library(sqldf)


### read in da file

day_04 <- read.csv("/Users/seankuhn/Downloads/aoc_2024_day_04.csv")

### part 1

# XMAS can be written horizontal, vertical, diagonal, written backwards, or even overlapping other words

seq_col <- seq(1, 140, 1)
seq_row <- seq(1, 140, 1)

catch_df <- data.frame()

for(r in seq_row){
  
  sub_row <- day_04 %>% filter(index == r)
  
  for(c in seq_col){
    
    sub_char <- substr(sub_row$input, c, c)
    temp_df <- as.data.frame(cbind(r, c, sub_char))
    catch_df <- rbind(catch_df, temp_df)
    
  }
  
}

x <- catch_df

### rows: loop through on r 1:140, filter, paste w/ collapse, look for XMAS and SAMX

catch_row <- data.frame()

for(i in seq_row){
  
  x_sub <- filter(x, r == i)
  x_flat <- paste(x_sub$sub_char, collapse = "")
  count_xmas <- str_count(x_flat, "XMAS")
  count_samx <- str_count(x_flat, "SAMX")
  count_total <- count_xmas + count_samx
  temp_df <- as.data.frame(cbind(i, count_total))
  catch_row <- rbind(catch_row, temp_df)
  
}

sum(catch_row$count_total)

### cols: loop through on c 1:140, filter, paste w/ collapse, look for XMAS and SAMX

catch_col <- data.frame()

for(i in seq_col){
  
  x_sub <- filter(x, c == i)
  x_flat <- paste(x_sub$sub_char, collapse = "")
  count_xmas <- str_count(x_flat, "XMAS")
  count_samx <- str_count(x_flat, "SAMX")
  count_total <- count_xmas + count_samx
  temp_df <- as.data.frame(cbind(i, count_total))
  catch_col <- rbind(catch_col, temp_df)
  
}

sum(catch_col$count_total)

### NW to SE diagonals

seq_diag <- seq(-140, 140, 1)

d_starting_points <- as.data.frame(cbind(seq(-140, 140, 1), rep(1, 281)))
colnames(d_starting_points) <- c("row", "column")

x$r <- as.numeric(x$r)
x$c <- as.numeric(x$c)

catch_nw_se <- data.frame()

for(d in seq_diag){
  
  start_at <- d_starting_points %>% filter(row == d)
  
  d_additions <- as.data.frame(cbind(seq(0,139,1),seq(0,139,1)))
  colnames(d_additions) <- c("row_add", "col_add")
  
  d_additions$start_row <- start_at$row
  d_additions$start_col <- start_at$column
  
  d_additions$row <- with(d_additions, start_row + row_add)
  d_additions$col <- with(d_additions, start_col + col_add)
  
  x_sub <- inner_join(x, d_additions, by = c("r" = "row", "c" = "col"))
  x_flat <- paste(x_sub$sub_char, collapse = "")
  count_xmas <- str_count(x_flat, "XMAS")
  count_samx <- str_count(x_flat, "SAMX")
  count_total <- count_xmas + count_samx
  temp_df <- as.data.frame(cbind(d, count_total))
  catch_nw_se <- rbind(catch_nw_se, temp_df)
  
}

sum(catch_nw_se$count_total)


### SW to NE diagonals

seq_diag <- seq(1, 280, 1)

d_starting_points <- as.data.frame(cbind(seq(1,280,1), rep(1,280)))
colnames(d_starting_points) <- c("row", "column")

catch_sw_ne <- data.frame()

for(d in seq_diag){
  
  start_at <- d_starting_points %>% filter(row == d)
  
  d_additions <- as.data.frame(cbind(seq(0, -139, -1), seq(0, 139, 1)))
  colnames(d_additions) <- c("row_add", "col_add")
  
  d_additions$start_row <- start_at$row
  d_additions$start_col <- start_at$column
  
  d_additions$row <- with(d_additions, start_row + row_add)
  d_additions$col <- with(d_additions, start_col + col_add)
  
  x_sub <- inner_join(x, d_additions, by = c("r" = "row", "c" = "col"))
  x_flat <- paste(x_sub$sub_char, collapse = "")
  count_xmas <- str_count(x_flat, "XMAS")
  count_samx <- str_count(x_flat, "SAMX")
  count_total <- count_xmas + count_samx
  temp_df <- as.data.frame(cbind(d, count_total))
  catch_sw_ne <- rbind(catch_sw_ne, temp_df)
  
}

sum(catch_sw_ne$count_total)

a <- sum(catch_sw_ne$count_total) + 
  sum(catch_nw_se$count_total) +
  sum(catch_row$count_total) +
  sum(catch_col$count_total)

print(paste0("the answer to part 1 is ", a))


### part 2: come on man lol

# repurpose the diagonal code to identify the location of each diagonal SAM / MAS

### NW to SE diagonals

seq_diag <- seq(-140, 140, 1)

d_starting_points <- as.data.frame(cbind(seq(-140, 140, 1), rep(1, 281)))
colnames(d_starting_points) <- c("row", "column")

catch_grid_nw_se <- data.frame()

for(d in seq_diag){
  
  start_at <- d_starting_points %>% filter(row == d)
  
  d_additions <- as.data.frame(cbind(seq(0,139,1),seq(0,139,1)))
  colnames(d_additions) <- c("row_add", "col_add")
  
  d_additions$start_row <- start_at$row
  d_additions$start_col <- start_at$column
  
  d_additions$row <- with(d_additions, start_row + row_add)
  d_additions$col <- with(d_additions, start_col + col_add)
  
  x_sub <- inner_join(x, d_additions, by = c("r" = "row", "c" = "col"))
  
  x_sub_a <- x_sub %>%
    mutate(vertex = ifelse(is.na(lag(sub_char, 1)), "ZZZ",
                                 ifelse(is.na(lead(sub_char, 1)), "ZZZ",
                                              paste0(lag(sub_char, 1), sub_char, lead(sub_char, 1)))),
           count_it = ifelse(vertex %in% c("SAM", "MAS"), 1, 0)) %>%
    select(r, c, sub_char, vertex, count_it) %>%
    mutate(seq_diag = d)

  catch_grid_nw_se <- rbind(catch_grid_nw_se, x_sub_a)
  
}

### SW to NE diagonals

seq_diag <- seq(1, 280, 1)

d_starting_points <- as.data.frame(cbind(seq(1,280,1), rep(1,280)))
colnames(d_starting_points) <- c("row", "column")

catch_grid_sw_ne <- data.frame()

for(d in seq_diag){
  
  start_at <- d_starting_points %>% filter(row == d)
  
  d_additions <- as.data.frame(cbind(seq(0, -139, -1), seq(0, 139, 1)))
  colnames(d_additions) <- c("row_add", "col_add")
  
  d_additions$start_row <- start_at$row
  d_additions$start_col <- start_at$column
  
  d_additions$row <- with(d_additions, start_row + row_add)
  d_additions$col <- with(d_additions, start_col + col_add)
  
  x_sub <- inner_join(x, d_additions, by = c("r" = "row", "c" = "col"))
  
  x_sub_a <- x_sub %>%
    mutate(vertex = ifelse(is.na(lag(sub_char, 1)), "ZZZ",
                           ifelse(is.na(lead(sub_char, 1)), "ZZZ",
                                  paste0(lag(sub_char, 1), sub_char, lead(sub_char, 1)))),
           count_it = ifelse(vertex %in% c("SAM", "MAS"), 1, 0)) %>%
    select(r, c, sub_char, vertex, count_it) %>%
    mutate(seq_diag = d)
  
  catch_grid_sw_ne <- rbind(catch_grid_sw_ne, x_sub_a)
  
}

sub_sw_ne <- catch_grid_sw_ne %>% filter(count_it == 1)
sub_nw_se <- catch_grid_nw_se %>% filter(count_it == 1)

x_combine <- inner_join(sub_sw_ne, sub_nw_se, by = c("r" = "r", "c" = "c"))

print(paste0("the answer to part 2 is ", nrow(x_combine)))