###### here's a commit of the first day solution from RStudio, made @ 13:07 ET


### load packages, just dplyr today

library(dplyr)


### read in da file

day_01 <- read.csv("/Users/seankuhn/Downloads/aoc_2024_day_01.csv")


### part 1: order each vector, calc sum of abs diff.

dc1 <- day_01 %>%
  select(c1) %>%
  arrange(c1)

dc2 <- day_01 %>%
  select(c2) %>%
  arrange(c2)

d1 <- cbind(dc1, dc2)
d1$diff <- with(d1, abs(c1-c2))
print(paste0("the answer to part 1 is ", sum(d1$diff)))


### part 2: figure out how often each number from the left list appears in the right list 
# and calc. "similarity score" as {value} * {# appearances}
# and then sum the row-wise similarity scores

dc1$index <- seq(1, nrow(dc1), 1)

d_catch <- data.frame()

for(d in dc1$index){
  
  dc1_sub <- filter(dc1, index == d)
  check_value <- dc1_sub$c1[1]
  dc2$check_value <- check_value
  dc2$eval <- with(dc2, ifelse(c2 == check_value, 1, 0))
  output <- sum(dc2$eval)
  d_catch <- rbind(d_catch, output)
  
}

dc1$matches <- d_catch$X0
dc1$product <- with(dc1, c1 * matches)
print(paste0("the answer to part 2 is ", sum(dc1$product)))
