pacman::p_load(tidyverse)

agent <- c("A","A","A","A","B","B","B","B")
box <- c(1,2,3,4,1,2,3,4)

empty_matrix <- tibble( agent = agent,
                        box = box,
                        outcome = as.numeric(rep(0,8)))

write_csv(empty_matrix, "empty_matrix.csv")

