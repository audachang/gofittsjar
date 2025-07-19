require(dplyr)
require(tidyr)

A <- c(100,500)
W <- c(10,20,30)

df <- tibble(A =rep(A,times=1, each=3), W = c(W, W))
df <- df %>%
  mutate(ID = log2(A/W+1))

# Sort df by the 'ID' column in ascending order
sorted_df <- df[order(df$ID), ]

