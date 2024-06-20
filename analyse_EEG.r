library(tidyverse)

eeg <- read_delim("eegAbsolventenfeier-20240620-122950.csv", col_names = F) %>%
  select(2:9, 24) %>%
  mutate(across(X2_x9))