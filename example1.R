set.seed(42)
library(tidyverse)

iris2 <- iris %>% 
  select(-Species)

head(iris2)

wkmeans(data = iris2, k = 3)