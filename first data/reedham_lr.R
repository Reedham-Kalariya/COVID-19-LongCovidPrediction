library(tidyverse)
rm(list = ls())
df = iris

df = df %>%
  mutate(Species = as.character(Species) == 'versicolor')

mod = glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
          family = binomial(),
          data = df)

summary(mod)
exp(coef(mod))
plot(mod)
