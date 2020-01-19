
############################################ A ############################################

library(ggplot2)
library(dplyr)
library(gapminder)

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) + 
  geom_point() +
  facet_wrap(~ year, nrow = 3, ncol = 4) + 
  scale_x_log10()

############################################ B ############################################

airquality %>%
  pivot_longer(1:4, names_to = "Measure", values_to = "Value") %>%
  ggplot(aes(x = Day, y = Value, color = Measure)) + 
  geom_point(na.rm = T) + 
  geom_line() + 
  facet_grid(Measure ~ Month, scales = "free")

############################################ C ############################################

set.seed(21)

df <- data.frame(levels = factor(rep(c("A", "B"), each = 200)), 
                 value = c(rnorm(200), rnorm(200, mean = .8)))

df %>%
  ggplot(aes(x = value)) + 
  geom_density(aes(fill = levels), alpha = .7)


diamonds %>%
  ggplot(aes(x = price)) + 
  geom_density(aes(fill = color), alpha = .2) 


diamonds %>%
  ggplot(aes(x = depth)) + 
  geom_density(aes(fill = cut), alpha = .4) +
  facet_wrap(~ color, nrow = 4, ncol = 2)
