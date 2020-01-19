
############################################ A ############################################

library(ggplot2)
library(dplyr)


iris_long <- cbind(1:150, iris) %>%
  gather(Measurement, Value, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
  separate(Measurement, into = c("Part", "Measurement"))

iris_long$`Part` <- as.factor(iris_long$`Part`)
iris_long$`Measurement` <- as.factor(iris_long$`Measurement`)

iris_long <- pivot_wider(iris_long, names_from = "Measurement", values_from = "Value") %>%
  select(2:5)


iris_long %>%  
  ggplot(aes(x = Width, y = Length, color = Species, size = Part, alpha = 0.5)) +
  geom_point()

############################################ B ############################################

library(gapminder)

gapminder <- gapminder

# LifeExp for 2007

gapminder %>%
  filter(year == 2007) %>% 
  ggplot(aes(gdpPercap, lifeExp, colour = continent, size = pop)) +
  geom_point() + 
  scale_x_log10() 

# Scatter+Line at one plot for mean LifeExp

gapminder %>%
  group_by(continent, year) %>%
  summarise(meanlifeExp = mean(lifeExp)) %>%
  ggplot(aes(x = year, y = meanlifeExp, color = continent)) +
  geom_line() + 
  geom_point(shape = 21, size = 2, aes(fill = continent)) +
  ylim(0, 80)

# BAR PLOTS

# Population in Oceania from 1970 to 2000 by countries

gapminder %>%
  filter(continent == "Oceania", year >= 1970, year <= 2000) %>%
  group_by(pop, year, country) %>%
  ggplot(aes(x = year, y = pop, fill = country)) +
  geom_bar(stat = "identity", position = "dodge", width = 3.5) +
  scale_y_log10() +
  theme_minimal()

# Mean life expetancy through years in Americas

gapminder %>%
  filter(continent == "Americas") %>%
  group_by(year) %>%
  summarise(mean_lifeExp = mean(as.numeric(lifeExp)), sd_lifeExp = sd(as.numeric(lifeExp))) %>%
  ggplot(aes(x = year, y = mean_lifeExp, ymin = (mean_lifeExp - sd_lifeExp),
             ymax = (mean_lifeExp + sd_lifeExp))) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_errorbar(width = 1, color = "navy") +
  coord_flip() +
  theme_minimal()

# Afghanistan's GDP per capita over years

gapminder %>%
  filter(country == "Afghanistan") %>%
  group_by(gdpPercap, year) %>%
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_bar(stat = "identity", fill = "darkolivegreen1", color = "darkolivegreen4") +
  theme_minimal()

