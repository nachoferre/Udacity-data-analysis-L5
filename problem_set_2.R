stone <- diamonds
library(ggplot2)
ggplot(stone, aes(x = x, y = price)) + 
  geom_point() +
  geom_smooth()

with( stone, cor.test(x, price))
with( stone, cor.test(y, price))
with( stone, cor.test(z, price))


ggplot(stone, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(0,100, 2))

with( stone, cor.test(depth, price))

ggplot(stone, aes(x = carat, y = price)) + 
  geom_point(shape = 1) +
  xlim(c(0, quantile(stone$carat, 0.99))) +
  ylim(c(0, quantile(stone$price, 0.99)))


stone$volume = stone$x * stone$y * stone$z
ggplot(stone, aes(x = volume, y = price)) + 
  geom_point(shape = 1) 

with( subset(stone, volume < 800 & volume > 0), cor(volume, price))

stone_clear <- subset(stone, volume < 800 & volume > 0)

ggplot(stone_clear, aes(x = volume, y = price)) + 
  geom_point(shape = 1) +
  geom_smooth()

library(dplyr)
diamondsByClarity <- stone_clear %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n()) %>%
  arrange(clarity)

library(gridExtra)
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

p1 <- ggplot(diamonds_mp_by_clarity, aes(x = clarity, y = mean_price)) + 
  geom_bar(stat="identity")
p2 <- ggplot(diamonds_mp_by_color, aes(x = color, y = mean_price)) + 
  geom_bar(stat="identity")

grid.arrange(p1, p2, ncol = 2)











