library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(gmodels)
library(sqldf)
library(ggpubr)
library(GGally)
wine <- read.csv("/Users/jillienchu/Desktop/ALY6010/Project/Wine_tasting.csv", header = TRUE) #Importing dataset
head(wine)
wine$price[is.na(wine$price)] <- mean(wine$price, na.rm = TRUE)
wine$X <- NULL
wine$description <- NULL
wine$designation <- NULL
wine$region_1 <- NULL
wine$region_2 <- NULL
wine$taster_name <- NULL
wine$taster_twitter_handle <- NULL
wine
West_World = c("US", "Mexico", "Argentina", "Chile", "Canada")
wine$Direction <- ifelse(wine$country %in% West_World,"West World", "East World")
west <- sqldf("SELECT country, province, points, price, variety, winery, Direction from wine where Direction like '%West World%'")
east <- sqldf("SELECT country, province, points, price, variety, winery, Direction from wine where Direction like '%East World%'")
summary(wine)
str(wine)

wine %>% group_by(Direction) %>% summarise(n = n(), min = min(points), q1 = quantile(points, 0.25), median = median(points), mean = mean(points), sd = sd(points), q3 = quantile(points, 0.75), max = max(points))
wine %>% group_by(Direction) %>% summarise(n = n(), min = min(price), q1 = quantile(price, 0.25), median = median(price), mean = mean(price), sd = sd(price), q3 = quantile(price, 0.75), max = max(price))

ggboxplot(wine, x = "Direction", y = "price", ylab = "Wine Price", xlab = "Directions", color = "Direction", title = "Box Plot Price by Direction", ggtheme = theme_minimal())
ggboxplot(wine, x = "Direction", y = "points", ylab = "Wine Rating", xlab = "Directions", color = "Direction", title = "Box Plot Rating by Direction", ggtheme = theme_minimal())

#Checking for which test to conduct - does the average wine points differ from 85 points?
ggboxplot(wine$points, ylab = "Wine Points", xlab = FALSE, title = "Box Plot for Wine Points", ggtheme = theme_minimal())
ggqqplot(wine$points, title = "Q-Q Plot for Wine Points", ylab = "Wine Points", ggtheme = theme_minimal())
shapiro.test(wine$points)
test3 <- wilcox.test(wine$points, mu = 85)
test3

#Checking for which test to conduct for difference of price between East and West World wines - Is the mean average wine price same for East and West World
ggboxplot(wine, x = "Direction", y = "price", ylab = "Wine Price", xlab = "Directions", color = "Direction", title = "Box Plot Price by Direction", ggtheme = theme_minimal())
ggqqplot(wine$price, title = "Q-Q Plot for Price", ylab = "Wine Price", ggtheme = theme_minimal())
with(wine, shapiro.test(price[Direction == "West World"]))
with(wine, shapiro.test(price[Direction == "East World"]))
var.test(price ~ Direction, data = wine)

#Wilcox Test - not normally distributed
test1 <- wilcox.test(price ~ Direction, data = wine, exact = FALSE)
test1

#checking for which test to conduct for difference of ratings between East and West World wines - Is the mean average wine rating of East World same as the average wine rating of West World
ggboxplot(wine, x = "Direction", y = "points", ylab = "Wine Rating", xlab = "Directions", color = "Direction", title = "Box Plot Rating by Direction", ggtheme = theme_minimal())
ggqqplot(wine$points, title = "Q-Q Plot for Wine Rating", ylab = "Wine Rating", ggtheme = theme_minimal())
with(wine, shapiro.test(points[Direction == "West World"]))
with(wine, shapiro.test(points[Direction == "East World"]))
var.test(points ~ Direction, data = wine)

#Wilcox Test - not normally distributed
test2 <- wilcox.test(points ~ Direction, data = wine, exact = FALSE)
test2


summary(east)
summary(west)
ggcorr(wine %>% mutate_if(is.factor, as.numeric), label = TRUE)

#Final Project Test 1

wine %>% group_by(points) %>% summarise(n = n(), min = min(price), q1 = quantile(price, 0.25), median = median(price), mean = mean(price), sd = sd(price), q3 = quantile(price, 0.75), max = max(price))
ggboxplot(wine, x = "points", y = "price", ylab = "Wine Price", xlab = "Wine Points", color = "points", title = "Box Plot: Wine Price by Wine Points", ggtheme = theme_minimal())
plot3 <- ggqqplot(wine$price, title = "Q-Q Plot for Wine Prices", ylab = "Wine Price", ggtheme = theme_minimal())
plot3
shapiro.test(wine$price)
shapiro.test(wine$points)
cor(wine$price, wine$points)
wine %>% ggplot(aes(x = points, y = price)) + geom_point(alpha=0.7) + geom_smooth(method="lm") + labs(title = "Wine price and their points")
model1 <- lm(price ~ points, data = wine)
summary(model1)

#Final Project Test 2
wine %>% group_by(Direction) %>% summarise(n = n(), min = min(price), q1 = quantile(price, 0.25), median = median(price), mean = mean(price), sd = sd(price), q3 = quantile(price, 0.75), max = max(price))
ggboxplot(wine, x = "Direction", y = "price", ylab = "Wine Price", xlab = "Direction", color = "Direction", title = "Box Plot: Wine Price by Direction", ggtheme = theme_minimal())
ggqqplot(wine$price, title = "Q-Q Plot for Price", ylab = "Wine Price", ggtheme = theme_minimal())
wine$DirectionWorld <- ifelse(wine$Direction == "East World", 1, 0)
shapiro.test(wine$price)
shapiro.test(wine$DirectionWorld)
cor(wine$price, wine$DirectionWorld)
#Scatterplot with regression lines
wine %>% ggplot(aes(x = DirectionWorld, y = price)) + geom_point(alpha=0.7) + geom_smooth(method="lm") + labs(title = "Wine price and the Direction")
model2 <- lm(price ~ DirectionWorld, data = wine)
summary(model2)


#Final Project Test 3

wine %>% group_by(Direction) %>% summarise(n = n(), min = min(points), q1 = quantile(points, 0.25), median = median(points), mean = mean(points), sd = sd(points), q3 = quantile(points, 0.75), max = max(points))
ggboxplot(wine, x = "Direction", y = "points", ylab = "Wine Points", xlab = "Direction", color = "Direction", title = "Box Plot: Wine Points by Direction", ggtheme = theme_minimal())
ggqqplot(wine$points, title = "Q-Q Plot for Wine Rating", ylab = "Wine Rating", ggtheme = theme_minimal())
shapiro.test(wine$points)
shapiro.test(wine$DirectionWorld)
cor(wine$points, wine$DirectionWorld)
model3 <- lm(points ~ Direction, data = wine)
summary(model3)
#Scatterplot with regression line
wine %>% ggplot(aes(x = DirectionWorld, y = points)) + geom_point(alpha=0.7) + geom_smooth(method="lm") + labs(title = "Wine points and the Direction")

