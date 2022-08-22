# install.packages("readr", repos = "http://cran.us.r-project.org")
# install.packages("stringr", repos = "http://cran.us.r-project.org")
install.packages("tidyverse")
install.packages("plotly", repos = "http://cran.us.r-project.org")
install.packages("scales")
install.packages("styler", repos = "http://cran.us.r-project.org")
install.packages("kableExtra")
install.packages("Hmisc")

library(readr)
library(stringr)
library(ggplot2)
library(plotly)
library(dplyr)
library(magrittr)
library(scales)
library(styler)
library(kableExtra)

urlfile <- "https://raw.githubusercontent.com/Bioinformatics-Research-Network/skill-assessments/main/R%20for%20Data%20Science/gapminder_clean.csv"

mydata <- read_csv(url(urlfile))

names(mydata) <- str_replace_all(names(mydata), c(" " = "."))

# names(mydata) <-str_replace_all(names(mydata), c(" " = ".", '\\(|\\)' = "/"))


mydata_1962 <- mydata %>%
  filter(Year == 1962)

ggplot(mydata_1962, aes(x = `CO2.emissions.(metric.tons.per.capita)`, y = gdpPercap)) +
  geom_point() +
  ggtitle("Year 1962") +
  labs(y = "GDP per capita", x = "CO2 emissions (metric tons per capita)")

# cor(mydata_1962$`CO2.emissions.(metric.tons.per.capita)`, mydata_1962$gdpPercap, use = "complete.obs")

cor.test(mydata_1962$`CO2.emissions.(metric.tons.per.capita)`, mydata_1962$gdpPercap, )

all_years <- unique(mydata$Year)

year_cor_co2_gdp <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Year", "Correlation"))

for (year in all_years) {
  subset <- mydata %>%
    filter(Year == year)
  correlation <- cor(subset$`CO2.emissions.(metric.tons.per.capita)`, subset$gdpPercap, use = "complete.obs")
  year_cor_co2_gdp[nrow(year_cor_co2_gdp) + 1, ] <- c(year, correlation)
}

year_cor_co2_gdp[order(year_cor_co2_gdp$Correlation, decreasing = TRUE), ]

mydata_1967 <- mydata %>%
  filter(Year == 1967)

plot_1967 <- ggplot(mydata_1967, aes(x = `CO2.emissions.(metric.tons.per.capita)`, y = gdpPercap)) +
  geom_point(aes(size = pop, colour = continent)) +
  ggtitle("Year 1967") +
  labs(y = "GDP per capita", x = "CO2 emissions (metric tons per capita)")

ggplotly(plot_1967)

mydata_1967$continent

one_way_anova_1967 <- aov(`Energy.use.(kg.of.oil.equivalent.per.capita)` ~ continent, data = mydata_1967)

summary(one_way_anova_1967)

# p-value is low (p < 0.001), it appears that depending on the continent, there is a difference in energy use

mydata_1990 <- mydata %>%
  filter(Year > 1990) %>%
  filter(continent == "Europe" | continent == "Asia")

one_way_anova_1990 <- aov(`Imports.of.goods.and.services.(%.of.GDP)` ~ continent, data = mydata_1967)

summary(one_way_anova_1990)

# There is no significant difference between Europe and Asia with respect to
#' Imports of goods and services (% of GDP)' in the years after 1990

pop_dens_avg <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Country", "Average.population.density"))

all_countries <- unique(mydata$Country.Name)

for (country in all_countries) {
  subset <- mydata %>%
    filter(Country.Name == country)
  average <- mean(as.numeric(subset$`Population.density.(people.per.sq..km.of.land.area)`), na.rm = TRUE)
  pop_dens_avg[nrow(pop_dens_avg) + 1, ] <- c(country, average)
}

pop_dens_avg$Average.population.density <- as.numeric(as.character(pop_dens_avg$Average.population.density))

pop_dens_avg <- pop_dens_avg[order(pop_dens_avg$Average.population.density, decreasing = TRUE), ]

head(pop_dens_avg)

# Macao region in China has the highest 'Population density (people per sq. km of land area)' across all years

head(all_years, n = 1)

tail(all_years, n = 1)

mydata_2007 <- mydata %>%
  filter(Year == 2007)

exp_increase <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Country", "Life.expectancy.increase.numerical", "Life.expectancy.increase.percentage"))

for (country in all_countries) {
  print(country)
  subset_1962 <- mydata_1962 %>%
    filter(Country.Name == country)
  subset_2007 <- mydata_2007 %>%
    filter(Country.Name == country)
  increase_num <- subset_2007$`Life.expectancy.at.birth,.total.(years)` - subset_1962$`Life.expectancy.at.birth,.total.(years)`
  increase_perc <- round(subset_2007$`Life.expectancy.at.birth,.total.(years)` / subset_1962$`Life.expectancy.at.birth,.total.(years)` * 100, digits = 1)
  if (length(increase_num) == 0) {
    increase_num <- NA
    increase_perc <- NA
  }
  print(increase_num)
  print(increase_perc)
  exp_increase[nrow(exp_increase) + 1, ] <- c(country, increase_num, increase_perc)
}

exp_increase$Life.expectancy.increase.numerical <- as.numeric(as.character(exp_increase$Life.expectancy.increase.numerical))

exp_increase$Life.expectancy.increase.percentage <- as.numeric(as.character(exp_increase$Life.expectancy.increase.percentage))

head(exp_increase[order(exp_increase$Life.expectancy.increase.numerical, decreasing = TRUE), ])


# In the Maldives life expectancy has grown 37 years, what is a growth of 196%.
# Bhutan life expectancy has grown 33 years, what is over 200%.

plot_1962 <- ggplot(mydata_1962, aes(x=`CO2.emissions.(metric.tons.per.capita)` ,y=gdpPercap)) + 
  geom_point(color = "firebrick") + 
  ggtitle("Year 1962") + 
  labs(y= "GDP per capita", x = expression("CO"[2]*" emissions (metric tons per capita)")) +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15))

print(plot_1962)

sum(is.na(mydata_1962$`CO2.emissions.(metric.tons.per.capita)`))
