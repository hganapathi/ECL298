#load in packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(car)
library(broom)
library(patchwork)
library(glmnet)
library(stars)
library(tigris)
options(tigris_use_cache = TRUE)

install.packages("stars")
install.packages("car")
install.packages("patchwork")
install.packages("glmnet")

#set working directory
getwd()
setwd("c:/Users/hgana/Desktop/Aggregated Yield Data/Crop Yield CSVs")
getwd()

#First, we do some exploratory data plotting for yield of various tree fruits over time in California counties.
#read in data for the tree crops we're studying
apricots <- read.csv("C://Users//hgana//Desktop//Aggregated Yield Data//Apricots 1980-2022.csv")
cherries <- read.csv("C://Users//hgana//Desktop//Aggregated Yield Data//Cherries 1980-2022.csv")
olives <- read.csv("C://Users//hgana//Desktop//Aggregated Yield Data//Olives 1980-2022.csv")
grapes <- read.csv("C://Users//hgana//Desktop//Aggregated Yield Data//Grapes (table) 1980-2022.csv")
peaches <- read.csv("C://Users//hgana//Desktop//Aggregated Yield Data//Peaches 1980-2022.csv") 
nectarines <- read.csv("C://Users//hgana//Desktop//Aggregated Yield Data//Nectarines 1980-2022.csv")

# Create the plot for apricots
ggplot(apricots, aes(x = year, y = production)) +
  geom_line() +  # Line plot to show trends over time
  geom_point() + # Optionally add points for visibility
  facet_wrap(~ county, scales = "free_y") +  # Facet by County, with free y-axis scales
  labs(title = "Yield of Apricots over Time for Different Counties", x = "Year", y = "Yield (tons)") +
  theme_minimal() 

#Make a plot of yield over time by county for cherries
ggplot(cherries, aes(x = year, y = yield)) +
  geom_line() +  # Line plot to show trends over time
  geom_point() + # Optionally add points for visibility
  facet_wrap(~ county, scales = "free_y") +  # Facet by County, with free y-axis scales
  labs(title = "Yield of Cherries over Time for Different Counties", x = "Year", y = "Yield (tons/acre)") +
  theme_minimal() 
#Make a plot of yield over time by county for table grapes
ggplot(grapes, aes(x = year, y = yield)) +
  geom_line() +  # Line plot to show trends over time
  geom_point() + # Add points for visibility
  facet_wrap(~ county, scales = "free_y") +  # Facet by County, with free y-axis scales
  labs(title = "Yield of Table Grapes over Time for Different Counties", x = "Year", y = "Total Yield (tons)") +
  theme_minimal() 
#Make a plot of yield over time by county for olives
ggplot(olives, aes(x = year, y = yield)) +
  geom_line() +  # Line plot to show trends over time
  geom_point() + # Add points for visibility
  facet_wrap(~ county, scales = "free_y") +  # Facet by County, with free y-axis scales
  labs(title = "Yield of Olives over Time for Different Counties", x = "Year", y = "Total Yield (tons)") +
  theme_minimal() 
#Make a plot of yield over time by county for peaches
ggplot(peaches, aes(x = year, y = yield)) +
  geom_line() +  # Line plot to show trends over time
  geom_point() + # Add points for visibility
  facet_wrap(~ county, scales = "free_y") +  # Facet by County, with free y-axis scales
  labs(title = "Yield of Peaches over Time for Different Counties", x = "Year", y = "Total Yield (tons/acre)") +
  theme_minimal() 
#Make a plot of yield over time by county for nectarines
ggplot(nectarines, aes(x = year, y = yield)) +
  geom_line() +  # Line plot to show trends over time
  geom_point() + # Add points for visibility
  facet_wrap(~ county, scales = "free_y") +  # Facet by County, with free y-axis scales
  labs(title = "Yield of Nectarines over Time for Different Counties", x = "Year", y = "Total Yield (tons)") +
  theme_minimal() 

#From the plots above, we see that yield does seem to be a volatile variable for these tree fruits in many counties, warranting a study of an explanation for the changes in year-over-year yield.
#We've done some processing and cleaning of data to develop a dataset with yield, county, year, and various climate variables for cherries.


#read CSV with climate and yield data for 2000 to 2019 analysis
df <- read.csv("c:/Users/hgana/Desktop/Aggregated Yield Data/Crop Yield CSVs/combined_climate_data.csv")
df$county <- as.factor(df$county) #creates a fixed effects factor for counties
df$year <- as.factor(df$year)

#read in county data

#Download all US states (cartographic boundary, simplified)
states_sf <- states(cb = TRUE, year = 2024)

#Filter to California
ca_sf <- states_sf %>%
  filter(STUSPS == "CA")

#Basic plots for assessing relationships between predictors (climate variables) and yield
#First, plots for df, which contains analysis of counties from 2000 to 2019
plot(df$dormancychill, df$yield)
plot(df$bloomppt, df$yield)
plot(df$bloomtmean, df$yield)



#read CSV with climate and yield data for 1980 to 2019 analysis
df2 <- read.csv("c:/Users/hgana/Desktop/Aggregated Yield Data/Crop Yield CSVs/combined_climate_data2.csv")
df2$county <- as.factor(df2$county) #creates a fixed effects factor for counties
df2$year <- as.factor(df2$year)

#Now, we show plots for df2, which contains analysis of counties from 1980 to 2019
plot(df2$dormancychill, df2$yield)
plot(df2$bloomppt, df2$yield)
plot(df2$bloomtmean, df2$yield)



# Regressions for 2000 to 2019 analysis
# regress includes all variables for cherries
# regress2 excludes dormancytmean due to multicollinearity issues
regress <- lm(yield ~ dormancychill + bloomtmean + bloomppt + dormancytmean + county + year, data = df)
summary(regress)

regress2 <- lm(yield ~ dormancychill + bloomtmean + bloomppt + dormancytmean + county + year, data = df2)
summary(regress2)
vif(regress2)

#County-level time trends analysis
trends_regress <- lm(yield ~ dormancychill+ bloomtmean + bloomppt + dormancytmean + county + year * as.factor(county), data = df)

summary(trends_regress)

county_trends <- broom::tidy(trends_regress) %>%
  filter(str_detect(term, "year:as.factor"))


# Group by county and calculate the slope of the trend
county_trends_summary <- df %>%
  group_by(county) %>%
  do(trends_regress = lm(yield ~ year, data = .)) %>%
  broom::tidy(model) %>%
  filter(term == "year") %>%
  select(county, trend_slope = estimate)


# Basic assessment of residuals and q-q etc. 
par(mfrow = c(2, 2))  # arrange 4 plots
plot(regress)
plot(regress2)


# effect sizes
# Tidy the model output
regress_tidy <- tidy(regress)

#Predict the model

# Add predictions to your data
df$predicted <- predict(regress)
df2$predicted <- predict(regress2)

# Scatter plot of actual vs. predicted
# 2000 - 2019 analysis
ggplot(df, aes(x = predicted, y = yield)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "longdash", size = 2) +
  theme_minimal() +
  labs(title = "Actual vs. Predicted Yield for Analysis 2", x = "Predicted Yield (tons/acre)", y = "Actual Yield (tons/acre)")

# 1980-2019 analysis
ggplot(df2, aes(x = predicted, y = yield)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "darkblue", linetype = "longdash", size = 2) +
  theme_minimal() +
  labs(title = "Actual vs. Predicted Yield for Analysis 2", x = "Predicted Yield (tons/acre)", y = "Actual Yield (tons/acre)")


# Basic plot
ggplot(df, aes(x = x, y = yield)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
  theme_minimal()

# effect of each variable 

predictors <- c("dormancychill", "bloomtmean", "bloomppt", "dormancytmean")

for (var in predictors) {
  p <- ggplot(df, aes(x = .data[[var]], y = yield)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
    theme_minimal() +
    labs(title = paste("Yield vs.", var),
         x = var,
         y = "Yield")
  
    print(p)
}

plots <- lapply(predictors, function(var) {
  ggplot(df, aes(x = .data[[var]], y = yield)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
    theme_minimal() +
    labs(title = paste("Yield vs.", var), x = var, y = "Yield")
})

# Arrange plots in a 2x2 grid
(plots[[1]] | plots[[2]]) /
  (plots[[3]] | plots[[4]])

# Create a list of plots
plots <- lapply(predictors, function(var) {
  ggplot(df, aes(x = .data[[var]], y = yield)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink") +
    theme_minimal() +
    labs(title = paste("Yield vs.", var), x = var, y = "Yield (tons/acre)")
})

# Combine plots in 2x2 layout
(plots[[1]] | plots[[2]]) /
  (plots[[3]] | plots[[4]])

# Create a list of plots
plots <- lapply(predictors, function(var) {
  ggplot(df2, aes(x = .data[[var]], y = yield)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
    theme_minimal() +
    labs(title = paste("Yield vs.", var), x = var, y = "Yield (tons/acre)")
})

# Combine plots in 2x2 layout
(plots[[1]] | plots[[2]]) /
  (plots[[3]] | plots[[4]])

# Plot coefficients (excluding intercept if desired)
ggplot(regress_tidy, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
  theme_minimal() +
  labs(title = "Regression Coefficients with 95% CI", x = "Estimate", y = "Term")

# just regresses yields on dormancy chill hours for 2000 to 2019 
regresschill <- lm(yield ~ dormancychill, data = df)
summary(regresschill)
plot(df$dormancychill, df$yield)

# Regressions for 1980 to 2019 anaylsis
# regress3 includes all variables for cherries
# regress4 excludes dormancytmean due to multicollinearity issues
regress3 <- lm(yield ~ dormancychill + bloomtmean + bloomppt + dormancytmean + county, data = df2)
summary(regress3)

regress4 <- lm(yield ~ dormancychill + bloomtmean + bloomppt + county, data = df2)
summary(regress2)
vif(regress)




