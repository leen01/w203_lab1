library(car)
library(dplyr)  
library(ggplot2)
library(tidyverse)
library(GGally)
library(magrittr)
file <-"~/Desktop/happiness_whr_1.csv"
happiness_df<-read.csv(file)
happiness_df[is.na(happiness_df)] = 0
summary(happiness_df)

# find the most correlated variables of life.ladder
ggcorr(happiness_df, label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)

# classifying High and Low GDP based on happiness score mean
happiness_df$classification = ifelse(happiness_df$Log.GDP.per.capita> mean(happiness_df$Log.GDP.per.capita),'High GDP','Low GDP')
#head(happiness_df)

# country vs happiness score sorted 
happiness_df <- happiness_df %>% select(Country.name , Life.Ladder,Log.GDP.per.capita,classification)
#head(happiness_df) 

# examine the relationship between life ladder and log GDP
boxplot(happiness_df$Life.Ladder  ~  happiness_df$Country.name)

#the boxplots to indicate a difference in the mean scores
happiness_df %>% boxplot(happiness_df$Life.Ladder  ~classification, data = ., ylab = "Life Ladder")

# Summary statistics of the happiness scores for the two groups(High GDP & Low GDP) 
happiness_df %>% group_by(classification) %>% summarise(Min = min(Life.Ladder,na.rm = TRUE),
                                                    Q1 = quantile(Life.Ladder,probs = .25,na.rm = TRUE),
                                                    Median = median(Life.Ladder,na.rm = TRUE),
                                                    Q3 = quantile(Life.Ladder,probs = .75,na.rm = TRUE),
                                                    Max = max(Life.Ladder,na.rm = TRUE),
                                                    Mean = mean(Life.Ladder, na.rm = TRUE),
                                                    SD = sd(Life.Ladder, na.rm = TRUE),
                                                    n = n(),
                                                    Missing = sum(is.na(Life.Ladder))) -> table1
knitr::kable(table1)

# to investigate normality  for high GDP 
happiness_df_high_gdp <- happiness_df %>% filter(classification == "High GDP")
happiness_df_high_gdp$Life.Ladder %>% qqPlot(dist="norm", main = "QQ plot - Happiness Scores - High GDP", col= 'dark blue', col.lines = 'sky blue')

# to investigate normality  for low GDP 
happiness_df_low_gdp<- happiness_df %>% filter(classification == "Low GDP")
happiness_df_low_gdp$Life.Ladder %>% qqPlot(dist="norm", main = "QQ plot - Happiness Scores - Low GDP", col= 'red', col.lines = 'pink')

# Testing equal variances
leveneTest( Life.Ladder~ classification , data =happiness_df)

t.test(Life.Ladder ~ classification,data = happiness_df,var.equal = TRUE,alternative = "two.sided")



