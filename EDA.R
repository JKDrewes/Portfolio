source("/home/nixy/Desktop/Personal_Projects/Health_Coverage_IL/Census_Data.R")
library(tidyverse)
library(ggplot2)
library(GoodmanKruskal)
library(Amelia)
library(WebPower)

str(explore_sample)

nrow(explore_sample) #99350

Amelia::missmap(explore_sample) #Maybe impute COW?

explore_sample %>%
  group_by(HICOV) %>%
  tally() %>%
  mutate(percentage = (n / sum(n))) #0.0845 uninsured, 0.915 insured

cat_cols = sapply(explore_sample, function(x) is.factor(x) || is.character(x))
df_cat = explore_sample %>% 
  select(HICOV, names(explore_sample)[cat_cols])

GKtauDataframe(df_cat, dgts = 3, includeNA = "ifany") #Goodman Kruskal Tau is a measure of association between categorical variables

cont_cols = sapply(explore_sample, function(x) is.numeric(x) && !is.factor(x))
df_cont = explore_sample %>% 
  select(HICOV, names(explore_sample)[cont_cols])

ggplot(df_cont, aes(x=as.factor(HICOV), y=HINCP)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Insurance") #Looks like an exponential distribution

ggplot(df_cont, aes(x=as.factor(HICOV), y=AGEP)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Insurance") #Older people tend to be insured more

ggplot(df_cont, aes(x=as.factor(HICOV), y=WKHP)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Insurance") #Almost no difference between the two

summary(df_cont)

#It looks like the most important variable here is age, leaving others to be hardly impactful at all.