library('ggplot2')
library('ggthemes') 
library('mice')
library('dplyr') 
library('mice')
library('randomForest') 
library('data.table')
library('gridExtra')
library('corrplot') 
library('GGally')
library('e1071')

df<-read.csv("R projects/house_prices.csv")
head(df)

str(df)
col(df)

# verificando quais colunas são character na base
sum(sapply(df[,1:21], typeof) == "character")

# Removing the date column
new_df <- subset(df, select = -c(date))
str(new_df)
# Obtain summary statistics

# Obtain summary statistics

summary(new_df[,sapply(new_df[,1:20], typeof) == "integer"])
base<-data_frame(new_df)
dim(base)
