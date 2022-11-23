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

df<-read.csv("R projects/Data-Science-with-R/house_prices.csv")
head(df)

str(df)
col(df)

# verificando quais colunas são character na base
sum(sapply(df[,1:21], typeof) == "character")

# Removing the date column
new_df <- subset(df, select = -c(date))
str(new_df)
# Obtain summary statistics

summary(new_df[,sapply(new_df[,1:20], typeof) == "integer"])
base<-data_frame(new_df)
dim(base)

head(base)

## Bar plot/Density plot function

## Bar plot function

plotHist <- function(data_in, i) 
{
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

## Density plot function

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = base$price)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}


## Function to call both Bar plot and Density plot function

doPlots <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


## Barplots for the categorical features

doPlots(base, fun = plotHist, ii = 3:4, ncol = 2)

doPlots(base, fun = plotHist, ii = 8:10, ncol = 2)

## Round values on column "price"

base$price <- round(base$price, 2)

## Box Plot Price x bedrooms
ggplot(base, aes(x = bedrooms, y = price)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  #scale_y_continuous(labels=label_dollar()) +
  theme_few

max(base$price)

#Density plots for numerics  variables
doPlots(base, fun = plotDen, ii = 3:4, ncol = 2)


