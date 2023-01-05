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

df<-read.csv("house_prices.csv")
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

# Explore the correlation
correlations <- cor(na.omit(base[,-1]))

# Correlations

row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")



# Verifying the missing values 

# Looking at the distribution and summary of the target variable

summary(base$price)
quantile(base$price)

# Conclusion: From summary, it was observed that minimum price is greater than 0

## Histogram for target variable

hist(base$price)

# Plotting 'sqft_living' too see if there are any outliers

ggplot(base,aes(y=price,x=sqft_living))+geom_point()

## Taking all the missing data indices in one variables

Missing_indices <- sapply(base,function(x) sum(is.na(x)))
Missing_Summary <- data.frame(index = names(base),Missing_Values=Missing_indices)
Missing_Summary[Missing_Summary$Missing_Values > 0,]


#LotFrontage
#Imputing missing Lot Frontage by the median

base$sqft_lot[which(is.na(base$sqft_lot))] <- median(base$sqft_lot,na.rm = T)

base$sqft_lot

base

#determining skew of each numeric variable
Column_classes <- sapply(names(base),function(x){class(base[[x]])})
numeric_columns <-names(Column_classes[Column_classes != "factor"])

skew <- sapply(numeric_columns,function(x){skewness(base[[x]],na.rm = T)})

# Let us determine a threshold skewness and transform all variables above the treshold.

skew <- skew[skew > 0.75]

# transform excessively skewed features with log(x + 1)

for(x in names(skew)) 
{
  base[[x]] <- log(base[[x]] + 1)
}

# Train and test dataset creation

train<- rep(1,16209)
test<- rep(0,5404)
isTrain<- c(train,test)

# create the column isTrain
base <- cbind(base, isTrain)
head(base)

train <- base[base$isTrain==1,]
test <- base[base$isTrain==0,]
smp_size <- floor(0.75 * nrow(train))

## setting the seed to make the partition reproducible75 % == 16254

set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

train_new <- train[train_ind, ]
validate <- train[-train_ind, ]
train_new <- subset(train_new,select=-c(id,isTrain))
validate <- subset(validate,select=-c(id,isTrain))
nrow(train_new)

# Removing the long column
train_new <- subset(train_new, select = -c(long))
str(train_new)


# Build the Model
library(randomForest)
house_model <- randomForest(price~.,
                            data = train_new)

# Variable importance
importance    <- importance(house_model)
varImpPlot(house_model)

# Final Prediction
# Predict using the test set

prediction <- predict(house_model,test)

# Evaluation RMSE function

RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

# RMSE

RMSE1 <- RMSE(prediction, validate$price)
RMSE1

RMSE1 <- round(RMSE1, digits = 5)
test

# Output file
prediction[which(is.na(prediction))] <- mean(prediction,na.rm=T)
submit <- data.frame(Id=test$id,price=prediction)
write.csv(submit,file="House_Price_Pradeep.csv",row.names=F)
