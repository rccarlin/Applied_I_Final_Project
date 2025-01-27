library(tidyverse)
library(MASS)
library(corrplot)

ver4 = read.csv("C:\\Users\\riley\\F23 Applied\\austinHousingData.csv")


##### cleaning the data set#####
ver4 = subset(ver4, select = -c(description, homeImage, streetAddress, latest_saledate, garageSpaces) ) 

ver4 = ver4[ver4$homeType %in% "Single Family",]

ver4 = subset(ver4, select = -c(homeType)) 

ver4$city = as.factor(ver4$city)
ver4$zipcode = as.factor(ver4$zipcode)
# ver4$latestPriceSource = as.factor(ver4$latestPriceSource)

# maybe looking at decades will be easier?
ver4 = ver4 %>% mutate(decadeBuilt = yearBuilt %/% 10 * 10)
ver4$decadeBuilt = as.factor(ver4$decadeBuilt)

# for the presentation, may need to say how many unique variables there are in the stuff you're dropping!
# length(unique(ver4$latest_saledate))
# ver4$latest_saledate = as.factor(ver4$latest_saledate)

ver4 = ver4 %>% mutate(source = ifelse(latestPriceSource == "Agent Provided", "Agent Provided", 
                                       ifelse(latestPriceSource == "Broker Provided", "Broker Provided", "Other") ))

ver4 = subset(ver4, select = -c(latestPriceSource)) 


#### Looking at raw data#####

## price over year
ggplot(ver4, aes(x= yearBuilt, y= latestPrice)) + geom_point(color = "red") + 
  scale_y_continuous(trans= "log10") + 
  labs(title= "Price over Year Built", y= "Latest Price", x= "Year Built") +
  scale_x_continuous(breaks = seq(1900, 2030, by= 20))

# histogram of the years
ggplot(ver4, aes(x= yearBuilt)) + geom_histogram(color= "white", fill= "red", binwidth = 10)
ggplot(ver4, aes(x= decadeBuilt)) + geom_bar(color= "black", fill= "red") +
  labs(title= "Freqency of Decade Built", y= "Count", x= "Decade Built") + 
  scale_y_continuous(breaks = seq(0, 3000, by= 500))

# histogram of the prices
ggplot(ver4, aes(x= latestPrice)) + geom_histogram(color= "black", fill= "blue") + scale_x_continuous(trans= "log10")


ggplot(ver4, aes(x= decadeBuilt)) + geom_histogram(color= "black", fill= "red", binwidth=1)


# prices by decade
ggplot(ver4, aes(decadeBuilt, latestPrice)) + geom_boxplot() + scale_y_continuous(trans= "log10")+ ggtitle("Price over Decade Built")


# correlation
par(mar=c(0,0,0,0),xpd=NA)
corrplot(cor(ver4[sapply(ver4, is.numeric)]), tl.cex=.8, title = "Correlation of Numeric Variables" ,mar=c(0,0,2,0))
# might need to remove parking, but what's happeneing with elementary schools?
cor(ver4[sapply(ver4, is.numeric)])
# ver4 = subset(ver4, select = -c(garageSpaces))

# latest sale price source
ggplot(ver4, aes(x= latestPriceSource,)) + geom_bar(color= "black", fill= "red") + 
  labs(title= "Frequency of Price Sources", y= "Count", x= "Source of Latest Price") + 
  scale_y_continuous(trans= "log10")


##### simple linear regression #####

# naive lm by year
lm_price_year <- lm(latestPrice~yearBuilt, ver4) 
summary(lm_price_year)
# 1209.2 slope with high significance
# suggests that the newer the house is, the more expensive it is, on average
# but the R^2 is in the garbage... .003 horrible model

# graph it with the line

# naive lm by decade
lm_price_decade <- lm(latestPrice~decadeBuilt, ver4)
summary(lm_price_decade)
# and just like that, now most of the slopes are negative, and many have very large p values
# and even the "significant" ones are more than when all the years were together
# R2 still trash at .032

# check residuals
plot(fitted(lm_price_decade), resid(lm_price_decade), main = "Decade Residuals over Fitted", xlab="Fitted Price", ylab= "Residual")
abline(0,0)


##### All variables ######

# years not decade
# ver4 = subset(ver4, select = -c(latest_saledate)) 

lm_price_all_years = lm(latestPrice ~. -decadeBuilt, ver4)
summary(lm_price_all_years)

#decade not years
lm_price_all_decades = lm(latestPrice ~. -yearBuilt, ver4)
summary(lm_price_all_decades)
plot(fitted(lm_price_all_decades), resid(lm_price_all_decades), 
     main = "All Residuals over Fitted", xlab="Fitted Price", ylab= "Residuals")
abline(0,0)


##### model selection######

# train test split 70% 30%
set.seed(2023)
samp = sample(seq_len(nrow(ver4)), size = floor(.75 * nrow(ver4)))
train = ver4[samp,]
test = ver4[-samp,]


## AIC Step####

# with year, not decade
intercept_only_year = lm(latestPrice ~ 1, train)
end_model_year = lm(latestPrice ~. -decadeBuilt, train)
stepAIC(intercept_only_year, scope= 
          list(lower=intercept_only_year,upper=end_model_year), direction="both", trace = 5)

stepAICyear = lm(formula = latestPrice ~ livingAreaSqFt + zipcode + numOfStories + 
                   numOfBedrooms + numOfBathrooms + numPriceChanges + numOfWaterfrontFeatures + 
                   hasAssociation + latest_saleyear + avgSchoolDistance + source + 
                   yearBuilt + numOfHighSchools + avgSchoolSize + MedianStudentsPerTeacher + 
                   longitude + avgSchoolRating + latest_salemonth + hasHeating + 
                   numOfWindowFeatures + numOfPatioAndPorchFeatures + numOfElementarySchools + 
                   numOfPrimarySchools + numOfCommunityFeatures + hasView, data = train)

summary(stepAICyear)
stepAICyearTest = lm(formula = latestPrice ~ livingAreaSqFt + zipcode + numOfStories + 
                       numOfBedrooms + numOfBathrooms + numPriceChanges + numOfWaterfrontFeatures + 
                       hasAssociation + latest_saleyear + avgSchoolDistance + source + 
                       yearBuilt + numOfHighSchools + avgSchoolSize + MedianStudentsPerTeacher + 
                       longitude + avgSchoolRating + latest_salemonth + hasHeating + 
                       numOfWindowFeatures + numOfPatioAndPorchFeatures + numOfElementarySchools + 
                       numOfPrimarySchools + numOfCommunityFeatures + hasView, data = test)

summary(stepAICyearTest)

# with decades
intercept_only_year = lm(latestPrice ~ 1, train)
end_model_decade = lm(latestPrice ~. -yearBuilt, train)
stepAIC(intercept_only_year, scope= 
          list(lower=intercept_only_year,upper=end_model_decade), direction="both", trace = 5)
stepAICdecade = lm(formula = latestPrice ~ livingAreaSqFt + zipcode + numOfStories + 
                     decadeBuilt + numOfBedrooms + numOfWaterfrontFeatures + numPriceChanges + 
                     numOfBathrooms + hasAssociation + latest_saleyear + avgSchoolDistance + 
                     source + numOfHighSchools + avgSchoolSize + MedianStudentsPerTeacher + 
                     longitude + avgSchoolRating + latest_salemonth + hasHeating + 
                     numOfWindowFeatures + numOfPatioAndPorchFeatures + numOfElementarySchools + 
                     numOfPrimarySchools + numOfCommunityFeatures + numOfPhotos, 
                   data = train)
summary(stepAICdecade)
stepAICdecadeTest = lm(formula = latestPrice ~ livingAreaSqFt + zipcode + numOfStories + 
                     decadeBuilt + numOfBedrooms + numOfWaterfrontFeatures + numPriceChanges + 
                     numOfBathrooms + hasAssociation + latest_saleyear + avgSchoolDistance + 
                     source + numOfHighSchools + avgSchoolSize + MedianStudentsPerTeacher + 
                     longitude + avgSchoolRating + latest_salemonth + hasHeating + 
                     numOfWindowFeatures + numOfPatioAndPorchFeatures + numOfElementarySchools + 
                     numOfPrimarySchools + numOfCommunityFeatures + numOfPhotos, 
                   data = test)

summary(stepAICdecadeTest)


# BIC
# with year, not decade
end_model_year_bic = lm(latestPrice ~. -decadeBuilt, train)
step(intercept_only_year, scope= 
          list(lower=intercept_only_year,upper=end_model_year_bic), k=log(10680))

stepBICyear = lm(formula = latestPrice ~ livingAreaSqFt + zipcode + numOfStories + 
                   numOfBedrooms + numOfBathrooms + numPriceChanges + numOfWaterfrontFeatures + 
                   hasAssociation + latest_saleyear + avgSchoolDistance + source + 
                   yearBuilt + numOfHighSchools + avgSchoolSize + MedianStudentsPerTeacher + 
                   longitude + avgSchoolRating, data = train) 
summary(stepBICyear)
stepBICyearTest = lm(formula = latestPrice ~ livingAreaSqFt + zipcode + numOfStories + 
                       numOfBedrooms + numOfBathrooms + numPriceChanges + numOfWaterfrontFeatures + 
                       hasAssociation + latest_saleyear + avgSchoolDistance + source + 
                       yearBuilt + numOfHighSchools + avgSchoolSize + MedianStudentsPerTeacher + 
                       longitude + avgSchoolRating, data = test) 

summary(stepBICyearTest)

# with decades
intercept_only_year = lm(latestPrice ~ 1, train)
end_model_decade = lm(latestPrice ~. -yearBuilt, train)
stepAIC(intercept_only_year, scope= 
          list(lower=intercept_only_year,upper=end_model_decade), direction="both", trace = 5)
stepAICdecade = lm(formula = latestPrice ~ livingAreaSqFt + zipcode + numOfStories + 
                     decadeBuilt + numOfBedrooms + numOfWaterfrontFeatures + numPriceChanges + 
                     numOfBathrooms + hasAssociation + latest_saleyear + avgSchoolDistance + 
                     source + numOfHighSchools + avgSchoolSize + MedianStudentsPerTeacher + 
                     longitude + avgSchoolRating + latest_salemonth + hasHeating + 
                     numOfWindowFeatures + numOfPatioAndPorchFeatures + numOfElementarySchools + 
                     numOfPrimarySchools + numOfCommunityFeatures + numOfPhotos, 
                   data = train)
summary(stepAICdecade)
stepAICdecadeTest = lm(formula = latestPrice ~ livingAreaSqFt + zipcode + numOfStories + 
                         decadeBuilt + numOfBedrooms + numOfWaterfrontFeatures + numPriceChanges + 
                         numOfBathrooms + hasAssociation + latest_saleyear + avgSchoolDistance + 
                         source + numOfHighSchools + avgSchoolSize + MedianStudentsPerTeacher + 
                         longitude + avgSchoolRating + latest_salemonth + hasHeating + 
                         numOfWindowFeatures + numOfPatioAndPorchFeatures + numOfElementarySchools + 
                         numOfPrimarySchools + numOfCommunityFeatures + numOfPhotos, 
                       data = test)

summary(stepAICdecadeTest)


## LASSO#####
library(glmnet)
input = data.matrix(subset(train, select = -c(latestPrice, decadeBuilt)))

# finding lambda
cross = cv.glmnet(x= input, y= train$latestPrice, alpha=1)
cross$lambda.min  #5068.584, 8070.619 6105.127 5562.765
plot(cross)

bestTrain = glmnet(input, train$latestPrice, alpha= 1, lamda= 3183.218)
coef(bestTrain)[,60]

y = train$latestPrice
y_pred = predict(bestTrain, s= 3183.218, newx= input)
# sst <- sum((y - mean(y))^2)
# sse <- sum((y_pred - y)^2)
# rsq = sse/sst
rsq = cor(y, y_pred) ^ 2
adrsq = 1 - (1- rsq)*(10680-1)/(10680-41-1)


y_new = test$latestPrice
x_new = data.matrix(subset(test, select = -c(latestPrice, decadeBuilt)))
y_pred_new = predict(bestTrain, s= 3183.218, newx= x_new)
sst_new <- sum((y_new - mean(y_new))^2)
sse_new <- sum((y_pred_new - y_new)^2)
rsq_new = cor(y_new, y_pred_new) ^ 2
adrsq_new = 1 - (1- rsq_new)*(3561-1)/(3561-41-1)


1 - ((1-r.squared)*(nobs(x)-1)/(nobs(x)-length(x$coefficients)-1))
