rm(list = ls()) 
setwd("F:/train_cab") 
getwd() 
# #loading Libraries 
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "e1071","geosphere", 
      "DataCombine", "pROC", "doSNOW", "class", "readxl","ROSE","dplyr", "plyr", "reshape","xlsx", 
      "pbapply", "unbalanced", "dummies", "MASS" , "gbm" ,"Information", "rpart", "tidyr", "miscTools") 


# #install.packages if not 
lapply(x, install.packages) 

# #load libraries 
lapply(x, require, character.only = TRUE) 
rm(x) 

#Input Data Source 
df = data.frame(read.csv('train_cab.csv')) 
df2 = data.frame(read.csv('test.csv')) 



########################################################################### 
# EXPLORING DATA # 
########################################################################### 


#viewing the data 
head(df) 

########################################################################################## ###################### 
#structure of data or data types 
str(df) 

#Summary of data 
summary(df) 

#unique value of each count 
apply(df, 2,function(x) length(table(x))) 


df$pickup_datetime <- gsub('\\ UTC','',df$pickup_datetime) 


#Splitting Date and time 
df$Date <- as.Date(df$pickup_datetime) 
df$Year <- substr(as.character(df$Date),1,4) 
df$Month <- substr(as.character(df$Date),6,7) 
df$Weekday <- weekdays(as.POSIXct(df$Date), abbreviate = F) 
df$Date <- substr(as.character(df$Date),9,10) 
df$Time <- substr(as.factor(df$pickup_datetime),12,13) 


#Now we can drop the column pickup_datetime as we have different columns 
df = subset(df, select = -c(pickup_datetime)) 



######################################################################### 
# Checking Missing data # 
######################################################################### 
apply(df, 2, function(x) {sum(is.na(x))}) # in R, 1 = Row & 2 = Col 


#Creating dataframe with missing values present in each variable 
null_val = data.frame(apply(df,2,function(x){sum(is.na(x))})) 
null_val$Columns = row.names(null_val) 
names(null_val)[1] = "null_percentage" 

#Calculating percentage missing value 
null_val$null_percentage = (null_val$null_percentage/nrow(df)) * 100 



# Sorting null_val in Descending order 
null_val = null_val[order(-null_val$null_percentage),] 
row.names(null_val) = NULL 

# Reordering columns 
null_val = null_val[,c(2,1)] 

#viewing the % of missing data for all variales 
null_val 

#We have seen that null values are very less in our data set i.e. less than 1%. 
#So we can delete the columns having missing values df <- DropNA(df) 


#Verifying missing values after deletion 
sum(is.na(df)) 

names(df) 


# Convert degrees to radians- Our data is already in radians, so skipping this step 
#deg2rad <- function(deg) return(deg*pi/180) 

# Calculates the geodesic distance between two points specified by 
# radian latitude/longitude using the Haversine formula 
lat1 = df['pickup_latitude'] 
lat2 = df['dropoff_latitude'] 
long1 = df['pickup_longitude'] 
long2 = df['dropoff_longitude'] 


##### Function to calculate distance ###### 


gcd_hf <- function(long1, lat1, long2, lat2)
  {
  R = 6371.145 # Earth mean radius [km] 
  delta.long = (long2 - long1) 
  delta.lat = (lat2 - lat1) 
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2 
  c <- 2 * atan2(sqrt(a),sqrt(1-a)) 
  d = R * c 
  return(d) # Distance in km 
  }
 

#Running the function for all rows in dataframe 
for (i in 1:nrow(df)) 
{ 
  df$distance[i]= gcd_hf(df$pickup_longitude[i], df$pickup_latitude[i], df$dropoff_longitude[i], 
                         df$dropoff_latitude[i]) 
} 


#Now we can drop the columns for latitude/longitude as we have new column- Distance 
df = subset(df, select = -c(pickup_latitude,dropoff_latitude,pickup_longitude,dropoff_longitude)) 


########################################################################################## ########## 
########################################################################################## ########## 
#We have seen that fare_amount has negative values which should be removed 


df$fare_amount[df$fare_amount<=0] <- NA 
df$fare_amount[df$fare_amount>500] <- NA 



sum(is.na(df)) 


#So we can delete the columns having missing values 
df <- DropNA(df) 

#Verifying missing values after deletion 
sum(is.na(df)) 

summary(df) 



###removing passangers count more than 6 
df$passenger_count[df$passenger_count<1] <- NA 
df$passenger_count[df$passenger_count>6] <- NA 

sum(is.na(df)) 


df <- DropNA(df) 


sum(is.na(df)) 


summary(df) 
###removing outliers in distance 


df$distance[df$distance <= 0] <- NA 
df$distance[df$distance > 500] <- NA 

sum(is.na(df)) 


df <- DropNA(df) 


sum(is.na(df)) 


summary(df) 





# From the above EDA and problem statement categorizing data in 2 categories "continuous" and "categorical" 
#Fare_amount being our target variable is excluded from the list. 
cont = c( 'distance') 


cata = c('Weekday', 'Month', 'Year' , 'Time', 'Date' , 'passenger_count') 


######################################################################### 
# Visualizing the data # 
######################################################################### 

#library(ggplot2) 


#Plot fare amount Vs. the days of the week. 
ggplot(data = df, aes(x = reorder(Weekday,fare_amount), y = fare_amount))+ 
  geom_bar(stat = "identity")+ 
  labs(title = "Fare Amount Vs. days", x = "Days of the week", y = "Fare")+ 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ 
  theme(axis.text.x = element_text( color="black", size=6, angle=45)) 

#Plot Fare amount Vs. months 
ggplot(df,aes(x = reorder(Month,fare_amount), y = fare_amount))+ 
  geom_bar(stat = "identity")+ 
  #ylim = c(0,1000) + 
  labs(title = "Fare Amount Vs. Month", x = "Month", y = "Fare")+ 
  theme(axis.text.x = element_text( color="#993333", size=8)) 



################################################################ 
# Outlier Analysis # 
################################################################ 


#We have done manual updation so we will skip this step 


################################################################ 
# Feature Selection # 
################################################################ 

## Dimension Reduction 
#We have already excluded the below columns that were redundant: 
#pickup_datetime, 
#pickup_latitude, 
#dropoff_latitude, 
#pickup_longitude, 
#dropoff_longitude 
#pickup_datetime 
#We will remove Time column also as it is not required 
##df = subset(df, select = -c(Time)) 

################################################################ 
# Feature Scaling # 
################################################################ 
#We will go for Normalization. 


#Viewing data before Normalization. 
head(df) 


signedlog10 = function(x) 
  { 
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x))) 
}

df$fare_amount  <- as.numeric(df$fare_amount)
df$fare_amount
df$fare_amount = signedlog10(df$fare_amount)
df$distance = signedlog10(df$distance) 

##checking distribution 
hist(df$fare_amount) 
hist(df$distance) 






#Normalization 
for(i in cont) 
{ 
  print(i) 
  df[,i] = (df[,i] - min(df[,i]))/(max(df[,i])-min(df[,i])) 
} 





df$distance




#Viewing data after Normalization. 
head(df) 



#Creating dummy variables for categorical variables 
library(mlr) 
df1 = dummy.data.frame(df, cata) 



#Viewing data after adding dummies 
head(df1) 

#df1 = df 


################################################################ 
# Sampling of Data # 
################################################################ 

# #Divide data into trainset and testset using stratified sampling method 


#install.packages('caret') 
library(caret) 
set.seed(101) 
split_index = createDataPartition(df1$fare_amount, p = 0.7, list = FALSE) 
trainset = df1[split_index,] 
testset  = df1[-split_index,] 


#Checking df Set Target Class 
table(trainset$fare_amount) 


####FUNCTION to calculate MAPE#### 
MAPE = function(y, yhat){ 
  mean(abs((y - yhat)/y))*100 
} 


########################################################################################## ################################################################## 
## Basic approach for ML - Models 
## 
## We will first get a basic idea of how different models perform on our preprocesed data and then select the best model and make it ## 
## more efficient for our Dataset 
## 
########################################################################################## ################################################################## 


#------------------------------------------Decision tree-------------------------------------------# 
#Develop Model on training data 
fit_DT = rpart(fare_amount ~., data = trainset, method = "anova") 


#Variable importance 
fit_DT$variable.importance 

# distance Time05 passenger_count 
## 725793.64246 431.82787 13.85704 
#Lets predict for test data 
pred_DT_test = predict(fit_DT, testset) 

# For test data 
print(postResample(pred = pred_DT_test, obs = testset$fare_amount)) 


#Compute R^2 
dt_r2 = rSquared(testset$fare_amount, testset$fare_amount - pred_DT_test) 
print(dt_r2) 

#Compute MSE 
dt_mse = mean((testset$fare_amount - pred_DT_test)^2) 
print(dt_mse) 

#Compute MAPE 
dt_mape = MAPE(testset$fare_amount, pred_DT_test) 
print(dt_mape) 

# RMSE  Rsquared MAE 
# 0.12 0.59 0.01 






#------------------------------------------Linear Regression-------------------------------------------# 


#Develop Model on training data 
fit_LR = lm(fare_amount ~ ., data = trainset) 


#Lets predict for test data 
pred_LR_test = predict(fit_LR, testset) 


# For test data 
print(postResample(pred = pred_LR_test, obs = testset$fare_amount)) 


#Compute R^2 
lr_r2 = rSquared(testset$fare_amount, testset$fare_amount - pred_LR_test) 
print(lr_r2) 

#Compute MSE 
lr_mse = mean((testset$fare_amount - pred_LR_test)^2) 
print(lr_mse) 

#Compute MAPE 
lr_mape = MAPE(testset$fare_amount, pred_LR_test) 
print(lr_mape) 
##RMSE  Rsquared MAE 
##0.13 0.53 0.01 

#-----------------------------------------Random Forest----------------------------------------------# 


#Develop Model on training data 
fit_RF = randomForest(fare_amount~., data = trainset)


#Lets predict for test data 
pred_RF_test = predict(fit_RF, testset) 
# For test data 
print(postResample(pred = pred_RF_test, obs = testset$fare_amount)) 


#Compute R^2 
rf_r2 = rSquared(testset$fare_amount, testset$fare_amount - pred_RF_test) 
print(rf_r2) 

#Compute MSE 
rf_mse = mean((testset$fare_amount - pred_RF_test)^2) 
print(rf_mse) 

#Compute MAPE 
rf_mape = MAPE(testset$fare_amount, pred_RF_test) 
print(rf_mape) 

# RMSE  Rsquared MAE 
# 

#R2 
# 

#MSE 
# 

#MAPE 
# 

#--------------------------------------------XGBoost-------------------------------------------# 
#Develop Model on training data 
fit_XGB = gbm(fare_amount~., data = trainset, n.trees = 500, interaction.depth = 2) 


#Lets predict for test data 
pred_XGB_test = predict(fit_XGB, testset, n.trees = 500) 


# For test data 
print(postResample(pred = pred_XGB_test, obs = testset$fare_amount)) 


#Compute R^2 
xgb_r2 = rSquared(testset$fare_amount, testset$fare_amount - pred_XGB_test) 
print(xgb_r2) 

#Compute MSE 
xgb_mse = mean((testset$fare_amount - pred_XGB_test)^2) 
print(xgb_mse) 

#Compute MAPE 
xgb_mape = MAPE(testset$fare_amount, pred_XGB_test) 
print(xgb_mape) 

# RMSE  Rsquared MAE 
# 

#R2 
# 

#MSE 
# 

#MAPE 
# 


#################-------------------------------Viewing summary of all models------------------------------ ############### 
# Create variables 
MSE <- c(dt_mse, lr_mse, rf_mse, xgb_mse) 
r2 <- c(dt_r2, lr_r2, rf_r2, xgb_r2) 
MAPE <- c(dt_mape, lr_mape, rf_mape, xgb_mape) 


# Join the variables to create a data frame 
results <- data.frame(MSE,r2,MAPE) 
results 
 