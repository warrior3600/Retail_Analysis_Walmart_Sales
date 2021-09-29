#Load the libraries
library(dplyr)
library(ggplot2)
library(caTools)
library(MLmetrics)
library(corrplot)
library(plotrix)

#load the dataset
setwd("C:/Users/HP/Downloads/Walmart_store_sales")
dataset <- read.csv("Walmart_store_sales.csv")
dataset1 <- dataset #Copying into another dataframe for analysis 

##BASIC STATS ANALYSIS##
#Task 1: Determination of which store has the maximum sales
names(dataset1)
stat <- summarize(group_by(dataset1,Store),sales_sum = sum(Weekly_Sales)) #Getting the sum for each store in a seperate dataframe
max_sum <- stat[which.max(stat$sales_sum),] #returns the store with max sales
ggplot(data=stat, aes(x=Store, y=sales_sum)) +
  geom_bar(stat="identity", fill="darkblue")+
  ggtitle("Weekly sales for each Store") +
  geom_text(aes(label=sales_sum), vjust= -1, size=3) +
  theme_minimal()  #Displays a bar chart of the weekly sales of each store

#Task 2: Determination of which store has the maximum std deviation between the sales of each week and finding the coeff of variance
names(dataset1)
salessd <- summarise(group_by(dataset1,Store),sales_sd = sd(Weekly_Sales), sales_mean = mean(Weekly_Sales)) #Getting the sd for each store in a seperate dataframe
stat <- merge(stat,salessd,by ='Store',all.x = TRUE)
max_sd <- stat[which.max(stat$sales_sd),] #returns the store with max variation in sales
ggplot(data=stat, aes(x=Store, y=sales_sd)) +
  geom_bar(stat="identity", fill="orange")+
  geom_text(aes(label=sales_sd), vjust= -1, size=3) +
  ggtitle("Variation of Weekly sales for each Store") +
  theme_minimal()#Displays a bar chart of the weekly sales of each store
stat$coeff_var <- stat$sales_sd/stat$sales_mean #finding the coeff of variance

#Task 3: Determination of which store has good quaterly growth for the quarter Q3-2012
dataset1$Month <- as.integer(substr(dataset1$Date,4,5))
dataset1$Year <- as.integer(substr(dataset1$Date,7,10))
q3 <- subset(dataset1,Year == 2012 & (Month == 7 | Month == 8 | Month == 9))
q2 <- subset(dataset1,Year == 2012 & (Month == 4 | Month == 5| Month == 6))
q3_sales <- summarise(group_by(q3,Store),Q3_sales = sum(Weekly_Sales)) #Getting the sum for each store for third quarter
q2_sales <- summarise(group_by(q2,Store),Q2_sales = sum(Weekly_Sales)) #Getting the sum for each store for second quarter
q3_sales <- merge(q3_sales,q2_sales,by = "Store",all.x = TRUE)
q3_sales$netgrowth <- ((q3_sales$Q3_sales - q3_sales$Q2_sales)/q3_sales$Q3_sales)*100      #Obtaining the net growth of each store from second to third quarter
View(subset(q3_sales,netgrowth > 0))

#Task 4: Find out which holiday period has a positive impact and has higher sales than the mean sales in non holiday season
holiday_df <- data.frame(Date = c("12-02-2010", "11-02-2011", "10-02-2012", "8-02-2013", "10-09-2010", "9-09-2011", "7-09-2012", "6-09-2013","26-11-2010", "25-11-2011", "23-11-2012", "29-11-2013", "31-12-2010", "30-12-2011", "28-12-2012", "27-12-2013"),
                                Name_Holiday = c("Super Bowl","Super Bowl","Super Bowl","Super Bowl","Labour Day","Labour Day","Labour Day","Labour Day","Thanksgiving","Thanksgiving","Thanksgiving","Thanksgiving","Christmas","Christmas","Christmas","Christmas"))
dataset1 <- merge(dataset1,holiday_df,by = 'Date',all.x = TRUE)
non_holiday <- subset(dataset1,Holiday_Flag == 0)   
noholiday_mean <- mean(non_holiday$Weekly_Sales)    #Mean sales for non holiday season
super_bowl <- subset(dataset1,Name_Holiday == "Super Bowl" )
super_bowl_mean <- mean(super_bowl$Weekly_Sales)    #Mean sales on super bowl days
labour <- subset(dataset1,Name_Holiday == "Labour Day" )
labour_mean <- mean(labour$Weekly_Sales)          #Mean sales on Labour days
thanksgiving <- subset(dataset1,Name_Holiday == "Thanksgiving" )
thanksgiving_mean <- mean(thanksgiving$Weekly_Sales)     #Mean sales on thanksgiving days
christmas <- subset(dataset1,Name_Holiday == "Christmas" )
christmas_mean <- mean(christmas$Weekly_Sales)      #Mean sales on Christmas days
holiday_df <- data.frame(Name = c("Super Bowl","Labour Day","Thanksgiving","Christmas"),
                         Mean_sales = c(super_bowl_mean,labour_mean,thanksgiving_mean,christmas_mean)) 
holiday_df$Positive_Impact <- holiday_df$Mean_sales > noholiday_mean #Checking which Holiday has a positive or negative impact
ggplot()+
  geom_bar(aes(x=holiday_df$Name, y = holiday_df$Mean_sales, fill = holiday_df$Positive_Impact),stat = "identity",position=position_dodge())+
  xlab("Holidays")+
  ylab("Mean sales")+
  ggtitle("Graphical analysis of holiday sales with non- holiday sales")+
  theme_minimal()

#Task 5:Monthly and semester view of sales in units
ggplot(data = dataset1, aes(x=Month,y= Weekly_Sales))+
  geom_bar(stat = "identity",fill = "red")+
  xlab("Month")+
  ylab("Sales")+
  ggtitle("Graphical analysis of Monthly sales")+
  theme_minimal()
dataset1$semester <- ifelse(dataset1$Month %in% c(1,2,3,4,5,6),1,2)
ggplot()+
  geom_bar(aes(x=dataset1$semester,y=dataset1$Weekly_Sales),stat = "identity",fill = "green",width = 0.5)+
  xlab("Semesterwise") +
  ylab("Sales")+
  ggtitle("Graphical analysis of Semester Sales")+
    theme_minimal()
ggplot(data = dataset1, aes(x=Month,y= Temperature))+
  geom_bar(stat = "identity",fill = "steelblue")+
  xlab("Month")+
  ylab("Temeprature")+
  ggtitle("Average Monthly Temperature ")+
  theme_minimal()

#Micelanneaous Analysis
#par(mfrow=c(3,2))
#hist(dataset1$Store, col = 'light blue', main = "Stores")
#hist(dataset1$Temperature, col = 'light blue', main = "Temperature")
#hist(dataset1$Fuel_Price, col = 'light blue', main = "Fuel Price")
#hist(dataset1$CPI, col = 'light blue', main = "CPI")
#hist(dataset1$Unemployment, col = 'light blue', main = "Unemployment")

#ggplot(dataset1, aes(x = Month,y = Weekly_Sales )) + 
#  geom_col() +
#  facet_wrap(~Year) + 
#  ggtitle("Weekly Sales Distribution")

#par(mfrow=c(3,2))
#for(i in 4:8)
#{
#  plot(dataset[,1], 
#      dataset$Weekly_Sales, 
#      main=names(dataset[i]), 
#       ylab=names(dataset$Weekly_Sales), 
#       xlab="", col='indianred4')
#}

##DATA MODELLING: LINEAR REGRESSION##
#Create the week column and drop the date column
dataset <- dataset1 #Copying the new analysed dataframe 
arrange(dataset,Store)
Date <- unique(dataset$Date)
Week <- seq(1:length(Date))
week_df <- data.frame(Date,Week)
dataset <- merge(dataset,week_df,by = "Date",all.x = TRUE)
dataset$Date <- NULL
arrange(dataset,Week)

#Replace NA values in holiday with 0s
dataset$Name_Holiday[is.na(dataset$Name_Holiday)] <- 0
View(dataset)

#Check for missing values
Noofna <- dim(dataset[is.na(dataset),])[1]
if(Noofna > 0 )
{
  cat("No.of missing values:",Noofna)
  cat("\n Removing missing values....")
  dataset <- dataset[complete.cases(datset),]
  cat("\n Removed succcessfully!")
}

#Check for outliers
boxplot(dataset[,-10],main = "Outlier detection", col=c("blue","red")) #We remove the non numeric Holiday column from our boxplot analysis
#Getting rid of the outliers by some means
iqr <-IQR(dataset$Weekly_Sales)
quant <- quantile(dataset$Weekly_Sales)
ll <- round(quant[2] - iqr*1.5)
ul <- round(quant[4] + iqr*1.5)
#Extracting the outliers beyond the upper and lower limits
View(subset(dataset,Weekly_Sales >ul | dataset$Weekly_Sales < ll)) #only a few outliers are present, hence we drop them
dataset <- dataset[!(dataset$Weekly_Sales > ul | dataset$Weekly_Sales < ll),] #Outliers deleted!!
boxplot(dataset[,-10],main = "Outlier detection", col=c("blue","red"))

##check for the corelation between the variables
corr = cor(dataset[, -10])
View(corr)
corrplot(corr = corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))
names(dataset)

#Create dummy variables for the holiday categorical column
holiday_fact <- as.factor(dataset$Name_Holiday)
dummy_holiday <- data.frame(model.matrix(~holiday_fact))[,-1]
#Merging the dummy variables to the final dataset
dataset <- cbind(dataset,dummy_holiday)
dataset <- subset(dataset,select = -Name_Holiday) #Dropping the categorical column

#Model-1
#Splitting the data into train and test sets
set.seed(123)
sample <- sample.split(dataset, SplitRatio = 0.7)
trainSet <- subset(dataset, sample ==T)
testSet <- subset(dataset, sample == F)
#Create the model
model1 = lm(formula = Weekly_Sales ~.,data = trainSet)
summary(model1)

#Model-2
names(dataset)
#Create the model
model2 = lm(formula = Weekly_Sales ~ Store+CPI+Unemployment+Week+Temperature+Fuel_Price+holiday_factChristmas+holiday_factLabour.Day+holiday_factSuper.Bowl+holiday_factThanksgiving,data = trainSet) #removing the semester, month and year factors
summary(model2)

#Model-3
names(dataset)
#Create the model
model3 = lm(formula = Weekly_Sales ~ Store+CPI+Unemployment+Week+Temperature+holiday_factChristmas+holiday_factLabour.Day+holiday_factSuper.Bowl+holiday_factThanksgiving,data = trainSet) #removing fuel price factor
summary(model3)

#Model-4
names(dataset)
#Create the model
model4 = lm(formula = Weekly_Sales ~Store+CPI+Unemployment+Temperature+Fuel_Price+holiday_factChristmas+holiday_factLabour.Day+holiday_factSuper.Bowl+holiday_factThanksgiving,data = trainSet) #removing the week factor
summary(model4)

#Model-5
names(dataset)
#Create the model
model5 = lm(formula = Weekly_Sales ~Store+CPI+Unemployment+Week+Temperature+Fuel_Price+holiday_factLabour.Day+holiday_factSuper.Bowl+holiday_factThanksgiving,data = trainSet)#removing the christmas holiday factor
summary(model5)

#Model-6
names(dataset)
#Create the model
model6 = lm(formula = Weekly_Sales ~ Store+CPI+Unemployment+Week+Temperature+Fuel_Price+holiday_factChristmas+holiday_factLabour.Day+holiday_factSuper.Bowl+holiday_factThanksgiving +semester,data = trainSet)#adding semester factor 
summary(model6)

#Model-7
names(dataset6)
#Create the model
model7 = lm(formula = Weekly_Sales ~ Store+CPI+Unemployment+Week+Temperature+holiday_factChristmas+holiday_factLabour.Day+holiday_factSuper.Bowl+holiday_factThanksgiving +semester+Year,data = trainSet)#adding year factor and removing fuel price
summary(model7)

#Test and find the predictions with the test set
testSet$pred_price <- predict(model7,newdata = testSet) #we select model 7 due to best value of rsqr and adjusted rsqr
View(subset(testSet, select = -c(holiday_factChristmas,holiday_factLabour.Day,holiday_factSuper.Bowl,holiday_factThanksgiving)))

ggplot()+
  geom_point(aes(x = testSet$Weekly_Sales,y = testSet$pred_price))+
  xlab("Actual price")+
  ylab("Predicted price")+
  ggtitle("Graphical Analysis of actual vs predicted prices")

#Using MAPE and RMSE values
MAPE(testSet$pred_price,testSet$Weekly_Sales)
RMSE(testSet$pred_price,testSet$Weekly_Sales)