getwd()
setwd("C://Users//Assignments//Forecasting")
install.packages("forecast")
install.packages("smooth")
library(forecast)
library(fpp)
library(smooth)
library(readxl)
Cocacola <- read_excel("C://Users//Assignments//Forecasting//CocaCola_Sales_Rawdata.xlsx")
View(Cocacola) # Quarterly 4 months 
windows()
plot(Cocacola$Sales,type="o")
Q1 <-  ifelse(grepl("Q1",Cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",Cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",Cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",Cocacola$Quarter),'1','0')

# So creating 12 dummy variables 

CocacolaData<-cbind(Cocacola,Q1,Q2,Q3,Q4)
View(CocacolaData)
colnames(CocacolaData)

CocacolaData["t"]<- 1:42
View(CocacolaData)
CocacolaData["log_Sales"]<-log(CocacolaData["Sales"])
CocacolaData["t_square"]<-CocacolaData["t"]*CocacolaData["t"]
attach(CocacolaData)

cola_train<-CocacolaData[1:36,]

cola_test<-CocacolaData[37:40,]

########################### LINEAR MODEL #############################

cola_linear_model<-lm(Sales~t,data=cola_train)
summary(cola_linear_model)

linear_pred<-data.frame(predict(cola_linear_model,interval='predict',newdata =cola_test))
View(linear_pred)
rmse_linear<-sqrt(mean((cola_test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 644.018 and Adjusted R2 Vaue - 79.22%

expo_model<-lm(log_Sales~t,data=cola_train)
summary(expo_model)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=cola_test))
rmse_expo<-sqrt(mean((cola_test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 524.7351 and Adjusted R2 - 80.17 %

Quad_model<-lm(Sales~t+t_square,data=cola_train)
summary(Quad_model)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=cola_test))
rmse_Quad<-sqrt(mean((cola_test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 434.7185 and Adjusted R2 - 85.96 %


sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=cola_train)
summary(sea_add_model)

sea_add_pred<-data.frame(predict(sea_add_model,newdata=cola_test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 1785.135

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=cola_test))
rmse_Add_sea_Linear<-sqrt(mean((cola_test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 534.6979 and Adjusted R2 - 87.61

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=cola_train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=cola_test))

rmse_Add_sea_Quad<-sqrt(mean((cola_test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 236.7075 and Adjusted R2 - 95.49%

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = cola_train)
summary(multi_sea_model)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=cola_test,interval='predict'))
rmse_multi_sea<-sqrt(mean((cola_test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 1871.203

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = cola_train)
summary(multi_add_sea_model)

multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=cola_test,interval='predict'))

rmse_multi_add_sea<-sqrt(mean((cola_test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 335.1026 and Adjusted R2 - 89.86%

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive Seasonality with Quadratic trend  has least RMSE value

new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CocacolaData)
new_model_pred<-data.frame(predict(new_model,newdata=CocacolaData,interval='predict'))

new_model_fin <- new_model$fitted.values

View(new_model_fin)

# pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Quarter <- as.data.frame(CocacolaData$Quarter)

Final <- as.data.frame(cbind(Quarter,CocacolaData$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 

plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",
     col.axis="Green",type="s")

View(Final)
write.csv(Final,file="CocaCola_Final.csv",col.names = F,row.names = F)

# plot(Final$new_model_fin,type="o")

##### assignment 2. using Airlines Passengers data set

install.packages("rmarkdown")
install.packages("forecast")
install.packages("fpp")
install.packages("smooth")
install.packages("readxl")

library(forecast)
library(fpp)
library(readxl)
library(smooth)
library(rmarkdown)

Airlines<-read_excel("C://Users//Assignments//Forecasting//Airlines+Data.xlsx")
View(Airlines) # Seasonality 12 months 
windows()
plot(Airlines$Passengers,type="o")
# So creating 12 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
# View(X)

colnames(X)<-month.abb # Assigning month names 
# View(X)
AirlinesData<-cbind(Airlines,X)
View(AirlinesData)
colnames(AirlinesData)

AirlinesData["t"]<- 1:96
View(AirlinesData)
AirlinesData["log_Passenger"]<-log(AirlinesData["Passengers"])
AirlinesData["t_square"]<-AirlinesData["t"]*AirlinesData["t"]
attach(AirlinesData)

train<-AirlinesData[1:84,]

test<-AirlinesData[85:96,]

########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 53.19924


######################### Exponential #################################

expo_model<-lm(log_Passenger~t,data=train)
summary(expo_model)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.05736  and Adjusted R2 - 82.18 %

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 48.05189 and Adjusted R2 - 79.12%

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 132.8198

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 35.34896 and Adjusted R2 - 94.75%
######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad 
# 26.36082 and Adjusted R2 - 95.24%

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 
# 140.0632
######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 

multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea 
# 10.51917 and Adjusted R2 - 97.23%

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value

new_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = AirlinesData)
new_model_pred<-data.frame(predict(new_model,newdata=AirlinesData,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(Airlines$Month)

Final <- as.data.frame(cbind(Month,AirlinesData$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)

plot(Final$Passengers,main = "ActualGraph", xlab="Passengers(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 

plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Passengers(Predicted)", ylab="Quarter",
     col.axis="Green",type="s")

write.csv(Final,file="Airlines_Pass_Final.csv",col.names = F,row.names = F)

#### plastic sales forecasting 

library(forecast)
library(fpp)
library(readxl)
library(smooth)
library(rmarkdown)

Plastics<-read.csv("C://Users//Assignments//Forecasting//PlasticSales.csv") # read the  data
View(Plastics) # Seasonality 12 months 
windows()
plot(Plastics$Sales,type="o")

# So creating 12 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)

colnames(X)<-month.abb # Assigning month names 
View(X)
Plasticsdata<-cbind(Plastics,X)
View(Plastics)
colnames(Plastics)

Plasticsdata["t"]<- 1:60
View(Plasticsdata)
Plasticsdata["log_Sales"]<-log(Plasticsdata["Sales"])
Plasticsdata["t_square"]<-Plasticsdata["t"]*Plasticsdata["t"]
attach(Plasticsdata)

train<-Plasticsdata[1:48,]

test<-Plasticsdata[49:60,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear 
# 260.9378 and Adjusted R2 Value = 31.50

######################### Exponential #################################

expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo 
# 268.6938  and Adjusted R2 - 30.25 %

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)


Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 
# 297.4067 and Adjusted R2 - 30.48%

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_add_model)

sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 
# 235.6027 and Adjusted R2 Value = 69.85

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Linear_model)

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))

rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 
# 135.5536 and Adjusted R2 - 96.45%

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Quad_model)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))

rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad 
# 218.1939 and Adjusted R2 - 97.68 %

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))

rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 
# 239.6543

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_add_sea_model) 

multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea 
# 160.6833 and Adjusted R2 - 97.51%

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value

new_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Plasticsdata)
new_model_pred<-data.frame(predict(new_model,newdata=Plasticsdata,interval='predict'))

new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

Month <- as.data.frame(Plasticsdata$Month)

Final <- as.data.frame(cbind(Month,Plasticsdata$Sales, new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",
     col.axis="blue",type="o")

plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months",
     col.axis="Green",type="s")

View(Final)
write.csv(Final,file="Plastic_Sales_Final_M.csv",col.names = F,row.names = F)
