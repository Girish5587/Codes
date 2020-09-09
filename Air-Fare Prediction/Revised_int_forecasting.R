################### Data Collection ##############
library(data.table) # Due to large Dataset
airfare_rev <- fread(file.choose()) # Import the original Client data
View(airfare_rev)
dim(airfare_rev) # Rows 278466 x  Colmn 4
class(airfare_rev) # data.table
summary(airfare_rev)
str(airfare_rev) # all 4 variable format is chr
attach(airfare_rev)

library(Hmisc)
describe(airfare_rev)

###################### Datatype ########################

# Change datatype of InvoiceDate  to POSIXct
library(lubridate)
airfare_rev$InvoiceDate <- dmy_hm(airfare_rev$InvoiceDate) 
class(airfare_rev$InvoiceDate) # POSIXct

library(dplyr)
airfare_rev <- airfare_rev %>%
  mutate(Year = year(airfare_rev$InvoiceDate),Month =month(airfare_rev$InvoiceDate,label = TRUE),
         Day = wday(airfare_rev$InvoiceDate,label = TRUE),Date = day(airfare_rev$InvoiceDate),Hour = hour(airfare_rev$InvoiceDate),
         Minutes = minute(airfare_rev$InvoiceDate))

# change datatype of NetFare
airfare_rev$NetFare <- as.numeric(airfare_rev$NetFare) # change to numeric

airfare_rev<- airfare_rev[,2:10] # removed InvoiceDate as we have splitted & added

str(airfare_rev) # crosscheck the changed datatype as above

################# EDA Plot without imputing missing values ###############
library(dplyr)
library(ggplot2)

# Filter by Itinerytype & plot Avg of NetFare by Product Type (Barplot)

airfare_rev %>%
  filter(ItineraryType=="Domestic")%>%
  group_by(Day)%>%
  summarise(number=n(),mean_Netfare = mean(NetFare,na.rm = T),Minimum=min(NetFare,na.rm = T),Maximum=max(NetFare,na.rm = T))%>%
  ggplot(aes(x = Day, y = mean_Netfare,fill = Day ))+
  geom_bar(stat = "identity") + theme_classic() +
  labs(x = "WeekDay",y = "Avg NetFare",title = paste("Avg NetFare by WeekDay for Itinery 'Domestic'"))

# Filter by Itinerytype & plot Avg of NetFare by Product Type (Barplot)
airfare_rev %>%
  filter(ItineraryType=="International")%>%
 group_by(Day)%>%
 summarise(number=n(),mean_Netfare = mean(NetFare,na.rm = T),Minimum=min(NetFare,na.rm = T),Maximum=max(NetFare,na.rm = T))%>%
  ggplot(aes(x = Day, y = mean_Netfare,fill = Day ))+
  geom_bar(stat = "identity") + theme_classic() +
  labs(x = "WeekDay",y = "Avg NetFare",title = paste("Avg NetFare by WeekDay for Itinery 'International'"))
########## Missing Value #########

sum(is.na(airfare_rev)) # 60903
sapply(airfare_rev,function(x) sum(is.na(x))) # 60891,0,0,2,2,2,2,2,2

# Replace blank fields with NA in PT,IT
airfare_rev$ProductType [airfare_rev$ProductType==""] <- NA
airfare_rev$ItineraryType [airfare_rev$ItineraryType==""] <-NA

sum(is.na(airfare_rev)) # 93682
sapply(airfare_rev,function(x) sum(is.na(x))) # 60891,2,32777,2,2,2,2,2,2

####### Missing Value Imputation ######
# Define Mode Function 
getmode <- function(v) {
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v , uniqv)))] }

# Imputation of missing values with mode in ItineryType
getmode(airfare_rev$ItineraryType) # Domestic
airfare_rev$ItineraryType[is.na(airfare_rev$ItineraryType)] <- getmode(airfare_rev$ItineraryType)
sum(is.na(airfare_rev$ItineraryType)) # 0

# Removing NA from ProductType
library(dplyr)
library(tidyr)
airfare_rev<- airfare_rev%>%
  drop_na(ProductType)
sum(is.na(airfare_rev$ProductType)) # 0

##### Imputate missing values of NetFare based on ProductType

# First crosscheck NA values in netfare based on PT 'refund' & 'payment' prior to imputation
airfare_rev %>%
  filter(ProductType=="refund")%>%
  group_by(NetFare)%>%
  summarise(number=n(),mean_Netfare = mean(NetFare),Minimum=min(NetFare),Maximum=max(NetFare))
# Found 5495 records with NA value in NetFare under PT 'refund'

airfare_rev %>%
  filter(ProductType=="payment")%>%
  group_by(NetFare)%>%
  summarise(number=n(),mean_Netfare = mean(NetFare),Minimum=min(NetFare),Maximum=max(NetFare))
# Found 55394 records with NA value in NetFare under PT 'payment'

airfare_rev$NetFare[is.na(airfare_rev$NetFare)] <- 0

sapply(airfare_rev,function(x) sum(is.na(x))) # 0,0,0,0,0,0,0,0,0 

##### Split the data based on Itinerytype for future model building ####
# Create new dataset for domestic 
#domestic <- airfare_rev %>%
# filter(ItineraryType=="Domestic")

# Create new dataset for international
international <- airfare_rev %>%
  filter(ItineraryType=="International")
##############################Forecasting##########################
#____________________________________________________________________
#Merging the split of date using sprintf fun and created a new international dataset
InvoiceDate1<-with(international,dmy(sprintf('%02d%02d%04d',Date,Month,Year)))
InvoiceDate1     
#Creat a new data frame with only NetFare and InvoiceDate
international1<-data.frame(international$NetFare,InvoiceDate1)
#View(international1)
names(international1)<-c("NetFare","InvoiceDate1")
#str(international1)
#######Grouping based on Dates############
#Created a new internatinal2 data frame inorder to group the date
international2<-data.frame(international1)%>%
  group_by(InvoiceDate1)%>%
  summarise(mean_netfare=mean(NetFare,na.rm = T))
names(international2)<-c("InvoiceDate1","M_NetFare")
View(international2)
#Plot
plot(international2$M_NetFare,main='Plot of Mean NetFare',type = 'o')
#ggplot Netfare v/s Invoicedates
ggplot(international2,aes(x=InvoiceDate1,y=M_NetFare,group=1))+
  geom_point()+geom_line(color="#69b3a3")+theme_classic()+
  labs(X="date",y="M_NetFare",title="Netfare v/s Invoicedate")+
  scale_x_date(date_labels = "%y-%b-%d",date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=60,hjust = 1))
#######Grouping based on MONTHs############
#Created a new internatinal2 data frame inorder to group the month
international3<-data.frame(international1)%>%
  group_by(month=floor_date(InvoiceDate1,"month"))%>%
  summarise(mean_netfare=mean(NetFare,na.rm = T))
names(international3)<-c("InvoiceDate1","M_NetFare")
#View(international3)
#ggplot Netfare v/s month
ggplot(international3,aes(x=InvoiceDate1,y=M_NetFare,group=1))+
  geom_point()+geom_line(color="#69b3a3")+theme_classic()+
  labs(X="Month",y="M_NetFare",title="Netfare v/s Invoicedate based on Month")+
  scale_x_date(date_labels = "%y-%b",date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=60,hjust = 1))

##################
#___________________________________________________________________
#Split data into Test and Train and conver international2 to Time Series
#Merging the split of date using sprintf fun and created a new international dataset
InvoiceDate1<-with(international,dmy(sprintf('%02d%02d%04d',Date,Month,Year)))
InvoiceDate1     
#Creat a new data frame with only NetFare and InvoiceDate
international1<-data.frame(international$NetFare,InvoiceDate1)
#View(international1)
names(international1)<-c("NetFare","InvoiceDate1")
#str(international1)
#######Grouping based on Dates############
#Created a new internatinal2 data frame inorder to group the date
international2<-data.frame(international1)%>%
  group_by(InvoiceDate1)%>%
  summarise(mean_netfare=mean(NetFare,na.rm = T))
names(international2)<-c("InvoiceDate1","M_NetFare")
View(international2)
#Plot
plot(international2$M_NetFare,main='Plot of Mean NetFare',type = 'o')
#ggplot Netfare v/s Invoicedates
ggplot(international2,aes(x=InvoiceDate1,y=M_NetFare,group=1))+
  geom_point()+geom_line(color="#69b3a3")+theme_classic()+
  labs(X="date",y="M_NetFare",title="Netfare v/s Invoicedate")+
  scale_x_date(date_labels = "%y-%b-%d",date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=60,hjust = 1))
#######Grouping based on MONTHs############
#Created a new internatinal2 data frame inorder to group the month
international3<-data.frame(international1)%>%
  group_by(month=floor_date(InvoiceDate1,"month"))%>%
  summarise(mean_netfare=mean(NetFare,na.rm = T))
names(international3)<-c("InvoiceDate1","M_NetFare")
#View(international3)
#ggplot Netfare v/s month
ggplot(international3,aes(x=InvoiceDate1,y=M_NetFare,group=1))+
  geom_point()+geom_line(color="#69b3a3")+theme_classic()+
  labs(X="Month",y="M_NetFare",title="Netfare v/s Invoicedate based on Month")+
  scale_x_date(date_labels = "%y-%b",date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=60,hjust = 1))

##################
#___________________________________________________________________
#Split data into Test and Train and conver international2 to Time Series
#set.seed(101) 
#sample <- sample.int(n = nrow(international2), size = floor(.9*nrow(international2)))
#train <- international2[sample, ]
#test  <- international2[-sample, ]
dat_train<-international2[1:284,]
dat_test<-international2[285:406,]
#_________________________________________________________________________________________________________________________
################################FORECASTING########################
#_____________________________________________________________________
library(forecast)
library(readr)
library(ggplot2)
library(fpp2)
library(fpp)
library(TTR)
library(dplyr)
library(MLmetrics)
library(stats)
#convert the data into a time series object 
dat_ts <- ts(dat_train$M_NetFare, start = c(2018,91), frequency = 365)
#dat_ts <- ts(dat_train$M_NetFare, start = c(2018,91), end=c(2018,161),frequency = 365)
#utility function for calculating Mean Absolute Percentage Error (or MAPE)
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}
#library(tseries)
adf.test(dat_ts) 
#kpss.test(dat_ts)
#adf.test(dat_ts, alternative = c("stationary", "explosive"),
        # k = trunc((length(dat_ts)-1)^(1/3)))

################
naive_mod <- naive(dat_ts,h=122)#'h' specifies the number of values you want to forecast
autoplot(naive_mod,type='b')
checkresiduals(naive_mod)#p-value < 2.2e-16(0.00000000000000022)
summary(naive_mod)#Residual sd: 33137.3147 
accuracy(naive_mod,dat_test$M_NetFare)
#df_nm<-as.data.frame(naive_mod)
#dat_test$naive =df_nm$`Point Forecast`
#MAPE_Naive<-mape(dat_test$M_NetFare, dat_test$naive)
#MAPE_Naive 
#Ac_naive<-accuracy(dat_test$M_NetFare, dat_test$naive)

################# Time series using Facebook's Prophet #########
# Plot
View(international2) # Date wise Data
library(ggplot2)
qplot(InvoiceDate1,M_NetFare,data = international2)
summary(international2)
ds <- international2$InvoiceDate1
y <- international2$M_NetFare
df_prophet_1 <- data.frame(ds,y)
qplot(ds,y,data = df_prophet_1)

# Forecasting with Facebook's Phrophet
library(prophet)
prophet_model_1 <- prophet(df_prophet_1)

# Prediction
future_1 <- make_future_dataframe(prophet_model_1,periods = 90)
tail(future_1)
prophet_forecast_1 <- predict(prophet_model_1,future_1)
tail(prophet_forecast_1[c('ds','yhat','yhat_lower','yhat_upper')])

# Plot Forecast
plot(prophet_model_1,prophet_forecast_1)
prophet_plot_components(prophet_model_1,prophet_forecast_1)

# MAPE
pred_prophet_1 <- prophet_forecast_1$yhat[1:406]
MAPE_prophet_1 <- mape(international2$M_NetFare,pred_prophet_1)
MAPE_prophet_1 # 247.8666
mape(international2$M_NetFare,pred_prophet_1)*100 # 247.8666

summary(international2)
# RMSE
library(Metrics)
rmse(international2$M_NetFare,pred_prophet_1) 
#20515.04


