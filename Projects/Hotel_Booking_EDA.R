df<-read.csv("hotel_bookings.csv",header = T)
View(df)
summary(df)
# Features
# 
# hotel: Resort Hotel or City Hotel
# is_canceled: Value indicating if the booking was canceled (1) or not (0)
# lead_time: Number of days between the booking date to the arrival date
# arrival_date_year: Year of arrival
# arrival_date_month: Month of arrival
# arrival_date_week_number: Week number according to year of arrival
# arrival_date_day_of_month: Day of arrival
# stays_in_weekend_nights: Number of weekend nights booked (Saturday or Sunday)
# stays_in_week_nights: Number of week nights booked (Monday to Friday)
# adults: Number of adults
# children: Number of children
# babies: Number of babies
# meal: Type of meal booked
# country: Country of origin
# market_segment: Market segment designation, typically influences the price sensitivity
# distribution_channel: Booking distribution channel, refers to how the booking was made
# is_repeated_guest: Value indication if the booking was from a repeated guest (1) or not (0)
# previous_cancellations: Number of previous cancellations prior to current booking
# previous_bookings_not_canceled: Number of previous booking not canceled prior to current booking
# reserved_room_type: Code of room type reserved
# assigned_room_type: Code for the type of room assigned to the booking
# booking_changes: Number of changes made to the booking since entering the hotel management system
# deposit_type: Type of deposit made for the reservation
# agent: ID of the travel agency that made the booking
# company: ID of the company/organization that made the booking or is responsible for payment
# days_in_waiting_list: Number of days booking was in the waiting list until it was confirmed
# customer_type: Type of booking
# adr: Average Daily Rate (the sum of transactions divided by the number of nights stayed)
# required_car_parking_spaces: Number of car parking spaces requested
# total_of_special_requests: Number of special requests made by the customer
# reservation_status: Last reservation status (Canceled, Check-Out, No-Show)
# reservation_status_date: Date at which the last status was set

#Spliting into train test split
smp_size<-floor(0.75*(nrow(df)))
set.seed(123)
train_ind<-sample(seq_len(nrow(df)),size=smp_size)

train_df<-df[train_ind,]
test_df<-df[-train_ind,]


#Printing no. of rows in each train,test,df
nrow(train_df)
nrow(test_df)
nrow(df)

#Copying dataset for dummy purpose
df2=train_df

colSums(is.na(df))

sum(is.na(df$children))
sum(df$country=="NULL")
sum(df$agent=="NULL")
sum(df$company=="NULL")

df2=df
sum(is.na(df2$children))
sum(df2$country=="NULL")
sum(df2$agent=="NULL")
sum(df2$company=="NULL")


x<-df2$children
sort(table(x))


df2$children
sort(table(x))

#10 children in outlier
which(grepl(10,df2$children))

grep("children",colnames(df2))

df2[329,11]

dim(df2)
#Removing 10 from dataframe
df2=df2[-c(329),]
dim(df2)

df2[329,11]

##########################

x<-df2$children
sort(table(x))

#mode
names(table(x))[table(x)==max(table(x))]

#Replacing na with mode
df2$children[is.na(df2$children)]<-names(table(x))[table(x)==max(table(x))]
#checking if any na's
sum(is.na(df2$children))

#checking for country column
x_country<-df2$country
sort(table(x_country))
names(table(x_country))[table(x_country)==max(table(x_country))]

df2$country[df2$country=="NULL"]<-names(table(x_country))[table(x_country)==max(table(x_country))]

#missing agent can be made 0 since bookings are made private
df2$agent[df2$agent=="NULL"]<-"0"
sum(df2$agent=="NULL")

#similarly for company
df2$company[df2$company=="NULL"]<-"0"
sum(df2$company=="NULL")


str(df2)
class(df2$children)="integer"
typeof(df2$children)
typeof(df2$babies)
#dataset for correlation matrix
a<-c()
k<-1
for(i in 1:length(colnames(df2))){
  if(typeof(df2[[i]])=="integer" || typeof(df2[[i]])=="double"){
    a[k]=i
    k=k+1
  }
}
a
temp<-df2[a]
View(temp)
#Total guest
temp["guest_stayed"]=temp[["adults"]]+temp[["children"]]+temp[["babies"]]
View(temp)

#create col with total nights booked
temp["nights_stayed"]=temp[["stays_in_week_nights"]]+temp[["stays_in_weekend_nights"]]

#checking correlation matrix again
data<-cor(temp)
t<-data[,"is_canceled"]
t[order(t,decreasing = TRUE)]

# The strongest positive correlations (0.1 or more) are:
#   
#   lead_time
#   previous_cancellations
#   
# The strongest negative correlations (-0.1 or less) are:
#   
#   total_of_special_requests
#   required_car_parking_spaces
#   booking_changes

# install.packages("kdensity")
# library("kdensity")


hist(temp$lead_time,xlab = "Lead time days",col = "blue")
?hist



# install.packages("plyr")
# library("plyr")
# count(lead_time_1)
lead_time_1=temp[temp[,"lead_time"]<100,]
nrow(lead_time_1)
lead_time_2=temp[temp[,"lead_time"]<365,]
nrow(lead_time_2)
print(nrow(lead_time_2)-nrow(lead_time_1))
lead_time_3=temp[temp[,"lead_time"]>=365,]
nrow(lead_time_3)


#Cancelation increased if lead time increased
lead_cancel_1=table(lead_time_1$is_canceled)
total<-lead_cancel_1[1]+lead_cancel_1[2]
per<-lead_cancel_1[2]/total
cat("Percentage of cancelled booking between: 0 to 99:",per)
lead_cancel_2=table(lead_time_2$is_canceled)
total<-lead_cancel_2[1]+lead_cancel_2[2]
per<-lead_cancel_2[2]/total
cat("Percentage of cancelled booking between:100 to 364:",per)
lead_cancel_3=table(lead_time_3$is_canceled)
total<-lead_cancel_3[1]+lead_cancel_3[2]
per<-lead_cancel_3[2]/total
cat("Percentage of cancelled booking between: 365 or more",per)


#Previous cancellations rates
x=table(df2$previous_cancellations)

cat("Never canceled: ",mean(df2$previous_cancellations==0)*100)
cat("Cancelled once: ",mean(df2$previous_cancellations==1)*100)
cat("Cancelled more than 10 times: ",mean(df2$previous_cancellations>=10)*100)

#Booking space canceled on no parking space
x=table(df2$required_car_parking_spaces)
no_park_cancel=df2[df2["is_canceled"]==1,]
no_park_cancel=no_park_cancel[no_park_cancel["required_car_parking_spaces"]==0,]
park_cancel=df2[df2["is_canceled"]==0,]
park_cancel=park_cancel[park_cancel["required_car_parking_spaces"]==0,]
Percentage_BookingCanceled_NoParkingSapce=(nrow(no_park_cancel))/(nrow(no_park_cancel)+nrow(park_cancel))*100
cat("Percentage of booking space canceled on no parking space:",Percentage_BookingCanceled_NoParkingSapce)


#Data Cleaning
df3=train_df
library(dplyr)
df3["guest_stayed"]=df3[["adults"]]+df3[["children"]]+df3[["babies"]]
View(df3)
df3=df3[df3$guest_stayed>0,]
table(df3$guest_stayed)
#dropping adults,children,babies column
df3[,c("adults","children","babies")]<-list(NULL)
View(df3)

#Defining X_train,y_train
X_train=df3[-c(2)]
View(X_train)
y_train=df3["is_canceled"]

# Removing the Following Columns:
#   Numerical Attributes:
#     arrival_date_year: This category references towards certain years. This could be problematic for instances during years that do not appear in the training data, or perhaps have bias towards certain years specifically due to the unequal amounts of observations in the training data.
#     arrival_date_day_of_month: The column arrival date week of month generalizes this.
#     booking_changes: Could change over time, potentially causing data leakage.
#     days_in_waiting_list: Could constantly change over time. Additionally, there are many instances. This could prevent the model from generalizing.
#     agent & company: Represented by an ID. These columns are uninformative since they contain a substantial amount of various numerical values without having an actual numerical meaning. Since other columns (such as market segment) indicate the type of reservation, these columns won't be needed.
# Categorical Attributes:
#     country: There are many categories, most with few instances. In order to make a model that generalizes, it is better to dismiss this category.
#     assigned_room_type: Similar to reserved_room_type and seems like the reserved room is a more suitable choice.
#     reservation_status: Major data leakage! The categories are Check-Out, Canceled and No-Show. This is exactly what we are trying to predict.
#     reservation_status_date: This is the date when the reservation status was last changed, and therefore is irrelevant.

num_features=c("lead_time", "stays_in_weekend_nights", "stays_in_week_nights", "adults", "children", "babies",
               "is_repeated_guest", "previous_cancellations", "previous_bookings_not_canceled", "adr",
               "required_car_parking_spaces", "total_of_special_requests")
cat_features=c("hotel", "meal", "market_segment",
               "distribution_channel", "reserved_room_type", "deposit_type", "customer_type")

#Removing features
X_train[,c("arrival_date_year","arrival_date_day_of_month","booking_changes","days_in_waiting_list","agent","company",
           "country","assigned_room_type","reservation_status","reservation_status_date")]<-list(NULL)
View(X_train)


#SC and undefined are same in meal,hence replacing Undefined with meal
library(stringi)
table(X_train$meal)
X_train["meal"]=stri_replace(X_train$meal,"SC",regex = "Undefined")
table(X_train$meal)
#checking na value in hotel column
sum(is.na(X_train$hotel))
X_train[is.na(X_train$hotel),]="City Hotel"
sum(is.na(X_train$hotel))
sum(is.na(X_train))

#install.packages("mltools")
library(mltools)
library(data.table)
#install.packages("caret")
#library(caret)
# library("reshape2")

#Removing months and arriving week
X_train=X_train[-c(3,4)]
View(X_train)

#Memory limit reached
X_dum=X_train[1:10000,]
X_dum=cbind(ID=1:nrow(X_dum),X_dum)

#One hot encoding
dmy <- dummyVars(X_dum[cat_features], data = X_dum)
trsf <- data.frame(predict(dmy, newdata = X_dum))
View(trsf)
colnames(trsf)

X_dum[,c("hotel", "meal", "market_segment",
          "distribution_channel", "reserved_room_type", "deposit_type", "customer_type")]<-list(NULL)
X_dum=cbind(X_dum,trsf)
X_dum

#Normalize
for(i in 1:length(colnames(X_dum))){
  if(typeof(X_dum[[i]])!="integer"){
    class(X_dum[[i]])="integer"
  }
  #print(typeof(X_dum[[i]]))
}
X_dum=X_dum[-c(1)]
View(X_dum)
normalize=function(x){
  return((x- min(x)) /(max(x)-min(x)))
}
norm_data=as.data.frame(apply(X_dum,2,normalize))
View(norm_data)
y=y_train
y1=y[1:7000,]
View(y1)
#y_test_dum=y_test_dum["is_canceled"]
norm_data_train=norm_data[1:7000,]
norm_data_test=norm_data[7001:10000,]
View(norm_data_train)
#Contains knn function
library(class)
knn_pred_5 <- knn(train=norm_data_train,test=norm_data_test,cl=y1, k=5)
View(knn_pred_5)
#for confusion matrix
library(caret)
confusionMatrix(table(knn_pred_5,y[7001:10000,1]))
View(knn_pred_5)


#logistic regression
logistic_data_train=norm_data_train
logistic_data_train=cbind(logistic_data_train,y1)
View(logistic_data_train)
logistic_data_test=norm_data_test
target=y[7001:10000,1]
logistic_data_test=cbind(logistic_data_test,target)
View(logistic_data_test)
y_t=logistic_data_test["target"]
logistic<-glm(y1~lead_time+previous_cancellations,logistic_data_train,family = "binomial")
summary(logistic)
res<-predict(logistic,logistic_data_test,type="response")
View(res)
confmatrix1<-table(Actual_value=logistic_data_test$target,Predicted_value=res>0.2)
confmatrix2<-table(Actual_value=logistic_data_test$target,Predicted_value=res>0.3)
confmatrix3<-table(Actual_value=logistic_data_test$target,Predicted_value=res>0.4)
confmatrix1
confmatrix2
confmatrix3

