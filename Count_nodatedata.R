setwd("/Users/TyrusYuen/Google Drive/Sync/Year 4 Sem 1/STAT 4011/Project 1")
rawdata<-read.csv("claims.csv",header=TRUE)

library(car)
library(fastDummies)
rawdata$Month<-recode(rawdata$Month,"'Jan'=1 ;'Feb'=2; 'Mar'=3;'Apr'=4;'May'=5;'Jun'=6;'Jul'=7;'Aug'=8;'Sep'=9;'Oct'=10;'Nov'=11;'Dec'=12")
rawdata$MonthClaimed<-recode(rawdata$MonthClaimed,"'Jan'=1 ;'Feb'=2; 'Mar'=3;'Apr'=4;'May'=5;'Jun'=6;'Jul'=7;'Aug'=8;'Sep'=9;'Oct'=10;'Nov'=11;'Dec'=12")
rawdata$DayOfWeek<-recode(rawdata$DayOfWeek,"'Monday'=2;'Tuesday'=3;'Wednesday'=4;'Thursday'=5; 'Friday'=6;'Saturday'=7;'Sunday'=1")
rawdata$DayOfWeekClaimed<-recode(rawdata$DayOfWeekClaimed,"'Monday'=2;'Tuesday'=3;'Wednesday'=4;'Thursday'=5; 'Friday'=6;'Saturday'=7;'Sunday'=1")
rawdata$Sex<-recode(rawdata$Sex,"'Male'=1;'Female'=0")
rawdata$PastNumberOfClaims<-recode(rawdata$PastNumberOfClaims,"'none'=0")
rawdata$NumberOfSuppliments<-recode(rawdata$NumberOfSuppliments,"'none'=0")
rawdata$AgentType<-recode(rawdata$AgentType,"'External'=0;'Internal'=1")
rawdata$Fault<-recode(rawdata$Fault,"'Policy Holder'=0;'Third Party'=1")
rawdata$AccidentArea<-recode(rawdata$AccidentArea,"'Urban'=0;'Rural'=1")
rawdata$PoliceReportFiled<-recode(rawdata$PoliceReportFiled,"'Yes'=1;'No'=0")
rawdata$WitnessPresent<-recode(rawdata$WitnessPresent,"'Yes'=1;'No'=0")

rawdata$Make<-recode(rawdata$Make,"'Lexus'=1;'Ferrari'=2;'Mecedes'=3;'Porche'=4;'Jaguar'=5; 'BMW'=6;'Nisson'=7;'Saturn'=8;'Mercury'=9;'Saab'=10;'Dodge'=11;'VW'=12;'Ford'=13;'Accura'=14;'Chevrolet'=15;'Mazda'=16;'Honda'=17;'Toyota'=18;'Pontiac'=19")
rawdata$MaritalStatus<-recode(rawdata$MaritalStatus,"'Widow'=1;'Divorced'=2;'Single'=3;'Married'=4")
rawdata$PolicyType<-recode(rawdata$PolicyType,"'Sport - Liability'=1;'Utility - Liability'=2;'Sport - All Perils'=3;'Utility - Collision'=4;'Utility - All Perils'=5;'Sport - Collision'=6;'Sedan - All Perils'=7;'Sedan - Liability'=8;'Sedan - Collision'=9")
rawdata$BasePolicy<-recode(rawdata$BasePolicy,"'All Perils'=1;'Liability'=2;'Collision'=3")
rawdata$VehicleCategory<-recode(rawdata$VehicleCategory,"'Utility'=1;'Sport'=2;'Sedan'=3")
rawdata$VehiclePrice<-recode(rawdata$VehiclePrice,"'20000 to 29000'=2;'30000 to 39000'=3;'40000 to 59000'=4;'60000 to 69000'=5;'less than 20000'=1;'more than 69000'=6")
rawdata$VehiclePrice<-recode(rawdata$VehiclePrice,"'20000 to 29000'=2;'30000 to 39000'=3;'40000 to 59000'=4;'60000 to 69000'=5;'less than 20000'=1;'more than 69000'=6")
rawdata$Days_Policy_Accident<-recode(rawdata$Days_Policy_Accident,"'none'=1;'1 to 7'=2;'15 to 30'=3;'8 to 15'=4;'more than 30'=5")
rawdata$Days_Policy_Claim<-recode(rawdata$Days_Policy_Claim,"'none'=1;'1 to 7'=2;'15 to 30'=3;'8 to 15'=4;'more than 30'=5")
rawdata$PastNumberOfClaims<-recode(rawdata$PastNumberOfClaims,"'0'=1;'1'=2;'2 to 4'=3;'more than 4'=4;")
rawdata$AgeOfVehicle<-recode(rawdata$AgeOfVehicle,"'new'=1;'2 years'=2;'3 years'=3;'4 years'=4;'5 years'=5;'6 years'=6;'7 years'=7;'more than 7'=8")
rawdata$AgeOfPolicyHolder<-recode(rawdata$AgeOfPolicyHolder,"'16 to 17'=1;'18 to 20'=2;'21 to 25'=3;'26 to 30'=4;'31 to 35'=5;'36 to 40'=6;'41 to 50'=7;'51 to 65'=8; 'over 65'=9")
rawdata$NumberOfSuppliments<-recode(rawdata$NumberOfSuppliments,"'0'=1;'1 to 2'=2;'3 to 5'=3;'more than 5'=4;")
rawdata$AddressChange_Claim<-recode(rawdata$AddressChange_Claim,"'no change'=1;'under 6 months'=2;'1 year'=3;'2 to 3 years'=4;'4 to 8 years'=5;")
rawdata$NumberOfCars<-recode(rawdata$NumberOfCars,"'1 vehicle'=1;'2 vehicles'=2;'3 to 4'=3;'5 to 8'=4;'more than 8'=5;")

#30 includes 'Under 6 months'
#15, 24, 25, 29, 30,31 included 'more than'/'Over'
#15, 21, 22, 23, 29, 31 are interval
#17 & 18 are ID number, not included in analysis


write.csv(rawdata,file="Count_nodatedata.csv")

#obtain the date by excel after recoding
