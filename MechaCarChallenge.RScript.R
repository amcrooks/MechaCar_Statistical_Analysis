#Deliverable 1
#read file
MechaCar <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
library(dplyr)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar) #generate multiple linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar)) #generate multiple linear regression model

#Deliverable 2
#read file
Suspension <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
#create summary table
total_summary <- Suspension %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))
lot_summary <- Suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Mediaon=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') #create summary table
#compare lots to population mean
t.test(Suspension$PSI, mu=1500)
t.test(subset(Suspension$PSI, Suspension$Manufacturing_Lot == "Lot1"), mu=1500)
t.test(subset(Suspension$PSI, Suspension$Manufacturing_Lot == "Lot2"), mu=1500)
t.test(subset(Suspension$PSI, Suspension$Manufacturing_Lot == "Lot3"), mu=1500)
