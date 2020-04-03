
# Packages ----------------------------------------------------------------

library('tidyverse')
library('readxl')
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(ggcorrplot)


# Read data ---------------------------------------------------------------

Flight_data_1<- read_excel("C:/Users/Vishnu/Desktop/Spring 2020/Statistical Modeling/FAA1-1.xls")
Flight_data_2<- read_excel("C:/Users/Vishnu/Desktop/Spring 2020/Statistical Modeling/FAA2-1.xls")

head(Flight_data_1)
dim(Flight_data_1)
#800 8
lapply(Flight_data_1,class)
str(Flight_data_1)

head(Flight_data_2)
dim(Flight_data_2)
#150 7 - no duration column
lapply(Flight_data_2,class)
str(Flight_data_2)



# Append datasets ----------------------------------------------------------


flights <- dplyr::bind_rows(Flight_data_1, Flight_data_2)


# Check for duplicates ----------------------------------------------------

sum(duplicated(flights[,!names(flights) %in% ('duration')]))

flights[duplicated(flights[,!names(flights) %in% ('duration')]),]

flights<-flights[!duplicated(flights[,!names(flights) %in% ('duration')]),]

dim(flights)

str(flights)

summary(flights)



# Data Cleaning and Exploration -------------------------------------------

lapply(flights,class)

sapply(flights, function(x) sum(is.na(x)))

unique_fd<-unique(flights)

unique_fd$aircraft<- as.factor(unique_fd$aircraft)


# Identify abnormal values ------------------------------------------------


summary(unique_fd)

sum(unique_fd$duration<40,na.rm = T)
sum(unique_fd$speed_ground < 30 | unique_fd$speed_ground>140,na.rm=T)
sum(unique_fd$speed_air < 30 | unique_fd$speed_air>140,na.rm=T)
sum(unique_fd$height < 6)
sum(unique_fd$distance > 6000)



# Cleaning abnormal values ------------------------------------------------


filtered_data<-unique_fd %>%
  filter(!((unique_fd$speed_ground < 30 | unique_fd$speed_ground>140) |
             (unique_fd$height < 6)|
             (unique_fd$distance > 6000) |
             !(unique_fd$duration>40 | is.na(unique_fd$duration)) |
             !(unique_fd$speed_air>0 | unique_fd$speed_air<=140 | is.na(unique_fd$speed_air))))

dim(filtered_data)
sum(filtered_data$duration<40,na.rm = T)
sum(filtered_data$speed_ground < 30 | filtered_data$speed_ground>140,na.rm=T)
sum(filtered_data$speed_air < 30 | filtered_data$speed_air>140,na.rm=T)
sum(filtered_data$height < 6)
sum(filtered_data$distance > 6000)


# Structure and Summary of Clean data -------------------------------------


str(filtered_data)
summary(filtered_data)



# Multinomial response ----------------------------------------------------


flight_clean<-filtered_data

flight_clean$Y<-ifelse(flight_clean$distance<1000,1,ifelse(flight_clean$distance>=1000 & 
                                                             flight_clean$distance<2500,2,3))
flight_clean$Y<-as.factor(flight_clean$Y)
flight_clean<-flight_clean[,-which(colnames(flight_clean)=='distance')]

names<-c(colnames(flight_clean))

plot1<-ggplot(flight_clean, aes(x = speed_ground,fill=Y)) + geom_histogram(position = 'dodge') 
plot2<-ggplot(flight_clean, aes(x = speed_air,fill=Y)) + geom_histogram(position = 'dodge') 
plot3<-ggplot(flight_clean, aes(x = pitch,fill=Y)) + geom_histogram(position = 'dodge') 
plot4<-ggplot(flight_clean, aes(x = no_pasg,fill=Y)) + geom_histogram(position = 'dodge') 
plot5<-ggplot(flight_clean, aes(x = duration,fill=Y)) + geom_histogram(position = 'dodge') 
plot6<-ggplot(flight_clean, aes(x = height,fill=Y)) + geom_histogram(position = 'dodge') 

plot_grid(plot1, plot2,plot3,plot4,plot5,plot6, labels = "AUTO")



# Histograms --------------------------------------------------------------

nums <- unlist(lapply(flight_clean, is.numeric)) 
flights_numeric<-flight_clean[,nums]
ggplot(gather(flights_numeric), aes(value)) + geom_histogram(bins = 10) + facet_wrap(~key, scales = 'free_x')


tablesq<-table(flight_clean$Y,flight_clean$aircraft,dnn=c('Response','Flight categories'))
chisq.test(tablesq)
view(tablesq)

# Correlation between all numeric variables -------------------------------

flights_corr<- flights_numeric[,!names(flights_numeric) %in% c("Y","aircraft")]
corr<-cor(flights_corr,use = "complete.obs")
ggcorrplot(corr, hc.order = TRUE, type = "lower",lab = TRUE)



# Removing highly correlated variable - speed_air -------------------------

flight_clean<-flight_clean[,-which(colnames(flight_clean)=='speed_air')]
head(flight_clean)


# Releveling data ---------------------------------------------------------

flight_clean$out<-relevel(flight_clean$Y,ref='1')
flight_clean<-flight_clean[,-which(colnames(flight_clean)=='Y')]
library('nnet')
mmod<-multinom(out~.,flight_clean)
summary(mmod)

z<- summary(mmod)$coefficients/summary(mmod)$standard.errors
p<-(1 - pnorm(abs(z), 0, 1)) * 2

flights<-flight_clean[complete.cases(flight_clean), ]
table(flights$out,predict(mmod),dnn=c("Actual","Pred"))

mmod1<-multinom(out~aircraft+speed_ground+height,flights)
mmod2<-step(mmod1,direction = 'backward',k=2)
summary(mmod2)
exp(coef(mmod2)*10)


z2<- summary(mmod2)$coefficients/summary(mmod2)$standard.errors
p2<-(1 - pnorm(abs(z2), 0, 1)) * 2
table(flights$out,predict(mmod2),dnn=c("Actual","Pred"))



# Poisson model -----------------------------------------------------------

filtered_data
summary(filtered_data)

par(mfrow=c(1,1))

hist(filtered_data$no_pasg)

nums <- unlist(lapply(filtered_data, is.numeric)) 
flights_numeric<-filtered_data[,nums]
corr<-cor(flights_numeric,use = "complete.obs")
corr[2,]


model_1<-glm(no_pasg~aircraft,family = poisson,filtered_data)
model_2<-glm(no_pasg~speed_ground,family = poisson,filtered_data)
model_3<-glm(no_pasg~speed_air,family = poisson,filtered_data)
model_4<-glm(no_pasg~height,family = poisson,filtered_data)
model_5<-glm(no_pasg~pitch,family = poisson,filtered_data)
model_6<-glm(no_pasg~distance,family = poisson,filtered_data)

summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)
summary(model_5)
summary(model_6)



model_p<-glm(no_pasg~.,family = poisson,filtered_data)
summary(model_p)

library(car)
vif(model_p)


flight_filtered2<-filtered_data[,-which(colnames(filtered_data)=='speed_air')]
model2_p<-glm(no_pasg~.,family = poisson,flight_filtered2)
summary(model2_p)
vif(model2_p)

step(model2_p)
drop1(model2_p,test = "Chisq")
round(predict(model2_p,type ="response"),1)
flight_filtered2$no_pasg

gof<-sum(residuals(model2_p,type='pearson')^2)
pchisq(gof,df.residual(model2_p),lower=F)

dp<-gof/model2_p$df.residual
summary(model2_p,dispersion=dp)

#NO VARIABLES
