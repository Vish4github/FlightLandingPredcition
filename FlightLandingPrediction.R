

# Packages ----------------------------------------------------------------



library('tidyverse')
library('readxl')


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

# long landing and risky landing ------------------------------------------


filtered_data$long.landing<-ifelse(filtered_data$distance>2500, 1, 0)
filtered_data$risky.landing<-ifelse(filtered_data$distance>3000, 1, 0)



# Removing distance column ------------------------------------------------


drops<-c("distance")
flight_data_clean<-filtered_data[ , !(names(filtered_data) %in% drops)]
names(flight_data_clean)


# Step 2 ------------------------------------------------------------------

hist(flight_data_clean$long.landing,main="Long Landing",
     xlab="Long landing",
     col="darkmagenta",
     freq=TRUE)

filtered_data$long.landing<-as.factor(filtered_data$long.landing)
filtered_data$risky.landing<-as.factor(filtered_data$risky.landing)

# Step 3 ------------------------------------------------------------------

model1 <- glm(long.landing ~ speed_ground,family = binomial, data = flight_data_clean)
model2 <- glm(long.landing ~ speed_air, family=binomial,data = flight_data_clean)
model3 <- glm(long.landing ~ duration,family=binomial, data = flight_data_clean)
model4 <- glm(long.landing ~ pitch,family=binomial, data = flight_data_clean)
model5 <- glm(long.landing ~ aircraft,family=binomial, data = flight_data_clean)
model6 <- glm(long.landing ~ height,family=binomial, data = flight_data_clean)
model7 <- glm(long.landing ~ no_pasg,family=binomial, data = flight_data_clean)


summary(model1)$coefficients


# Creating Table 2 --------------------------------------------------------

coeffs<- c(summary(model1)$coefficients[2,1],summary(model2)$coefficients[2,1],summary(model3)$coefficients[2,1],
                       summary(model4)$coefficients[2,1],summary(model5)$coefficients[2,1],
                       summary(model6)$coefficients[2,1],summary(model7)$coefficients[2,1])

odds.ratio<- exp(coeffs)

p_values<- c(summary(model1)$coefficients[2,4],summary(model2)$coefficients[2,4],summary(model3)$coefficients[2,4],
             summary(model4)$coefficients[2,4],summary(model5)$coefficients[2,4],
             summary(model6)$coefficients[2,4],summary(model7)$coefficients[2,4])

Table_2 <- data.frame(variable = c('speed_ground','speed_air', 'duration','pitch', 'aircraft', 'height', 'no_pasg' ), P_values = p_values, Coeffs = coeffs, OddsRatio=odds.ratio)
Table_2['Sign']<-apply(Table_2['Coeffs'], 1, function(x) { if(x<0){'Negative'} else{'Positive'} })
Table_2<- Table_2[order(abs(Table_2$P_values)),]


# Scaling Variables -------------------------------------------------------

flight_stand<-flight_data_clean

flight_stand[c(2:7)] <- lapply(flight_stand[c(2:7)], function(x) c(scale(x)))

models1 <- glm(long.landing ~ speed_ground,family=binomial, data = flight_stand)
models2 <- glm(long.landing ~ speed_air,family=binomial, data = flight_stand)
models3 <- glm(long.landing ~ duration,family=binomial, data = flight_stand)
models4 <- glm(long.landing ~ pitch,family=binomial, data = flight_stand)
models5 <- glm(long.landing ~ aircraft,family=binomial, data = flight_stand)
models6 <- glm(long.landing ~ height,family=binomial, data = flight_stand)
models7 <- glm(long.landing ~ no_pasg,family=binomial, data = flight_stand)


#class(flight_stand$aircraft)

# Creating Table 3 --------------------------------------------------------

coeffs_s<- c(summary(models1)$coefficients[2,1],summary(models2)$coefficients[2,1],summary(models3)$coefficients[2,1],
           summary(models4)$coefficients[2,1],summary(models5)$coefficients[2,1],
           summary(models6)$coefficients[2,1],summary(models7)$coefficients[2,1])
odds.ratio_s<- exp(coeffs_s)
p_values_s<- c(summary(models1)$coefficients[2,4],summary(models2)$coefficients[2,4],summary(models3)$coefficients[2,4],
             summary(models4)$coefficients[2,4],summary(models5)$coefficients[2,4],
             summary(models6)$coefficients[2,4],summary(models7)$coefficients[2,4])

Table_3 <- data.frame(variable = c('speed_ground','speed_air', 'duration','pitch', 'aircraft', 'height', 'no_pasg' ), 
                      Coefficient_vals = coeffs_s, P_value=p_values_s, Odds_ratio=odds.ratio_s)
Table_3['Sign']<-apply(Table_3['Coefficient_vals'], 1, function(x) { if(x<0){'Negative'} else{'Positive'} })
Table_3_ordered<- Table_3[order(-abs(Table_3$Coefficient_vals)),]


Table_3_ordered

view(Table_3_ordered)


# Step 4 Visualizing association -------------------------------------------------

flight_stand$long.landing<-as.factor(flight_stand$long.landing)
flight_stand$risky.landing<-as.factor(flight_stand$risky.landing)

flight_data_clean$risky.landing<-as.factor(flight_data_clean$risky.landing)
flight_data_clean$long.landing<-as.factor(flight_data_clean$long.landing)


par(mfrow=c(1,1))

ggplot(flight_stand, aes(x = speed_air,fill=long.landing)) + geom_histogram(position = 'dodge') 
ggplot(flight_stand, aes(x = duration,fill=long.landing)) + geom_histogram(position = 'dodge') 
#ggplot(flight_stand, aes(x = aircraft,fill=long.landing)) + geom_histogram(position = 'dodge') 
ggplot(flight_stand, aes(x = speed_ground,fill=long.landing)) + geom_histogram(position = 'dodge') 
ggplot(flight_stand, aes(x = height,fill=long.landing)) + geom_histogram(position = 'dodge') 
ggplot(flight_stand, aes(x = pitch,fill=long.landing)) + geom_histogram(position = 'dodge') 
ggplot(flight_stand, aes(x = no_pasg,fill=long.landing)) + geom_histogram(position = 'dodge') 


table.edu<- table(flight_stand$aircraft, flight_stand$long.landing)
table.edu
chisq.test(table.edu)


nums <- unlist(lapply(flight_stand, is.numeric)) 
flight_numeric<-flight_stand[,nums]
corr<-cor(flight_numeric,use = "complete.obs")

library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, type = "lower",lab = TRUE)


# Step 5 : Full model -----------------------------------------------------
head(flight_data_clean)
flights_full_model0<- flight_data_clean[,!names(flight_data_clean) %in% c("risky.landing","speed_air")]

full_model.flights.glm0<-glm(long.landing ~ .,family=binomial,data=flights_full_model0)
summary(full_model.flights.glm0)

vif(full_model.flights.glm0)
nums <- unlist(lapply(flights_full_model0, is.numeric)) 
flight_numeric<-flights_full_model0[,nums]
corr<-cor(flight_numeric,use="complete.obs")
ggcorrplot(corr, hc.order = TRUE, type = "lower",lab = TRUE)


drop1(full_model.flights.glm0,test="Chi")

full_model.flights.glm<-glm(long.landing ~ aircraft+speed_ground+height,family=binomial,data=flights_full_model0)
summary(full_model.flights.glm)

vif(full_model.flights.glm)
# Step 6 : Forward stepwise regression using AIC --------------------------

null_model_flights<-glm(long.landing~1,family = binomial,data=flights_full_model0)

full_model.flights.glm.AIC<-step(null_model_flights,scope=list(lower=null_model_flights,upper=full_model.flights.glm),
                                 direction='forward')
summary(full_model.flights.glm.AIC)

# Step 7 : Forward stepwise regression using BIC --------------------------

full_model.flights.glm.BIC<-step(null_model_flights,scope=list(lower=null_model_flights,upper=full_model.flights.glm),
                                 direction='forward',k=log(nrow(flights_full_model0)))

summary(full_model.flights.glm.BIC)
coef(full_model.flights.glm.BIC)
exp(0.926)

# Step 8 : Present one model ----------------------------------------------



# Step 9: 

#Risky landing-----------------------------------------------------------------

par(mfrow=c(1,1))

hist(flight_data_clean$risky.landing,main="Risky Landing",xlab="Risky Landing",col="darkmagenta",
     freq=TRUE)

model1.rl <- glm(risky.landing ~ speed_ground,family = binomial, data = flight_data_clean)
model2.rl <- glm(risky.landing ~ speed_air, family=binomial,data = flight_data_clean)
model3.rl <- glm(risky.landing ~ duration,family=binomial, data = flight_data_clean)
model4.rl <- glm(risky.landing ~ pitch,family=binomial, data = flight_data_clean)
model5.rl <- glm(risky.landing ~ aircraft,family=binomial, data = flight_data_clean)
model6.rl <- glm(risky.landing ~ height,family=binomial, data = flight_data_clean)
model7.rl <- glm(risky.landing ~ no_pasg,family=binomial, data = flight_data_clean)


summary(model1.rl)$coefficients


# Creating Table 2 --------------------------------------------------------

coeffs.rl<- c(summary(model1.rl)$coefficients[2,1],summary(model2.rl)$coefficients[2,1],summary(model3.rl)$coefficients[2,1],
                       summary(model4.rl)$coefficients[2,1],summary(model5.rl)$coefficients[2,1],
                       summary(model6.rl)$coefficients[2,1],summary(model7.rl)$coefficients[2,1])

odds.ratio.rl<- exp(coeffs.rl)

p_values.rl<- c(summary(model1.rl)$coefficients[2,4],summary(model2.rl)$coefficients[2,4],summary(model3.rl)$coefficients[2,4],
             summary(model4.rl)$coefficients[2,4],summary(model5.rl)$coefficients[2,4],
             summary(model6.rl)$coefficients[2,4],summary(model7.rl)$coefficients[2,4])

Table_2.rl <- data.frame(variable = c('speed_ground','speed_air', 'duration','pitch', 'aircraft', 'height', 'no_pasg' ), P_values = p_values.rl, Coeffs = coeffs.rl, OddsRatio=odds.ratio.rl)
Table_2.rl['Sign']<-apply(Table_2.rl['Coeffs'], 1, function(x) { if(x<0){'Negative'} else{'Positive'} })
Table_2.rl<- Table_2.rl[order(abs(Table_2.rl$P_values)),]


# Scaling Variables -------------------------------------------------------

flight_stand.rl<-flight_data_clean

flight_stand.rl[c(2:7)] <- lapply(flight_stand.rl[c(2:7)], function(x) c(scale(x)))

models1.rl <- glm(risky.landing ~ speed_ground,family=binomial, data = flight_stand.rl)
models2.rl <- glm(risky.landing ~ speed_air,family=binomial, data = flight_stand.rl)
models3.rl <- glm(risky.landing ~ duration,family=binomial, data = flight_stand.rl)
models4.rl <- glm(risky.landing ~ pitch,family=binomial, data = flight_stand.rl)
models5.rl <- glm(risky.landing ~ aircraft,family=binomial, data = flight_stand.rl)
models6.rl <- glm(risky.landing ~ height,family=binomial, data = flight_stand.rl)
models7.rl <- glm(risky.landing ~ no_pasg,family=binomial, data = flight_stand.rl)

#class(flight_stand$aircraft)

# Creating Table 3 --------------------------------------------------------

coeffs_s.rl<- c(summary(models1.rl)$coefficients[2,1],summary(models2.rl)$coefficients[2,1],summary(models3.rl)$coefficients[2,1],
           summary(models4.rl)$coefficients[2,1],summary(models5.rl)$coefficients[2,1],
           summary(models6.rl)$coefficients[2,1],summary(models7.rl)$coefficients[2,1])
odds.ratio_s.rl<- exp(coeffs_s.rl)
p_values_s.rl<- c(summary(models1.rl)$coefficients[2,4],summary(models2.rl)$coefficients[2,4],summary(models3.rl)$coefficients[2,4],
             summary(models4.rl)$coefficients[2,4],summary(models5.rl)$coefficients[2,4],
             summary(models6.rl)$coefficients[2,4],summary(models7.rl)$coefficients[2,4])

Table_3.rl <- data.frame(variable = c('speed_ground','speed_air', 'duration','pitch', 'aircraft', 'height', 'no_pasg'  ), Coefficient_vals = coeffs_s.rl, P_value=p_values_s.rl, Odds_ratio=odds.ratio_s.rl)
Table_3.rl['Sign']<-apply(Table_3.rl['Coefficient_vals'], 1, function(x) { if(x<0){'Negative'} else{'Positive'} })
Table_3_ordered.rl<- Table_3.rl[order(-abs(Table_3.rl$Coefficient_vals)),]

view(Table_3_ordered.rl)

# Visualizing association -------------------------------------------------

flight_stand.rl$long.landing<-as.factor(flight_stand.rl$long.landing)
flight_stand.rl$risky.landing<-as.factor(flight_stand.rl$risky.landing)

ggplot(flight_stand.rl, aes(x = speed_air,fill=risky.landing)) + geom_histogram(position = 'dodge') 
ggplot(flight_stand.rl, aes(x = duration,fill=risky.landing)) + geom_histogram(position = 'dodge') 
#ggplot(flight_stand, aes(x = aircraft,fill=long.landing)) + geom_histogram(position = 'dodge') 
ggplot(flight_stand.rl, aes(x = speed_ground,fill=risky.landing)) + geom_histogram(position = 'dodge') 
ggplot(flight_stand.rl, aes(x = height,fill=risky.landing)) + geom_histogram(position = 'dodge') 
ggplot(flight_stand.rl, aes(x = pitch,fill=risky.landing)) + geom_histogram(position = 'dodge') 
ggplot(flight_stand.rl, aes(x = no_pasg,fill=risky.landing)) + geom_histogram(position = 'dodge') 


table.rl<- table(flight_stand.rl$aircraft, flight_stand$risky.landing)
table.rl
chisq.test(table.rl)


nums.rl <- unlist(lapply(flight_stand.rl, is.numeric)) 
flight_numeric.rl<-flight_stand.rl[,nums.rl]
corr.rl<-cor(flight_numeric.rl,use = "complete.obs")

library(ggcorrplot)
ggcorrplot(corr.rl, hc.order = TRUE, type = "lower",lab = TRUE)


# Full model -----------------------------------------------------
head(flight_stand.rl)
flights_full_model0.rl<- flight_data_clean[,!names(flight_data_clean) %in% c("long.landing","speed_air")]

full_model.flights.glm.rl0<-glm(risky.landing ~ .,family=binomial,data=flights_full_model0.rl)
summary(full_model.flights.glm.rl0)

drop1(full_model.flights.glm.rl0)

full_model.flights.glm.rl<-glm(risky.landing ~ aircraft+speed_ground,family=binomial,data=flights_full_model0.rl)
summary(full_model.flights.glm.rl)

# Forward stepwise regression using AIC --------------------------

null_model_flights.rl<-glm(risky.landing~1,family = binomial,data=flights_full_model0.rl)

full_model.flights.glm.rl.AIC<-step(null_model_flights.rl,scope=list(lower=null_model_flights.rl,upper=full_model.flights.glm.rl),direction='forward')

summary(full_model.flights.glm.rl.AIC)

#  Forward stepwise regression using BIC --------------------------

full_model.flights.glm.rl.BIC<-step(null_model_flights.rl,scope=list(lower=null_model_flights.rl,upper=full_model.flights.glm.rl),direction='forward',k=log(nrow(flights_full_model0.rl)))

summary(full_model.flights.glm.rl.BIC)
coef(full_model.flights.glm.rl.BIC)
summary(full_model.flights.glm.BIC)

# Step 10 -----------------------------------------------------------------


# Step 11 ----------------------------------------------------------------


# Step 12 : ROC -----------------------------------------------------------


pred.prob<-predict(full_model.flights.glm.BIC,type="response")
pred_out<-ifelse(pred.prob<0.5,"no","yes")
flight_data_clean_prob<-data.frame(flight_data_clean,pred.prob,pred_out)

xtabs(~risky.landing+pred_out,flight_data_clean_prob)
thresh<-seq(0.01,0.5,0.01) 
sensitivity<-specificity<-rep( NA,length(thresh))

for(j in seq(along=thresh)){
pp<-ifelse(flight_data_clean_prob$pred.prob<thresh[j], "no","yes")
xx<-xtabs(~risky.landing+pp,flight_data_clean_prob)
specificity[j]<-xx[1,1]/(xx[1,1]+xx[1,2])
sensitivity[j]<-xx[2,2]/(xx[2,1]+xx[2,2])
}

par(mfrow =c(1,1))
matplot(thresh,cbind(sensitivity,specificity ),type="l",xlab ="Threshold", ylab ="Proportion", lty =1:2)
plot(1-specificity,sensitivity,type="l"); abline (0,1,lty=2)




pred.glm.ll<- predict(full_model.flights.glm.BIC, type="response")
library(ROCR)
pred.ll <- prediction(pred.glm.ll, flight_data_clean$long.landing)
#This function basically calculates many confusion matrices with different cut-off probability. Therefore, it requires two vectors as inputs - predicted probability and observed response (0/1). 

perf.ll <- performance(pred.ll, "tpr", "fpr")
#This line, performance() calculates TPR and FPR based all confusion matrices you get from previous step.

plot(perf.ll, colorize=TRUE)
# Get AUC value -----------------------------------------------------------
unlist(slot(performance(pred.ll, "auc"), "y.values"))


pred.glm.rl<- predict(full_model.flights.glm.rl.BIC, type="response")
pred.rl <- prediction(pred.glm.rl, flight_data_clean$risky.landing)
#This function basically calculates many confusion matrices with different cut-off probability. Therefore, it requires two vectors as inputs - predicted probability and observed response (0/1). 

perf.rl <- performance(pred.rl, "tpr", "fpr")
#This line, performance() calculates TPR and FPR based all confusion matrices you get from previous step.

plot(perf.rl, col="red", main="ROC")
plot(perf.ll, col="black",lty=2,add=TRUE)
legend(0.5,0.5,legend=c("Riskylanding", "Longlanding"),col=c("red", "black"), lty=1:2, cex=0.8)

# Get AUC value -----------------------------------------------------------
unlist(slot(performance(pred.rl, "auc"), "y.values"))



# Step 13 -----------------------------------------------------------------
new.var<-data.frame(aircraft="boeing",duration=200,no_pasg=80,speed_ground=115,speed_air=120,height=40,pitch=4)

new_pred.ll<-predict(full_model.flights.glm.BIC,newdata = new.var,type="response")
new_pred.ll_se<-predict(full_model.flights.glm.BIC,newdata = new.var,type="link",se=T)
new_pred.ll_se$fit
new_pred.ll_se$se.fit

library('faraway')
round(ilogit(c(17.89196-1.96*3.459814,17.89196+1.96*3.459814)),3)


new_pred.rl<-predict(full_model.flights.glm.rl.BIC,newdata = new.var,type="response")
new_pred.rl_se<-predict(full_model.flights.glm.rl.BIC,newdata = new.var,type="link",se=T)
new_pred.rl_se$fit
new_pred.rl_se$se.fit
round(ilogit(c(8.463332-1.96*2.089367,8.463332+1.96*2.089367)),3)


# Step 14 Other models for risky landing-----------------------------------------------------------------
flights.probit.model.rl<-glm(risky.landing~speed_ground+aircraft,family=binomial(link='probit'),data=flights_full_model0.rl)
summary(flights.probit.model.rl)
flights.cloglog.model.rl<-glm(risky.landing~speed_ground+aircraft,family=binomial(link='cloglog'),data=flights_full_model0.rl)
summary(flights.cloglog.model.rl)



# Step 15 -----------------------------------------------------------------
pred.glm.rl.probit<- predict(flights.probit.model.rl, type="response")
pred.rl.probit <- prediction(pred.glm.rl.probit, flight_data_clean$risky.landing)
perf.rl.probit <- performance(pred.rl.probit, "tpr", "fpr")

pred.glm.rl.cloglog<- predict(flights.cloglog.model.rl, type="response")
pred.rl.cloglog <- prediction(pred.glm.rl.cloglog, flight_data_clean$risky.landing)
perf.rl.cloglog <- performance(pred.rl.cloglog, "tpr", "fpr")


plot(perf.rl, col=1,lty=2, main="Y")
plot(perf.rl.probit, col=3,lty=3, add=T)
plot(perf.rl.cloglog, col=2,lty=4, add=T)
legend(0.5,0.5,legend=c("Logit", "Probit","cloglog"),col=c("red", "black","blue"),lty = 1:2,  cex=0.8)


# Step 16 -----------------------------------------------------------------
# Top 5 for probit --------------------------------------------------------
probit_final<-cbind(flight_data_clean,pred.glm.rl.probit)
head(probit_final)
Riskylandings.probit<- probit_final[order(-abs(probit_final$pred.glm.rl.probit)),]
head(Riskylandings.probit,5)



# Top 5 for logit ---------------------------------------------------------
logit_final<-cbind(flight_data_clean,pred.glm.rl)
head(logit_final)
Riskylandings.logit<- logit_final[order(-abs(logit_final$pred.glm.rl)),]
head(Riskylandings.logit,5)

# Top 5 for cloglog ---------------------------------------------------------
cloglog_final<-cbind(flight_data_clean,pred.glm.rl.cloglog)
head(cloglog_final)
Riskylandings.cloglog<- cloglog_final[order(-abs(cloglog_final$pred.glm.rl.cloglog)),]
head(Riskylandings.cloglog,5)


# Step 17 -----------------------------------------------------------------


new_pred.probit<-predict(flights.probit.model.rl,newdata = new.var,type="response")
new_pred.probit_se<-predict(flights.probit.model.rl,newdata = new.var,type="link",se=T)
new_pred.probit_se$fit
new_pred.probit_se$se.fit
round(ilogit(c(4.872041-1.96*1.127892,4.872041+1.96*1.127892)),3)


new_pred.cloglog<-predict(flights.cloglog.model.rl,newdata = new.var,type="response")
new_pred.cloglog_se<-predict(flights.cloglog.model.rl,newdata = new.var,type="link",se=T)
new_pred.cloglog_se$fit
new_pred.cloglog_se$se.fit
round(ilogit(c(5.16944-1.96*1.173423,5.16944+1.96*1.173423)),3)
