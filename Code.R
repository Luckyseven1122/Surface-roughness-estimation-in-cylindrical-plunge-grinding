install.packages("ISLR")
library(ISLR)
install.packages("car")
library(car)
install.packages("MASS")
library(MASS)
install.packages("openxlsx")
library(openxlsx)
install.packages("functional")
library(functional)

iData <- read.xlsx("C:/Users/Prachi/Downloads/Training set.xlsx" , sheet = "Training set", na.strings = "NA")
NonNumeric_Rows <- which(is.na(as.numeric(as.character(input[[1]]))))
FinaliData <- as.data.frame(data.matrix((iData[-c(NonNumeric_Rows),]), rownames.force = NA))
names(FinaliData) <- c("Ra","feed_rate","wheel_speed","work_speed","peak_power","mean_power","std_power","skewness_power","kurtosis_power","p2p_power","peak_mag_an","mean_an","skewness_an","kurtosis_an","total_energy_an","energy_band1_an","energy_band2_an","peak_mag_at","mean_at","skewness_at","kurtosis_at","total_energy_at","energy_band1_at","energy_band2_at")


install.packages("boot")
library(boot)

set.seed (17)
cv.error.10= rep (0 ,10)
glm.fit=glm(Ra~feed_rate + wheel_speed + work_speed + peak_power + mean_power + std_power + skewness_power + kurtosis_power + p2p_power + peak_mag_an + mean_an + skewness_an + kurtosis_an + total_energy_an + energy_band1_an + energy_band2_an + peak_mag_at + mean_at + skewness_at + kurtosis_at + total_energy_at + energy_band1_at + energy_band2_at,data = FinaliData)
cv.error.10=cv.glm(FinaliData ,glm.fit ,K=10)$delta[1]
cv.error.10
#for R2 value
#lm.fit=lm(Ra~feed_rate + wheel_speed + work_speed + peak_power + mean_power + std_power + skewness_power + kurtosis_power + p2p_power + peak_mag_an + mean_an + skewness_an + kurtosis_an + total_energy_an + energy_band1_an + energy_band2_an + peak_mag_at + mean_at + skewness_at + kurtosis_at + total_energy_at + energy_band1_at + energy_band2_at,data=FinaliData)
#summary(lm.fit)
summary(glm.fit)
plot(glm.fit)
crPlots(glm.fit)
plot(resid(glm.fit), type = 'l')
ssr<-mean((FinaliData$Ra - predict(glm.fit, FinaliData))[]^2)
tss<- mean((FinaliData$Ra - mean(FinaliData$Ra))[]^2)
R_squared<- 1-(ssr/tss)
R_squared


set.seed (17)
cv.error.10= rep (0 ,10)
glm.fit1=glm(Ra~ peak_power + mean_power + skewness_power + kurtosis_power + p2p_power + peak_mag_an + mean_an + total_energy_an +  mean_at +total_energy_at,data=FinaliData)
cv.error.10=cv.glm(FinaliData ,glm.fit1 ,K=10)$delta[1]
cv.error.10
#for R2 value
#lm.fit1=lm(Ra~ peak_power + mean_power + skewness_power + kurtosis_power + p2p_power + peak_mag_an + mean_an + total_energy_an +  mean_at +total_energy_at,data=FinaliData)
summary(glm.fit1)
plot(glm.fit1)
crPlots(glm.fit1)
plot(resid(glm.fit1), type = 'l')

set.seed (17)
cv.error.10= rep (0 ,10)
glm.fit2=glm(Ra~ peak_power + mean_power + p2p_power +  mean_at +total_energy_at,data=FinaliData)
cv.error.10=cv.glm(FinaliData ,glm.fit2 ,K=10)$delta[1]
cv.error.10
vif(glm.fit2)
#lm.fit2=lm(Ra~ peak_power + mean_power + p2p_power +  mean_at +total_energy_at,data=FinaliData)
summary(glm.fit2)
plot(glm.fit2)
crPlots(glm.fit2)
plot(resid(glm.fit2), type = 'l')

# no improvement in error
set.seed (17)
cv.error.10= rep (0 ,10)
glm.fit3=glm(Ra~ peak_power^2 + mean_power^2 + p2p_power^2 +  mean_at^2 +total_energy_at^2,data=FinaliData)
cv.error.10=cv.glm(FinaliData ,glm.fit3 ,K=10)$delta[1]
cv.error.10
#lm.fit3=lm(Ra~ peak_power^2 + mean_power^2 + p2p_power^2 +  mean_at^2 +total_energy_at^2,data=FinaliData)
summary(glm.fit3)
plot(glm.fit3)
crPlots(glm.fit3)
plot(resid(glm.fit3), type = 'l')

#increased the error
set.seed (17)
cv.error.10= rep (0 ,10)
glm.fit4=glm(Ra~ log(peak_power) + mean_power + p2p_power +  mean_at +total_energy_at,data=FinaliData)
cv.error.10=cv.glm(FinaliData ,glm.fit4 ,K=10)$delta[1]
cv.error.10

#check after removing collinear predictors and not a good fit as it increases error. 
set.seed (17)
cv.error.10= rep (0 ,10)
glm.fit5=glm(Ra~ mean_at +total_energy_at,data=FinaliData)
cv.error.10=cv.glm(FinaliData ,glm.fit5 ,K=10)$delta[1]
cv.error.10
vif(glm.fit5)
#lm.fit2=lm(Ra~ peak_power + mean_power + p2p_power +  mean_at +total_energy_at,data=FinaliData)
#summary(lm.fit2)
summary(glm.fit5)
plot(glm.fit5)
crPlots(glm.fit5)
plot(resid(glm.fit5), type = 'l')


set.seed (17)
cv.error.10= rep (0 ,10)
glm.fit6=glm(Ra~ sqrt(peak_power) + sqrt(mean_power) + sqrt(p2p_power) + mean_at +total_energy_at,data=FinaliData)
cv.error.10=cv.glm(FinaliData ,glm.fit6 ,K=10)$delta[1]
cv.error.10
vif(glm.fit6)
#lm.fit2=lm(Ra~ sqrt(peak_power) + sqrt(mean_power) + sqrt(p2p_power) +  sqrt(mean_at) +sqrt(total_energy_at),data=FinaliData)
summary(glm.fit6)
plot(glm.fit6)
crPlots(glm.fit6)
plot(resid(glm.fit6), type = 'l')

#----------------
# for K=5
set.seed (17)
cv.error.10= rep (0 ,10)
glm.fit7=glm(Ra~ peak_power + mean_power + p2p_power +  mean_at +total_energy_at,data=FinaliData)
cv.error.10=cv.glm(FinaliData ,glm.fit7 ,K=5)$delta[1]
cv.error.10
vif(glm.fit7)
#lm.fit2=lm(Ra~ peak_power + mean_power + p2p_power +  mean_at +total_energy_at,data=FinaliData)
summary(glm.fit7)
plot(glm.fit7)
crPlots(glm.fit7)
plot(resid(glm.fit7), type = 'l')