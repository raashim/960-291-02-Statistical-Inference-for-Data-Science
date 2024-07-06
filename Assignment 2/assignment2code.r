Final Code
HittersAE<-Hitters%>%subset(League=="A")%>%subset(Division=="E")
HittersAW<-Hitters%>%subset(League=="A")%>%subset(Division=="W")
HittersNE<-Hitters%>%subset(League=="N")%>%subset(Division=="E")
HittersNW<-Hitters%>%subset(League=="N")%>%subset(Division=="W")

#cleaned dataframes 
HittersAEclean<-HittersAE%>% subset(!is.na(Salary))
HittersAWclean<-HittersAW%>% subset(!is.na(Salary))
HittersNEclean<-HittersNE%>% subset(!is.na(Salary))
HittersNWclean<-HittersNW%>% subset(!is.na(Salary))

dim(HittersAE)
view(HittersAEclean)
dim(HittersAEclean)

# AE X and Y values
XAE<-HittersAEclean[,-c(20,19,14,15)]
view(XAE)
# the Y value is for Salary in AE
YAE<-HittersAEclean$Salary

# creating a matrix to compare X against Y for AE
lars(as.matrix(XAE),YAE)
HittersAEclean.lars <-lars(as.matrix(XAE),YAE)
attributes(HittersAEclean.lars)
HittersAEclean.lars$mu
mean(YAE)

# AW X and Y values
XAW<-HittersAWclean[,-c(20,19,14,15)]
YAW<-HittersAWclean$Salary

#creating a matrix to compare X against Y for AW
lars(as.matrix(XAW),YAW)
HittersAWclean.lars <- lars(as.matrix(XAW),YAW)
attributes(HittersAWclean.lars)
HittersAWclean.lars$mu

# NE X and Y values
XNE<-HittersNEclean[,-c(20,19,14,15)]
YNE<-HittersNEclean$Salary

#creating a matrix to compare X against Y for NE
lars(as.matrix(XNE),YNE)
HittersNEclean.lars <- lars(as.matrix(XNE),YNE)
attributes(HittersNEclean.lars)
HittersNEclean.lars$mu

# NW X and Y values
XNW<-HittersNWclean[,-c(20,19,14,15)]
YNW<-HittersNWclean$Salary

#creating a matrix to compare X against Y for NE
lars(as.matrix(XNW),YNW)
HittersNWclean.lars <- lars(as.matrix(XNW),YNW)
attributes(HittersNWclean.lars)
HittersNWclean.lars$mu

# minimum of the Cp model for AE
min(HittersAEclean.lars$Cp)
HittersAEclean.lars$Cp==min(HittersAEclean.lars$Cp)
HittersAEclean.lars$Cp[19]
par(mfrow=c(1,1))
plot(as.matrix(XAE)%*%HittersAEclean.lars$beta[19,]+HittersAEclean.lars$mu,YAE)

# minimum of the Cp model for AW
min(HittersAWclean.lars$mu)
HittersAWclean.lars$Cp==min(HittersAWclean.lars$Cp)
HittersAWclean.lars$Cp[19]
par(mfrow=c(1,1))
plot(as.matrix(XAW)%*%HittersAWclean.lars$beta[19,]+HittersAWclean.lars$mu,YAW)

# minimum of the Cp model for NE
min(HittersNEclean.lars$mu)
HittersNEclean.lars$Cp==min(HittersNEclean.lars$Cp)
HittersNEclean.lars$Cp[19]
par(mfrow=c(1,1))
plot(as.matrix(XNE)%*%HittersNEclean.lars$beta[19,]+HittersNEclean.lars$mu,YNE)

# minimum of the Cp model for NE
min(HittersNWclean.lars$mu)
HittersNWclean.lars$Cp==min(HittersNWclean.lars$Cp)
HittersNWclean.lars$Cp[19]
par(mfrow=c(1,1))
plot(as.matrix(XNW)%*%HittersNWclean.lars$beta[19,]+HittersNWclean.lars$mu,YNW)

# correlation coefficients (R)
cor(as.matrix(XAE)%*%HittersAEclean.lars$beta[19,]+HittersAEclean.lars$mu,YAE)
cor(as.matrix(XAW)%*%HittersAWclean.lars$beta[19,]+HittersAWclean.lars$mu,YAW)
cor(as.matrix(XNE)%*%HittersNEclean.lars$beta[19,]+HittersNEclean.lars$mu,YNE)
cor(as.matrix(XNW)%*%HittersNWclean.lars$beta[19,]+HittersNWclean.lars$mu,YNW)
# R^2
cor(as.matrix(XAE)%*%HittersAEclean.lars$beta[19,]+HittersAEclean.lars$mu,YAE)^2
cor(as.matrix(XAW)%*%HittersAEclean.lars$beta[19,]+HittersAEclean.lars$mu,YAW)^2
cor(as.matrix(XNE)%*%HittersNEclean.lars$beta[19,]+HittersNEclean.lars$mu,YNE)^2
cor(as.matrix(XNW)%*%HittersNWclean.lars$beta[19,]+HittersNWclean.lars$mu,YNW)^2
view(XAE)
view(XAW)
view(XNE)
view(XNW)

#Z for AE
ZAE<-cbind(YAE,XAE)
view(ZAE)
summary(lm(YAE~.,data=ZAE))
HittersAE.lm<-(lm(YAE~.,data=ZAE))
predict(HittersAE.lm,XAE)
plot(predict(HittersAE.lm,XAE),YAE)
HittersAW2 <-Hitters%>%subset(League=="A")

#Z for AW
ZAW<-cbind(YAW,XAW)
view(ZAW)
summary(lm(YAW~.,data=ZAW))
HittersAW.lm<-(lm(YAW~.,data=ZAW))
predict(HittersAW.lm,XAW)
plot(predict(HittersAW.lm,XAW),YAW)

#Z for NE
ZNE<-cbind(YNE,XNE)
view(ZNE)
summary(lm(YNE~.,data=ZNE))
HittersNE.lm<-(lm(YNE~.,data=ZNE))
predict(HittersNE.lm,XNE)
plot(predict(HittersNE.lm,XNE),YNE)

#Z for NE
ZNW<-cbind(YNW,XNW)
view(ZNW)
summary(lm(YNW~.,data=ZNW))
HittersNW.lm<-(lm(YNW~.,data=ZNW))
predict(HittersNW.lm,XNW)
plot(predict(HittersNW.lm,XNW),YNW)

# made 4 of everything above this

#HittersA<-Hitters%>%subset(League=="A")

#HittersAW<-HittersA%>%subset(Division=="W")

#HittersAWclean<-HittersAW%>%subset(!is.na(Salary))

#XW<-HittersAWclean[,-c(20,19,14,15)]

plot(predict(HittersAE.lm,XAW),HittersAWclean$Salary)
boxplot(predict(HittersAE.lm,XAW))
boxplot(predict(HittersAE.lm,XAW),predict(HittersAE.lm,XAE))

I1<-HittersAEclean.lars$Cp==min(HittersAEclean.lars$Cp)

I1

I2<-HittersAWclean.lars$Cp==min(HittersAWclean.lars$Cp)

I2

I3<-HittersNWclean.lars$Cp==min(HittersNWclean.lars$Cp)

I3

I4<-HittersNEclean.lars$Cp==min(HittersNEclean.lars$Cp)

I4

betalarsAE<-HittersAEclean.lars$beta[I1,]
betalarsAE

betalarsAW<-HittersAWclean.lars$beta[I2,]
betalarsAW

betalarsNE<-HittersNWclean.lars$beta[I3,]
betalarsNE

betalarsNW<-HittersNEclean.lars$beta[I4,]
betalarsNW

objects(pattern="clean")
HittersN<-Hitters%>%subset(League=="N")
HittersNW<-Hitters%>%subset(Division=="W")
HittersNWclean<-HittersNW%>%subset(!is.na(Salary))

# prediction models
HittersNWclean[,-c(10,19,14,15)]
#AE
predAENW<-as.matrix(HittersNWclean[,-c(20,19,14,15)])%*%betalarsAE
plot(predAENW,HittersNWclean$Salary)

predAEAE<-as.matrix(HittersAEclean[,-c(20,19,14,15)])%*%betalarsAE
plot(predAEAE, HittersAEclean$Salary)

predAEAW<-as.matrix(HittersAWclean[,-c(20,19,14,15)])%*%betalarsAE
plot(predAEAW, HittersAWclean$Salary)

predAENE<-as.matrix(HittersNEclean[,-c(20,19,14,15)])%*%betalarsAE
plot(predAENE, HittersNEclean$Salary)

#AW
predAWNW<-as.matrix(HittersNWclean[,-c(20,19,14,15)])%*%betalarsAW
plot(predAWNW,HittersNWclean$Salary)

predAWAE<-as.matrix(HittersAEclean[,-c(20,19,14,15)])%*%betalarsAW
plot(predAWAE, HittersAEclean$Salary)

predAWAW<-as.matrix(HittersAWclean[,-c(20,19,14,15)])%*%betalarsAW
plot(predAWAW, HittersAWclean$Salary)

predAWNE<-as.matrix(HittersNEclean[,-c(20,19,14,15)])%*%betalarsAW
plot(predAWNE, HittersNEclean$Salary)


#NW
predNWNW<-as.matrix(HittersNWclean[,-c(20,19,14,15)])%*%betalarsNW
plot(predNWNW,HittersNWclean$Salary)

predAENW<-as.matrix(HittersAEclean[,-c(20,19,14,15)])%*%betalarsNW
plot(predAENW, HittersAEclean$Salary)

predAWNW<-as.matrix(HittersAWclean[,-c(20,19,14,15)])%*%betalarsNW
plot(predAWNW, HittersAWclean$Salary)

predNENW<-as.matrix(HittersNEclean[,-c(20,19,14,15)])%*%betalarsNW
plot(predNENW, HittersNEclean$Salary)

#NE
predNENE<-as.matrix(HittersNEclean[,-c(20,19,14,15)])%*%betalarsNE
plot(predNENE, HittersNEclean$Salary)

predAENE<-as.matrix(HittersAEclean[,-c(20,19,14,15)])%*%betalarsNE
plot(predAENE, HittersAEclean$Salary)

predAWNE<-as.matrix(HittersAWclean[,-c(20,19,14,15)])%*%betalarsNE
plot(predAWNE, HittersAWclean$Salary)

predNWNE<-as.matrix(HittersNWclean[,-c(20,19,14,15)])%*%betalarsNE
plot(predNWNE, HittersNWclean$Salary)

boxplot(c(predAEAE),HittersAEclean$Salary, c(predAEAW), HittersAWclean$Salary)
attributes(HittersAEclean.lars)
#predAEAE<-as.matrix(HittersAEclean[,-c(20,19,14,15)])%*%betalarsAE
mean(predAEAE)
HittersAEclean.lars$mu
predAEAE<-predAEAE-mean(predAEAE)+HittersAEclean.lars$mu
boxplot(c(predAEAE),HittersAEclean$Salary,c(predAEAW), HittersAWclean$Salary)
plot(predAENW, HittersNWclean$Salary)
boxplot(c(predAEAE),HittersAEclean$Salary, c(predAEAW), HittersAWclean$Salary)
betalarsAE
betalarsAE!=0
IAE<-betalarsAE!=0
HittersAEcleanforlm<-HittersAEclean[,-c(20,19,14,15)][,IAE]
HittersAEcleanforlm[1:5,]
HittersAEcleanforlm<-cbind.data.frame(HittersAEcleanforlm, HittersAEclean$Salary)
HittersAEcleanforlm[1:5,]
dimnames(HittersAEcleanforlm)
dimnames(HittersAEcleanforlm)[[2]][13]<-"Salary"
colnames(HittersAEcleanforlm)
HittersAElm<-lm(Salary~.,data=HittersAEcleanforlm) 
# retrieving the coefficients for the AElm plot 
summary(HittersAElm)
predlmAE<-predict(HittersAElm)
boxplot(c(predAEAE),HittersAEclean$Salary,predlmAE)
plot(c(predAEAE), predlmAE)
Default
view(Default)

# Build LM model 
lm_predictors <- HittersAEclean[, -c(20,19,14,15)]
HittersAE_lm <- lm(YAE ~ ., data = lm_predictors)

# Generate LM predictions
pred_lm <- predict(HittersAE_lm, lm_predictors)

# Scatterplot - LARS vs LM
plot(predAEAE, YAE, col="red", main="AE LARS vs LM")
points(pred_lm, YAE, col="blue")
legend("topleft", c("LARS", "LM"), col=c("red", "blue"), pch=1) 

s1choice<-(c(1:length(I1))[I1])[1]
larscoef<-predict(HittersNEclean.lars,,s1choice,type="coefficient")

# Compare coefficients
print(larscoef)
print(coef(HittersAE_lm))

rf_model <- randomForest(XAE, YAE, ntree = 100)
predrfAE <- predict(rf_model, XAE)
plot(predrfAE, YAE, col="green", main="Random Forest Model for AE Salary")
cor(predrfAE,HittersAEclean$Salary)

Output as of now:













# Extra credit: Boxplots with relevant discussion

# these are the overall min & max salary values
min_salary <- min(HittersAEclean$Salary, HittersAWclean$Salary, HittersNEclean$Salary, HittersNWclean$Salary)
max_salary <- max(HittersAEclean$Salary, HittersAWclean$Salary, HittersNEclean$Salary, HittersNWclean$Salary)

# this sets a common y-axis limit for boxplots
ylim <- c(min_salary, max_salary)

par(mfrow = c(2, 2))
boxplot(HittersAEclean$Salary, main = "AE Division Salary", ylab = "Salary ($)", ylim = ylim)
boxplot(HittersAWclean$Salary, main = "AW Division Salary", ylab = "Salary ($)", ylim = ylim)
boxplot(HittersNEclean$Salary, main = "NE Division Salary", ylab = "Salary ($)", ylim = ylim)
boxplot(HittersNWclean$Salary, main = "NW Division Salary", ylab = "Salary ($)", ylim = ylim)
par(mfrow = c(1, 1))

The Relevant Discussion: 

The American East (AE) Division having high outlier salaries compared to the other divisions could be attributed to the fact that it contains several historically successful and popular baseball teams like the Boston Red Sox, New York Yankees, and Toronto Blue Jays. These big market teams likely have higher revenues that allow them to spend more on top player salaries, driving up the outlier salaries and average salary for the whole AE Division. The American West (AW) Division having the lowest high outlier salaries of all divisions makes sense given it does not contain the same level of historical successful and popular teams. The division lacks the big market teams that would drive up top salaries, so its maximum salaries are lower.

The popularity and success of teams like the Red Sox, Yankees, and Blue Jays allows them to command higher TV ratings, ticket sales, merchandise sales, and more. This higher revenue likely enables them to spend more on their players and their salaries. It makes sense that the AE Division would then have higher outlier salaries and a higher average salary compared to other divisions without those high revenue teams. The lack of historically dominant and popular teams in the AW Division leads to lower revenues and an inability to spend at the same level on top talent. This results in lower maximum salaries and a lower average salary for the division compared to the AE Division which has those big market teams. The economic success and popularity of teams like the Red Sox, Yankees and Blue Jays is a key driver of the higher outlier and average salaries in the AE Division compared to less prominent divisions like the AW. The lack of similar high-revenue teams in divisions brings down their salary metrics.













# Load data
HittersAE <- Hitters %>% subset(League=="A") %>% subset(Division=="E")
HittersAW <- Hitters %>% subset(League=="A") %>% subset(Division=="W")  
HittersNE <- Hitters %>% subset(League=="N") %>% subset(Division=="E")
HittersNW <- Hitters %>% subset(League=="N") %>% subset(Division=="W")

# Remove missing salaries 
HittersAEclean <- HittersAE %>% subset(!is.na(Salary))
HittersAWclean <- HittersAW %>% subset(!is.na(Salary))
HittersNEclean <- HittersNE %>% subset(!is.na(Salary))  
HittersNWclean <- HittersNW %>% subset(!is.na(Salary))

# Predictors
X <- HittersAEclean[, -c(20,19,14,15)]
XW <- HittersAWclean[, -c(20,19,14,15)] 
XNE <- HittersNEclean[, -c(20,19,14,15)]
XNW <- HittersNWclean[, -c(20,19,14,15)]

# Response
Y <- HittersAEclean$Salary
YW <- HittersAWclean$Salary
YNE <- HittersNEclean$Salary
YNW <- HittersNWclean$Salary

# Build LARS models
HittersAE_lars <- lars(X, Y)  
HittersAW_lars <- lars(XW, YW)
HittersNE_lars <- lars(XNE, YNE)
HittersNW_lars <- lars(XNW, YNW)

# AE LARS model predictions
predAEAE <- predict(HittersAE_lars, X)
predAEAW <- predict(HittersAE_lars, XW) 
predAENE <- predict(HittersAE_lars, XNE)
predAENW <- predict(HittersAE_lars, XNW)

# Scatterplots
plot(predAEAE, Y, main="AE Predictions on AE")
plot(predAEAW, YW, main="AE Predictions on AW")
plot(predAENE, YNE, main="AE Predictions on NE") 
plot(predAENW, YNW, main="AE Predictions on NW")

# Repeat for other divisions 

# AW 
predAWAE <- predict(HittersAW_lars, X)
predAWAW <- predict(HittersAW_lars, XW) 
predAWNE <- predict(HittersAW_lars, XNE)
predAWNW <- predict(HittersAW_lars, XNW)

plot(predAWAE, Y, main="AW Predictions on AE")
plot(predAWAW, YW, main="AW Predictions on AW")
plot(predAWNE, YNE, main="AW Predictions on NE")
plot(predAWNW, YNW, main="AW Predictions on NW")

# NE
predNEAE <- predict(HittersNE_lars, X)  
predNEAW <- predict(HittersNE_lars, XW)
predNEAW <- predict(HittersNE_lars, XW)
predNENW <- predict(HittersNE_lars, XNW)

plot(predNEAE, Y, main="NE Predictions on AE")
plot(predNEAW, YW, main="NE Predictions on AW") 
plot(predNEAW, YW, main="NE Predictions on AW")
plot(predNENW, YNW, main="NE Predictions on NW")

# NW
predNWAE <- predict(HittersNW_lars, X)
predNWAW <- predict(HittersNW_lars, XW)
predNWJNE <- predict(HittersNW_lars, XNE)
predNWJNW <- predict(HittersNW_lars, XNW)

plot(predNWAE, Y, main="NW Predictions on AE")
plot(predNWAW, YW, main="NW Predictions on AW")
plot(predNWJNE, YNE, main="NW Predictions on NE")
plot(predNWJNW, YNW, main="NW Predictions on NW")

# Boxplots
boxplot(predAEAE, Y)
boxplot(predAEAW, YW)
...

# Other boxplots




AE Coefficient against all the Divisions (4 plots) 

AE Coefficient against all the Divisions (4 plots) 




# Build LM model 
lm_predictors <- HittersAEclean[, -c(20,19,14,15)]
HittersAE_lm <- lm(Y ~ ., data = lm_predictors)

# Generate LM predictions
pred_lm <- predict(HittersAE_lm, lm_predictors)

# Scatterplot - LARS vs LM
plot(predAEAE, Y, col="red", main="AE LARS vs LM")
points(pred_lm, Y, col="blue")
legend("topleft", c("LARS", "LM"), col=c("red", "blue"), pch=1) 

hittersAE_lars <- lars(XAE, YAE)
predAEAE <- predict(hittersAE_lars, XAE)

# Compare coefficients
print(HittersAE_lars$beta) 
print(coef(HittersAE_lm))

