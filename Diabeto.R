dia <- read.csv("diabetes.csv")
str(dia)


dia$Outcome <- as.factor(dia$Outcome)

library(ggplot2)

unique(dia$Glucose)
library(ggplot2)
ggplot(dia[1:768,], aes(x = Glucose, fill = Outcome)) +
  geom_bar() +
  ggtitle("Glucose") +
  xlab("Glucose") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#5 values only use mean

unique(dia$BloodPressure)

ggplot(dia[1:768,], aes(x = BloodPressure, fill = Outcome)) +
  geom_bar() +
  ggtitle("BloodP") +
  xlab("BloodP") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#Use mean

unique(dia$SkinThickness)
ggplot(dia[1:768,], aes(x = SkinThickness, fill = Outcome)) +
  geom_bar() +
  ggtitle("SkinThickness") +
  xlab("SkinThickness") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#DROP SKIN THICKNESS FOR NOW


unique(dia$Insulin)
ggplot(dia[1:768,], aes(x = Insulin, fill = Outcome)) +
  geom_bar() +
  ggtitle("Insulin") +
  xlab("Insulin") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#DROP INSULIN FOR SURE

unique(dia$BMI)
ggplot(dia[1:768,], aes(x = BMI, fill = Outcome)) +
  geom_bar() +
  ggtitle("BMI") +
  xlab("BMI") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#Use mean by removing outliers

ggplot(dia[1:768,], aes(x = Glucose,y= BloodPressure)) +
  geom_bar(stat = "identity") +
  ggtitle("GlucosexBloodpressure") +
  xlab("Glucose") +
  ylab("BloodPressure") +
  labs(fill = "Outcome")

ggplot(dia[1:768,], aes(x = SkinThickness,y= BMI)) +
  geom_bar(stat = "identity") +
  ggtitle("SkinxBMI") +
  xlab("Skin") +
  ylab("BMI") +
  labs(fill = "Outcome")

ggplot(dia[1:768,], aes(x = Age,y= BloodPressure)) +
  geom_col() +
  ggtitle("AgexBP") +
  xlab("Age") +
  ylab("BP") +
  labs(fill = "Outcome")

ggplot(dia[1:768,], aes(x = Age, fill = Outcome)) +
  geom_bar() +
  ggtitle("AgexBP") +
  xlab("Age") +
  ylab("BP") +
  labs(fill = "Outcome")

plot(x = dia$Insulin,y = dia$Outcome,
     xlab = "Insulin",
     ylab = "Outcome",
     main = "BP vs DPF"
)
#No trend with any of the features

plot(x = dia$SkinThickness,y = dia$BMI,
     xlab = "skin",
     ylab = "BMI",
     main = "Skin vs BMI"
)
#linear regression can be used









#Replacing Glucose with mean

dia$Glucose[dia$Glucose==0] <- NA
dia_new <- dia[, -5]


hist(dia$Glucose,main="Glucose",xlim = c(0,200), ylim = c(0,50),breaks=100)
dia_new$Glucose[is.na(dia_new$Glucose)] <- ceiling(mean(na.omit(dia$Glucose)))

ggplot(dia_new[1:768,], aes(x = Glucose, fill = Outcome)) +
  geom_bar() +
  ggtitle("Glucose") +
  xlab("Glucose") +
  ylab("Total Count") +
  labs(fill = "Outcome")

str(dia_new)

#Replacing BMI with mean(Without outliers) 

dia_new$BMI[dia_new$BMI==0.0] <- NA
unique(dia_new$BMI)


hist(dia$BMI,main="BMI",xlim = c(10,100), ylim = c(0,45),breaks=100)
dia_new$BMI[is.na(dia_new$BMI)] <- ceiling(mean(na.omit(dia$BMI[dia$BMI<50])))

summary(dia_new)

library(lattice)
splom(~dia_new,groups=NULL,data=dia_new,axis.line.tck= 0, axis.text.alpha = 0)

str(dia_new)
diab <- dia_new

diab$Pregnancies<- as.numeric(diab$Pregnancies)
diab$Glucose<- as.numeric(diab$Glucose)
diab$BloodPressure<- as.numeric(diab$BloodPressure)
diab$SkinThickness<- as.numeric(diab$SkinThickness)
diab$BMI<- as.numeric(diab$BMI)
diab$DiabetesPedigreeFunction<- as.numeric(diab$DiabetesPedigreeFunction)
diab$Age<- as.numeric(diab$Age)
diab$Outcome<- as.numeric(diab$Outcome)

summary(diab)


library(corrplot)

cr <- cor(cbind(diab, NA))


corrplot(cr,type = "lower")
corrplot(cr, method = "number")


library(car)
model <- lm(SkinThickness~.,data = dia_new)
vif(model)

dia_new$SkinThickness[dia_new$SkinThickness==0] <- NA


summary(dia_new)
new_d <- dia_new[is.na(dia_new$SkinThickness),]
summary(new_d)

write.csv(new_d,file = "testy.csv",row.names = F)

new_d1 <- dia_new[!is.na(dia_new$SkinThickness),]
write.csv(new_d1,file = "testy1.csv",row.names = F)

model <- lm(SkinThickness~ BMI, data = new_d1)

summary(model)

predic<- predict(model, new_d1)
predic

lines(new_d1$SkinThickness ,col = "green")
lines(predic,col = "blue")

predic1<- predict(model, new_d)
predic1
length(predic1)


Skin <- 1:227


dat2 <- data.frame(Skin,predic1)



new_d$SkinThickness <- ifelse(is.na(new_d$SkinThickness), dat2$predic1, new_d$SkinThickness)


final_d <- rbind(new_d,new_d1)


plot(x = final_d$SkinThickness,y = final_d$BMI,
     xlab = "skin",
     ylab = "BMI",
     main = "Skin vs BMI"
)

#For Bloodpressure
summary(final_d)
splom(~final_d,groups=NULL,data=final_d,axis.line.tck= 0, axis.text.alpha = 0)

model1 <- lm(BloodPressure~.,data = final_d)
vif(model1)



final_d$BloodPressure[final_d$BloodPressure==0] <- NA
final_d$BloodPressure[is.na(final_d$BloodPressure)] <- ceiling(mean(na.omit(final_d$BloodPressure)))








#EXPLORATORY ANALYSIS


library(ggplot2)

ggplot(final_d[1:768,], aes(x = Pregnancies, fill = Outcome)) +
  geom_bar() +
  ggtitle("Preg") +
  xlab("Preg") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#Good Dependency


ggplot(final_d[1:768,], aes(x = Glucose, fill = Outcome)) +
  geom_bar() +
  ggtitle("Glucose") +
  xlab("Glucose") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#Very good Dependency


ggplot(final_d[1:768,], aes(x = BloodPressure, fill = Outcome)) +
  geom_bar() +
  ggtitle("BloodP") +
  xlab("BloodP") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#OK dependency


ggplot(final_d[1:768,], aes(x = SkinThickness ,fill = Outcome)) +
  geom_bar() +
  ggtitle("DPF") +
  xlab("DPF") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#Not so good Dependency


ggplot(final_d[1:768,], aes(x = BMI, fill = Outcome)) +
  geom_bar() +
  ggtitle("BMI") +
  xlab("BMI") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#Good Dependency


ggplot(final_d[1:768,], aes(x = DiabetesPedigreeFunction ,fill = Outcome)) +
  geom_bar() +
  ggtitle("DPF") +
  xlab("DPF") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#OK Dependency

ggplot(final_d[1:768,], aes(x = Age ,fill = Outcome)) +
  geom_bar() +
  ggtitle("Age") +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Outcome")
#Really good Depenency



#LOGISTIC REGRESSION

lmodel <- glm(Outcome~.,final_d,family = "binomial")
summary(lmodel)

#REALLY GOOD
lmodel <- glm(Outcome~.-Pregnancies,final_d,family = "binomial")
summary(lmodel)

lmodel <- glm(Outcome~.-Age,final_d,family = "binomial")
summary(lmodel)
#We Keep AGE


lmodel <- glm(Outcome~.-BloodPressure,final_d,family = "binomial")
summary(lmodel)
# keep

lmodel <- glm(Outcome~.-SkinThickness,final_d,family = "binomial")
summary(lmodel)
#Dont Consider

#keep preg
#Keep Glucose
#Keep BP
#Remove SkinThickness
#Keep BMI
#Keep BPF
#Keep Age maybe


trainR <- final_d


#Split Data
library(caTools)
split <- sample.split(trainR, SplitRatio = 0.9)

trainingF <- subset(trainR, split == "TRUE")
testingF <- subset(trainR, split == "FALSE")

summary(trainingF)
summary(testingF)


#trainingF <- trainingF[, -4]
#Ttest <- trainingF[,-3]


str(trainingF)
modelF <- glm(Outcome~.-Pregnancies-DiabetesPedigreeFunction-SkinThickness,trainingF,family = "binomial")

res <- predict(modelF,testingF,type="response")
res

table(Actualvalue = testingF$Outcome, Predictedvalue = res>0.52)

#res <- predict(modelF,trainingF,type="response")

modelF1 <- glm(Outcome~.,trainingF,family = "binomial")

res <- predict(model,testingF,type="response")
res

table(Actualvalue = testingF$Outcome, Predictedvalue = res>0.45)