library("lattice")

# Reading the file
file <- read.csv(file.choose())

# Exploring the data file
summary(file)
attach(file)
dotplot(Delivery.Time,main="Dot Plot of Delivery Time")
dotplot(Sorting.Time,main="Dot Plot of Sorting Time")
range(Delivery.Time)
range(Sorting.Time)
boxplot(Delivery.Time,col="dodgerblue4")
boxplot(Sorting.Time,col="red", horizontal = T)
plot(Delivery.Time,Sorting.Time)
hist(Delivery.Time,probability = T)
hist(Sorting.Time,probability = T)
plot(Delivery.Time,Sorting.Time ,main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="green", xlab="Delivery Time", 
     ylab="Sorting Time", pch=20) 

# First Model
reg <- lm(Delivery.Time~Sorting.Time)
summary(reg)
confint(reg,level = 0.95)
pred <- as.data.frame(predict(reg,interval = "predict"))
pred <- pred$fit
cor(pred,Delivery.Time)

# Second Model
reg2 <- lm(Delivery.Time~Sorting.Time+I(Sorting.Time*Sorting.Time))
summary(reg2)
confint(reg,level=0.95)
pred2 <- as.data.frame(predict(reg2,interval = "predict"))
pred2 <- pred2$fit
cor(pred2,Delivery.Time)

# Third Model
reg3 <- lm(Delivery.Time~log(Sorting.Time))
summary(reg3)
confint(reg3,level=0.95)
pred3 <- as.data.frame(predict(reg3,interval = "predict"))
pred3 <- pred3$fit
cor(pred3,Delivery.Time)

# Fourth Model
reg4 <- lm(log(Delivery.Time)~Sorting.Time)
summary(reg4)
confint(reg4,level = 0.95)
pred4 <- as.data.frame(predict(reg4,interval = "predict"))
pred4 <- exp(pred4$fit)
cor(pred4,Delivery.Time)
