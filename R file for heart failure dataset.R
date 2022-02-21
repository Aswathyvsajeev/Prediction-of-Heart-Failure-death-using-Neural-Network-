# install the packages and import the libraries required 
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages('caret')
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(ggcorrplot)
library(caret)
# import the dataset
heart_failure <- read.csv('heart_failure.csv')
# Find the size of the dataset
dim(heart_failure)
# Number of rows in the dataset
nrow(heart_failure)
# Number of variables in the dataset
ncol(heart_failure)
# convert the categorical variables as factors 
str(heart_failure)
#checking for missing values in the dataset
table(is.na(heart_failure))
# Visualisation 
summary(heart_failure)
 
# Let's visualize the number of survived and dead patients 
heart_failure$DEATH_EVENT <- as.factor(heart_failure$DEATH_EVENT)
str(heart_failure)
barchart <- ggplot(heart_failure,aes(x = DEATH_EVENT)) + 
geom_bar(fill=c("dark green","dark red"))+
labs(y = "count") + theme_classic()+ggtitle("Survived Vs Dead")+
geom_text(stat='count', aes(label=..count..))
barchart

# Let's find the number of male and females in the dataset
heart_failure$sex <- as.factor(heart_failure$sex)
barchart1 <- ggplot(heart_failure,aes(x = sex)) + 
  geom_bar(fill=c("dark blue"))+
  labs(y = "count") + theme_classic()+ggtitle("Male Vs Female")+
  geom_text(stat='count', aes(label=..count..))
barchart1
 
# Distribution of age in the dataset
ggplot(heart_failure,aes(x = age)) + geom_histogram(binwidth = 5,fill = "dark red",color="black")+
  labs(y = "count",x = "Age") + theme_classic()+ggtitle("Distribution of Age")

# Distribution of serum_creatinine
ggplot(heart_failure,aes(x = serum_creatinine)) + geom_histogram(binwidth = 1,fill = "dark red",color="black")+
 
   labs(y = "count",x = "Serum_creatinine") + theme_classic()+ggtitle("Distribution of serum_creatinine")
# distribution of ejection fraction
ggplot(heart_failure,aes(x = ejection_fraction)) + geom_histogram(binwidth = 5,fill = "dark red",color="black")+
  labs(y = "count",x = "ejection_fraction") + theme_classic()+ggtitle("Distribution of ejection_fraction")

# Death Vs Age
ggplot(heart_failure,aes(x = DEATH_EVENT,y = age,fill=DEATH_EVENT))+
  geom_jitter()+theme(legend.text = element_text(size=6,face="bold"))+
  geom_boxplot(size=1.2,alpha=0.3)+
  theme_classic()+
  theme(axis.text.x = element_text(size =6,face="bold"))+
  labs(x="DEATH_EVENT",y="Age")+ggtitle("Age Vs Death_event")+
  facet_wrap(~sex)

# Death Vs Sex 
barchart3 <- ggplot(heart_failure,aes(x = sex,fill = DEATH_EVENT)) + geom_bar()+
  labs(y = "count") + theme_classic()+ggtitle("Death_event by sex")
 
barchart3
#let's create correlation matrix for the dataset
heart_failure$DEATH_EVENT <- as.numeric(heart_failure$DEATH_EVENT)
heart_failure$sex <- as.numeric(heart_failure$sex )
heart_failure_corr <- cor(heart_failure)
ggcorrplot(heart_failure_corr,lab = TRUE,lab_size = 2.5)

#Factor analysis to reduce the features in the dataset
fact <- factanal(heart_failure[,-13],factor=3)
print(fact)
apply(fact$loadings^2,1,sum)


heart_failure$age <- (heart_failure$age-min(heart_failure$age))/(max(heart_failure$age)-min(heart_failure$age))
heart_failure$creatinine_phosphokinase <- (heart_failure$creatinine_phosphokinase-min(heart_failure$creatinine_phosphokinase))/(max(heart_failure$creatinine_phosphokinase)-min(heart_failure$creatinine_phosphokinase))
heart_failure$ejection_fraction <- (heart_failure$ejection_fraction-min(heart_failure$ejection_fraction))/(max(heart_failure$ejection_fraction)-min(heart_failure$ejection_fraction))
heart_failure$platelets <- (heart_failure$platelets-min(heart_failure$platelets))/(max(heart_failure$platelets)-min(heart_failure$platelets))
heart_failure$serum_creatinine <- (heart_failure$serum_creatinine-min(heart_failure$serum_creatinine))/(max(heart_failure$serum_creatinine)-min(heart_failure$serum_creatinine))
heart_failure$serum_sodium <- (heart_failure$serum_sodium-min(heart_failure$serum_sodium))/(max(heart_failure$serum_sodium)-min(heart_failure$serum_sodium))
heart_failure$time <- (heart_failure$time-min(heart_failure$time))/(max(heart_failure$time)-min(heart_failure$time))
# Let's convert the dependent or output variable as factor since it is a classification problem
#let's split the dataset before building the model.We are using seed function to get the same result everytime we run the model
#The sample is divided into 70% for training and 30% for testing

set.seed(42)
ind <- sample(2,nrow(heart_failure),replace= TRUE,prob=c(0.7,0.3))
train <- heart_failure[ind==1,]
test <- heart_failure[ind==2,]
dim(train)
dim(test)

install.packages('neuralnet')
library(neuralnet)
# let's build neural network model .There are 2 hidden layers ,first layer with 5 neurons and second one with 3 neurons
train$DEATH_EVENT<-as.factor(train$DEATH_EVENT)

n <- neuralnet(DEATH_EVENT~age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+
                 high_blood_pressure+platelets+serum_creatinine+
                 serum_sodium+sex+smoking+time,data = train,threshold=0.01,hidden = c(5,3),
               linear.output = FALSE)
plot(n)
print(n)

#Let's do the prediction on test dataset.The 13 attribute is output so it is not considered

pred <- predict(n,test[,-13])
pred<- ifelse(pred>0.5,1,0)
pred
idx <- apply(pred, 1, which.max)
#confusionMatrix
test$DEATH_EVENT <- as.factor(test$DEATH_EVENT)
predicted <- c(0,1)[idx]
predicted <- as.factor(predicted)
predicted
confusionMatrix(predicted,test$DEATH_EVENT)
# Try different combination of hidden layers to check if able to achieve more accuracy
n1 <- neuralnet(DEATH_EVENT~age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+
                 serum_sodium+sex+smoking+time,data = train,threshold=0.01,hidden = c(8,5),
               linear.output = FALSE)
plot(n1)

pred1 <- predict(n1,test[,-13])
pred1<- ifelse(pred1>0.5,1,0)

i1 <- apply(pred1,1, which.max)

predicted1 <- c(0,1)[i1]
predicted1 <- as.factor(predicted1)
confusionMatrix(predicted1,test$DEATH_EVENT)
#softplus <- function(x) log(1 + exp(x))
n2 <- neuralnet(DEATH_EVENT~age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+
                  serum_sodium+sex+smoking+time,data = train,threshold=0.01,hidden = c(12,8), act.fct = tanh,
                linear.output = FALSE)
pred2 <- predict(n2,test[,-13])
pred2<- ifelse(pred2>0.5,1,0)
#pred
i2 <- apply(pred2,1, which.max)
#idx
predicted2 <- c(0,1)[i2]
#predicted
predicted2 <- as.factor(predicted2)
confusionMatrix(predicted2,test$DEATH_EVENT)
