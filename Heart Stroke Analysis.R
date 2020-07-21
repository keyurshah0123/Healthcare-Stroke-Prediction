##################################ALY6020 CAPSTONE PROJECT GROUP 6 (HEALTHCARE STROKE DATASET)####################

####Loading library files###
library(tidyverse)
library(caret)
library(glmnet)

############Loading Healthcare Dataset#################

healthcare<- read.csv("C:/Users/shahk/Downloads/healthcare-dataset-stroke-data/healthcare.csv",header = TRUE)

###############Exploring the dataset##########

View(healthcare)
str(healthcare)
summary(healthcare)

#############Converting to factor and character###############

healthcare$hypertension<-as.factor(healthcare$hypertension)
healthcare$heart_disease<-as.factor(healthcare$heart_disease)

healthcare$gender<-as.character(healthcare$gender)
healthcare$work_type<-as.character(healthcare$work_type)
healthcare$stroke<-as.factor(healthcare$stroke)
healthcare$smoking_status<-as.character(healthcare$smoking_status)

str(healthcare)

###############Checking missing value#########
Missing_Data_Smoking = round(100*sum(healthcare$smoking_status =="")/nrow(healthcare),2)
Missing_Data_Smoking

Missing_Data_BMI = round(100*sum(is.na(healthcare$bmi))/nrow(healthcare),2)
Missing_Data_BMI

###################REPLACING BMI VALUE WITH MEAN AS IT HAS LESS NULL VALUES#####

#Taking mean of BMI

mean_bmi = mean(healthcare$bmi,na.rm = T)
mean_bmi

library(caret)

#REPLACING BY MEAN VALUE in BMI

healthcare=healthcare %>% 
  mutate(bmi= ifelse(is.na(bmi),mean_bmi,bmi))
View(healthcare$bmi)

#####Replacing null values of Smoking status

healthcare$smoking_status<-ifelse((healthcare$smoking_status==""),"never smoked",healthcare$smoking_status)
view(healthcare$smoking_status)

####Checking again the Null values###

Missing_Data_Smoking = round(100*sum(is.na(healthcare$smoking_status))/nrow(healthcare),2)
Missing_Data_Smoking

summary(healthcare)

view(healthcare)

#######################Exploratory Data Analysis(Plotting Graphs)##################

##How many People have Hypertension as per smoking status?

smoking_status_hyp1<-group_by(healthcare,smoking_status)
smoking_status_hyp1
filter(healthcare,hypertension==1)%>%
  ggplot(aes(y=hypertension, x=smoking_status,fill=smoking_status))+geom_col()+
  xlab("Smoking Status") + ylab("Hypertension")+
  ggtitle("Hypertension as per smoking status")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=15, angle = 20),
        axis.text.y=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))

#How many people have heart disease as per smoking status?

smoking_status_heart<-group_by(healthcare,smoking_status)
smoking_status_heart
filter(healthcare,heart_disease==1)%>%
  ggplot(aes(y=heart_disease, x=smoking_status,fill=smoking_status))+geom_col()+
  xlab("Smoking Status") + ylab("Heart Disease count")+
  ggtitle("Heart disease as per smoking status")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=15, angle = 20),
        axis.text.y=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))

#Glucose level of male and female?

glucose<-group_by(healthcare,gender)
glucose
summarise(glucose, mean_glucose=mean(avg_glucose_level))%>%
  ggplot(aes(y=mean_glucose, x=gender,fill=gender))+geom_col()+
  xlab("Gender") + ylab("Average Glucose level")+
  ggtitle("Average glucose level as per Gender")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=15, angle = 20),
        axis.text.y=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))

#Average glucose level of people as per work type?

glucose_work_type<-group_by(healthcare,work_type)
glucose_work_type
summarise(glucose_work_type, mean_glucose=mean(avg_glucose_level))%>%
  ggplot(aes(y=mean_glucose, x=work_type,fill=work_type))+geom_col()+
  xlab("Work type") + ylab("Average Glucose level")+
  ggtitle("Average glucose level as per work type")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=15, angle = 20),
        axis.text.y=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))

#Average age as per smoking status? 
age_smoking_status<-group_by(healthcare,smoking_status)
age_smoking_status
summarise(age_smoking_status, mean_glucose=mean(age))%>%
  ggplot(aes(y=mean_glucose, x=smoking_status,fill=smoking_status))+geom_col()+
  xlab("Smoking Status") + ylab("Average age")+
  ggtitle("Average age level as per smoking status")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=15, angle = 20),
        axis.text.y=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))

#How many people have hypertension as per smoking status?

smoking_status_hyp1<-group_by(healthcare,smoking_status)
smoking_status_hyp1
filter(train_data_With_Smoke_Data,hypertension==1)%>%
  ggplot(aes(y=hypertension, x=smoking_status,fill=smoking_status))+geom_col()+
  xlab("Smoking Status") + ylab("Hypertension")+
  ggtitle("Hypertension as per smoking status")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=15, angle = 20),
        axis.text.y=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))

#BMI as per work type

bmi_work_type<-group_by(train_data_With_Smoke_Data,work_type)
bmi_work_type
summarise(bmi_work_type, mean_bmi=mean(bmi))%>%
  ggplot(aes(y=mean_bmi, x=work_type,fill=work_type))+geom_col()+
  xlab("Work Type") + ylab("Average BMI")+
  ggtitle("Average BMI as per work status")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(colour = "Blue", size=15),
        axis.title.y=element_text(color="Red",size=15),
        axis.text.x=element_text(size=15, angle = 20),
        axis.text.y=element_text(size=15),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.position=("right"),
        legend.justification=c(1,1))

############Training a Model############################

#Diving Data into TRAINIING AND TEST for evaluation of model(80:20)# 


train_healthcare<-healthcare[1:39000,]
test_healthcare<-healthcare[39001:43400,]

str(train_healthcare)
str(test_healthcare)

##########Applying Logistic Regression ###########

model<-glm(stroke ~.,family=binomial ,data=train_healthcare)

summary(model)

result <- predict.glm(model,newdata=test_healthcare,type='response')

result

###Setting Threshold####

result <- ifelse(result > 0.02,1,0)
result<-as.factor(result)

sum(test_healthcare$stroke=='1')
sum(train_healthcare$stroke=='1')

#############Checking confusion Matrix#############3

library(caret)
confusionMatrix(data=result, reference=test_healthcare$stroke)

#######Applying Logistic on Significant Features#######

model_with_feat<-glm(stroke ~ gender+age+hypertension+heart_disease+avg_glucose_level,family=binomial ,data=train_healthcare)

summary(model_with_feat)

result_up <- predict.glm(model_with_feat,newdata=test_healthcare,type='response')

###Setting Threshold###

result_up <- ifelse(result_up > 0.02,1,0)
result_up<-as.factor(result_up)

sum(test_healthcare$stroke=='1')
sum(train_healthcare$stroke=='1')

library(caret)
confusionMatrix(data=result_up, reference=test_healthcare$stroke)

#####Applying RANDOM FOREST Model ########
library(randomForest)

####set seed###

set.seed(17)

str(train_healthcare)

###Random Forest Model###

Model <- randomForest(stroke ~ age +hypertension + heart_disease + avg_glucose_level,data =train_healthcare ,mtry = 5,importance = TRUE,do.trace = 100)
print(Model)

#####Predicting on Test Model####

health_pred <- predict(Model, test_healthcare)
health_pred

#######CrossTable Evaluation######

library(gmodels)
CrossTable(test_healthcare$stroke,health_pred ,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#######Confusion Matrix####

library(caret)
confusionMatrix(data=health_pred, reference=test_healthcare$stroke)

####Decision Tree##############

# create a random sample for training and test data
# use an older random number generator to match the book
set.seed(123) # use set.seed to use the same random number sequence as the tutorial

# check the proportion of class variable
prop.table(table(train_healthcare$stroke))
prop.table(table(test_healthcare$stroke))

## Training a model on the data ----
# build the simplest decision tree
install.packages("C50")
library(C50)
health_model <- C5.0(train_healthcare[-11], train_healthcare$stroke)

# display simple facts about the tree
health_model

# display detailed information about the tree
summary(health_model)

## Evaluating model performance ----
# create a factor vector of predictions on test data
health_pred <- predict(health_model, test_healthcare)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(test_healthcare$stroke, health_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#######Confusion Matrix####

library(caret)
confusionMatrix(data=health_pred, reference=test_healthcare$stroke)


#######################################END#########################################

