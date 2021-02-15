
prison_data<-read.csv("C:/Users/valay/OneDrive/Desktop/Ivey/Winter Term 2021/Big Data Analytics/Class 3/arrest data.csv")

#install.packages("lattice")
#install.packages("ggplot2")


head(prison_data)

library(corrplot)
x <- cor(prison_data[1:11])
corrplot(x, type="upper", order="hclust")

cor(prison_data)

#educ1 changes categorical variable to binary; if 12th or above then 1 or else 0

prison_fin <- glm (arrest~ +financial_aid, data=prison_data, family=binomial(link="logit"))
summary(prison_fin)
#0.05 p-value

prison_race <- glm (arrest~ +race, data=prison_data, family=binomial(link="logit"))
summary(prison_race)
#0.5 p-value

prison_workxp <- glm (arrest~ +work_exp, data=prison_data, family=binomial(link="logit"))
summary(prison_workxp)
#0.003 p-value

prison_marital <- glm (arrest~ +marital, data=prison_data, family=binomial(link="logit"))
summary(prison_marital)
#0.05 p-value

prison_parole <- glm (arrest~ +parole, data=prison_data, family=binomial(link="logit"))
summary(prison_parole)
#0.58 p-value

prison_conv <- glm (arrest~ +num_of_prior_convictions, data=prison_data, family=binomial(link="logit"))
summary(prison_conv)
#0.001 p-value

prison_educ1 <- glm (arrest~ +educ1, data=prison_data, family=binomial(link="logit"))
summary(prison_educ1)
#0.02 p-value

prison_empw <- glm (arrest~ +employed_weeks, data=prison_data, family=binomial(link="logit"))
summary(prison_empw)
#0.00 p-value


prison_logistic <- glm (arrest~ +financial_aid +work_exp +marital +num_of_prior_convictions +educ1 +employed_weeks, data=prison_data, family=binomial(link="logit"))

head(prison_logistic)
prison_logistic
summary(prison_logistic)

#Only two variables are significant, education and employed weeks
prison_logistic_true <- glm (arrest~ +educ1 +employed_weeks +financial_aid, data=prison_data, family=binomial(link="logit"))
prison_logistic_true
summary(prison_logistic_true)


#this one does not have wexp
#prison_logistic1 <- glm (arrest~ +fin +age +race +mar +paro +prio +educ1, data=prison_data, family=binomial(link="logit"))
#summary(prison_logistic1)
#prison_logistic1

#this one exlcudes age
#prison_logistic2 <- glm (arrest~ +fin +race +wexp +mar +paro +prio +educ1, data=prison_data, family=binomial(link="logit"))
#summary(prison_logistic2)
#prison_logistic2

pred <- predict(prison_logistic_true, prison_data, type = "response")
Output <- data.frame(actual = prison_data$arrest, predicted = pred)

classification <- ifelse(pred > 0.4, 1, 0)
#0.5 threshold based on confusion matrix specificity number

actual = prison_data$arrest
Output <- data.frame(actual, predprob=pred, classification)

install.packages("caret")
install.packages('e1071', dependencies=TRUE)
library(caret)

confusionMatrix(as.factor(classification), as.factor(actual))
#Confusion matrix gives you your predictions vs. actual (reference)
#accuracy of your predictions (overall what % of your predictions did you get correct?)

#specificity is the % of 1s that you will predict correctly
#Correct Predicted actual 1s divided by all actual 1s

#1-Sensitivity gives you % of predictions for 1s which are wrong 

install.packages("ROCR")
library(ROCR)
install.packages("rlang")

predObj <- prediction(pred, prison_data$arrest)
rocObj = performance(predObj, measure="tpr", x.measure="fpr")
aucObj = performance(predObj, measure="auc")
plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4)))
#Plot of ROCR (area under the curve)