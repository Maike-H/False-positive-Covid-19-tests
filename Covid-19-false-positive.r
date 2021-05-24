#Covid-19-Incidence of 150/100000 citizens
prev<-0.0015
#number of people tested
N<-100000

#testing people with probability of getting Covid-19 by Incidence
outcome<-sample(c("Corona","Healthy"),N,replace=TRUE, prob=c(prev,1-prev))
N_C<-sum(outcome==("Corona"))
N_H<-sum(outcome==("Healthy"))

#Accuracy of a Covid-19 PCR-test (99,9%)
accuracy<-0.999

#Calculating the test-outcome with Bayes' Rule
test<-vector("character",N)
test[outcome=="Corona"]<-sample(c("+","-"),N_C,replace=TRUE,prob=c(accuracy,1-accuracy))
test[outcome=="Healthy"]<-sample(c("-","+"),N_H,replace=TRUE,prob=c(accuracy,1-accuracy))

#Creating a table for the outcome
result<-table(outcome,test)
result

#Number of people per 100000, getting a false positive result
result[2,2]
