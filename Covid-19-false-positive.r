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

#Probability in percent of being healthy with getting a positive test
prob_healthy<-(result[2,2]/(result[2,2]+result[1,2]))*100
prob_healthy

#Running a Monte Carlo simulation to calculate the 95% confidence interval of this probability
#samplesize for Monte Carlo simulation
B<-10000
MC_prob<-replicate(B,{MC_outcome<-sample(c("Corona","Healthy"),N,replace=TRUE, prob=c(prev,1-prev))
MC_N_C<-sum(MC_outcome==("Corona"))
MC_N_H<-sum(MC_outcome==("Healthy"))
MC_test<-vector("character",N)
MC_test[MC_outcome=="Corona"]<-sample(c("+","-"),MC_N_C,replace=TRUE,prob=c(accuracy,1-accuracy))
MC_test[MC_outcome=="Healthy"]<-sample(c("-","+"),MC_N_H,replace=TRUE,prob=c(accuracy,1-accuracy))
MC_result<-table(MC_outcome,MC_test)
MC_prob_healthy<-(MC_result[2,2]/(MC_result[2,2]+MC_result[1,2]))*100
mean(MC_prob_healthy)
})
mean(MC_prob)

#Calculation for 95% confidence interval
N<-1000 #samplesize for standard error
X_hat<-mean(MC_prob)/100 #expected value
SE_hat<-sqrt(X_hat*(1-X_hat)/N) #standard error
ci<-c(lower<-X_hat-qnorm(0.975)*SE_hat,upper<-X_hat+qnorm(0.975)*SE_hat)
ci

#95% of the probabilities of getting a false positive covid-19 PCR-test are between these two values (in percent):
ci[1]*100
ci[2]*100
