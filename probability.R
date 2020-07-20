library(gtools)
library(ggplot2)
library(dplyr)
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
queen <- paste("Queen", suits)
kings<- paste("King", suits)
jack<- paste('Jack', suits)
mean(deck %in% kings)
hands <- permutations(52,3, v = deck)
hands
first_card <- hands[,1]
first_card
second_card <- hands[,2]
second_card
third_card<- hands[,3]
x<- permutations(5, 2, v = c(1,2,3,4,5))
prob<-sum(first_card %in% kings & second_card %in% queen) / sum(first_card %in% kings)
sum(first_card %in% kings & second_card %in% queen&third_card %in% jack) / (sum(first_card %in% kings)*prob)

#probability that two people have same birthday among 50 people
results <- replicate(10000, { 
  bdays <- sample(1:365, 50, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)
#probability of picking two cards where both happen to be aces
case2<- replicate(10000, {
  picked_values<- sample(deck, 2, replace=FALSE)
  all(picked_values %in%jack)
})
mean(case2)
4/52*3/51
#probability of rolling 6 in a die
case3<- replicate(10000, {
  picked_values<- sample(1:6, 1, replace=TRUE)
  all(picked_values == 6)
})
mean(case3)
#proving the central limit theorem
die<- 1:6
case4<- replicate(10000, {
  values<- sample(1:6, 4, replace = TRUE)
  sum(values)
})
mean(case4)
hist(case4, breaks = 20)
ggplot()+geom_density(aes(case4), fill = 'red', color = NA)
mean(case4)/4
pnorm(24, mean(case4), sd(case4))
#proving the law of large numbers
numbers<- 1:10

values<- vector('numeric', 700)
for(i in 1:700){
  values[i] <- mean(sample(numbers,i , replace = TRUE))
}
values
plot(1:700, mean(numbers)/values)




#a normal distribution
values<- rnorm(100000)
ggplot()+geom_density(aes(values))
dnorm(0)# finds out the certain probability density
qnorm(0.75)#finds out the certain quantile
#a uniform distribution
values<- runif(100000,0, 40)# with minimum value of 0 and maximum with 40
ggplot()+geom_density(aes(values))
dunif(20, 0, 40)# finds out the certain probability density, in uniform distribution probability denisty is a constant
qunif(0.75, 0, 40)#finds out the certain quantile
#an exponential distribution
values<- rexp(10000, 1/5)# two arguments one is the number of values the other is the y- intercept, i.e. the reciprocal of the mean
ggplot()+geom_density(aes(values))
pexp(3, 1/5)
dexp(3, 1/5)

#a monte carlo simulation that proves that confidence level is correct
p<- 0.35
confidence_levels_check<- replicate(10000,{
                                    X<- sample(c(0,1), size= 1000, replace =TRUE, prob = c(1-p, p))
                                    average<- mean(X)
                                    standard_error<- sqrt(average*(1-average)/1000)
                                    between(p, average-(qnorm(0.975)*standard_error), average+(qnorm(0.975)*standard_error))})


#the average weight of adults is 65kg, Patricia believed the true average was different, she tested 40 patients of their weights
#and found that the mean weight was 67kg  and standard deviation of 3.4 
#with a 95% confidence level, calculate p value should we accept or reject null hypothesis
#alternative hypothesis not equal to 65kg, since this hypothesis is not equal to a value we are going to have a two tailed test
z_score_rejection<- qnorm(0.975)
alpha<- 1-0.95
z_score_case<- abs(67-65)/(3.4/sqrt(i))
p_value<-2*(1- pnorm(z_score_case))
p_value<alpha
#since p -value is less than our significance level, alpha, we can say that we should reject the null hypothesis



