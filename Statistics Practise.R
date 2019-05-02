install.packages("titanic")
library(titanic)
library(dplyr)
library(sqldf)
a=titanic_train    # inbuilt data frame in this package
View(a)
head(a,1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

summary(a$Age)
summary(a$Fare)
a1 = sd(a$Age, na.rm = T)
a2 = var(a$Age, na.rm = T)
a3 = IQR(a$Age, na.rm = T)   # Inter Quartile Range
a3
sqrt(a2)

f1= sd(a$Fare)
f1
f2=var(a$Fare)
f2
f3 = IQR(a$Fare, na.rm = T)   # Inter Quartile Range
f3

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7. select random sample of 500 records
set.seed(12) # It will select the same sample again and again
a1=a[sample(nrow(a), 500),]  # replace by default is false. If we don't specify anything
View(a1)

set.seed(12)
a2=a[sample(nrow(a), 500, replace = F),]
View(a2)
?sample

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 8. compute the mean fare by PClass
aggregate(a$Fare, by = list(a$Pclass), mean)              # group by aggregate


sqldf("select PClass,  avg(Fare) from a group by PClass")  # group by sqldf

a3 =a %>% select(Fare, PClass) %>% group_by(Pclass) %>% summarise(mean(Fare)) # By dplyr commands - select, groupby and summarise

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 9. Seperate the records of all the classes and compute the mean

t1=mean(a[a$Pclass=='1', 'Fare'])
t2=mean(a[a$Pclass=='2', 'Fare'])
t3=mean(a[a$Pclass=='3', 'Fare'])

t1;t2;t3

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 10. Assuming the age is following normal distribution
# 11.	 What is the propability of a person with age greater than 50 (use pnorm)

?pnorm

mean(a$Age , na.rm = T)
sd(a$Age , na.rm = T)

t4 = pnorm(50, mean(a$Age, na.rm = T), sd(a$Age, na.rm = T), lower.tail = F )  # This will give the probability of age where
t4                                                                       #age is greater  than 50. basically lower.tail
                                                                      #set to 'F' will choose results where age is greater than 50

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 12.	 what is the propability of fiding a person between the age of 40 and 50?


mean(a$Age , na.rm = T)
sd(a$Age , na.rm = T)

t5 =pnorm(40, mean(a$Age, na.rm = T), sd(a$Age, na.rm = T), lower.tail = T )# This will give the probability of age where age is less  than 40
t6 =pnorm(50, mean(a$Age, na.rm = T), sd(a$Age, na.rm = T), lower.tail = T )# This will give the probability of age where age is less  than 50

t7= t6-t5
t7        # Answer

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 13.	 what is 75th percentile age?


summary(a$Age)    # 3rd Quantile= 38 = 75th percentile

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 13.	 what is 75th percentile age?


summary(a$Age)    # 3rd Quantile= 38 = 75th percentile
t8=quantile(a$Age, probs=0.75, na.rm = T)  # with quantile function
t8
t9=quantile(a$Age, probs = seq(0,1,by = 0.1), na.rm = T)  # 10 quantiles values
t9   # 38
t10 = qnorm(0.75 , mean(a$Age, na.rm = T), sd(a$Age, na.rm = T), lower.tail = T)
# we can get same value through qnorm, But the result will differ slightly because qnorm will consider the
#distribution as normal
t10   #39.49709
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 14.	 age of the 95% of the people ( lower(2.5%) and upper(97.5%))?

t11=quantile(a$Age, probs = c(0.025, 0.975), na.rm = T)  # 10 quantiles values
t11   # between 2 to 62 -- So there are atleast 95% people with age less than or equal to 62
      # and 2.5% are there with age less than or equal to 2
t12 = qnorm(c(0.025, 0.975) , mean(a$Age, na.rm = T), sd(a$Age, na.rm = T), lower.tail = T)
t12    # qnorm considers the distribution as normal, so results are slightly different here, i.e. 1.227706, 58.170529

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 15.	Plot the probability density of age variable

x=a$Age
y=density(x,na.rm = T)

?plot

plot(y, main = "Age Plot", Xlab="Age", ylab = "Density")

# Histogram
hist(x)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 16.	Compute Z values for Fare variable

a$Zscore= (a$Fare-mean(a$Fare, na.rm = T))/sd(a$Fare, na.rm = T)
View(a)

range(a$Zscore)        # -0.6480577 -- 9.6617402
range(a$Fare)          # 0.0000  --  512.3292

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 17.	Convert the Fare into standard normal values

sdn = (a$Fare - min(a$Fare))/(max(a$Fare) - min(a$Fare))

range(sdn)   # always between 0 and 1

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 18.	 Is there a difference in mean age of Males and Females on Titanic?
# 19.	 Set a Alternate hypothesis and Null hypothesis
# 20.	 Alternate hypothesis is Mean age of Men is greater than mean age of females


x= a[a$Sex =='male', 'Age']
View(x)
y=a[a$Sex =='female', 'Age']
View(y)

t.test(x,y,alternative = "two.sided")  # since p-value=0.01181 < 0.05, so we go with alternate hypothesis
# and here alternative is "two.sided means that mean age of male is not equal to mean age of female

t.test(x,y,alternative = "greater")  # again p-value = 0.005907 < 0.05, so we again go with alternate hypothesis
# It was single sided and alternative="greater" so alternate hypothesis is avg age of male(x) is greater than avg age of female

t.test(x,y,alternative = "less")  # Now p-value = 0.9941 > 0.05, so this time we will go with Null hypothesis
# It was single sided and alternative="less" so null hypothesis is avg age of male(x) is greater than avg age of female

# again we need not to calculate x and y separetly. we can simply calculate them with the help of a formula

t.test(a$Age ~ a$Sex, alternative ="less")
#a$age will select ages
#a$sex will prform group by in alphabtical order(i.e. female first and then male)

t.test(Age,Sex,data =a,alternative='greater')


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 21.	 What is the propability of survival?

x=a[a$Survived==1, 'PassengerId']
length(x)
nrow(a)

Prob_survival = length(x)/nrow(a)
Prob_survival  # 0.3838384 answer

# or

prop.table(table(a$Survived))[2]   # will give the probability of survival i.e. 2nd column direct where survived=1


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 22.	 what is the propability of survial for Males?
# 23.	 what is the propability of survival of females?

m1 = a[a$Survived==1 & a$Sex=='male', 'PassengerId']    # for survival of male
m2=a[ a$Sex=='male', 'PassengerId']
Prob_survival_male =  length(m1)/length(m2)
Prob_survival_male                # 0.1889081

#OR

prop.table(table(a[a$Sex=='male', "Survived"]))[2]    # 0.1889081

f1 = a[a$Survived==1 & a$Sex=='female', 'PassengerId']
f2=a[ a$Sex=='female', 'PassengerId']
Prob_survival_female =  length(f1)/length(f2)
Prob_survival_female              # 0.7420382

#OR

prop.table(table(a[a$Sex=='female', "Survived"]))[2]  #0.7420382

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#26.	 compute the odss of survival of Males
#27.	 compute the odds of survival for females

# given the person is male what is the probability that he survived
Prob_survival_male           # Answer
1-Prob_survival_male

# given the person is female what is the probability that she survived
Prob_survival_female           # Answer
1-Prob_survival_female


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#26. Perfrom hypothesis testing on survival of Males vs Survival of females
#table(titanic_train$Sex, titanic_train$Survived)
#chisq.test(table(titanic_train$Sex, titanic_train$Survived))
#hist(titanic_train$Age)


options(scipen=999)  # to get p-value in decimals
chisq.test(table(a$Sex,a$Survived))  #chisquare test for bionomial variables
hist(a$Age)

# Null hypothesis is that survival is independent of gender. i.e. both males and females equally survived
# p-value = 0.00000.. < 0.05 so we will go with alternative hypothesis
# Alternative hypothesis is: they don't survive equally

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
