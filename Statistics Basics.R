# Statistics Basic


t=read.csv("train.csv", header=T, sep=',', stringsAsFactors = F)
t0=summary(t$User_ID)
t0
var(t0)
sd(t0)
sqrt(4525159)
getwd()
View(t)
#=======================================================================================================================

# One sample test
# Lets consider a bridge strength example where allowed strength value is 100
strength=c(101,102,100,99,103,101,100,101,102,103,104,99,100,98,101,102,103)
mean(strength)
#H0 : Mean(strength)<=100
#Ha : Mean(strength) >100

#t.test(x,y,alternative = ) # in case of 2 samples x and y
t.test(x=strength,mu=100, alternative ="greater")  # in case of only one sample 'x' and a theoretical value 'mu'
                                                   # mu=100(our theoretical value for bridge strentgth)
                                                   # by default the confidance level is set to 95%, we can change it if we want

qt(0.05,25)  # to calculat t-critical value
#Here t static value is=2.7863, and t critic value comes out to be 1.16,(t stat > t critic) so we can reject our null hypothesis here


# other way if p-value is less than 0.05 then we can reject the null Hypothesis
# function t.test(), gives us the p-value as well which gives us the probability of making type-1 error
# so one more rule there says that if v<=0.05 then reject the null hypothesis, otherwise not

#=======================================================================================================================

## one sided,  two sample test

girls=c(5,6,7,8,9,9,9,8,7,9,7,8,8,9,9,9,9,9,9,9)
length(girls)
mean(girls)
boys =c(5,4,5,6,8,8,9,7,5,7,5,9,8,8,7,6,7,5,8,8)
length(boys)
mean(boys)

#H0 : Mean(girls)<=Mean(boys)
#Ha : Mean(girls)>Mean(boys)

t.test(x=girls, y=boys, alternative='greater') # here we are comparing girls with boys for greater . which means that if
                                              # the mean of marks of girls is greater than the mean of marks of boys,
                                              #we will make a conclusion that on avg girls score more than boys.
qt(0.05, 38.854)  # tcritical=1.685,  IGNORE MINUS SIGN
#Here t stat =1.6197, and t citic=1.64(from t table with df=37.881), so (t stat<t critic), hence we can't reject the null hypothesis
# Also p value=0.0569, which is >0.05, so we can't reject our null hypothesis. We will have to stick to our null hypothesis
#which says that girls don't score more than boys.

#======================================================================================================================

# two sided

girls=c(5,6,7,8,9,4,5,8,7,6,7,8,8,9,9,9,9,9,9,9)
length(girls)
mean(girls)
boys =c(5,4,5,6,8,8,9,7,5,7,5,9,8,8,7,6,7,5,8,8)
length(boys)
mean(boys)

#H0 : Mean(girls) = Mean(boys)
#Ha : Mean(girls) != Mean(boys)

t.test(x=girls, y=boys, alternative='two.sided') # the only chaneg is alternative='two.sided'.
                                                #It can give only equal to or not equal to hypothesis
# still p value is 0.1136>0.05, so we have to go with the null hypothesis

#======================================================================================================================
