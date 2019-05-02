# correlation

#Chi square test

#H0 : smokers and cancer are independent of each other
#Ha : smoking causes cancer
smokers=c(rep('No',75), rep('Yes', 25))
cancer = c(rep('No', 70), rep('Yes',5 ), rep('No', 5), rep('Yes', 20))
table(cancer)
table(cancer, smokers)

chisq.test(table(smokers,cancer)) # X-squared = 49.938, df=1 an p-value is negligible, here x squared value is greater than x
                                  # X- critic value, so we go with the alternate hypothesis
                                  # since p value is very very less than 0.05 here so we go with the alternate hypothesis
