#Categorical variables analysis

library("stats")
library("datasets")
library("dplyr")
library("ISwR")
library("car")

#1.Comparation of means in two independent samples by using t-test
data(energy)
attach(energy)
count(energy, expend) 
tapply(expend, stature,mean)
t.test(expend ~ stature)


#2.Comparation of means in two dependent samples
data(intake)
attach(intake)
head(intake)
post - pre
mean(post - pre)
t.test(pre, post, paired = T)


#3.One-sample Wilcoxon test
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515,
              6805, 7515, 7516, 8230, 8770)
wilcox.test(d.intake, mu = 7725)


#4.Comparation of two independent samples
attach(energy)
wilcox.test(expend ~ stature)


#5.Comparation of two dependent samples
attach(intake)
wilcox.test(pre, post, paired = T)
wilcox.test(pre, post, paired = T, conf.int = T)


#6.Estimation of homogeneity of variances in two groups
attach(energy)
var.test(expend ~ stature, conf.level = 0.99)


#7.Estimation of homogeneity of variances in several groups
data(InsectSprays)
attach(InsectSprays)
leveneTest(count ~ spray, data = InsectSprays)
leveneTest(count ~ spray, data = InsectSprays, center = mean)


#8.Frequency analysis
qchisq(p = 0.95, df = 1)
mice <- matrix(c(13, 44, 25, 29), nrow = 2, byrow = T)
chisq.test(mice)

light <- c(12, 40, 45)
dark <- c(87, 34, 75)
very.dark <- c(3, 8, 2)
color.data <- matrix(c(light, dark, very.dark), nrow = 3,
                     dimnames = list(c("Pop1", "Pop2", "Pop3"),
                                     c("Light", "Dark", "Very dark")))
chisq.test(color.data)

#9.Fisher's exact test
(X <- matrix(c(1, 10, 8, 4), ncol = 2))
fisher.test(X)
