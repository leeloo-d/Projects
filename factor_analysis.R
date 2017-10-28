#Dimention decreasing models

library("ggplot2")
library("knitr")
library("HSAUR")

data(heptathlon)
head(heptathlon)
View(heptathlon)
print(cor(heptathlon), digits = 2)

#correlation matrix
h <- heptathlon[, -8]
hept_pca <- prcomp(h, scale = TRUE)
pc <- hept_pca$x
head(pc)


qplot(x = heptathlon$score, y = pc[, 1]) + 
  labs(x = "Heptathlon points sum", 
       y = "First principal component",
       title = "First component connection with the final result")

cor(heptathlon$score, pc[, 1])
summary(hept_pca)

exp_percent<-summary(hept_pca)$importance[2, ]

qplot(y = exp_percent, x = names(exp_percent)) + 
  geom_bar(stat = 'identity') +
  labs(x = "Principal components",
       y = "Shares of variance",
       title = "Shares of variance explained by principal components")

biplot(hept_pca,xlim = c(-0.8, 0.8), ylim = c(-0.6, 0.6))
