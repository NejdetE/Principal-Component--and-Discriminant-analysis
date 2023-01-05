install.packages("wooldridge")
install.packages("mvnormtest")
install.packages("MASS")
install.packages("ICSNP")
install.packages("HDtest")
install.packages("ggplot2")
library(wooldridge)
library(mvnormtest)
library(MASS)
library(ICSNP)
library(HDtest)
library(ggplot2)

#1 Principal component analysis
#data set
help("prison")
data("prison")
View(prison)

prison <- prison[(1:714), c(1:2, 6:8, 31, 41)]
prison <- subset(prison, year==93)
prison <- prison[, c(3:7)]
row.names(prison) <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
                       "GA", "HD", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                       "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
                       "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                       "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                       "WY")
prison <- prison[c(1:10,41:51),]

apply(prison, 2, mean) #1.1 variable means
apply(prison, 2, sd)   #1.2 variable standard deviations
apply(prison, 2, var)  #1.3 variable variances

prison.pca <- prcomp(prison[,1:5], center = T, scale. = T) #1.4 standardized data
prison.pca$rotation #1.5 eigenvectors
prison.pca$sdev^2/sum(prison.pca$sdev^2) #1.6 variances accounted for the pc:s
plot(prison.pca,type="l", main ="Scree-plot") #1.7 scree plot
cor(prison.pca$x[,1:2],prison[,1:5]) #1.8 loadings 

x1 <- expression(PC[Crime])
x2 <- expression(PC[Economy])

biplot(prison.pca,
       xlab = x1,
       ylab = x2) # Group observations
abline(h = 0, v = 0, lty = 2)

#2 Discriminant analysis
#data set
help("wage2")
data("wage2")
View(wage2)

#2.1 data and synthesization
wage2 <- wage2[(1:935), c(9,1,5)]
wage2 <- subset(wage2, wage <= 1500 & wage >= 500 & educ > 9 & educ < 16)

#2.2 Plot for observations 
obs.plot <- ggplot(wage2, aes(x = wage, y = educ, col = married)) +
  geom_point(size = 2) 
obs.plot + scale_colour_gradient(low = "black", high = "red")

qqnorm(wage2$wage, main = "Wage Normal Q-Q plot") #2.3 Normality assumption check
qqline(wage2$wage) #2.4 Normality assumption check
qqnorm(wage2$educ, main = "Education Normal Q-Q plot") #2.5 Normality assumption check
qqline(wage2$educ) #2.6 Normality assumption check

mshapiro.test(t(as.matrix(wage2[, c(2,3)]))) #2.7 Normality assumption check
wage2 <- wage2[order(wage2$married),]
testCov(wage2[(1:48), c(2,3)], wage2[(49:565), c(2,3)]) #2.8 Covariance matrices equality test
HotellingsT2(as.matrix(wage2[(1:48), c(2,3)]), as.matrix(wage2[(49:565), c(2,3)])) #2.9 Group mean test

fit <- lda(married ~ wage + educ, data = wage2, prior = c(0.5, 0.5))
fit

w1=(fit$scaling[1,1]/(sqrt(fit$scaling[1,1]^2+fit$scaling[2,1]^2)))
w2=(fit$scaling[2,1]/(sqrt(fit$scaling[1,1]^2+fit$scaling[2,1]^2)))
w1
w2

wage2[, 4] <- wage2[,2]*w1 + wage2[,3]*w2
wage2[, 5] <- 1
z1.mean=mean(wage2[(1:48), 4])
z2.mean=mean(wage2[(49:565), 4])
n1=fit$counts[1]
n2=fit$counts[2]
cutoff=(n1*z1.mean+n2*z2.mean)/(n1+n2)
wage2[, 5][wage2[,4] > cutoff] <- 1
wage2[, 5][wage2[,4] < cutoff] <- 0

#2.10 Classification
TN = sum(wage2[(1:48), 5]==0) #True Negative
TP = sum(wage2[(49:565), 5]==1) #True Positive
Sensitivity = TP/n2
Specificity = TN/n1
FP = 1 - Sensitivity #False Positive
FN = 1 - Specificity #False Negative
(TN + TP)/(n1+n2)
colnames(wage2) <- c("married", "wage", "educ", "Z-value", "Classification")
view(wage2)
