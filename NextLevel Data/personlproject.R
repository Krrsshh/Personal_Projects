pbinom(235,size=1000,prob=23.5)


1.96*(sqrt((0.3*0.7)/1000)+((0.26*0.74)/800))
0.3-0.26
0.04-0.02887448

x <- c(X1, X2)
n <- c(n1, n2)

prop.test(x, n)

x <- c(300, 208)

n <- c(1000, 800)
prop.test(x, n)

prop.test(c(315, 276), c(900, 600))

prop.test(c(276, 315), c(600, 900))

prop.test(240, 500)

x <- c(240, 470)
n <- c(500, 800)
prop.test(x, n)


X <- c(2,4,6,8)
Yes <- c(2,9,37,44)
No <- c(49,33,11,5)


probabilityYes <- (Yes/Yes+No)
round(probabilityYes, digits=3)

MyModel <- glm(cbind(Yes,No) ~ X, family= "binomial" )
summary(MyModel)


set.seed(123456)
X <- sample(0:5, size=500, replace=TRUE)
LO <- -6 + 2.2*X
ODD <- exp(LO)
PROB <- ODD/(1+ODD)
Y <- rbinom(500, 1, prob=PROB)
Outcome <- ifelse(Y==1, "yes", "no")
Q9Data <- data.frame(X, Y, Outcome)
View(Q9Data)

xtabs(~ Outcome + X, data=Q9Data)
Q9Data$Renew <- ifelse(Q9Data$Outcome=="yes",1,0)
Mymodel <- glm(Renew <- X,data=Q9Data, family = "binomial")
summary(Mymodel)

