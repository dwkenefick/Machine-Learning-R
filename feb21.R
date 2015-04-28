x = state.x77[,-4]
y = state.x77[,4]
install.packages("leaps")
require(leaps)



#subset selection
x1 = regsubsets(x,y)
summary(x,y)

#shows the best subset selection based on vaorious criteria 
par(mfrow = c(2,2))
plot(x1,scale = "r2")		#likes all of them
plot(x1,scale = "adjr2")	#max adjr2, likes pop murder hsgrad frost
plot(x1,scale = "Cp")		#minimize Cp ,likes pop,murder HSgrad, Frost


x2 = leaps(x,y,nbest = 4, method = "Cp")
x3 = leaps(x,y,nbest = 4, method = "adjr2")
x4 = leaps(x,y,nbest = 4, method = "r2")
par(mfrow = c(1,1))
plot (x2$size-1,x2$Cp,xlab="Num predictors",ylab="Cp") #plot all of the best using a given number of predictors, showing the 4 best. 

par(mfrow = c(2,2)) #plot all of the varios predictors
plot (x2$size-1,x2$Cp,xlab="Num predictors",ylab="Cp")
plot (x3$size-1,x3$adjr2,xlab="Num predictors",ylab="Adjr2")
plot (x4$size-1,x4$r2,xlab="Num predictors",ylab="r2")



x2$which[which.min(x2$Cp),] #tells you the best subset. 
x2$which[13:16] # four best with 4



#ridge regression
library(MASS)
states = data.frame(state.x77)
lmr = lm.ridge(Life.Exp~.,data = states)

#as.matrix turns things back into a matrix
statesm = as.matrix(states)

lmr = lm.ridge(Life.Exp~.,data = states, lambda=seq(0,100,1))
plot (lmr)
names (lmr)
lmr$coeff[,1]
sx = scale(states)
apply(sx,2,mean)	# syntax: array, dim, function
apply(sx,2,sd)
sx=sx/7
sum(sx[,1]^2)

#check agains standardized ols
x=sx[,-4]
y=sx[,4]
ols = lm(y~x)
ols
lmr$coef[,1]
lmr$coef[,50]	#some things get bigger, some smaller...

#but how do we choose our lambda? could split into test and training data. write a wrapper that randomly selects 100 people as a "test set" and takes the average of the penalized RSS test. then pick lambda that minimizes the avg penalized SS.  

bodyfat2 = read.table(file.choose(),header=TRUE)
olsbf = lm(Pct.BF~.,data = bodyfat2)
olsbf
stepbf = step(olsbf)

predolsbf = predict(olsbf,new=bodyfat2[bodyfat2$train==FALSE,])
predstepbf = predict(stepbf,new=bodyfat2[bodyfat2$train==FALSE,])

#checks how the 2 models did
cor(bodyfat2[bodyfat2$train==FALSE,1],predolsbf)
cor(bodyfat2[bodyfat2$train==FALSE,1],predstepbf)

#how about of we check sum squared residuals?
ybftest = bodyfat2[bodyfat2$train==FALSE,1]
rssols = sum((ybftest-predolsbf)^2)
rssstep = sum((ybftest-predstepbf)^2)
rsols
rssstep

lmrbf = lm.ridge(Pct.BF~.-train,data=bodyfat2[bodyfat2$train==FALSE,],lambda = seq(0,100,1))

install.packages("lars")
require(lars)
lasso1= lars(x,y)
plot(lasso1)

#whats RSS of the 50th model (labda = 50) 
