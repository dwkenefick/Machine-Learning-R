#Question 4, states

#predict life expectancy using 4 different models


require(MASS)
require(lars)


states.matrix = as.matrix(states)


#arrays to hold the results
	
ols = array(dim=c(50,1))
	
step = array(dim=c(50,1))
	
ridge = array(dim=c(50,1))


#OLS 
model
olsm = lm(Life.Exp~.,data = states)


#Step model
stepm = step(olsm)



#ridge 
model
ridgem = lm.ridge(Life.Exp~., data =states lambda = 1:100)





#Lasso model


intercept = array(1,dim=c(50,1))


states.y = states.matrix[,1]

states.X = states.matrix[,2:ncol(states.matrix)]



#so we also get the intercept term. 


states.X = cbind(states.X,intercept[,1])




lassom=lars(states.X,states.y)

which.min(lassom$Cp)


predlars4 = states.X%*%coef(lassom)[4,]

predlars5 = states.X%*%coef(lassom)[5,]

