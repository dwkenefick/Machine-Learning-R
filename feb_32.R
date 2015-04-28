#feb 23 notes

#MAth stuff
lms = lm(Pct.BF~Weight,data = bodyfat)
res = residuals(lms)
t(res)%*%res 	# gives the RSS 
res%*%t(res)   #doesnt work, the outer product


bodyfat = read.table(file.choose(), header = TRUE)
bf = bodyfat
bfs = scale(bf,scale = FALSE)
bfs = data.frame(bfs)
bfs$train=bf$train

bfsols = lm(Pct.BF~.-train-1,subset = train,data = bfs)
bfytest = bfs[bfs$train == FALSE,1] 	#get the testing set
bfolspred = predict(bfsols,new=bfs[bfs$train==FALSE,])

mean(bfolsspred)
mean(bfytest)

cor(bfolspred,bfytest)

bfsstep = step(bfsols)
bfssteppred = predict(bfsstep,new=bfs[bfs$train==FALSE,])
cor(bfolspred,bfytest)
cor(bfssteppred,bfytest)

require(MASS)
bfsridge = lm.ridge(Pct.BF~.-train-1,subset = train, data =bfs, lambda = 1:100)

predsridge =matrix(NA,100,100)
for (i in 1:100){
	predsridge[,i]=as.matrix(bfs[bfs$train==FALSE,2:14])%*%coef(bfsridge)[i,]
}

corridge = NULL
for (i in 1:100){
	corridge[i]=cor(bfytest,predsridge[,i])	
}

install.packages("lars")
require(lars)

Xbfs = as.matrix(bfs[bfs$train == TRUE,2:14])
ybf=bfs[bfs$train==TRUE,1]
lassobf=lars(Xbfs,ybf)


which.min(lassobf$Cp)
round(coef(lassobf),2)

predlars6 = as.matrix(bfs[bfs$train == FALSE,2:14])%*%coef(lassobf)[6,]
predlars5 = as.matrix(bfs[bfs$train == FALSE,2:14])%*%coef(lassobf)[5,]

cor(bfolspred,bfytest)
cor(predlars5,bfytest)
cor(predlars6,bfytest)

#
#
#
#
#

