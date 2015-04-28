require(MASS)

require(nnet)

Cf=data.frame(Tetrahydrocortisone=log(Cushings[,1]),Pregnanetriol=log(Cushings[,2]),Type=Cushings$Type)


plot(Cf[,1], Cf[,2], type="n",xlab = "log Tetrahydrocortisone", ylab ="log Pregnanetriol")


text(Cf[,1],Cf[,2],labels=Cf$Type,col=as.numeric(Cf$Type)+2)



data = Cf

Cf = Cf[Cf$Type != "u",]

Cf$Type = factor(as.character(Cf$Type))


Cfmulti = multinom(Type~., data = Cf[Cf$Type != "u",])

Cflda = lda(Type~., data = Cf)

Cfqda = qda(Type~., data = Cf)


xp <- seq(0.6, 4.0, length = 100)

yp <- seq(-3.25, 2.45, length = 100)


cushT <- expand.grid(Tetrahydrocortisone = xp,Pregnanetriol = yp)


Z.m = predict(Cfmulti, new = cushT,type="probs")

Z.lda = predict(Cflda, new = cushT, type = "posterior")

Z.qda = predict(Cfqda, new = cushT, type = "posterior")


Z.lda = Z.lda$posterior

Z.qda = Z.qda$posterior



zp3 = Z.lda[,3]-pmax(Z.lda[,2],Z.lda[,1])

zp1 = Z.lda[,1]-pmax(Z.lda[,2],Z.lda[,3])


zp1 = as.matrix(zp1)

zp3 = as.matrix(zp3)


contour(xp,yp,matrix(zp1,100),levels=0,add=T)

contour(xp,yp,matrix(zp13,100),levels=0,add=T)


zp3 = Z.qda[,3]-pmax(Z.qda[,2],Z.qda[,1])

zp1 = Z.qda[,1]-pmax(Z.qda[,2],Z.qda[,3])


zp1 = as.matrix(zp1)
zp3 = as.matrix(zp3)


plot(data[,1], data[,2], type="n",xlab = "log Tetrahydrocortisone", ylab ="log Pregnanetriol")
text(Cf[,1],Cf[,2],labels=Cf$Type,col=as.numeric(Cf$Type)+2)

contour(xp,yp,matrix(zp1,100),levels=0,add=T)

contour(xp,yp,matrix(zp3,100),levels=0,add=T)



zp3 = Z.m[,3]-pmax(Z.m[,2],Z.m[,1])

zp1 = Z.m[,1]-pmax(Z.m[,2],Z.m[,3])


zp1 = as.matrix(zp1)

zp3 = as.matrix(zp3)


plot(data[,1], data[,2], type="n",xlab = "log Tetrahydrocortisone", ylab ="log Pregnanetriol")

text(Cf[,1],Cf[,2],labels=Cf$Type,col=as.numeric(Cf$Type)+2) 

contour(xp,yp,matrix(zp1,100),levels=0,add=T)

contour(xp,yp,matrix(zp3,100),levels=0,add=T)