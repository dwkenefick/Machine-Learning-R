#march 2nd
#setup
require(MASS)
ir = iris
ir = iris[,1:4]
log.ir = log(ir)
spec = iris[,5]
ir.lda = lda(log.ir,spec)
ir.ld = predict(ir.lda,dimen=2)$x
head(ir.ld)

#plotting. LD1 and 2 are linear combinations that separate 
#all of the groups (bayes)
levels(spec)=c("s","ve","vi")

eqscplot(ir.ld,type="n",xlab="LD1",ylab="LD2")
text(ir.ld,labels=spec,col=as.numeric(spec))

#one dim
plot(ir.lda,dimen =1)
plot(ir.lda,type = "density",dimen =1)

logir = cbind(log.ir,spec)
write.table(logir,"logir.txt",sep ="\t",row.names=FALSE)
getwd()


#work in jmp.... 
analize>multivariate methods > descriminant
rows>color or mark by rows

in analysis:
score options: show interesting rows only
score options : only show interesting rows
score optons : save formulas
cannonical options : show 50% contours

to change priors:

#Back to R

??"logistic regreson"
#logistic regression
log.ir = data.frame(log.ir,spec)
ir.glm1=glm(spec~.,subset = spec != "s",family = "binomial",data = log.ir)
ir.glm1
#what do these coefficients mean? coeficient on the "log odds"
predict(ir.glm1) #log odds of being VI
round(predict(ir.glm1, type ="response"),5)

#can plot, but kind of useless

#one variable, simple model
ir.sglm = glm(spec~Petal.Length,subset = spec!="s",data = log.ir,family = "binomial")
round(predict(ir.sglm, type ="response"),5)

#how to plot it?
temp = data.frame(Petal.Length = seq(0,2,len=1000))
temppred = predict(ir.sglm,new=temp,type="response")
with(temp,plot(temppred~Petal.Length,type="l"))

#can also do stepwise regresion
step(ir.glm1)

#back to jmp
analize>fit model
red arrow> confusion matrix
