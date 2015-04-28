##March 9th
#2 issues: size of the tree , and missing values
#
.first = function(){
	require(MASS)
	require(lars)
	require(nnet)
	
	}
	
summary(cpus)
library(rpart)
cpus.rp=rpart(log10(perf)~.,cpus[,2:8],cp=0)
plot(cpus.rp,uniform = TRUE)
text(cpus.rp,digits=3,cex=.6)



printcp(cpus.rp)

to change the number of cross validations:
cpus.rp=rpart(log10(perf)~.,cpus[,2:8],cp=0,xval=100)
plot(cpus.rp,uniform = TRUE)
text(cpus.rp,digits=3,cex=.6)
printcp(cpus.rp)

cpus.rp2=prune(cpus.rp,cp=.006)
plot(cpus.rp2,uniform = TRUE)
text(cpus.rp2,digits=3,cex=.6)

write.table(cpus,file="cpus.txt",sep = "\t")


dep = read.delim(file.choose(),header = TRUE)
depp = rpart(DRP~., data = dep)