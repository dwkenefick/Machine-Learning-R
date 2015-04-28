# code for feb 16th


#messing around with states
states = data.frame(states.x77)
attach (states)

lmsfull = lm(Life.Exp~.,data=states)
summary(lmsfull)
with(states,hist(Area))
with(states,boxplot(Area))
states[order(states$Area),]

#what is bodyfat ordered by? age. roughly

help(step)
step(lmsfull)
# stepwise figures out the best subset of variables to use acording to a certain criteria, like AIC or BIC
step(lmsfull , k=log(50))   	#k = log(n) -> bayes info critirion

#setting up for scatterplot matricies
?? "scatterplot matrix"
install.packages("car")
require (car)
scatterplotMatrix(states)

identify(Life.Exp~Population,labels=row.Names)

require(maps)

plot(state.center$x,state.center$y,type="n")
map('usa')
text(state.center$x,state.center$y, labels = round(lmsfull$residuals,1))



colors =  heat.colors(25)[ (round(lmsfull$residuals,1)*10)^2  ]
plot(state.center$x,state.center$y,type="n")
map('usa')
points(state.center$x,state.center$y, pch=1, cex = 3, col = colors)



write.table(states,"states.txt",sep="\t")
getwd()