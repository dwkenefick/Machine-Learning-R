#trees & ER . Type 1 error, predict, but none. 
#type two, no predict, but it happens
#trees - how to decide: heart attack
#or the titanic: Sex, Ticketclass, Child / adult

#overall suvival rate: 711/2201

#look at male female and ticket class. 

#tables
with(ti,table(Survived,Class))
tab1= with(ti,xtabs(~Survived+(Class=="1"|Class =="2")))
tab1 = t(table)

tab2= with(ti,xtabs(~Survived+(Sex)))
tab2 = t(tab2)

tab3= with(ti,xtabs(~Survived+((Class=="1"|Class =="2") |Class=="3")))
tab3 = t(tab3)

tab4= with(ti,xtabs(~Survived+(Class=="1")))
tab4 = t(tab4)

tab5= with(ti,xtabs(~Survived+(Class=="1"|Class=="C")))
tab5 = t(tab5)

tab6= with(ti,xtabs(~Survived+(Class=="1"|Class=="3")))
tab6 = t(tab6)


#"purity" formulas

entropy = function(table){
	L = -(table[1,1])/sum(table[1,])*log((table[1,1])/sum(table[1,]))-(table[1,2])/sum(table[1,])*log((table[1,2])/sum(table[1,]))
	
	
	
	R =  -(table[2,1])/sum(table[2,])*log((table[2,1])/sum(table[2,]))   -   (table[2,2])/sum(table[2,])*log((table[2,2])/sum(table[2,]))
	
	
	
	total = sum(table)
	pl = sum(table[1,])/total
	pr = sum(table[2,])/total
	list(L,R,L*pl + R*pr)
}

#table , 1st and second class, maximises entropy
#in jmp:
analize - modeling - partition
split