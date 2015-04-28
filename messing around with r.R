sd = read.delim(file.choose(),header = TRUE,sep=",")

#
#
#***2011 RESTATE MODEL***
#	reg l_settle l_damages l_filings_ddl l_assets auditor under restate derv_de derv_other sec public_pension non_common_stock /// 
#	criminal sec_11_10b non_major_exch year2004_later if case_id<999999
#
#

#identify the training and test sets of the data

train = sd[sd$train == 0,]
test = sd[sd$train == 1,]

y = subset(train,select=c(l_settle))
y=as.matrix(y)
x= subset(train, select = c(l_damages,l_filings_ddl,l_assets,auditor,under,restate,derv_de,derv_other,sec,public_pension,non_common_stock,criminal,sec_11_10b,non_major_exch,year2004_later))
x=as.matrix(x)


regression_model = lm(y~x)