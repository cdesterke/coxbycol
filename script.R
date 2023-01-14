
### data preparation
all<-read.table("CRC.txt",h=T,sep="\t")

library(dplyr)

all%>%select(10:20)->data

all%>%select(-(1:10))->cancer
cancer%>%select(OS.TIME,OS.STATUS,age,gender,nodes,tumor,response.category)->cancer
cancer%>%dplyr::rename(response="response.category")->cancer

save(cancer,file="cancer.rda")
rm(list=ls())
######

#####script

load(file="cancer.rda")
ls()


coxbycol(cancer$OS.TIME ,cancer$OS.STATUS ,data)


coxbycol<-function(time,event,data){
suppressWarnings({
	## install survival needed package if necessary
	if(!require(survival)){
    		install.packages("survival")
    		library(survival)}

	## define survival formula
		os<-Surv(time,event)
	
	## initiate vectors before loop
		beta<-rep(NA,ncol(data))
		hr<-rep(NA,ncol(data))
		pvals<-rep(NA,ncol(data))

	## start loop for on rows of data

		for (i in 1:ncol(data)){
			model<-coxph(os~as.vector(data[,i]))
			beta[i]<-summary(model)$coef[1,1]
			hr[i]<-summary(model)$coef[1,2]
			pvals[i]<-summary(model)$coef[1,5]
			}

	## form a dataframe 
		df<-data.frame(identifiers=colnames(data),coef.beta=beta,HR=hr,pvalues=pvals)

	## order p.values
		df<-df[order(df$pvalues,decreasing=F),]

	## compute adjusted pvalues
		df$adjpvals<-p.adjust(df$pvalues,method="fdr")
	
	## retun dataframe
		df
	})
}
##################