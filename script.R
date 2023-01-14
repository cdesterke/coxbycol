
### data preparation
all<-read.table("CRC.txt",h=T,sep="\t")

library(dplyr)

all%>%select(10:20)->data

all%>%select(-(1:10))->cancer
cancer%>%select(OS.TIME,OS.STATUS,age,gender,nodes,tumor,response.category)->cancer
cancer%>%dplyr::rename(response="response.category")->cancer

save(cancer,file="cancer.rda")
rm(list=ls())
######END data prepare

#####script

load(file="cancer.rda")
ls()
library(dplyr)
cancer%>%select(3:7)->data
df<-coxbycol(cancer$OS.TIME ,cancer$OS.STATUS ,data)
####END script


###funtion
coxbycol<-function(time,event,data){
suppressWarnings({
	## install survival needed package if necessary
	if(!require(survival)){
    		install.packages("survival")
    		library(survival)}
	if(!require(dplyr)){
    		install.packages("dplyr")
    		library(dplyr)}		

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
		
	
		## compute NLP
		df$NLP= -log(df$pvalues,10)
		## compute significance
		df%>%mutate(significance=ifelse(pvalues<=0.05,"YES","no"))->df
		## compute prognosis
		df%>%mutate(prognosis=case_when(HR==1~"zero.effect",
						HR>1~"unfavorable",
						HR<1~"favorable"))->df
	
	## retun dataframe
	return(df)
	})
}
##################
