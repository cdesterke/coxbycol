#####script

load(file="cancer.rda")
ls()

cancer%>%select(3:7)->data
df<-coxbycol(cancer$OS.TIME ,cancer$OS.STATUS ,data)
plotnlpbeta(df,5,"",16)

plotnlpbeta<-function(df,nb=10,title="Univariate Cox analysis",size=18){

		## load necessary packages
		  	if(!require(ggplot2)){
    	install.packages("ggplot2")
    	library(ggplot2)}
		df<-head(df,n=nb)
		
		
	
		## perform the barplot
		p=ggplot(data=df,aes(x=reorder(identifiers,NLP),y=NLP,fill=prognosis))+geom_bar(stat="identity")+
			ylim(0,max(df$NLP)+(max(df$NLP)/5))+
			coord_flip()+
			theme_minimal()+
			xlab("Covariates")+
			ylab("-log10 p-values Cox / (beta coef.)")+
			scale_fill_manual(values=c("lightskyblue1","plum2","grey"))+
			geom_hline(yintercept= -log(0.05,10), linetype="dashed", color = "red")+
			geom_text(aes(label=round(coef.beta,5)),hjust=0, vjust=0.5,color="navy",position= position_dodge(0),size=5,angle=0)+
			ggtitle(title) +theme(text = element_text(size = size))+theme(legend.position="bottom")

		return(p)
}
