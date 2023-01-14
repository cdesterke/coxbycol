
#####script

load(file="cancer.rda")
ls()

cancer%>%select(3:7)->data
df<-coxbycol(cancer$OS.TIME ,cancer$OS.STATUS ,data)


plothr<-function(df){

		## load necessary packages
		  	if(!require(ggplot2)){
    	install.packages("ggplot2")
    	library(ggplot2)}

			if(!require(pals)){
    	install.packages("pals")
    	library(pals)}

		## perform the barplot
		p=ggplot(data=df,aes(x=reorder(identifier,beta.score),y=NLP,fill=family))+geom_bar(stat="identity")+
			ylim(0,max(res$NLP)+(max(res$NLP)/5))+
			coord_flip()+
			theme_minimal()+
			xlab("Combinatorial TF")+
			ylab("Negative log10 of p-values")+
			scale_fill_manual(values=cols25())+
			geom_text(aes(label=round(NLP,2)),hjust=0, vjust=0.5,color="navy",position= position_dodge(0),size=5,angle=0)+
			ggtitle("") +theme(text = element_text(size = 16))+theme(legend.position="none")

		return(p)
}
