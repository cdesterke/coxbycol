# coxbycol
loop cox on columns of a dataframe


### compute analyses
```r
##load functions from R files and data
load(file="cancer.rda")
ls()

cancer%>%select(3:7)->data
df<-coxbycol(cancer$OS.TIME ,cancer$OS.STATUS ,data)
head(df)
```


![res](https://github.com/cdesterke/coxbycol/blob/main/results.png)
