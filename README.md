# coxbycol
loop cox on columns of a dataframe


### compute analyses
```r
##load functions from R files and data
source("script.R")
source("plotbeta.R")
source("plotnlpr.R")

load(file="cancer.rda")
ls()

cancer%>%select(3:7)->data
df<-coxbycol(cancer$OS.TIME ,cancer$OS.STATUS ,data)
head(df)
```


![res](https://github.com/cdesterke/coxbycol/blob/main/results.png)

### graph output
```r
library(patchwork)
p1<-plotbeta(df,nb=5,title="",size=16)
p2<-plotnlphr(df,nb=5,title="",size=16)
p1+p2
```
![plot](https://github.com/cdesterke/coxbycol/blob/main/patchwork.png)
