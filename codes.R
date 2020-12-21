#iris dataset
data=iris
head(data)

#covariance
cov(data[-5])

#correlation
cor(data[-5])

# MANOVA test
res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man)

# Look to see which differ ( using ANOVA)

summary.aov(res.man)

#principal component
fit=princomp(data[1:4],cor=TRUE)
summary(fit)
#scree plot
plot(fit,type="lines",main="Principal components-Scree plot")
#principal component loading
coeff=loadings(fit)
print(coeff,digits=3,cutoff=0)

data_pcscores=predict(fit,data)
head(data_pcscores)


#kmeans
#standardizing ariables
data=data.frame(scale(data[1:4],center=TRUE,scale=TRUE))
#determine the mumber of clusters
wilks_lambda=numeric(10)
for(i in 1:10)
{
set.seed(i)
fit=kmeans(data,i)
wilks_lambda[i]=sum(fit$withinss)/fit$totss
}
plot(1:10,wilks_lambda,type="b",xlab="Number of clusters"
,ylab="ClustersWSS/TSS")

#kmeans cluster analysis
fit=kmeans(data,3)
data2=data.frame(data[1:4])
aggregate(data2,by=list(fit$cluster),FUN=mean)
#append cluster assignment
data3=data.frame(data,fit$cluster)
data3

#visualizing clusters
library(ggpubr)
library(factoextra)
fviz_cluster(fit, data = data3,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
             )