


#### A TWO-STEP APPROACH FOR CLUSTERING A MIX OF NUMERICAL AND CATEGORICAL DATA #####

#### STEPS 1 - 11 as narrated in the document



library(sqldf)

d2=read.table("cmc.csv",sep=",")

# Step 1 - Numerical attributes scaling
numericVec <- c('V1','V4')
categoryVec <- c('V2','V3','V5','V6','V7','V8','V9','V10')
for(i in 1:length(categoryVec))
{d3[[categoryVec[i]]] <- as.factor(d3[[categoryVec[i]]])}
minVar<-10000
length(numericVec)
d2$V1<-as.numeric(d2$V1)
d2$V4<-as.numeric(d2$V4)
minV1 <-min(d2$V1)
maxV1 <-max(d2$V1)
minV4 <-min(d2$V4)
maxV4 <-max(d2$V4)
normalize <- function(x)
{ 
  return((x- minV4) /(maxV4-minV4))
}
normalize_V1 <- function(x)
{ 
  return((x- minV1) /(maxV1-minV1))
}
d2$V1<-sapply(d2$V1, normalize_V1)
d2$V4<-sapply(d2$V4, normalize)

#Step 2 - Find the base Categorical attribute (V2)

str(d2)
levels(d2$V2)


# Step 3 - Find the numerical variable with lowest variance for the base attribute (V4)
for(i in 1:length(numericVec))
{ 
  #d2[,numericVec[i]] <- d2[,numericVec[i]].filter()
  #d2_filter <- filter(d2,camera=='North'&week=='Week_1'&day==1&type=='In')
  
  #d2[,numericVec[i]]<-scale(d2[,numericVec[i]],na.rm=TRUE)
  print(numericVec[i])
  tempSQL <- paste('select V2,stdev(',numericVec[i],') as variance from d2 group by V2',sep='')
  print(tempSQL)
  minVar2 <- as.numeric(sqldf(paste('select sum(variance) from (',tempSQL,')',sep='')))
  print(minVar2)
  if(minVar2<minVar)
  {minVar<-minVar2
  minVec<-numericVec[i]}
}
print(minVec) #V4



# Counting classes on the base attribute
baseFreq<-sqldf("select V2,count(*) as cnt from d2 group by V2")
#View(baseFreq)
baseFreq1<-sqldf("select V3,V2,count(*) as cnt from d2 group by V3,V2")

# Step 4 - Base attribute transformation into numercal one
V4_change<-sqldf("select V2,avg(V4) as V4_cng from d2 group by V2")
#View(V4_change)

# Base attributes Transfromation needed
baseNumberVec <- vector()
baseVector <- c('V2','V3','V5','V6','V7','V8','V9','V10')
BaseCat<-'V2'
basecatCount <- sqldf("select V2,count(*) as cnt from d2 group by V2")
comboList <- vector()
comboDF <- list()
indvDF <- list()
sql<-''

# Step 5 - Co-occurrence derivation and conversion of non base categorical into numerical
for(i in 1:length(baseVector))
{
  sql0 <- paste("select", baseVector[i],",count(*) as cnt from d2 group by",baseVector[i],sep = " ")
  indvDF[[baseVector[i]]] <- sqldf(sql0)
  comboList[i] <- paste(baseVector[i],BaseCat,sep="-")
  sql<-paste("select", baseVector[i],",V2, count(*) as cnt from d2 group by",baseVector[i],",V2",sep = " ")
  ##print(sql)
  
  comboDF[[paste(baseVector[i],BaseCat,sep="-")]]<-sqldf(sql)
}

newDF <- data.frame()   

for(i in 1:length(baseVector))
{#print(baseVector[i])
  #  ##print(-1)
  mylist <- list()
  
  newVec<-vector()
  mainTable <- baseVector[i]
  for(j in 1: nrow(d2))
  {
    temp<-as.data.frame(comboDF[[i]])
    filter1<-toString(d2[[baseVector[i]]][j])
    #print(filter1)
    if(filter1 %in% names(mylist))
      perEle <- mylist[[filter1]]
    else
    {
      #print('else')
      sql_temp_bs <- paste('select * from temp where ',mainTable,' = \'',filter1,'\'',sep = "")
      #print(sql_temp_bs)
      df1 <- sqldf(sql_temp_bs)
      #print(2)
      perEle<-0
      #print(1)
      for(k in 1:nrow(df1))
      {  
        v1<-vector() 
        v2<-vector()
        filter2<-df1$V2[k]
        sql_temp <- paste('select cnt from temp where ',mainTable,' = \'',filter1,'\' and V2 = \'',filter2,'\'',sep = "")
        #print(sql_temp)
        val1<-sqldf(sql_temp)
        #print(val1)
        
        temp2<-as.data.frame(indvDF[[i]])
        sql_temp2 <- paste('select cnt from temp2 where ',mainTable,' = \'',filter1,'\'',sep="")
        #print(sql_temp2)
        val2<-sqldf(sql_temp2)
        #print('val2')
        #print(val2)
        sql_temp3 <- paste('select cnt from baseFreq where V2 = \'',filter2,'\'',sep="")
        val3<-sqldf(sql_temp3)
        #print('val3')
        #print(val3)
        co_occ<-abs(val1)/(abs(val2)+abs(val3)-abs(val1))
        #print('co_occ')
        #print(co_occ)
        sql_temp4 <- paste('select V4_cng from V4_change where V2 = \'',filter2,'\'',sep="")
        bs_num<-sqldf(sql_temp4)
        #print('bs_num')
        #print(bs_num)
        perEleMul<-co_occ*bs_num
        #print(perEleMul)
        perEle<-perEle+perEleMul
        #print(perEle)
        
      }
      #print('filter')
      #print(filter1)
      str(filter1)
      mylist[[toString(filter1)]] <- perEle
      #print(mylist)
    }
    newVec[length(newVec)+1]<-perEle
  }
  d2[,paste(baseVector[i],'new',sep='_')]<-as.vector(unlist(newVec))
  #d2<-cbind(d2, newVec)
}
newDF<-sqldf('select b.V4_cng as v2_num from d2 a join V4_change b on a.V2=b.V2')
d2$V2_new <- newDF$v2_num

keeps <- c("V1", "V2_new","V3_new","V4","V5_new","V6_new","V7_new","V8_new","V9_new","V10_new")
alterDF<-(d2[keeps])
str(alterDF)
alterDF$V2<-as.numeric(as.character(alterDF$V2))

# Step 6 - Verifying if our dataframe consists of only numerical attributes
final <- alterDF[complete.cases(alterDF), ]
final$V2<-as.numeric(as.character(final$V2))



# Step 7 - hierarchical clustering and plots
#newDF<-sqldf('select b.V8_cng as v6_num from d2 a join V8_change b on a.V6=b.V6')
dist_mat <- dist(final, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
cut_avg <- cutree(hclust_avg, k = 491)
#install.packages('dendextend', dependencies = TRUE)
rect.hclust(hclust_avg , k = 491, border = 2:6)
abline(h = 3, col = 'red')
library(dendextend)
library(dplyr)
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)
final_new<-final
final_cl <- mutate(final_new, cluster = cut_avg)
final_cl$cluster <- as.factor(final_cl$cluster)
levels(final_cl$cluster)

# Step 8 - Centroids Calculation -- can be tweaked as mode for categorical variables
centroids <-sqldf('select cluster,avg(V1) as v1,avg(V2_new) as v2,avg(V3_new) as v3,avg(V4) as v4,avg(V5_new) as v5,avg(V6_new) as v6,avg(V7_new) as v7,avg(V8_new) as v8,avg(V9_new) as v9,avg(V10_new) as v10 from final_cl group by cluster')
#test<-sqldf('select cluster,V1_new,V4_new,1 as value from final_cl')
library(reshape)


# Step 9 - Additional Attributes added to centroids
new_data <- sqldf('select *,1 as value from final_cl')
centroids_sort<-sqldf('select * from centroids order by cluster')
v2 <- cast(new_data, cluster~V2_new, sum)
v2_sort <-('select * from v2 order by cluster')
v2_centroids <-sqldf('select a.*,b.* from centroids_sort a join v2 b on a.cluster=b.cluster')
View(v2_centroids) 
v2_centroids <- v2_centroids[,-12]


v3 <- cast(new_data, cluster~V3_new, sum)

v2_v3_centroids <-sqldf('select a.*,b.* from v2_centroids a join v3 b on a.cluster=b.cluster')
colnames(v2_v3_centroids) 
v2_v3_centroids <- v2_v3_centroids[,-16]


v5 <- cast(new_data, cluster~V5_new, sum)

v2_v3_v5centroids <-sqldf('select a.*,b.* from v2_v3_centroids a join v5 b on a.cluster=b.cluster')
colnames(v2_v3_v5centroids) 
v2_v3_v5centroids <- v2_v3_v5centroids[,-20]


v6 <- cast(new_data, cluster~V6_new, sum)

v2_v3_v5_v6centroids <-sqldf('select a.*,b.* from v2_v3_v5centroids a join v6 b on a.cluster=b.cluster')
colnames(v2_v3_v5_v6centroids) 
v2_v3_v5_v6centroids <- v2_v3_v5_v6centroids[,-22]

v7 <- cast(new_data, cluster~V7_new, sum)


v2_v2_v5_v6_v7centroids <-sqldf('select a.*,b.* from v2_v3_v5_v6centroids a join v7 b on a.cluster=b.cluster')
colnames(v2_v2_v5_v6_v7centroids) 
v2_v2_v5_v6_v7centroids <- v2_v2_v5_v6_v7centroids[,-24]
View(v7)

v8 <- cast(new_data, cluster~V8_new, sum)


v2_v2_v5_v6_v7_v8centroids <-sqldf('select a.*,b.* from v2_v2_v5_v6_v7centroids a join v8 b on a.cluster=b.cluster')
colnames(v2_v2_v5_v6_v7_v8centroids) 
v2_v2_v5_v6_v7_v8centroids <- v2_v2_v5_v6_v7_v8centroids[,-28]

v9 <- cast(new_data, cluster~V9_new, sum)

View(v9)

v2_v2_v5_v6_v7_v8_v9centroids <-sqldf('select a.*,b.* from v2_v2_v5_v6_v7_v8centroids a join v9 b on a.cluster=b.cluster')

colnames(v2_v2_v5_v6_v7_v8_v9centroids)
v2_v2_v5_v6_v7_v8_v9centroids <- v2_v2_v5_v6_v7_v8_v9centroids[,-32]


v10 <- cast(new_data, cluster~V10_new, sum)


v2_v2_v5_v6_v7_v8_v9_v10centroids <-sqldf('select a.*,b.* from v2_v2_v5_v6_v7_v8_v9centroids a join v10 b on a.cluster=b.cluster')
View(v1_v4_v5_v6_v7_v9_v10centroids) 
colnames(v2_v2_v5_v6_v7_v8_v9_v10centroids)
v2_v2_v5_v6_v7_v8_v9_v10centroids <- v2_v2_v5_v6_v7_v8_v9_v10centroids[,-34]



cluster_hier_clustering <- v2_v2_v5_v6_v7_v8_v9_v10centroids
v2_v2_v5_v6_v7_v8_v9_v10centroids <- v2_v2_v5_v6_v7_v8_v9_v10centroids[,-1] 

# Step 10 - K-means clustering
nclusters <- 8   ##### give alternatives as 2,3 and 4

km_res <- kmeans(v2_v2_v5_v6_v7_v8_v9_v10centroids, nclusters,nstart=1)
cluster_kmeans_centroid <- cbind(v2_v2_v5_v6_v7_v8_v9_v10centroids, cluster_kmeans = km_res$cluster)
cluster_kmeans_centroid <- cbind(cluster_kmeans_centroid, cluster_hier = cluster_hier_clustering$cluster)
Both_final_clusters<-sqldf('select a.*,b.cluster_kmeans as kmeans from final_cl a left join cluster_kmeans_centroid b on a.cluster = b.cluster_hier')
colnames(Both_final_clusters)

# Step 11 - Entropy calculation

s1<-sqldf('select * from Both_final_clusters where kmeans=2')

# change the base vector - based on this class the cluster disorder is calculated

#baseVector <- c('V2_new','V3_new','V5_new','V6_new','V7_new','V9_new','V10_new')
baseVector <- c('V3_new')   # try giving 'V3_new'
entrophy<-0
# library(sqldf)
for(i in 1:nclusters)
{  sql_temp_bs <- paste('select * from Both_final_clusters where kmeans = ',i,sep = "")
  df1 <- sqldf(sql_temp_bs)
  
  perEle<-0
  inter<-0
  for(k in 1:length(baseVector))
  {  
    v1<-vector() 
    v2<-vector()
    class_levels <- levels(as.factor(df1[[baseVector[k]]]))

    classes <- length(class_levels)
    
    c<-0
    for(z in 1:classes)
    {
      sql_temp <- paste('select ',baseVector[k],',count(*) as cnt from df1 where round(',baseVector[k],',4) = round(',class_levels[z],',4) group by ',baseVector[k],sep = "")
      print(sql_temp)
      sql_temp_df <- sqldf(sql_temp)
      val1<-as.integer(sqldf('select cnt from sql_temp_df'))
       
      if(val1!=0)
      {
         c<-c+(((val1/nrow(df1)) * log(val1/nrow(df1))))
        if(z==classes)
        { #c <- -(c)
        inter<- inter+ c}
        
      }
    }
  }
  entrophy = entrophy+( (nrow(df1)/1473)*inter )
}        

print(entrophy)

# Additional Work - Computation of k-prototype clustering

library(clustMixType)
  kpres <- kproto(d3, 4)
#clprofiles(kpres, d3)
#View(sqldf("select * from d3 where kmeans = 3"))

Both_final_clusters_1 <- cbind(d3, cluster_kmeans = km_res$cluster)
#View(Both_final_clusters_1)
baseVector <- c('V3')
entrophy_1<-0
# library(sqldf)
for(i in 1:4)
{
  #print(baseVector[i])
  #  ##print(-1)
  
  sql_temp_bs <- paste('select * from Both_final_clusters_1 where cluster_kmeans = ',i,sep = "")
  df1 <- sqldf(sql_temp_bs)
  #print(2)
  perEle<-0
  #print(1)
  inter<-0
  for(k in 1:length(baseVector))
  {  
    v1<-vector() 
    v2<-vector()
    #filter2<-df1$V6[k]
    class_levels <- levels(as.factor(df1[[baseVector[k]]]))
    
    classes <- length(class_levels)
    
    c<-0
    for(z in 1:classes)
    {
      sql_temp <- paste('select ',baseVector[k],',count(*) as cnt from df1 where ',baseVector[k],' = ',class_levels[z],' group by ',baseVector[k],sep = "")
      sql_temp_df <- sqldf(sql_temp)
      val1<-sqldf('select cnt from sql_temp_df')
      #print(val1)
      
      
      if(val1!=0)
      { 
        c<-c+(((val1/nrow(df1)) * log(val1/nrow(df1))))
        if(z==classes)
        { c <- -(c)
        inter<- inter+ c}
        
      }
    }
  }
  entrophy_1 = entrophy_1+( (nrow(df1)/1473)*inter )
} 

print(entrophy_1)


