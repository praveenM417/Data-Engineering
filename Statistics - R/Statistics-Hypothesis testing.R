#Student Name: Praveen Mohanprasad
#Student Number: D18128998
#Program number: TU059
#R packages needed: semTools, ggplot2, pastecs, psych, gmodels, stats, dplyr, car, ggpubr, userfriendlyscience
#R version: 3.6.1

#Importing Data for Maths Course
d1=read.table("student-mat.csv",sep=";",header=TRUE)

#Importing Data for Portugese Course
d2=read.table("student-por.csv",sep=";",header=TRUE)
###############################################HYPOTHESIS 1###################################################
#Merging both Datasets
my_datadf=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

#Calculate average marks of mathas and portugese.
library(dplyr)
my_datadf <- my_datadf %>% rowwise() %>% mutate(G3=mean(c(G3.x, G3.y), na.rm=T))

##### Analysis of the variables of interest for Hypothesis 1 #####
#1.G3
#check for the normality of average score of maths and portugese
pastecs::stat.desc(my_datadf$G3, basic=F)
tpskew<-semTools::skew(my_datadf$G3)
tpkurt<-semTools::kurtosis(my_datadf$G3)
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]
#Creating standardised scores and sorting them to avoid the skewness.
sort(scale(my_datadf$G3))
library(ggplot2)
#Create histogram
gg <- ggplot(my_datadf, aes(sort(scale(my_datadf$G3))))
gg <- gg + labs(x="Average Score")
gg <- gg + labs(y="Students")
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(sort(scale(my_datadf$G3)), na.rm=TRUE), sd=sd(sort(scale(my_datadf$G3)), na.rm=TRUE)))
gg
#Create a qqplot
qqnorm(my_datadf$G3)
qqline(my_datadf$G3, col=2) #show a line on theplot

#2.SEX
#Filter for males
guys <- filter(my_datadf,sex == "M")
#Histogram plot for guys wrt their marks against frequencies
hist(guys$G3, col="blue", xlab="Students (Male)", main="Male - Marks")
#Filter for females
girls <- filter(my_datadf,sex == "F")
#Histogram plot for guys wrt their marks against frequencies
hist(girls$G3, col="blue", xlab="Students (Female)",main="Female - Marks")
#Histogram plot for guys and girls against their strength.
library(ggpubr)
df <- my_datadf %>%
  group_by(sex) %>%
  summarise(counts = n())
df
ggplot(my_datadf, aes(sex)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
##### Hypothesis1 Test #####
library(psych)
#Descriptive Statistics
describeBy(my_datadf$G3,group=my_datadf$sex)
#install.packages("car")
library(car)
# Levene's test for homogeneity of variance.
leveneTest(G3~sex, data = my_datadf)
#T test for mean comparison
t.test(my_datadf$G3~my_datadf$sex) 
#Means are not equal for male and femal groups
#Result from the homogeneity test assuming variances are equal
t.test(my_datadf$G3~my_datadf$sex, var.equal=TRUE) 
#Effect size
etaSquared<-((-2.602*-2.602))/((-2.602*-2.602)+(184+198-2))


###############################################HYPOTHESIS 2###################################################
#Merging both Datasets
my_datadf=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

#Calculate average marks of mathas and portugese.
library(dplyr)
my_datadf <- my_datadf %>% rowwise() %>% mutate(G3=mean(c(G3.x, G3.y), na.rm=T))

##### Analysis of the variables of interest for Hypothesis 2 #####
#1.G3
#check for the normality of average score of maths and portugese
pastecs::stat.desc(my_datadf$G3, basic=F)
tpskew<-semTools::skew(my_datadf$G3)
tpkurt<-semTools::kurtosis(my_datadf$G3)
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]
#Creating standardised scores and sorting them to avoid the skewness.
sort(scale(my_datadf$G3))
library(ggplot2)
#Create histogram
gg <- ggplot(my_datadf, aes(sort(scale(my_datadf$G3))))
gg <- gg + labs(x="Average Score")
gg <- gg + labs(y="Students")
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(sort(scale(my_datadf$G3)), na.rm=TRUE), sd=sd(sort(scale(my_datadf$G3)), na.rm=TRUE)))
gg
#Create a qqplot
qqnorm(my_datadf$G3)
qqline(my_datadf$G3, col=2) #show a line on theplot

#2.AGE
library(ggpubr)
df <- my_datadf %>%
  group_by(age) %>%
  summarise(counts = n())
df
ggplot(my_datadf, aes(age)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

##### Hypothesis2 Test #####
#Plot a scatter to preview the corelation
plot(my_datadf$age,my_datadf$G3)

#Simple scatterplot of positive affect and perceived stress
#aes(x,y)
scatter <- ggplot(my_datadf, aes(my_datadf$age, my_datadf$G3))
scatter + geom_point() + labs(x = "age", y = "Marks") 

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Age", y = "Marks") 


#Pearson method to calculate the cor and p value
stats::cor.test(my_datadf$G3, my_datadf$age, method='pearson')

###############################################HYPOTHESIS 3###################################################

#Import Libraries
library(dplyr)
library(ggplot2)
library(ggpubr)

##### Analysis of the variables of interest for Hypothesis 3 #####
#1.Address
#Visualise the counts of address levels in histogram
df <- my_datadf %>%
  group_by(address) %>%
  summarise(counts = n())
df
ggplot(my_datadf, aes(address)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
#2.Internet
#Visualise the counts of Internet levels in histogram
df <- my_datadf %>%
  group_by(internet) %>%
  summarise(counts = n())
df
ggplot(my_datadf, aes(internet)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

##### Hypothesis3 Test #####
#import the library gmodels
library(gmodels)

#Use the Crosstable function
#CrossTable(predictor, outcome, fisher = TRUE, chisq = TRUE, expected = TRUE)

gmodels::CrossTable(my_datadf$address, my_datadf$internet, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#Create your contingency table
mytable<-xtabs(~internet+address, data=my_datadf)
mytable

ctest<-chisq.test(mytable, correct=TRUE)#chi square test
#correct=TRUE to get Yates correction needed for 2x2 table

ctest#will give you the details of the test statistic and p-value
ctest$expected#expected frequencies
ctest$observed#observed frequencies
ctest$p.value

#Effect
PHI <- sqrt(15.266/382)
PHI


###############################################HYPOTHESIS 4###################################################


##### Analysis of the variables of interest for Hypothesis 4 #####
#1.G3
#check for the normality of average score of maths and portugese
pastecs::stat.desc(comb_dset$G3, basic=F)
tpskew<-semTools::skew(comb_dset$G3)
tpkurt<-semTools::kurtosis(comb_dset$G3)
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]
#Data is slightly skewed hence we need to normalise it
#Create standardised scores and sort
sort(scale(comb_dset$G3))
#install.packages("ggplot2")
library(ggplot2)
#Create histogram
gg <- ggplot(comb_dset, aes(x=sort(scale(comb_dset$G3))))
gg <- gg + labs(x="Average standardised G3 Score")
gg <- gg + labs(y="Students")
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(sort(scale(comb_dset$G3)), na.rm=TRUE), sd=sd(sort(scale(comb_dset$G3)), na.rm=TRUE)))
gg

#Create a qqplot
qqnorm(comb_dset$G3)
qqline(comb_dset$G3, col=2) #show a line on theplot

#2.Dalc (Work day alcohol consumption)
#calculate average alcohol consumption for work days
comb_dset$dalc_avg <- (comb_dset$Dalc.x + comb_dset$Dalc.y)/2
comb_dset$Dalc <- ifelse(comb_dset$dalc_avg<1.5,1,ifelse(comb_dset$dalc_avg<2.5,2,
                                                         ifelse(comb_dset$dalc_avg<3.5,3,
                                                                ifelse(comb_dset$dalc_avg<4.5,4,5))))
#Renaming Levels
comb_dset$Dalc <- as.factor(comb_dset$Dalc)
levels(comb_dset$Dalc) <- c("very low","low","average","high","very high")

#round off the means
meansRoundedForG3<- round(tapply(comb_dset$G3, comb_dset$Dalc, mean), digits=2) 

#Histogram for Dalc against frequencies
df <- comb_dset %>%
  group_by(Dalc) %>%
  summarise(counts = n())
df
ggplot(comb_dset, aes(Dalc)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

##### Hypothesis4 Test #####
#Preview differences using DescribeBy
library(psych)
describeBy(comb_dset$G3,group=comb_dset$Dalc)

#bartlett.test
stats::bartlett.test(comb_dset$G3, comb_dset$Dalc)

#Mean differences plot
plot(meansRoundedForG3)

#Mean differences boxplot
plotmeans(comb_dset$G3~comb_dset$Dalc, digits=2,ccol="red", mean.labels=T, main="Plot of weekday alochol consumption by Grade 3")

boxplot(comb_dset$G3~comb_dset$Dalc, main="Plot of weekday alcohol consumtion by Grade 3 (mean is black dot)", 
        xlab="weekday alcohol consumption", ylab="Grade 3", col=rainbow(7))

points(means2, col="black", pch=18)

#Anova Test
ano_study_alc<- aov(comb_dset$G3~comb_dset$Dalc)
summary(ano_study_alc)

#Post-Hoc Test
TukeyHSD(ano_study_alc)

library(userfriendlyscience) 

#run a one-way anova test using the correct post-hoc test Tukey in our case to know Effect size
one.way <- oneway(comb_dset$Dalc, y = comb_dset$G3, posthoc = 'Tukey') 
one.way

###############################################HYPOTHESIS 5###################################################
#merge Datasets
comb_dset=merge(maths_dset,port_dset,by=c("school","sex","age","address",
                                          "famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
#Calculate average of maths and portugese marks as g3
library(dplyr)
comb_dset <- comb_dset %>% rowwise() %>% mutate(G3=mean(c(G3.x, G3.y), na.rm=T))

#calculate average family relationship

comb_dset$famrel_avg <- (comb_dset$famrel.x + comb_dset$famrel.y)/2
comb_dset$famrel <- ifelse(comb_dset$famrel_avg<1.5,1,ifelse(comb_dset$famrel_avg<2.5,2,
                                                             ifelse(comb_dset$famrel_avg<3.5,3,
                                                                    ifelse(comb_dset$famrel_avg<4.5,4,5))))
#Renaming Levels
comb_dset$famrel <- as.factor(comb_dset$famrel)
levels(comb_dset$famrel) <- c("very bad","bad","average","good","excellent")

#round off the means
meansRounded<- round(tapply(comb_dset$G3, comb_dset$famrel, mean), digits=2)
meansRounded

#Import Libraries
library(dplyr)
library(ggplot2)
library(ggpubr)

##### Analysis of the variables of interest for Hypothesis 5 #####
#1.Family Relationship
#plot histogram on family relationship levels and their frequencies
df <- comb_dset %>%
  group_by(famrel) %>%
  summarise(counts = n())
df
ggplot(comb_dset, aes(famrel)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

#Create a qqplot
qqnorm(comb_dset$G3)
qqline(comb_dset$G3, col=2) #show a line on theplot

##### Hypothesis5 Test #####
#Preview differences using DescribeBy
library(psych)
describeBy(comb_dset$G3,group=comb_dset$famrel)
plot(meansRounded)

#bartlett.test for homogeneity of variances
stats::bartlett.test(comb_dset$G3, comb_dset$famrel)
#Mean differences boxplot
library(gplots)
plotmeans(comb_dset$G3~comb_dset$famrel, digits=2,ccol="red", mean.labels=T, main="Plot of family relationship by Grade 3")
boxplot(comb_dset$G3~comb_dset$famrel, main="Plot of family relationship by Grade 3 (mean is black dot)",
        xlab="family relationship", ylab="Grade 3", col=rainbow(7))
points(means3, col="black", pch=18)

#Anova Test
ano_study_famrel<- aov(comb_dset$G3~comb_dset$famrel)
summary(ano_study_famrel)

#Post-Hoc Test
TukeyHSD(ano_study_famrel)

#install.packages("userfriendlyscience")
library(userfriendlyscience)

#run a one-way anova test using the correct post-hoc test Tukey in our case to know Effect size
one.way <- oneway(comb_dset$famrel, y = comb_dset$G3, posthoc = 'Tukey')
one.way


















