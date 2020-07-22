library(ggplot2)
#Read English file into Dataframe
English <- read.delim("English_letter.txt",header = FALSE,sep=";")
#View(English)
#Adding header
colnames(English)<-c('Language','letter','counts')

#calculating Frequency column
English<-
  mutate(English,frequency=round(counts/sum(counts),4))
#Bar chart for english letters
ggplot(data=English, aes(x=letter, y=frequency)) +
  geom_bar(stat="identity", fill="steelblue")+
  xlab("letter - English")+
  geom_text(aes(label=frequency), vjust=-0.3, size=3.5)+
  theme_minimal()

#Read French file into Dataframe
French <- read.delim("French_letter.txt",header = FALSE,sep=";")
#Adding header
colnames(French)<-c('Language','letter','counts')
#calculating Frequency column
French<-
  mutate(French,frequency=round(counts/sum(counts),4))
#Bar chart for French letters
ggplot(data=French, aes(x=letter, y=frequency)) +
  geom_bar(stat="identity", fill="steelblue")+
  xlab("letter - French")+
  geom_text(aes(label=frequency), vjust=-0.3, size=3.5)+
  theme_minimal()

#Read Italian file into Dataframe
Italian <- read.delim("Italian_letter.txt",header = FALSE,sep=";")
#Adding header
colnames(Italian)<-c('Language','letter','counts')
#calculating Frequency column
Italian<-
  mutate(Italian,frequency=round(counts/sum(counts),4))
#Bar chart for Italian letters
ggplot(data=Italian, aes(x=letter, y=frequency)) +
  geom_bar(stat="identity", fill="steelblue")+
  xlab("letter - Italian")+
  geom_text(aes(label=frequency), vjust=-0.3, size=3.5)+
  theme_minimal()

#Merge the above three dataframes to get all the languages in a single dataframe
All_languages <- merge(English,French,all = TRUE)
All_languages <- merge(All_languages,Italian,all=TRUE)

#Stacked Bar chart to compare frequency of letters across all the three languages

ggplot(All_languages, aes(fill=Language, y=frequency, x=letter)) + 
  geom_bar(position="dodge", stat="identity")

#Grouped Bar chart to compare frequency of letters across all the three languages

ggplot(All_languages, aes(fill=Language, y=frequency, x=letter)) + 
  geom_bar(position="dodge", stat="identity")



