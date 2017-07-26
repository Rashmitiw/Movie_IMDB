#IMDB Movie set
# set working directory

setwd("E:/R set/coursera")
# load required library
library(ggplot2)
library(dplyr)
library(reshape2)
#load the data
movie<-read.csv("movie_metadata.csv",stringsAsFactors = FALSE,header=TRUE,na.strings="")
head(movie)

# Data cleaning
# can remove unnecessary column
#rename col name

names(movie)

# Director of top 10 grossing movies

# subset of movie of dir_name ,movie_title , gross

gross10<-unique(subset(movie,rank(desc(gross))<=11,is.nan=TRUE,c(director_name,actor_1_name,movie_title,gross)))
head(gross10)
gross10=arrange(gross10,desc(gross))
 head(gross10)
 
#plot bar graph
 
g<-qplot(director_name,gross,data=gross10,fill=movie_title)+geom_bar(stat="identity",colour = "black") 
g<-g+geom_hline(yintercept = 0) + ggtitle("Top grossing director")
g<-g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
g<-g +xlab(" Director_name ")+ylab(" Gross Movie Earning / $")
g<-g+theme(plot.title = element_text(size = 30, face = "bold" ,hjust=0.5))+scale_fill_hue(name="Movie-Title")
g


# Top actor /actress 
g<-qplot(actor_1_name,gross,data=gross10,fill=movie_title)+geom_bar(stat="identity",colour = "black") 
g<-g+geom_hline(yintercept = 0) + ggtitle("Top grossing Actor")
g<-g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
g<-g +xlab(" Actor_name ")+ylab(" Gross Movie Earning / $")
g<-g+theme(plot.title = element_text(size = 30, face = "bold" ,hjust=0.5))+scale_fill_hue(name="Movie-Title")
g

#Maximum movie made by director
r=movie%>%group_by(director_name)%>%summarise(count=n())%>%arrange(desc(count))
r=na.omit(r)
r=r[1:20,]
 
g<-qplot(director_name,count,data=r,fill=director_name)+geom_bar(stat="identity",colour = "black") 
g<-g+geom_hline(yintercept = 0) + ggtitle("Director with Maximum No of Movies")
g<-g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
g<-g +xlab(" Director-name ")+ylab(" Count- no of movies ")
g<-g+theme(plot.title = element_text(size = 30, face = "bold" ,hjust=0.5))+scale_fill_hue(name="Director-Name")
g


#change in gross earning over years

#Analyse the distribution of gross earnings. 
#Min, max and mean for all films between specified period. 

#distribution of gross movie earnings.

ggplot(movie, aes(x = gross)) + geom_histogram(colour = "black")

#There is a strong positive skew to these data. 
#The mean is less reliable than the median as a measure of 
#expected gross earnings ($47 644 515 vs $25 043 962).
summary(movie$gross)

#We need to plot mean, max and min gross earnings across the specified time. 
#We can accomplish this using a line plot. 
#The data are grouped by year with summary statistics added.

temp<- movie%>%group_by(title_year)%>% summarize(gmean=mean(gross,na.rm=TRUE),gmax=max(gross,na.rm=TRUE),gmin=min(gross,na.rm=TRUE))
head(temp)
#remove NA
temp<-na.omit(temp)
head(temp)

# melt the dataframe and convert title year as id variable and gmin gmax and gmean as measure variable

temp <-melt(temp,id="title_year")

ggplot(data=temp,aes(x=title_year ,y=value ,colour =variable))+geom_line(size=1)+
ylab("Gross Earnings") +
  scale_colour_manual(values = c("blue", "red", "yellow", "green")) +
  xlab("") +
  guides(colour = guide_legend(NULL)) +
  ggtitle("Summary Statistics for Gross Earnings") +
  theme(plot.title = element_text(size = 15, face = "bold",hjust=0.5))

##To just select a range within the total time period, the user can specify a start and end year.

yearRange <- function() {
  
  start <- readline("Enter a start year: ")  
  end <- readline("Enter an end year:")
  
  start <- as.numeric(unlist(strsplit(start, ",")))
  end <- as.numeric(unlist(strsplit(end, ",")))
  
  ggplot(data = temp, aes(x = title_year, y = value, colour = variable)) + 
    geom_line(size = 1, alpha = 0.5) +
    scale_colour_manual(values = c("blue", "red", "yellow", "green")) +
    ylab("Gross movie earnings / $") +
    xlab("") +
    coord_cartesian(xlim = c(start, end)) +
    guides(colour = guide_legend(NULL)) +
    ggtitle("Summary Statistics for Gross Earnings") +
    theme(plot.title = element_text(size = 15, face = "bold"))
  
}

#IMDB Score and Gross Earnings Relation
#category subset

#r1<- movie%>%filter(imdb_score<1)%>%select(gross)%>%summarise(gmean=mean(gross,na.rm=TRUE),gsd=sd(gross,na.rm=TRUE))%>%mutate(labelx ="0 to <1")
#r2<- movie%>%filter(imdb_score>=1 & imdb_score<2)%>%select(gross)%>%summarise(gmean=mean(gross,na.rm=TRUE),gsd=sd(gross,na.rm=TRUE))%>%mutate(labelx ="1 to <2")
#r3<- movie%>%filter(imdb_score>=2 & imdb_score<3)%>%select(gross)%>%summarise(gmean=mean(gross,na.rm=TRUE),gsd=sd(gross,na.rm=TRUE))%>%mutate(labelx ="2 to <3")
#r4<- movie%>%filter(imdb_score>=3 & imdb_score<4)%>%select(gross)%>%summarise(gmean=mean(gross,na.rm=TRUE),gsd=sd(gross,na.rm=TRUE))%>%mutate(labelx ="3 to <4")
#r5<- movie%>%filter(imdb_score>=4 & imdb_score<5)%>%select(gross)%>%summarise(gmean=mean(gross,na.rm=TRUE),gsd=sd(gross,na.rm=TRUE))%>%mutate(labelx ="4 to <5")
#r6<- movie%>%filter(imdb_score>=5 & imdb_score<6)%>%select(gross)%>%summarise(gmean=mean(gross,na.rm=TRUE),gsd=sd(gross,na.rm=TRUE))%>%mutate(labelx ="5 to <6")
#r7<- movie%>%filter(imdb_score>=6 & imdb_score<7)%>%select(gross)%>%summarise(gmean=mean(gross,na.rm=TRUE),gsd=sd(gross,na.rm=TRUE))%>%mutate(labelx ="6 to <7")
#r8<- movie%>%filter(imdb_score>=7 & imdb_score<8)%>%select(gross)%>%summarise(gmean=mean(gross,na.rm=TRUE),gsd=sd(gross,na.rm=TRUE))%>%mutate(labelx ="7 to <8")
#r9<- movie%>%filter(imdb_score>=8 & imdb_score<9)%>%select(gross)%>%summarise(gmean=mean(gross,na.rm=TRUE),gsd=sd(gross,na.rm=TRUE))%>%mutate(labelx ="8 to <9")
#r10<- movie%>%filter(imdb_score>=9 & imdb_score<10)%>%select(gross)%>%summarise(gmean=mean(gross,na.rm=TRUE),gsd=sd(gross,na.rm=TRUE))%>%mutate(labelx ="9 to <10")


r1<- movie%>%filter(imdb_score<1)%>%select(gross)
r2<- movie%>%filter(imdb_score>=1 & imdb_score<2)%>%select(gross)
r3<- movie%>%filter(imdb_score>=2 & imdb_score<3)%>%select(gross)
r4<- movie%>%filter(imdb_score>=3 & imdb_score<4)%>%select(gross)
r5<- movie%>%filter(imdb_score>=4 & imdb_score<5)%>%select(gross)
r6<- movie%>%filter(imdb_score>=5 & imdb_score<6)%>%select(gross)
r7<- movie%>%filter(imdb_score>=6 & imdb_score<7)%>%select(gross)
r8<- movie%>%filter(imdb_score>=7 & imdb_score<8)%>%select(gross)
r9<- movie%>%filter(imdb_score>=8 & imdb_score<9)%>%select(gross)
r10<- movie%>%filter(imdb_score>=9 )%>%select(gross)

gross_list <- list(r1$gross, r2$gross, r3$gross, r4$gross, r5$gross, r6$gross, r7$gross, r8$gross, r9$gross, r10$gross)
str(gross_list)

gross_mean<-sapply(gross_list ,mean,na.rm=T)
gross_sds<-sapply(gross_list,sd,na.rm=T)
gross_mean[1]<-0
gross_sds[1]<-0

#create labels interval

labels <- c("0 to < 1", "1 to < 2", "2 to < 3", "3 to < 4", "4 to < 5", "5 to < 6", "6 to < 7", "7 to < 8", "8 to < 9", "9 to 10")

#create data frame

score_gross<-data.frame(labels,gross_mean,gross_sds)


#plot graph
g<-qplot(labels,gross_mean,data=score_gross)+geom_bar(stat="identity",colour = "black",fill="light blue") 
g<-g+geom_hline(yintercept = 0) + ggtitle("Average gross earning vs IMDB score")
g<-g +xlab(" IMDB Score ")+ylab(" Average Gross Earning / $")
g<-g+theme(plot.title = element_text(size = 30, face = "bold" ,hjust=0.5))
g

