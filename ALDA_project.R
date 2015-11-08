library(gdata)
help("read.xls")
Movie_Titles<-read.xls("C:/Users/Kartikeya/Desktop/ALDA Project/movie_titles.xls",header= F,perl = "C:/Perl64/bin/perl.exe")

name(Movie_Titles)<- c("Movie_ID","Release_Year","Movie_Title")

setwd("C:/Users/Kartikeya/Desktop/ALDA Project/Dataset/training_set/")
files_list<- list.files()

i<-1
  
for(file in files_list)
{
  dataset<-read.table(text=readLines(file)[-1],header=FALSE,sep=",")
  ratings<-dataset[,c(2)]
  average<-mean(ratings)
  standard_deviation<-sd(ratings)
  number<-length(ratings)
  
    if(!exists("Movie_Summary"))
    {
      Movie_Summary<-data.frame(Movie_ID=i,Average_Movie_Rating=average,Standard_Deviation=standard_deviation,Number_of_Ratings=number)
    }
  
    else if(exists("Movie_Summary"))
    {
      
      Movie_Summary_temp<-data.frame(Movie_ID=i,Average_Movie_Rating=average,Standard_Deviation=standard_deviation,Number_of_Ratings=number)
      Movie_Summary<-rbind(Movie_Summary,Movie_Summary_temp)
      rm(Movie_Summary_temp)
    }
    i<-i+1
}
