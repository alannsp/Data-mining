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
i<-1
  
for(file in files_list)
{
  dataset<-read.table(text=readLines(file)[-1],header=FALSE,sep=",")
  ratings<-dataset[,c(2)]
    number<-length(ratings)
    user_id<-dataset[,c(1)]
if (i=1)
user_id_summary<-c()
initia<-data.frame(movie_id=double(),rating=integer(),data_rating=double())
user_summary<-data.frame(user_id=double(),user_info=initia)
for (j in 1:number)
  if(is.element(user_id[j],user_id_summary))
  {
  temp1<-dataframe(movie_id=i,rating=dataset[j,2],data_rating=as.numeric(substr(dataset[j,3])))
  user_summary$user_info[user_summary$user_id==user_id[j]]<
  -rbind(user_summary$user_info[user_summary$user_id==user_id[j]],temp1)
  }
    
    else
{
  append(user_id_summary,user_id[j])
  temp2<-data.frame(movie_id=i,rating=dataset[j,2],data_rating=as.numeric(substr(dataset[j,3])))
  temp3<-data.frame(user_id=user_id[j],user_info=temp2)
  user_summary=rbind(user_summary,temp3)

}
}
