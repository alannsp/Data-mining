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



***************************NEW VERSION************************************************

library(gdata)
help("read.xls")
Movie_Titles<-read.xls("C:/Users/Kartikeya/Desktop/ALDA Project/movie_titles_small.xls",header= F,perl = "C:/Perl64/bin/perl.exe")

names(Movie_Titles)<- c("Movie_ID","Release_Year","Movie_Title")

setwd("C:/Users/Kartikeya/Desktop/ALDA Project/Dataset/training_set_small/")
files_list<- list.files()

i<-1
user_summary<-c()
for(file in files_list)
{
  dataset<-read.table(text=readLines(file)[-1],header=FALSE,sep=",")
  ratings<-dataset[,c(2)]
  average<-mean(ratings)
  standard_deviation<-sd(ratings)
  number<-length(ratings)
  
  #Movie Summary
  
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
  
  #User Summary
  
  user_id<-dataset[,c(1)]
  
  
  
  for(j in 1:number)
  {
    
    if(!is.element(user_id[j],user_summary))
    {
      release_year<- Movie_Titles[,c(2)]
      rating_year<- substr(dataset[,c(3)],1,4)
      difference=strtoi(rating_year[j])-release_year[i]
      user_summary<-append(user_summary,user_id[j])    
      assign(paste("User",user_id[j],sep=""),data.frame(Movie_ID=i,Rating=ratings[j],Difference=difference))
    }
    
    else
    {
      release_year<- Movie_Titles[,c(2)]
      rating_year<- substr(dataset[,c(3)],1,4)
      difference=strtoi(rating_year[j])-release_year[i]
      user_temp<-data.frame(Movie_ID=i,Rating=ratings[j],Difference=difference)
      
      temp<-get(paste("User",user_id[j],sep=""))
      temp<-rbind(temp,user_temp)
      assign(paste("User",user_id[j],sep=""),temp)
      rm(user_temp)
      rm(temp)
    }
    
  }
  
  i<-i+1
}

(kc<-kmeans(Movie_Summary[,c(2,3,4)],2))

#Release_year_temp<-Movie_Titles[,c[2]]
#cbind(Movie_Summary,Release_Year= Release_year_temp)
