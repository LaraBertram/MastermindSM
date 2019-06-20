#Behavioral plots
rm(list=ls())

library(plyr)
library(stringr)

getwd()

d<-read.csv2("C:/Users/larab/Documents/Promotion_Dateien_Backups_Apps/mastermind-master/MastermindSharmaMittal/cleanedDataMmind18_emap.csv")

names(d)[1] <- "id"

d$idn <- as.numeric(substr(d$id, 6, 7)) #get 6th and 7th part of string, deleting emap

d<-subset(d, !is.na(d$idn))#delete all NAs

#i = 1
for (i in 1:nrow(d)){ 
  s1<-gsub("\\[|\\]", "", paste(d$guesses[i])) #gsub replaces patterns, pace forgets factor -> remove brackets
  m1<-matrix(as.numeric(strsplit(s1, ";")[[1]]), ncol=3, byrow = TRUE)#split by semicolon
  s2<-gsub("\\[|\\]", "", paste(d$truecode[i])) #same for true code, must be repeated as many times as guesses made
  m2<-as.numeric(strsplit(s2, ";")[[1]])
  m2<-matrix(rep(m2, nrow(m1)), ncol=3, byrow=TRUE)
  s3<-gsub("\\[|\\]", "", paste(d$codejar[i]))
  m3<-as.numeric(strsplit(s3, ";")[[1]])
  m3<-matrix(rep(m3, each=nrow(m2)), ncol=6) #code jar recipe
  
  dummy<-as.data.frame(cbind(m1, m2, m3))      
  names(dummy)<-c(paste0("guess", 1:3), paste0("truth", 1:3), paste0("code", 1:6))
  dummy$game<-i #particular game
  dummy$id<-d$idn[i] #id of this guy
  if (i ==1){dat<-dummy} #initialization
  if (i>1){dat<-rbind(dat, dummy)} #concatenate
}
dat<-dat[order(dat$id),]


k<-1 #new Id variable, starting with 1
dat$n<-k
for (i in 2:nrow(dat)){
  if (dat$id[i]==dat$id[i-1]){
    dat$n[i]<-k
  }
  if (dat$id[i]!=dat$id[i-1]){
    k<-k+1
    dat$n[i]<-k
  }
}
head(dat, 200)
dat$id<-dat$n
dat$n<-NULL
head(dat)
dat$id
dat$game<-dat$game-ave(dat$game, dat$id, FUN=min)+1
head(dat, 200)

write.csv(dat, "mastermindcleanEmapTraining.csv")
