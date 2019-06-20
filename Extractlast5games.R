#extract last 5 games
d<-read.csv("C:/Users/larab/Documents/Promotion_Dateien_Backups_Apps/mastermind-master/MastermindSharmaMittal/mastermindcleanEmapTraining.csv")
d$X<-NULL

dd<-ddply(d, ~id, summarize, m=max(game))

dd<-subset(dd, m>=5)
d<-subset(d, id %in% dd$id)

d$game<-d$game-ave(d$game, d$id, FUN=max)+5
d<-subset(d, game>0)
head(d, 300)

write.csv(d, "mastermindproto2.csv")
#and then copy it to python folder!