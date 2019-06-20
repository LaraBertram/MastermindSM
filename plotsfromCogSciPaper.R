install.packages("ggbeeswarm")
install.packages("lsr") 
install.packages("ggridges")
install.packages("entropy")
install.packages("gridExtra")
install.packages("BayesFactor")
install.packages("wesanderson")

library(stringr)
library(plyr)
library(BayesFactor)

#Behavioral plots
rm(list=ls())

packages<-c("plyr", "ggplot2", "gridExtra","ggbeeswarm", "lsr",'ggridges','entropy','zoo', 'wesanderson')
lapply(packages, require, character.only = TRUE)


dall<-read.csv2("C:/Users/larab/Documents/Promotion_Dateien_Backups_Apps/mastermind-master/MastermindSharmaMittal/cleanedDataMmind18_emap.csv")
names(dall)[1] <- "id"

d<-read.csv("C:/Users/larab/Documents/Promotion_Dateien_Backups_Apps/mastermind-master/MastermindSharmaMittal/mastermindcleanEmapTraining.csv")



dall$jarname<-mapvalues(dall$jarname, unique(dall$jarname), c("Very High", "Low", "High", "Very Low"))
dall$jarname<-factor(dall$jarname, levels=c("Very Low", "Low", "High", "Very High"))
dp<-ddply(dall, ~id+jarname, summarize, m=mean(truncguesses))#for every person in condition mean numer of guesses

library(wesanderson)
#wes_palette("cavalcanti")

#Boxplot of the Perfomance for each Condition and Time Pressure
p1 <- ggplot(dp, aes(x =jarname, y = m, fill = jarname, color = jarname))+
  geom_quasirandom(position = position_jitter(width = .05), dodge.width=0.66, size = 1.5, alpha = 0.85, varwidth=T) +
  geom_boxplot(width = .2, position =  position_dodge(width=0.66), outlier.shape = NA, alpha = 0.25, color = 'black') +
  #scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  ylab('Number of queries')+
  xlab('Entropy')+
  ggtitle("a: Performance")+
  scale_fill_manual(values =c(wes_palette("Cavalcanti1")[2],  "#56B4E9", "#E69F00", wes_palette("Cavalcanti1")[5]), name = '')+
  scale_color_manual(values =c(wes_palette("Cavalcanti1")[2], "#56B4E9", "#E69F00", wes_palette("Cavalcanti1")[5]), name = '')+
  theme(text = element_text(size=20,  family="sans"),
        legend.position = "none")
p1

for (i in 1:nrow(dall)){
  rt<-strsplit(paste(dall$rt[i]), ";")[[1]]
  re<- str_replace(rt, "\\]", "")
  re<-str_replace(re, "\\[", "")
  re <- as.numeric(re) 
  re <- diff(c(0, re)) #get differential
  dall$rttotal[i]<-mean(re)
}


dt<-ddply(dall, ~id+jarname, summarize, m=mean(log(rttotal)))


#number of past guesses 
p2 <- ggplot(dt, aes(x =jarname, y = m, fill = jarname, color = jarname))+
  geom_quasirandom(position = position_jitter(width = .05), dodge.width=0.66, size = 1.5, alpha = 0.85, varwidth=T) +
  geom_boxplot(width = .2, position =  position_dodge(width=0.66), outlier.shape = NA, alpha = 0.25, color = 'black') +
  #scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  ylab('Time in log-ms')+
  xlab('Entropy')+
  ggtitle("b: Time spent thinking")+
  scale_fill_manual(values =c(wes_palette("Cavalcanti1")[2],  "#56B4E9", "#E69F00", wes_palette("Cavalcanti1")[5]), name = '')+
  scale_color_manual(values =c(wes_palette("Cavalcanti1")[2], "#56B4E9", "#E69F00", wes_palette("Cavalcanti1")[5]), name = '')+
  theme(text = element_text(size=20,  family="sans"),
        legend.position = "none")
p2


dguess<-data.frame(id=numeric(), cond=numeric(), correct=numeric()) #initialize data frame
for(i in 1:nrow(dall)){
  correct<-c(rep(0, dall$truncguesses[i]), rep(1, 20-dall$truncguesses[i]))
  id<-rep(dall$id[i], 20)
  cond<-rep(dall$jarname[i],20)
  dguess<-rbind(dguess, data.frame(id, cond, correct))
}

dguess$trial<-rep(1:20, i)

#dguess$trial<-round(dguess$trial/2)*2
se<-function(x){sd(x)/sqrt(length(x))}

dg<-ddply(dguess, ~cond+trial, summarize, m=mean(correct), se=1.96*se(correct))
pd <- position_dodge(.5)

p3 <- ggplot(dg, aes(x =trial, y = m, fill = cond, color = cond))+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width=.8, size = .8, position=pd, linetype=1) +
  #line
  geom_line(size=0.8, position=pd)+
  geom_point(position=pd)+
  #geom_line(data=dgl, aes(x =trial, y = m, fill = cond, color = cond, group=id), size=2, alpha=0.1) +
  #scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  ylab('Proportion correct')+
  xlab('Number of past guesses')+
  ggtitle("c: Solutions over time")+
  scale_fill_manual(values =c(wes_palette("Cavalcanti1")[2],  "#56B4E9", "#E69F00", wes_palette("Cavalcanti1")[5]), name = '')+
  scale_color_manual(values =c(wes_palette("Cavalcanti1")[2], "#56B4E9", "#E69F00", wes_palette("Cavalcanti1")[5]), name = '')+
  theme(text = element_text(size=20,  family="sans"),
        legend.position = "top")
p3



dall$s1<-as.numeric(substr(dall$feedback_smiley,2,2))
dall$n1<-as.numeric(substr(dall$feedback_neutral,2,2))
dd1<-ddply(dall, ~jarname+id, summarize, s1=mean(s1), n1=mean(n1), f1=mean(3-n1-s1))

df<-data.frame(props=c(dd1$s1, dd1$n1, dd1$f1), 
               jarname=rep(dd1$jarname, 3),
               feedback=rep(c(":-)", ":-|", ":-("), each=nrow(dd1)))

df$feedback<-factor(df$feedback, levels=c(":-)", ":-|", ":-("))

p4 <- ggplot(df, aes(x =jarname, y = props, fill = jarname, color = jarname))+
  geom_quasirandom(position = position_jitter(width = .05), dodge.width=0.66, size = 1.5, alpha = 0.85, varwidth=T) +
  geom_boxplot(width = .2, position =  position_dodge(width=0.66), outlier.shape = NA, alpha = 0.25, color = 'black') +
  #scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  ylab('Proportion')+
  xlab('Entropy')+
  ggtitle("d: First feedback")+
  facet_wrap(~feedback, nrow=3)+
  scale_fill_manual(values =c(wes_palette("Cavalcanti1")[2],  "#56B4E9", "#E69F00", wes_palette("Cavalcanti1")[5]), name = '')+
  scale_color_manual(values =c(wes_palette("Cavalcanti1")[2], "#56B4E9", "#E69F00", wes_palette("Cavalcanti1")[5]), name = '')+
  theme(text = element_text(size=20,  family="sans"), legend.position = "none")
p4



#apply(dd1[,2:4],1, sum)


library(gridExtra)


pdf("mastermindbehavioral.pdf", width=9, height=9)
grid.arrange(p1,p2, p3, p4, nrow=2)
dev.off()

dd<-ddply(dall, ~id, summarize, 
          m=mean(truncguesses[jarname=="Very Low"]-truncguesses[jarname=="Low"]))

dd$m<-ifelse(dd$m=="NaN", mean(na.omit(dd$m)), dd$m)
ttestBF(dd$m)
t.test(dd$m)
cohensD(dd$m)


#brms


