library("ggplot2", lib.loc="~/R/R-3.6.0/library")



##Nationals .574
## Mets .531
Nats<-0.574
Mets<-0.531



simulate.season<-function(Games,Win.Prob,s=10000) {
  results<-c("W","L")
  Wins.array<-c()
  for (i in 1:s) {
    season<-sample(results,size=Games,replace=TRUE,prob=c(Win.Prob,(1-Win.Prob)))
    season.table<-table(season)
    Wins<-season.table["W"][[1]]/Games
    Wins.array<-c(Wins.array,Wins)
  }
  
  # Wins.array<-round(Wins.array,4)
  Wins.array.df<-as.data.frame(Wins.array)
  return(Wins.array.df)
}





sim.162.Mets.df<-simulate.season(Games=162,Win.Prob = Mets)
all.df<-sim.162.Mets.df
names(all.df)<-c("WinP")
all.df$Games<-162
all.df$Team<-"Mets"
sim.162.Nats.df<-simulate.season(Games=162,Win.Prob = Nats)
all1.df<-sim.162.Nats.df
names(all1.df)<-c("WinP")
all1.df$Games<-162
all1.df$Team<-"Nats"
all.df<-rbind(all.df,all1.df)
rm(all1.df)



sim.60.Mets.df<-simulate.season(Games=60,Win.Prob = Nats)
all1.df<-sim.60.Mets.df
names(all1.df)<-c("WinP")
all1.df$Games<-60
all1.df$Team<-"Mets"
all.df<-rbind(all.df,all1.df)
rm(all1.df)

sim.60.Nats.df<-simulate.season(Games=60,Win.Prob = Nats)
all1.df<-sim.60.Nats.df
names(all1.df)<-c("WinP")
all1.df$Games<-60
all1.df$Team<-"Nats"
all.df<-rbind(all.df,all1.df)

rm(all1.df)



### Violen Plot?
## Git

p <- ggplot(all.df, aes(y=factor(Games), x=WinP))
p + geom_violin((aes(fill = factor(Team))))

## Set factor order
