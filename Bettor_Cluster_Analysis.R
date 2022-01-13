setwd("~/Gambler Analysis Project")
library(dplyr)
library(ggplot2)
library(cluster)
#install.packages("factoextra")
library(factoextra)
#install.packages("rlang")

set.seed(1)

BetData <- read.csv("PopTrendsBData3Aggs.csv")

BetDataTotal <- BetData %>%
  select(c(UserID,StakeF,StakeL,StakeA,WinF,WinL,WinA,BetsF,BetsL,BetsA)) %>%
  mutate(AvgStakeF = StakeF/BetsF,
         AvgStakeL = StakeL/BetsL,
         AvgStakeA = StakeA/BetsA,
         NetF = WinF - StakeF,
         NetL = WinL - StakeL,
         NetA = WinA - StakeA,
         AvgNETF = NetF/BetsF,
         AvgNETL = NetL/BetsL,
         AvgNETA = NetA/BetsA)

#All bets
ClusterBetData <- BetDataTotal %>%
  select(c(BetsA,AvgStakeA,AvgNETA))
ClusterBetData[is.na(ClusterBetData)] = 0
ClusBetDataScaled <- scale(ClusterBetData, center = TRUE, scale = TRUE)

#num clusters

#Elbow Method
elbow <- c()
for (k in 1:10){
  elbow_kmeans <- kmeans(ClusBetDataScaled, centers = k)
  elbow[k] <- elbow_kmeans$tot.withinss
}
elbowdata <- data.frame(k = 1:10, TotWSS = elbow)
ggplot(elbowdata, aes(x =k, y = TotWSS)) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = 1:10)
#3 clusters



All_clus <- kmeans(ClusBetDataScaled, centers = 3)
ClusterBetData$Cluster <- All_clus$cluster
ClusAll <- ClusterBetData %>%
  group_by(Cluster) %>%
  summarize_if(is.numeric,mean)
ClusAll
#Fixed bets

ClusterBetDataF <- BetDataTotal %>%
  filter(StakeF > 0) %>%
  select(c(BetsF,AvgStakeF,AvgNETF))
ClusterBetDataF[is.na(ClusterBetDataF)] = 0
ClusBetDataScaledF <- scale(ClusterBetDataF, center = TRUE, scale = TRUE)

elbowF <- c()
for (k in 1:10){
  elbow_kmeansF <- kmeans(ClusBetDataScaledF, centers = k)
  elbowF[k] <- elbow_kmeansF$tot.withinss
}
elbowdataF <- data.frame(k = 1:10, TotWSS = elbowF)
ggplot(elbowdataF, aes(x =k, y = TotWSS)) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = 1:10)
# would choose 3 clusters

Fixed_clus <- kmeans(ClusBetDataScaledF, centers = 3)
ClusterBetDataF$Cluster <- Fixed_clus$cluster
ClusFixed <- ClusterBetDataF %>%
  group_by(Cluster) %>%
  summarize_if(is.numeric,mean)
ClusFixed


#Live bets

ClusterBetDataL <- BetDataTotal %>%
  filter(StakeL > 0) %>%
  select(c(BetsL,AvgStakeL,AvgNETL))
ClusterBetDataL[is.na(ClusterBetDataL)] = 0
ClusBetDataScaledL <- scale(ClusterBetDataL, center = TRUE, scale = TRUE)

elbowL <- c()
for (k in 1:10){
  elbow_kmeansL <- kmeans(ClusBetDataScaledL, centers = k)
  elbowL[k] <- elbow_kmeansL$tot.withinss
}
elbowdataL <- data.frame(k = 1:10, TotWSS = elbowL)
ggplot(elbowdataL, aes(x =k, y = TotWSS)) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = 1:10)
# would choose 3 clusters

Live_clus <- kmeans(ClusBetDataScaledL, centers = 3)
ClusterBetDataL$Cluster <- Live_clus$cluster
ClusLive <- ClusterBetDataL %>%
  group_by(Cluster) %>%
  summarize_if(is.numeric,mean)
ClusLive








This paper utilized data from the Transparency Project (www.thetransparencyproject.org), Division on Addiction, the Cambridge Health Alliance, a teaching affiliate of Harvard Medical School