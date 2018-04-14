setwd("E:\\GLO-4027\\Data")
train <- read.csv2('trainpretraite.csv', stringsAsFactors = T)
##########fonctions du démarrage à froid##########
profilEvaluation <- function(gender, age, occupation){
  train[which((train$Gender == gender) & (train$Age == age) & (train$Occupation == occupation)),]
}

meanMoviesFromProfil <- function(gender, age, occupation){
  result <- list()
  getEval <- profilEvaluation(gender, age, occupation)
  print(dim(getEval))
  print(paste(gender, age, occupation,'MMP', sep='_'))
  ### retrourne la moyenne de tout les films d'un profil 
  dt <- as.data.frame(aggregate(getEval[,c('rating')],by = list(getEval$MovieID), FUN=mean ))
  colnames(dt) <- c('Id','Profil')
  return(dt)
}



meanMoviesFromGlobal <- function(gender, age, occupation){
  ### Retourne la moyenne de tous les films dans l'ensemble du jeu de donnée selon les films évalués par le profil
  result <- list()
  getEval <- subset(train, MovieID %in% unique(profilEvaluation(gender, age, occupation)$MovieID))
  print(dim(getEval))
  print(paste(gender, age, occupation,'MMG', sep='_'))
  dt <- as.data.frame(aggregate(getEval[,c('rating')],by = list(getEval$MovieID), FUN=mean ))
  colnames(dt) <- c('Id','Global')
  return(dt)
}

### Creer la matrice de similitude
library(apcluster)
library(data.table)
train <- as.data.table(train)
MoyenneFilmProfil <- train[,.(.N, rating = mean(rating)), keyby=.(Gender, Age, Occupation, MovieID)]
MoyenneFilmProfil$Profil_ID <- paste(MoyenneFilmProfil$Gender,MoyenneFilmProfil$Age, MoyenneFilmProfil$Occupation, sep='_' )
MoyenneFilmProfil$Profil_ID <- as.factor(MoyenneFilmProfil$Profil_ID)
library(tidyr)
M <- spread(MoyenneFilmProfil[,c(4,6,7)], key= MovieID, value=rating, fill=0)
Profil <- data.frame(Id=M[,1])
sim  <- linKernel( M[,-1], normalize = T)
colnames(sim) <- Profil$Profil_ID
rownames(sim) <- Profil$Profil_ID
dim(sim)

ProfilSimilaire <- function(gender, age, occupation) {
  # fonction permettant d'aller chercher les 5 % les plus similaires
  Profil_ID <- paste(gender,age, occupation, sep='_' )
  v1 <- data.frame(Cos=sim[Profil_ID,])
  borne <- quantile(v1[-which(Profil_ID %in% rownames(v1)),c('Cos')],0.95) 
  return(subset(v1,Cos >= borne & Cos < 1))
}

meanProfilSimilaire <- function(gender, age, occupation){
  # fonction qui calcul la moyenne des films(seulement ceux lié exactement au profil) selon les profils similaires
  listProfilSimaire <- ProfilSimilaire(gender, age, occupation)
  profils <- strsplit((rownames(listProfilSimaire)), "_")
  kl <-   head(train,1)
  kl2 <- kl[-1,]
  for(p in profils) {
    kl2 <- rbind(kl2, subset(train, train$Gender == p[[1]] & train$Age == p[[2]] & train$Occupation == p[[3]]))
  }
  kl3 <- as.data.frame(aggregate(kl2[,c('rating')],by = list(kl2$MovieID), FUN=mean ))
  colnames(kl3) <- c('Id','rating')
  return(kl3[which(unique(profilEvaluation(gender, age, occupation)$MovieID) %in% kl3$Id),])
}


meanMovies <- function(gender, age, occupation){
  meanPerProfil <- meanMoviesFromProfil(gender, age, occupation)
  meanGlobal <- meanMoviesFromGlobal(gender, age, occupation)
  meanPerProfilSimilaire <- meanProfilSimilaire(gender, age, occupation)
  X1 <- cbind(meanPerProfil,meanGlobal)
  all <- merge(X1[,-3],meanPerProfilSimilaire,by="Id",all.x=TRUE)
  all[is.na(all)] <- 0
  all$ratingGlobal <- (all$Profil + all$Global + all$rating)/3
  return(all)
}


###########Fonctions utilisateur avec evaluation########
train[train$UserID, train$Gender, train$Age, train$Occupation]
matriceSimilitudeUtilisateur <- train[,.(Gender, Age, Occupation, UserID), keyby=.(Gender, Age, Occupation, UserID)]
ml <- matriceSimilitudeUtilisateur[,.(Gender, Age, Occupation, UserID),]
ss<-ml[,c(1,2,3)]
ml$Age <- as.factor(ml$Age)
ml$Occupation <- as.factor(ml$Occupation)

unlist(strsplit(colnames(ss), '[.]'))[2 * (1:ncol(ss))]
X1 <- cbind(model.matrix(~Gender-1, ml),model.matrix(~Age-1, ml),
model.matrix(~Occupation-1, ml))
M <- linKernel(X1,normalize = T)
M[3,8]
colnames(M)<-ml$UserID
rownames(M)<-ml$UserID

head(M[4,])
# Pour chaques utilisateur estimé lerating en fonction de global, Profil, rating

lkk <- meanMovies(gender, age, occupation)
lm(rating ~ Profil + Global + rating - 1, data=lkk)

ech <- train[sample(1:400000,150),c('rating','Gender', 'Age', 'Occupation','MovieID')]
yy <- data.frame()
for(i in 1:(dim(ech)[1])){
  ec<-ech[i,c('Gender', 'Age', 'Occupation')]
  yy <- rbind(yy,data.frame(ec,meanMovies(ec$Gender, ec$Age, ec$Occupation)))
  
}
colnames(yy) <- c('Gender','Age', 'Occupation','MovieID', 'Profil','Global','rating','ratingGlobal')
all <- merge(ech,yy,by=c('Gender','Age','Occupation','MovieID'),all.x=TRUE)
lm(rating.x ~ Profil + Global + rating.y - 1, data=all)
lm(rating.x-rating.y~-1+I(Profil-rating.y)+I(Global-rating.y),data=all)


### 
# utilisateur 1 profil global rating *rating
# utilisateur 1 profil
# utilisateur 2 profil


