####changer le chemin ici dépendemment d'ou vous ouvrez le fichier.
setwd("E:/GLO-4027/Github/GLO-4027_project_team3")
train <- read.csv2('trainpretraite.csv', stringsAsFactors = T)
##########fonctions du démarrage à froid##########
profilEvaluation <- function(gender, age, occupation){
  train[which((train$Gender == gender) & (train$Age == age) & (train$Occupation == occupation)),]
}

meanMoviesFromProfil <- function(gender, age, occupation){
  result <- list()
  getEval <- profilEvaluation(gender, age, occupation)
  ### retrourne la moyenne de tout les films d'un profil 
  dt <- as.data.frame(aggregate(getEval[,c('rating')],by = list(getEval$MovieID), FUN=mean ))
  colnames(dt) <- c('Id','Profil')
  return(dt)
}



meanMoviesFromGlobal <- function(gender, age, occupation){
  ### Retourne la moyenne de tous les films dans l'ensemble du jeu de donnée selon les films évalués par le profil
  result <- list()
  getEval <- subset(train, MovieID %in% unique(profilEvaluation(gender, age, occupation)$MovieID))
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
  profilEval <- profilEvaluation(gender, age, occupation)
  if(nrow(profilEval) != 0){
    meanPerProfil <- meanMoviesFromProfil(gender, age, occupation)
    meanGlobal <- meanMoviesFromGlobal(gender, age, occupation)
    meanPerProfilSimilaire <- meanProfilSimilaire(gender, age, occupation)
    X1 <- cbind(meanPerProfil,meanGlobal)
    all <- merge(X1[,-3],meanPerProfilSimilaire,by="Id",all.x=TRUE)
    all[is.na(all)] <- 0
    all$ratingGlobal <- (all$Profil + all$Global + all$rating)/3
  }
  else{
    all <- as.data.frame(aggregate(train[,c('rating')],by = list(train$MovieID), FUN=mean ))
    colnames(all) <- c('Id', 'ratingGlobal')
  }
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

##### A optimiser ######
##### Evaluation note selon similitude entre utilisateur
getSim <- function(userid,vl) {
  return(tapply(vl$UserID,list(vl$UserID),function(x) M[paste(userid),paste(x) ]))
}
ratingSimilitudeUser <- function(userid){
  vlt <- data.frame(MovieID=c(), UserID=c(), rating=c(), V2=c())
  listnonregarde <- train[which(!(unique(train$MovieID) %in% unique(subset(train, UserID==userid)$MovieID))), c("MovieID")]
  for(movie in unique(listnonregarde$MovieID)) {
    vl <- subset(train, MovieID==movie)[,c('MovieID','UserID','rating')]
    vlt <- rbind(vlt,cbind(vl,getSim(userid,vl)))
  }
  rating <- vlt[,.(R = sum(rating*V2)/sum(V2)),keyby=.(MovieID)]
  rating[order(-R),]
  return(rating)
}
##### Evalutation notes selon similitude entre film

#######MAIN#######
top100movies <- function(userid){
  profil <- users[which(users$ID == userid),]
  evaluationUser <- train[which((train$UserID == userid)),]
  if(nrow(evaluationUser) == 0){
    meanResult <- meanMovies(profil$Gender, profil$Age, profil$Occupation)
    ordermeanResult <- meanResult[order(meanResult$ratingGlobal, decreasing = TRUE),]
    top100 <- head(ordermeanResult,200)
    extractIDandRating <- subset(top100, select=c("Id", "ratingGlobal"))
  }
  else{
    extractIDandRating <- ''
  }
  return(extractIDandRating)
}


######Test sur tous les utilisateurs.  On boucle sur la fonction top10movies####
users <- read.csv2('users.dat', stringsAsFactors = T)
bestMoviesPerUser <- function(){
  temp <- users[501:1000,]
  sampleTokaggle <- data.frame()
  for(userid in temp$ID){
    print(userid)
    result <- top100movies(userid)
    userIdandmoviesId <- paste(userid, result$Id, sep='_')
    sampleTokaggle <- rbind(sampleTokaggle, data.frame(userid, result$ratingGlobal, userIdandmoviesId))
  }
  colnames(sampleTokaggle) <- c('user','rating','id')
  return(sampleTokaggle)
}
write.csv(bestMoviesPerUser(), file = "sample2.csv", row.names=F, quote = FALSE)

##### Pour chaques utilisateur estimé lerating en fonction de global, Profil, rating. test pour pondération
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


