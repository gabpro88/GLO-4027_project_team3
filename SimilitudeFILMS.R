setwd("~/DataTrain")
train <- read.csv2('trainpretraite.csv', stringsAsFactors = T)

MovieInfo
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


###########Fonctions utilisateur avec evaluation  ########
matriceSimilitudeUtilisateur <- train[,.(Gender, Age, Occupation, UserID), keyby=.(Gender, Age, Occupation, UserID)]
ml <- matriceSimilitudeUtilisateur[,.(Gender, Age, Occupation, UserID),]
ss<-ml[,c(1,2,3)]
ml$Age <- as.factor(ml$Age)
ml$Occupation <- as.factor(ml$Occupation)

unlist(strsplit(colnames(ss), '[.]'))[2 * (1:ncol(ss))]
X1 <- cbind(model.matrix(~Gender-1, ml),model.matrix(~Age-1, ml),
            model.matrix(~Occupation-1, ml))
M <- linKernel(X1,normalize = T)
colnames(M)<-ml$UserID
rownames(M)<-ml$UserID


########Recherche IMDB ####################
devtools::install_github("rmhogervorst/imdb")
library(imdb)
Sys.setenv(OMDB_KEY='84fbf0e7') #used
Sys.setenv(OMDB_KEY='d48ba016') #used
Sys.setenv(OMDB_KEY='dfed24d7') #used
Sys.setenv(OMDB_KEY='2ebd30ac') #used
Sys.setenv(OMDB_KEY='fff9070a') 


### Pour chaques films, trouver les informations IMDB
X1 <- unique(train[,c('MovieID','Title','Annee')])
key1 <- Sys.getenv('OMDB_KEY')
#MovieInfo <- data.frame() 
for( i in 3382:dim(X1)[1]) {
  tryCatch({
  tt <- gsub("\\s*\\([^\\)]+\\)","",X1$Title[i])
  tt <- gsub("(.*),.*", "\\1", tt)
  MovieInfo <- rbind(MovieInfo,cbind(X1$MovieID[i],imdbMovies(tt, key1)))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Certains films ont besoins de se faire specifier la date de sortie : Modification de la requete imdb
imdbMoviesYear <- function(moviename,year, key){
  link<-gsub(pattern = " ",
             replacement = "%20", x=(paste("http://www.omdbapi.com/?t=",moviename,"&y=",year,"&type=movie&r=json&plot=full",'&apikey=', key, sep = "")))
  hold<-jsonlite::fromJSON(link)
  if(hold$Response != "True")stop("response is not valid")
  df <- data.frame(Title = hold$Title,
                   Year = hold$Year,
                   Rated = hold$Rated,
                   Released = hold$Released,
                   Runtime = hold$Runtime,
                   Genre = hold$Genre,
                   Director = hold$Director,
                   Writer = hold$Writer,
                   Actors = hold$Actors,
                   Plot = hold$Plot,
                   Language = hold$Language,
                   Country = hold$Country,
                   Awards = hold$Awards,
                   Posterlink = hold$Poster,
                   Metascore = hold$Metascore,
                   imdbRating = hold$imdbRating,
                   imdbVotes = hold$imdbVotes,
                   imdbID = hold$imdbID,
                   stringsAsFactors = FALSE)
  ToS_message()
  df
  
}

X2 <- X1[which(!(unique(X1$MovieID) %in% unique(MovieInfo$`X1$MovieID[i]`))),]
MovieInfo2 <- data.frame()
for( i in 1:dim(X2)[1]) {
  tryCatch({
    tt <- gsub("\\s*\\([^\\)]+\\)","",X2$Title[i])
    tt <- gsub("(.*),.*", "\\1", tt)
    MovieInfo2 <- rbind(MovieInfo2,cbind(X2$MovieID[i],imdbMoviesYear(tt,X2$Annee[i], key1)))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
########################################################################################

t3 <- MovieInfoComplete
v11 <- do.call(rbind.data.frame, strsplit(t3$Actors, split=","))
colnames(v11) <- c('Principal','Principal2','Principal3','Soutien')

Actors1 <- data.frame(actor=as.character(v11$Principal), stringsAsFactors = F)
Actors2 <- data.frame(actor=as.character(v11$Principal2), stringsAsFactors = F)
Actors3 <- data.frame(actor=as.character(v11$Principal3), stringsAsFactors = F)
Actors4 <- data.frame(actor=as.character(v11$Soutien), stringsAsFactors = F)
Actorss <- rbind(Actors1,Actors2,Actors3, Actors4)
abc <- table(Actorss$actor)[which(table(Actorss$actor) > 4)] # plus de 3 apparence (439)
length(abc)
acteursPlusConnu <- rownames(abc)
for (i in 1:(length(acteursPlusConnu))) {
  v11 <- cbind(v11,ifelse(v11$Principal %like% acteursPlusConnu[i] | v11$Principal2 %like% acteursPlusConnu[i] | v11$Principal3 %like% acteursPlusConnu[i] | v11$Soutien %like% acteursPlusConnu[i],1,0))
  colnames(v11) <- c(names(v11)[c(1:(dim(v11)[2])-1)], paste("Actors", acteursPlusConnu[i], sep="."))
}
dim(abc)
t4 <- cbind(t3,v11)
v12 <- do.call(rbind.data.frame, strsplit(t4$Director, split=","))
colnames(v12) <- c('Principal','Principal2','Principal3','Principal4','Principal5','Principal6', 'Principal7', 'Principal8', 'Principal9','Principal10','Principal11','Principal12')
head(v12)
Director1 <- data.frame(actor=as.character(v12$Principal), stringsAsFactors = F)
abc2 <- table(Director1$actor)[which(table(Director1$actor) > 2)] # plus de 2 apparence (439)
DirecteurPlusConnu <- rownames(abc2)
for (i in 1:(length(DirecteurPlusConnu))) {
  v12 <- cbind(v12,ifelse(v12$Principal %like% DirecteurPlusConnu[i] | v12$Principal2 %like% DirecteurPlusConnu[i] | v12$Principal3 %like% DirecteurPlusConnu[i] | v12$Principal4 %like% DirecteurPlusConnu[i] | v12$Principal5 %like% DirecteurPlusConnu[i] | v12$Principal6 %like% DirecteurPlusConnu[i] | v12$Principal7 %like% DirecteurPlusConnu[i] | v12$Principal8 %like% DirecteurPlusConnu[i] | v12$Principal9 %like% DirecteurPlusConnu[i] | v12$Principal10 %like% DirecteurPlusConnu[i] | v12$Principal11 %like% DirecteurPlusConnu[i] | v12$Principal12 %like% DirecteurPlusConnu[i],1,0))
  colnames(v12) <- c(names(v12)[c(1:(dim(v12)[2])-1)], paste("Directors", DirecteurPlusConnu[i], sep="."))
}
dim(t4)
head(t4,1)
dim(v12)
t5 <- cbind(t4[,c(2, 9:26,31:dim(t4)[2])],v12[,12:dim(v12)[2]])
head(t5,1)
### similarite trouver
MM <- linKernel(t5[,2:dim(t5)[2]], normalize = T)
colnames(MM) <- t5[,1]
rownames(MM) <- t5[,1]

### similarite non trouver
listeNonTrouver <- unique(train$MovieID)[which(!(unique(train$MovieID) %in% t5$MovieID))]
a1 <- unique(train[,c(3,11:28)])
MMManquant <- linKernel(a1[,-1],normalize = T)
colnames(MMManquant) <- c(a1$MovieID) 
rownames(MMManquant) <- c(a1$MovieID)


#### Main loop : #### Similitude entre les usages
similitudeMovie <- function(MovieNonVu, MovieVu) {
  if(MovieVu %in% listeNonTrouver | MovieNonVu %in% listeNonTrouver) {
    return(MMManquant[paste(MovieVu),paste(MovieNonVu)])
  }else{
    return(MM[paste(MovieVu), paste(MovieNonVu)])
  }
}
#### Similitude entre les films 
### Pour tout les films j non vu du profil i
   #### Pour tout les films k que l'usage i a vu 
      ### Similitude sim(j,k)*rating(k) 
RatingPredictedUser <- function(user) {
listRegarde <- train[which((unique(train$MovieID) %in% unique(subset(train, UserID==user)$MovieID))), c('MovieID','rating')]
listnonregarde <- train[which(!(unique(train$MovieID) %in% unique(subset(train, UserID==user)$MovieID))), c("MovieID",'UserID','rating')]
ratingPredMovie <- c()
ratingPredUser <- c()
for(i in 1:dim(listnonregarde)[1]) {
  simmm <- apply(listRegarde,1, function(x) similitudeMovie(listnonregarde[i,1],x['MovieID'] ))
  ratingPredMovie <- c(ratingPredMovie,sum(simmm*listRegarde$rating)/sum(simmm))
  vl <- subset(train, MovieID == listnonregarde$MovieID[i])[,c('MovieID','UserID','rating')]
  ratingPredUser <- c(ratingPredUser,sum(vl$rating*M[paste(user),paste(vl$UserID)])/sum(M[paste(user),paste(vl$UserID)]))
}
return(cbind(listnonregarde, ratingPredMovie,ratingPredUser))
}

predicted <- data.frame()
train <- as.data.frame(train)
for( u in unique(train$UserID)[1:15]) {
 predicted <- rbind(predicted, cbind(u, RatingPredictedUser(u)))
}

## Creation d'un echantillon aleatoire de 30 individu

echAleatoire <- subset(train, UserID==sample(unique(train$UserID),30))

## On enleve deux films a l'individu s'il a ecouter plus de deux films sinon on enleve 1.
library(plyr)
ligneAEnlever <- ddply(echAleatoire, .(UserID), function(x) x[sample(nrow(x),ifelse(nrow(x)==1,1,2)),])
train2 <- subset(train, !(MovieID %in% ligneAEnlever$MovieID & UserID %in% ligneAEnlever$UserID))

# Prediction sur un jeu test sans les observations
RatingPredictedUserTrain <- function(user) {
  listRegarde <- train2[which((unique(train2$MovieID) %in% unique(subset(train2, UserID==user)$MovieID))), c('MovieID','rating')]
  listnonregarde <- train2[which(!(unique(train2$MovieID) %in% unique(subset(train2, UserID==user)$MovieID))), c("MovieID",'UserID','rating')]
  ratingPredMovie <- c()
  ratingPredUser <- c()
  for(i in 1:dim(listnonregarde)[1]) {
    simmm <- apply(listRegarde,1, function(x) similitudeMovie(listnonregarde[i,1],x['MovieID'] ))
    ratingPredMovie <- c(ratingPredMovie,sum(simmm*listRegarde$rating)/sum(simmm))
    vl <- subset(train2, MovieID == listnonregarde$MovieID[i])[,c('MovieID','UserID','rating')]
    ratingPredUser <- c(ratingPredUser,sum(vl$rating*M[paste(user),paste(vl$UserID)])/sum(M[paste(user),paste(vl$UserID)]))
  }
  return(cbind(listnonregarde, ratingPredMovie,ratingPredUser))
}

predicted <- data.frame()
train2 <- as.data.frame(train2)
for( u in unique(ligneAEnlever$UserID)) {
  predicted <- rbind(predicted, cbind(u, RatingPredictedUserTrain(u)))
}
unique(predicted$u)
ff <- merge(predicted,train[,c('MovieID','UserID','rating')], by=c('MovieID','UserID'))

predicted$pp <- (predicted$ratingPredMovie + predicted$ratingPredUser)/2

predicted <- as.data.table(predicted)
abc <- predicted[order(u,-pp)]
abc2 <- abc[,.(pp=max(pp)),keyby=.(u,MovieID,ratingPredMovie,ratingPredUser)]
abc3 <- abc2[order(u,-pp)]

manip <- ddply(abc3, .(u), function(x)   tibble::rowid_to_column(x, "Pos"))
colnames(manip) <- c('Pos','UserID','MovieID','ratingPredMovie','ratingPredUser','pp')
vl <- merge(ligneAEnlever, manip, by=c("UserID","MovieID"))
vl$abs <- abs(vl$rating - vl$pp)
vl <- as.data.table(vl)
vl[,median(vl$Pos),]
vl[,mean(abs(vl$rating - vl$pp)),]
q1 <- qplot(vl$Pos, geom="histogram") + ggtitle("Distribution de la position prédite") + xlab("Position") + ylab("Fréquence") +stat_bin(binwidth = 200)
q2 <- qplot(vl$abs, geom="histogram") + ggtitle("Distribution de l'écart absolu") + xlab("Écart absolu") + ylab("Fréquence") + stat_bin(binwidth = 0.5)
grid.arrange(q1,q2, ncol=2)



### attention NAN sont 0 puisque 0/0 
### Fin #### 












