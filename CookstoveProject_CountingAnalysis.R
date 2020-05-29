# Get the data
setwd('C:/Users/sliu44/Downloads')
stove <- read.csv('Cookstove.csv')
stove <- stove[!is.na(stove['my_rand11']),]

# number of times each level appearing
lev <- c('This stove produces a lot of smoke.',
         'This stove is expected to last 10 years.',
         'This stove takes 2 hours to cook beans.',
         'Wood pellets and solar power are needed to run this stove.',
         'To adjust the heat produced by this stove, you fan a flame or control the amount of fuel.',
         'It is recommended to take this stove in for servicing 4 times a year.',
         'This stove is expected to last 5 years.',
         'Wood pellets and electricity are needed to run this stove.',
         'To adjust the heat produced by this stove, you use a knob.',
         'To adjust the heat produced by this stove, you use a vent.',
         'This stove takes 3 hours to cook beans.',
         'Instructions on how to use this stove are provided at time of purchase and in two more in-home demonstrations.',
         'You make monthly payments that cover installments for the stove, while the fuel is purchased separately as needed.',
         'This stove takes 5 hours to cook beans.',
         'Instructions on how to use this stove are provided at time of purchase.',
         'It is recommended to take this stove in for servicing every year.',
         'Charcoal is needed to run this stove.',
         'You make one up-front cash payment for the stove, while the fuel is purchased separately as needed.',
         'It is recommended to take this stove in for servicing every 5 years.',
         'This stove can only balance a small pot whilst you are stirring food in it and not a medium or large pot.',
         'You make monthly payments that cover installments for the stove and a fee for a set quantity of fuel.',
         'This stove is expected to last 2 years.',
         'This stove produces little or no smoke.',
         'This stove can balance a small, medium, or large pot whilst you are stirring food in it.',
         'Instructions on how to use this stove are provided at time of purchase and in one more in-home demonstration.',
         'This stove produces a medium amount of smoke.',
         'This stove can balance either a small or medium pot whilst you are stirring food in it but not a large pot.',
         'Electricity is needed to run this stove.')
seta <- c(2,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1,1,2,1,2,2)
setb <- c(2,2,2,2,2,2,2,2,2,2,2,1,2,1,1,1,2,2,2,1,1,1,2,2,2,2,2,2)
setc <- c(1,1,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1)
num <- data.frame(levels=lev, setA=seta, setB=setb, setC=setc)

# Counting Analysis
split_var <-  "compound" # Input the column name that you want to use to split here
setcount <- data.frame(table(stove[split_var]))
setcount <- setcount[setcount$Freq != 0,]

for (i in setcount[,1]){
  
  substove <- stove[stove[split_var]==i,]
  setnum <- data.frame(table(substove$my_rand11))
  for (m in 1:3){
    if (m %in% setnum$Var1){
      next
    }else{
      setnum <- data.frame(rbind(as.matrix(setnum),c(m,0)))
      setnum <- transform(setnum, Freq = as.numeric(as.character(Freq)))
    }
  }
  num_new <- data.frame(Levels=num$levels,
                        TimesShown=num$setA*setnum[setnum$Var1==1,]$Freq
                        +num$setB*setnum[setnum$Var1==2,]$Freq
                        +num$setC*setnum[setnum$Var1==3,]$Freq)
  
  best <- NA
  worst <- NA
  
  for (j in 8:67){
    
    # edit the columns
    edcol <- substring(as.vector(substove[,j]),4)
    
    if (j%%2 == 0){
      best <- c(best, edcol)
    }else{
      worst <- c(worst, edcol)
    }
  }
  
  b <- data.frame(table(best))
  colnames(b) <- c("Levels","TimesSelectedBest")
  w <- data.frame(table(worst))
  colnames(w) <- c("Levels", "TimesSelectedWorst")
  out <- merge(b, w, by="Levels", all.x = TRUE, all.y = TRUE)
  out <- out[out$Levels != "",]
  out <- merge(out, num_new, by="Levels", all.x = TRUE, all.y = TRUE)
  out[is.na(out)] <- 0
  out["BestCountProportion"] <- round(out$TimesSelectedBest/out$TimesShown,3)
  out["WorstCountProportion"] <- round(out$TimesSelectedWorst/out$TimesShown,3)
  out["Scores"] <- out$BestCountProportion - out$WorstCountProportion
  out <- out[order(-out$Scores),]
  
  filename <- paste("CountingAnalysis_",i,".csv", sep = "")
  write.csv(out, file = filename, quote = TRUE, row.names = FALSE)
}



############################################################
# check if 'answered fully', 'partially answered' or 'not answered'

stove <- read.csv('Cookstove.csv')
comment <- numeric(dim(stove)[1])

for (i in 1:dim(stove)[1]){
  best_prep <- substring(as.vector(stove$H0a),4)[i]
  worst_prep <- substring(as.vector(stove$H0b),4)[i]
  queset <- as.vector(stove$my_rand11)[i]
  
  if (is.na(queset)){
    comment[i] <- NA
  }else{
    if (best_prep != "" & worst_prep != ""){
      comment[i] <- "Answered Fully"
    }else if (best_prep == "" & worst_prep != ""){
      comment[i] <- "Partially Answered"
    }else if (best_prep != "" & worst_prep == ""){
      comment[i] <- "Partially Answered"
    }else{
      comment[i] <- "Not Answered"
    }
  }
  
}

stove["Comments"] <- comment
write.csv(stove, file = 'cookstoveWithComments.csv', quote = TRUE, row.names = FALSE)

sum(stove$Comments == "Answered Fully", na.rm = TRUE)
sum(stove$Comments == "Partially Answered", na.rm = TRUE)
sum(stove$Comments == "Not Answered", na.rm = TRUE)

