##########################################################
# Counting Analysis

# Get the data
setwd('C:/Users/sliu44/Downloads')
stove <- read.csv('Cookstove.csv') # raw data file
stove <- stove[!is.na(stove['my_rand11']),] # only keep the respondents using BWS

# total number of times each level appearing
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


###############################################################################################
# Statistical Comparison
# input files are raw data file and the outputs of Counting Analysis

set.seed(100)
whichvar <- 'my_rand11' #'A8''compound''GHrespond' #input which variable to compare
cookstove <- read.csv('Cookstove.csv')
cookstove <- cookstove[! is.na(cookstove$my_rand11),]
levs <- unique(cookstove[,whichvar])

dat <- NA
for(i in levs){
  cafile <- read.csv(paste('CountingAnalysis_', i, '.csv', sep = ''))
  ca <- cafile[order(cafile$Levels),]
  colnames(ca) <- c("Levels",
                    paste("TimesSelectedBest.", i, sep = ''),
                    paste("TimesSelectedWorst.", i, sep = ''),
                    paste("TimesShown.", i, sep = ''),
                    paste("BestCountProportion.", i, sep = ''),
                    paste("WorstCountProportion.", i, sep = ''),
                    paste("Scores.", i, sep = ''))
  ca[paste('CompareLevel.',i, sep = '')] <- i
  dat <- data.frame(dat, ca)
}
dat <- dat[,-1]
k <- 2
rm <- c()
while(k <= length(levs)){
  rm <- c(rm, (k-1)*8+1)
  k <- k+1
}
dat <- dat[,-rm]


# Function to compute test statistic
pvalueb <- c()
pvaluew <- c()

for (i in 1:28){
  
  bestcount <- c()
  worstcount <- c()
  timesshown <- c()
  
  for(j in 1:length(levs)){
    bestcount <- c(bestcount, as.numeric(dat[i,c(2+7*(j-1))]))
    worstcount <- c(worstcount, as.numeric(dat[i,c(3+7*(j-1))]))
    timesshown <- c(timesshown, as.numeric(dat[i,c(4+7*(j-1))]))
  }
  
  if(sum(bestcount>=5)==length(levs)){
    pe <- sum(bestcount)/sum(timesshown)
    x2 <- sum((bestcount-pe*timesshown)^2/(pe*timesshown))
    pvalueb <- c(pvalueb, pchisq(x2, df = length(levs)-1, lower.tail = FALSE))
    
  }else{
    simset <- c(rep('S', sum(bestcount)), rep('N', sum(timesshown-bestcount)))
    test.stat <- c()
    p <- sum(bestcount)/sum(timesshown)
    for(si in 1:10000){
      simshuf <- sample(simset, length(simset))
      x2k <- c()
      st <- 0
      for(k in timesshown){
        set <- simshuf[(st+1):(st+k)]
        st <- st + k
        x2k <- c(x2k, sum(set == 'S'))
      }
      x2 <- sum((x2k - p*timesshown)^2/(p*timesshown))
      test.stat <- c(test.stat, x2)
    }
    obsp <- sum((bestcount - p*timesshown)^2/(p*timesshown))
    pvalueb <- c(pvalueb, mean(test.stat>obsp))
    
  }
  
  if(sum(worstcount>=5)==length(levs)){
    pe <- sum(worstcount)/sum(timesshown)
    x2 <- sum((worstcount-pe*timesshown)^2/(pe*timesshown))
    pvaluew <- c(pvaluew, pchisq(x2, df = length(levs)-1, lower.tail = FALSE))
    
  }else{
    simset <- c(rep('S', sum(worstcount)), rep('N', sum(timesshown-worstcount)))
    test.stat <- c()
    p <- sum(worstcount)/sum(timesshown)
    for(si in 1:10000){
      simshuf <- sample(simset, length(simset))
      x2k <- c()
      st <- 0
      for(k in timesshown){
        set <- simshuf[(st+1):(st+k)]
        st <- st + k
        x2k <- c(x2k, sum(set == 'S'))
      }
      x2 <- sum((x2k - p*timesshown)^2/(p*timesshown))
      test.stat <- c(test.stat, x2)
    }
    obsp <- sum((worstcount - p*timesshown)^2/(p*timesshown))
    pvaluew <- c(pvaluew, mean(test.stat>obsp))
  }
}
sigb <- ifelse(pvalueb < 0.05/3, '*', '')
sigw <- ifelse(pvaluew < 0.05/3, '*', '')
dat["pvalue.Best"] <- pvalueb
dat["sig.Best"] <- sigb
dat["pvalue.Worst"] <- pvaluew
dat["sig.Worst"] <- sigw


## pairwise test
pairbest <- matrix(0, ncol = length(levs)^2-length(levs))
for(i in 1:28){
  onerow <- c()
  onename <- c()
  
  for(m in levs){
    for(n in levs){
      if(m<n){
        name <- make.names(paste(m,n,'BestPairwise',sep = '.'))
        onename <- c(onename, paste('pvalue',name, sep = '.'),
                     paste('sig', name, sep = '.'))
        
        pairb <- make.names(paste('TimesSelectedBest', c(m,n), sep = '.'))
        pairt <- make.names(paste('TimesShown', c(m,n), sep = '.'))
        
        if(sum(dat[i,pairb]>=5) == 2 &
           sum((dat[i,pairt]-dat[i,pairb])>=5)==2){
          x <- as.numeric(dat[i,pairb])
          n <- as.numeric(dat[i,pairt])
          p <- sum(x)/sum(n)
          zn <- diff(x/n)
          zd <- sqrt(p*(1-p)*sum(1/n))
          z <- zn/zd
          pvalue <- 2*pnorm(abs(z), lower.tail = FALSE)
          sig <- ifelse(pvalue<0.05/3, '*', '')
          onerow <- c(onerow, pvalue, sig)
        }else{
          x <- as.numeric(dat[i,pairb])
          n <- as.numeric(dat[i,pairt])
          simset <- c(rep('S', sum(x)), rep('N', sum(n-x)))
          test.stat <- c()
          for(si in 1:10000){
            simshuf <- sample(simset, length(simset))
            set1 <- simshuf[1:n[1]]
            set2 <- simshuf[(n[1]+1):(n[1]+n[2])]
            test.stat <- c(test.stat, mean(set1=='S')-mean(set2=='S'))
          }
          obsp <- abs(diff(x/n))
          pvalue <- mean(test.stat>obsp)
          sig <- ifelse(pvalue<0.05/3, '*', '')
          onerow <- c(onerow, pvalue, sig)
        }
        
      }
    }
  }
  pairbest <- rbind(pairbest,onerow)
}

pairbest <- pairbest[-1,]
colnames(pairbest) <- onename
dat <- data.frame(dat, pairbest)

pairworst <- matrix(0, ncol = length(levs)^2-length(levs))
for(i in 1:28){
  onerow <- c()
  onename <- c()
  
  for(m in levs){
    for(n in levs){
      if(m<n){
        name <- make.names(paste(m,n,'WorstPairwise',sep = '.'))
        onename <- c(onename, paste('pvalue',name, sep = '.'),
                     paste('sig', name, sep = '.'))
        
        pairw <- make.names(paste('TimesSelectedWorst', c(m,n), sep = '.'))
        pairt <- make.names(paste('TimesShown', c(m,n), sep = '.'))
        
        if(sum(dat[i,pairw]>=5) == 2 &
           sum((dat[i,pairt]-dat[i,pairw])>=5)==2){
          x <- as.numeric(dat[i,pairw])
          n <- as.numeric(dat[i,pairt])
          p <- sum(x)/sum(n)
          zn <- diff(x/n)
          zd <- sqrt(p*(1-p)*sum(1/n))
          z <- zn/zd
          pvalue <- 2*pnorm(abs(z), lower.tail = FALSE)
          sig <- ifelse(pvalue<0.05/3, '*', '')
          onerow <- c(onerow, pvalue, sig)
        }else{
          x <- as.numeric(dat[i,pairw])
          n <- as.numeric(dat[i,pairt])
          simset <- c(rep('S', sum(x)), rep('N', sum(n-x)))
          test.stat <- c()
          for(si in 1:10000){
            simshuf <- sample(simset, length(simset))
            set1 <- simshuf[1:n[1]]
            set2 <- simshuf[(n[1]+1):(n[1]+n[2])]
            test.stat <- c(test.stat, mean(set1=='S')-mean(set2=='S'))
          }
          obsp <- abs(diff(x/n))
          pvalue <- mean(test.stat>obsp)
          sig <- ifelse(pvalue<0.05/3, '*', '')
          onerow <- c(onerow, pvalue, sig)
        }
        
      }
    }
  }
  pairworst <- rbind(pairworst,onerow)
}
pairworst <- pairworst[-1,]
colnames(pairworst) <- onename
dat <- data.frame(dat, pairworst)

write.csv(dat, 
          file = paste('StatisticalComparison_', whichvar, '.csv', sep = ''), 
          quote=TRUE, row.names = FALSE)