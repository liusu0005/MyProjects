set.seed(100)
setwd('E:/NCSUlessons/ST/ST542/StoveProject')
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



