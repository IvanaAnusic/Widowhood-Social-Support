iCleanPersonalityBHPS <- function(persFile, newFile=paste("clean",persFile), nPath=pathWorking) {
  persData <- read.csv(paste (nPath,persFile, sep=""))
  names(persData) <- c("pid","a1","a2","a3", "c1", "c2", "c3", "e1", "e2", "e3", "n1","n2", "n3", "o1", "o2", "o3")
  persVars <- names(persData)[2:ncol(persData)]
  # i don't know why the other way didn't work... 
  for (i in 1:length(persVars)) {
    persData[,persVars[i]] <- recode(persData[,persVars[i]], '-999:0=NA; 8:999=NA')
  }
  cor(persData[2:ncol(persData)], use="pairwise.complete.obs")
  # recode a1, c2, e3, n3
  persData$a1 <- 8 - persData$a1
  persData$c2 <- 8 - persData$c2
  persData$e3 <- 8 - persData$e3
  persData$n3 <- 8 - persData$n3
  # compute scale means
  persData$pA <- rowMeans(persData[,c("a1","a2","a3")], na.rm=T)
  persData$pC <- rowMeans(persData[,c("c1","c2","c3")], na.rm=T)
  persData$pE <- rowMeans(persData[,c("e1","e2","e3")], na.rm=T)
  persData$pN <- rowMeans(persData[,c("n1","n2","n3")], na.rm=T)
  persData$pO <- rowMeans(persData[,c("o1","o2","o3")], na.rm=T)
  
  persData <- persData[,c("pid","pA","pC","pE","pN","pO")]
  
  write.csv(persData, paste(nPath,newFile,sep=""), row.names=F)
}

iCleanLifeSatBHPS <- function(lsFile, newFile=paste("clean",lsFile), nPath=pathWorking) {
  lsData <- read.csv (paste(nPath,lsFile,sep=""))
  lsVars <- paste("ls_", firstWaveOfLS:lastWaveOfLS, sep="")
  lsData[,lsVars] <- recode(lsData[,lsVars], "-999:0=NA; 8:999=NA")
  write.csv(lsData, paste(nPath,newFile,sep=""), row.names=F)
}

iCleanRelSatBHPS <- function(file, newFile=paste("clean",file), nPath=pathWorking) {
  d <- read.csv (paste(nPath,file,sep=""))
  vars <- paste("relsat_", firstWaveOfLS:lastWaveOfLS, sep="")
  d[,vars] <- recode(d[,vars], "-999:0=NA; 8:999=NA")
  write.csv(d, paste(nPath,newFile,sep=""), row.names=F)
}

iMatch <- function (c, e, controlVar) {
  # select only those in the control group who have at least two waves of data 
  print("Selecting only those who have two waves of data")
  cTemp <- aggregate(c, by=list(c$pid), FUN=sum)
  cTemp <- cTemp[cTemp[,controlVar]>1,]
  cTemp$twoWaves <- 1
  cTemp <- cTemp[,c("Group.1", "twoWaves")]
  names(cTemp) <- c("pid", "twoWaves")
  c <- merge(cTemp, c)
  # combine event & control files
  print("Combining event & control files")
  both <- merge(e, c, all=T)
  both$event <- ifelse(is.na(both[,controlVar]), 1, 0)   # is the person in the event group (1) or control group (0)
  both$control <- ifelse(both$event==0, 1, 0)     # is the person in the event group (0) or control group (1)
  agBoth <- both[,c("pid", "sex", "firstCasmin", "firstInc", "firstAge", "event", "control")]
  print("Aggregating to find means for each person, to plot and transform income")
  agBoth <- aggregate(agBoth, by=list(agBoth$pid), FUN=mean)
  agBoth$Group.1 <- NULL
  print(paste("Event people that are missing gender info:",nrow(agBoth[is.na(agBoth[,"sex"]) & agBoth$event==1,])))
  print(paste("Event people that are missing education info:",nrow(agBoth[is.na(agBoth[,"firstCasmin"]) & agBoth$event==1,])))
  print(paste("Event people that are missing income info:",nrow(agBoth[is.na(agBoth[,"firstInc"]) & agBoth$event==1,])))
  print(paste("Event people that are missing age info:",nrow(agBoth[is.na(agBoth[,"firstAge"]) & agBoth$event==1,])))
  print("plotting income distribution")
  plot(density(agBoth[!is.na(agBoth$firstInc),"firstInc"]))
  agBoth$lnInc <- log(agBoth$firstInc + 1)
  print("plotting distribution of ln(income)")
  plot(density(agBoth[!is.na(agBoth$lnInc),"lnInc"]))
  agBoth$cFirstAge <- scale(agBoth$firstAge, scale=F)
  agBoth$cLnInc <- scale(agBoth$lnInc, scale=F)
  print("Selecting only complete cases")
  comp <- agBoth[complete.cases(agBoth),]   # select only complete cases
  # matching
  print("Matching...")
  reg <- glm(control ~ sex + cFirstAge + I(cFirstAge^2) + as.factor(firstCasmin) + cLnInc, family=binomial(link="logit"), data=comp)
  pscores <- predict(reg, type="link")
  matches <- matching(z=comp$control, score=pscores)
  matched <- comp[matches$matched,]
  # tag these cases as ones to use in the analyses
  print("Tagging matches")
  matched$use <- 1
  matched2 <- matched[,c("pid", "use", "cFirstAge", "cLnInc")]
  # combine with original dataset, so  you'll know which cases to use
  print("Combining original & matched datasets")
  EventControl <- merge(both, matched2, all.x=T, by="pid")
  EventControl <- EventControl[!is.na(EventControl$use),]
  # make linear trend start at the first wave that they are in the study
  print("Creating a linear trend, react, and adapt variables")
  EventControl$lin <- EventControl$wave - EventControl$firstWave
  return(EventControl)
}

iConRA <- function (eventData, controlData, controlVar, prefix="") {
  EventControl <- iMatch(c=controlData, e=eventData, controlVar=controlVar)
  # set react and adapt to 0 for control group, leave it as is for event group
  EventControl$react <- ifelse(EventControl$event==1, EventControl[,paste(prefix,"React",sep="")], 0)
  EventControl$adapt <- ifelse(EventControl$event==1, EventControl[,paste(prefix,"Adapt",sep="")], 0)
  # analyses
  print("Running final regression on matched cases")
  RA <- lmer( ls ~ lin + control + react + adapt + 
    sex + cFirstAge + I(cFirstAge^2) + as.factor(firstCasmin) + cLnInc + 
    lin*sex + lin*cFirstAge + lin*I(cFirstAge^2) + lin*as.factor(firstCasmin) + lin*cLnInc + (react + adapt | pid), data=EventControl)
  RAnocovar <- lmer( ls ~ lin + control + react + adapt + (react + adapt | pid), data=EventControl)
  print("Plotting the means")
  meansAll <- aggregate(EventControl, by=list(EventControl$control, EventControl$lin), FUN=mean)
  meansPlot <- qplot(x=lin, y=ls, colour=control, data=meansAll, main=prefix)
  # return results: complete cases, matched cases, matching regression, final regression
  results <- list(EventControl=EventControl, RA=RA, RAnocovar=RAnocovar,meansPlot=meansPlot)
  return(results)
}


