library(lme4)
library(ggplot2)
library(arm)

source(".../Nonlinear functions.R")
pathWorking <- ".../datafilesHILDA/" # path where all the new files are stored and worked with

data <- read.csv(paste(pathWorking, "allData.csv", sep=""))
names(data)

firstYearOfStudy <- 2001


# GET SOCIAL SUPPORT IN YEARS BEFORE, YEAR OF, AND YEARS AFTER WIDOWHOOD
# get widowhood year
wyear <- data[,c("pid","wyear")]
wyear <- aggregate(data, by=list(data$pid), FUN=mean, na.rm=T)
wyear[1:5,]
wyear <- wyear[,c("pid","wyear")]
# get social support
ss <- read.csv(paste(pathWorking, "clean ssWide.csv", sep=""))
ss[1:5,]
names(ss) <- c("pid",1,2,3,4,5,6,7,8,9,10)
ssLong <- melt(ss, id.var="pid")
ssLong[1:5,]
names(ssLong) <- c("pid","wave","ss")
ssLong$wave <- as.numeric(levels(ssLong$wave))[ssLong$wave]
ssPeriod <- merge(wyear, ssLong, by=c("pid"), all.x=T)
# figure means for mean ss in years before, year of, and years after widowhood
ssPeriod[1:5,]
ssPeriod$year <- ssPeriod$wave + (firstYearOfStudy-1)
ssPeriod <- ssPeriod[!is.na(ssPeriod$ss),]
ssPeriod$period <- ifelse(ssPeriod$year < ssPeriod$wyear, 0, ifelse(ssPeriod$year == ssPeriod$wyear, 1, ifelse(ssPeriod$year > ssPeriod$wyear, 2, NA)))
ssPeriod <- aggregate(ssPeriod, by=list(ssPeriod$pid, ssPeriod$period), FUN=mean)
ssPeriod[1:5,]
baseSS <- ssPeriod[ssPeriod$period==0, c("pid","ss")]
reactSS <- ssPeriod[ssPeriod$period==1, c("pid","ss")]
adaptSS <- ssPeriod[ssPeriod$period==2, c("pid","ss")]
names(baseSS) <- c("pid", "ssBase")
names(reactSS) <- c("pid", "ssReact")
names(adaptSS) <- c("pid", "ssAdapt")
# combine with life satisfaction data
data <- merge(data, baseSS, all.x=T)
data <- merge(data, reactSS, all.x=T)
data <- merge(data, adaptSS, all.x=T)
data[1:50,]
# keep only variables you will use
data <- data[,c("pid","wyear","wave","ls","wSeq","wBef","wAft","wReact","wAdapt","sex","yrBirth","ageWid","ssBase","ssReact","ssAdapt")]

agData <- aggregate(data, by=list(data$pid), FUN=mean)
mean(agData[,c("ssBase","ssReact","ssAdapt")], na.rm=T)
sd(agData[,c("ssBase","ssReact","ssAdapt")], na.rm=T)
cor(agData[,c("ssBase","ssReact","ssAdapt")], use="pairwise.complete.obs")
nrow(agData)
nrow(agData[!is.na(agData$ssBase),])
nrow(agData[!is.na(agData$ssReact),]) 
nrow(agData[!is.na(agData$ssAdapt),])
nrow(agData[!is.na(agData$ssBase) & !is.na(agData$ssReact),])
nrow(agData[!is.na(agData$ssBase) & !is.na(agData$ssAdapt),])
nrow(agData[!is.na(agData$ssReact) & !is.na(agData$ssAdapt),])
nrow(agData[!is.na(agData$ssBase) & !is.na(agData$ssReact) & !is.na(agData$ssAdapt),]) 
t.test(agData$ssBase,agData$ssReact, paired=T)
t.test(agData$ssBase,agData$ssAdapt, paired=T)
t.test(agData$ssReact,agData$ssAdapt, paired=T)
nrow(agData)
table(agData$sex,useNA="ifany")
mean(agData$ageWid,na.rm=T)
sd(agData$ageWid,na.rm=T)
table(agData$ageWid, useNA="ifany")
# center variables
agData$cAge <- scale(agData$ageWid, scale=F)
agData$cSSbase <- scale(agData$ssBase, scale=F)
agData$cSSreact <- scale(agData$ssReact, scale=F)
agData$cSSadapt <- scale(agData$ssAdapt, scale=F)
agData[1:5,]
nrow(agData)
agData <- agData[,c("pid","cAge", "cSSbase", "cSSreact","cSSadapt")]
data <- merge(data, agData, by="pid", all=T)
nrow(data)
data[1:5,]


# --------------------------------------------- nonlinear model ------------------------------------------ #

# base social support + sex + age
sBsexageR <- c(asympBef=8.485, changeBef=.755, reactDiff=-.894, asympDiff=-.516, changeAft=.522, sexAB=.035, sexRD=.011, sexAD=-.260, ageAB=.018, ageRD=.008, ageAD=-.003, baseAB=.656, baseRD=-.058, baseAD=-.302)
nlBsexageR <- nlmer( ls ~ iAsympDiffR_3modG(wSeq, wBef, wAft, sex, cAge, cSSbase, asympBef, changeBef, reactDiff, asympDiff, changeAft, sexAB, sexRD, sexAD, ageAB, ageRD, ageAD, baseAB, baseRD, baseAD) ~ (asympBef + reactDiff + asympDiff|pid), data=data, start=sBsexageR, verbose=T)

# react social support + sex + age
sRsexageR <- c(asympBef=8.540, changeBef=.807, reactDiff=-.955, asympDiff=-.513, changeAft=.502, sexAB=.008, sexRD=-.003, sexAD=-.283, ageAB=.017, ageRD=.005, ageAD=-.004, reactAB=.260, reactRD=.292, reactAD=.222)
nlRsexageR <- nlmer( ls ~ iAsympDiffR_3modG(wSeq, wBef, wAft, sex, cAge, cSSreact, asympBef, changeBef, reactDiff, asympDiff, changeAft, sexAB, sexRD, sexAD, ageAB, ageRD, ageAD, reactAB, reactRD, reactAD) ~ (asympBef + reactDiff + asympDiff|pid), data=data, start=sRsexageR, verbose=T)

# adapt social support + sex + age
sAsexageR <- c(asympBef=8.571, changeBef=.741, reactDiff=-.863, asympDiff=-.542, changeAft=.531, sexAB=.038, sexRD=.058, sexAD=-.186, ageAB=.018, ageRD=.012, ageAD=-.004, adaptAB=.355, adaptRD=.308, adaptAD=.491)
nlAsexageR <- nlmer( ls ~ iAsympDiffR_3modG(wSeq, wBef, wAft, sex, cAge, cSSadapt, asympBef, changeBef, reactDiff, asympDiff, changeAft, sexAB, sexRD, sexAD, ageAB, ageRD, ageAD, adaptAB, adaptRD, adaptAD) ~ (asympBef + reactDiff + asympDiff|pid), data=data, start=sAsexageR, verbose=T)


nlResultsList <- c("nlBsexageR","nlRsexageR","nlAsexageR")
nlResults <- iTableResults(nlResultsList)
nlResults

# plot
sdBase <- sd(agData$cSSbase, na.rm=T)
sdReact <- sd(agData$cSSreact, na.rm=T)
sdAdapt <- sd(agData$cSSreact, na.rm=T)
xB <- fixef(nlBsexageR)
xR <- fixef(nlRsexageR)
xA <- fixef(nlAsexageR)
pB <- data.frame(seq=seq(-5,5, by=.5), yr=seq(0,20, by=1))
for (i in 1:length(xB)) {
  pB[,names(xB)[i]] <- xB[i]
}
pB
pB$bef <- ifelse(pB$seq < 0, 1, 0)
pB$aft <- ifelse(pB$seq >= 0, 1, 0)
pR <- data.frame(seq=seq(-5,5, by=.5), yr=seq(0,20, by=1))
for (i in 1:length(xR)) {
  pR[,names(xR)[i]] <- xR[i]
}
pR
pR$bef <- ifelse(pR$seq < 0, 1, 0)
pR$aft <- ifelse(pR$seq >= 0, 1, 0)
pA <- data.frame(seq=seq(-5,5, by=.5), yr=seq(0,20, by=1))
for (i in 1:length(xA)) {
  pA[,names(xA)[i]] <- xA[i]
}
pA
pA$bef <- ifelse(pA$seq < 0, 1, 0)
pA$aft <- ifelse(pA$seq >= 0, 1, 0)

pB$lsLowSSbase <-   pB$bef * ((pB$asympBef + pB$baseAB*(-1*sdBase)) + (pB$reactDiff + pB$baseRD*(-1*sdBase))*(1/(1-(pB$changeBef)))^pB$seq) +
  pB$aft * (((pB$asympBef + pB$baseAB*(-1*sdBase))+(pB$reactDiff + pB$baseRD*(-1*sdBase))+(pB$asympDiff+pB$baseAD*(-1*sdBase))) + 
  (0-(pB$asympDiff+pB$baseAD*(-1*sdBase)))*(1-(pB$changeAft))^pB$seq)
pB$lsAvgSSbase <-   pB$bef * ((pB$asympBef + pB$baseAB*(0*sdBase)) + (pB$reactDiff + pB$baseRD*(0*sdBase))*(1/(1-(pB$changeBef)))^pB$seq) +
  pB$aft * (((pB$asympBef + pB$baseAB*(0*sdBase))+(pB$reactDiff + pB$baseRD*(0*sdBase))+(pB$asympDiff+pB$baseAD*(0*sdBase))) + 
  (0-(pB$asympDiff+pB$baseAD*(0*sdBase)))*(1-(pB$changeAft))^pB$seq)
pB$lsHighSSbase <-   pB$bef * ((pB$asympBef + pB$baseAB*(1*sdBase)) + (pB$reactDiff + pB$baseRD*(1*sdBase))*(1/(1-(pB$changeBef)))^pB$seq) +
  pB$aft * (((pB$asympBef + pB$baseAB*(1*sdBase))+(pB$reactDiff + pB$baseRD*(1*sdBase))+(pB$asympDiff+pB$baseAD*(1*sdBase))) + 
  (0-(pB$asympDiff+pB$baseAD*(1*sdBase)))*(1-(pB$changeAft))^pB$seq)

pR$lsLowSSreact <-   pR$bef * ((pR$asympBef + pR$reactAB*(-1*sdReact)) + (pR$reactDiff + pR$reactRD*(-1*sdReact))*(1/(1-(pR$changeBef)))^pR$seq) +
  pR$aft * (((pR$asympBef + pR$reactAB*(-1*sdReact))+(pR$reactDiff + pR$reactRD*(-1*sdReact))+(pR$asympDiff+pR$reactAD*(-1*sdReact))) + 
  (0-(pR$asympDiff+pR$reactAD*(-1*sdReact)))*(1-(pR$changeAft))^pR$seq)
pR$lsAvgSSreact <-   pR$bef * ((pR$asympBef + pR$reactAB*(0*sdReact)) + (pR$reactDiff + pR$reactRD*(0*sdReact))*(1/(1-(pR$changeBef)))^pR$seq) +
  pR$aft * (((pR$asympBef + pR$reactAB*(0*sdReact))+(pR$reactDiff + pR$reactRD*(0*sdReact))+(pR$asympDiff+pR$reactAD*(0*sdReact))) + 
  (0-(pR$asympDiff+pR$reactAD*(0*sdReact)))*(1-(pR$changeAft))^pR$seq)
pR$lsHighSSreact <-   pR$bef * ((pR$asympBef + pR$reactAB*(1*sdReact)) + (pR$reactDiff + pR$reactRD*(1*sdReact))*(1/(1-(pR$changeBef)))^pR$seq) +
  pR$aft * (((pR$asympBef + pR$reactAB*(1*sdReact))+(pR$reactDiff + pR$reactRD*(1*sdReact))+(pR$asympDiff+pR$reactAD*(1*sdReact))) + 
  (0-(pR$asympDiff+pR$reactAD*(1*sdReact)))*(1-(pR$changeAft))^pR$seq)

pA$lsLowSSadapt <-   pA$bef * ((pA$asympBef + pA$adaptAB*(-1*sdAdapt)) + (pA$reactDiff + pA$adaptRD*(-1*sdAdapt))*(1/(1-(pA$changeBef)))^pA$seq) +
  pA$aft * (((pA$asympBef + pA$adaptAB*(-1*sdAdapt))+(pA$reactDiff + pA$adaptRD*(-1*sdAdapt))+(pA$asympDiff+pA$adaptAD*(-1*sdAdapt))) + 
  (0-(pA$asympDiff+pA$adaptAD*(-1*sdAdapt)))*(1-(pA$changeAft))^pA$seq)
pA$lsAvgSSadapt <-   pA$bef * ((pA$asympBef + pA$adaptAB*(0*sdAdapt)) + (pA$reactDiff + pA$adaptRD*(0*sdAdapt))*(1/(1-(pA$changeBef)))^pA$seq) +
  pA$aft * (((pA$asympBef + pA$adaptAB*(0*sdAdapt))+(pA$reactDiff + pA$adaptRD*(0*sdAdapt))+(pA$asympDiff+pA$adaptAD*(0*sdAdapt))) + 
  (0-(pA$asympDiff+pA$adaptAD*(0*sdAdapt)))*(1-(pA$changeAft))^pA$seq)
pA$lsHighSSadapt <-   pA$bef * ((pA$asympBef + pA$adaptAB*(1*sdAdapt)) + (pA$reactDiff + pA$adaptRD*(1*sdAdapt))*(1/(1-(pA$changeBef)))^pA$seq) +
  pA$aft * (((pA$asympBef + pA$adaptAB*(1*sdAdapt))+(pA$reactDiff + pA$adaptRD*(1*sdAdapt))+(pA$asympDiff+pA$adaptAD*(1*sdAdapt))) + 
  (0-(pA$asympDiff+pA$adaptAD*(1*sdAdapt)))*(1-(pA$changeAft))^pA$seq)

names(pB)
pB <- pB[,c("seq","lsLowSSbase","lsAvgSSbase","lsHighSSbase")]
pR <- pR[,c("seq","lsLowSSreact","lsAvgSSreact","lsHighSSreact")]
pA <- pA[,c("seq","lsLowSSadapt","lsAvgSSadapt","lsHighSSadapt")]
p <- merge(pB, pR, by="seq", all=T)
p <- merge(p, pA, by="seq", all=T)
pLow <- p[,c("seq","lsLowSSbase","lsLowSSreact","lsLowSSadapt")]
pAvg <- p[,c("seq","lsAvgSSbase","lsAvgSSreact","lsAvgSSadapt")]
pHigh <- p[,c("seq","lsHighSSbase","lsHighSSreact", "lsHighSSadapt")]
names(pLow) <- c("seq","lsSSbase","lsSSreact","lsSSadapt")
names(pAvg) <- c("seq","lsSSbase","lsSSreact","lsSSadapt")
names(pHigh) <- c("seq","lsSSbase","lsSSreact","lsSSadapt")
pLow$group <- "Low"
pAvg$group <- "Average"
pHigh$group <- "High"
p <- rbind(pLow, pAvg, pHigh)

plotBase <- ggplot(data=p, aes(x=seq, y=lsSSbase)) +
  geom_line(data=p, aes(x=seq, y=lsSSbase, linetype=group,group=group)) +
  scale_linetype_manual(values=c(1,2,3)) +
  opts(title="HILDA - Support during baseline",legend.position="none")+
  scale_y_continuous("Life Satisfaction",limits=c(6.5,9.5)) + 
  scale_x_continuous("Year", limits=c(-5.1,5.1), breaks=seq(-5,5,by=1))+ 
  geom_text(data=p[p$seq==-5,],aes(label=group),hjust=-.1,vjust=-.5) +
  opts(
    panel.grid.major=theme_blank(),
    panel.grid.minor=theme_blank(),
    panel.background = theme_blank(),
    axis.line = theme_segment())
plotReact <- ggplot(data=p, aes(x=seq, y=lsSSreact)) +
  geom_line(data=p, aes(x=seq, y=lsSSreact, linetype=group,group=group)) +
  scale_linetype_manual(values=c(1,2,3)) +
  opts(title="HILDA - Support during reaction",legend.position="none")+
  scale_y_continuous("Life Satisfaction",limits=c(6.5,9.5)) + 
  scale_x_continuous("Year", limits=c(-5.1,5.1), breaks=seq(-5,5,by=1))+ 
  geom_text(data=p[p$seq==-5,],aes(label=group),hjust=-.1,vjust=-.5) +
  opts(
    panel.grid.major=theme_blank(),
    panel.grid.minor=theme_blank(),
    panel.background = theme_blank(),
    axis.line = theme_segment())
plotAdapt <- ggplot(data=p, aes(x=seq, y=lsSSadapt)) +
  geom_line(data=p, aes(x=seq, y=lsSSadapt, linetype=group,group=group)) +
  scale_linetype_manual(values=c(1,2,3)) +
  opts(title="HILDA - Support during adaptation",legend.position="none")+
  scale_y_continuous("Life Satisfaction",limits=c(6.5,9.5)) + 
  scale_x_continuous("Year", limits=c(-5.1,5.1), breaks=seq(-5,5,by=1))+ 
  geom_text(data=p[p$seq==-5,],aes(label=group),hjust=-.1,vjust=-.5) +
  opts(
    panel.grid.major=theme_blank(),
    panel.grid.minor=theme_blank(),
    panel.background = theme_blank(),
    axis.line = theme_segment())
plotBase
plotReact
plotAdapt


png(paste(pathWorking,"HILDA figure.png",sep=""),width=12,height=4,units="in",res=200)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(plotBase,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(plotReact,vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(plotAdapt,vp=viewport(layout.pos.row=1,layout.pos.col=3))
dev.off()
