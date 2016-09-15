source("C:/Users/Ivana/Documents/My Dropbox/Ivana/My R Functions/LifeEventsFunctions.r")

pathWorkingGSOEP <- ".../datafilesGSOEP/"
pathWorkingBHPS <- "...S/datafilesBHPS/"
pathWorkingHILDA <- ".../datafilesHILDA/"

# ===================================================================================
# -------------------------   BHPS   ------------------------------------------------
# ===================================================================================

# SOCIAL SUPPORT
ssB <- read.csv(paste(pathWorkingBHPS, "ssLong.csv", sep=""))
ssB[1:5,]
ssvarsB <- c("ss1","ss2","ss3","ss4","ss5")
ssB[, ssvarsB] <- lapply (ssB[, ssvarsB], iRecode, "lo:0=NA; 3=0; 1=1; 2=2; 4:hi=NA")
ssB$ss <- rowMeans(ssB[,c("ss1","ss2","ss3","ss4","ss5")], na.rm=T)
table(ssB$ss, useNA="ifany")
ssB <- ssB[,c("pid","wave","ss")]
ssB <- ssB[!is.na(ssB$ss),]
ssB[1:5,]
ssB <- aggregate(ssB, by=list(ssB$pid), FUN=mean)
ssB <- ssB[,c("pid","ss")]

# SEX & AGE
demoB <- read.csv(paste(pathWorkingBHPS, "clean demoWide.csv", sep=""))
demoB[1:5,]

# SELF-REPORTED HEALTH
healthB <- read.csv(paste(pathWorkingBHPS, "healthLong.csv", sep=""))
healthB[1:5,]
table(healthB$health, useNA="ifany")
healthB$health <- recode(healthB$health, "lo:0=NA; 1=5; 2=4; 3=3; 4=2; 5=1; 6:hi=NA")
healthB <- healthB[!is.na(healthB$health),]
healthB <- aggregate(healthB, by=list(healthB$pid), FUN=mean)
healthB <- healthB[,c("pid","health")]

# RELATIONSHIP SATISFACTION
relB <- read.csv(paste(pathWorkingBHPS, "relsatLong.csv", sep=""))
relB[1:5,]
table(relB$relsat, useNA="ifany")
relB$relsat <- recode(relB$relsat, "lo:0=NA; 8:hi=NA")
relB <- relB[!is.na(relB$relsat),]
relB <- aggregate(relB, by=list(relB$pid), FUN=mean)
relB <- relB[,c("pid","relsat")]

# COMBINE AND CORRELATE
ssAllB <- merge(ssB, demoB, by="pid", all.x=T)
ssAllB <- merge(ssAllB, healthB, by="pid", all.x=T)
ssAllB <- merge(ssAllB, relB, by="pid", all.x=T)
ssAllB[1:5,]
cor(ssAllB[,c("ss","sex","yrBirth","health","relsat")], use="pairwise.complete.obs")




# ===================================================================================
# -------------------------   GSOEP   -----------------------------------------------
# ===================================================================================

# SOCIAL SUPPORT
ssG1 <- read.csv(paste(pathWorkingGSOEP, "ssLong.csv", sep=""))
ssG1[1:5,]
ssG2 <- read.csv(paste(pathWorkingGSOEP, "ssEastLong.csv", sep=""))
ssG2[1:5,]
table(ssG1$ss, useNA="ifany")
table(ssG2$ssEast, useNA="ifany")
table(ssG1$wave, useNA="ifany")
table(ssG2$wave, useNA="ifany")
names(ssG2) <- c("pid","wave","ss")
ssG <- merge(ssG1, ssG2, all=T)
nrow(ssG1)
nrow(ssG2)
nrow(ssG)
ssG[1:5,]
ssG$ss <- recode(ssG$ss, "lo:0=NA; 1=3; 2=2; 3=1; 4=0; 5:hi=NA")
table(ssG$ss, useNA="ifany")
ssG <- ssG[!is.na(ssG$ss),]
ssG <- aggregate(ssG, by=list(ssG$pid), FUN=mean)
ssG <- ssG[,c("pid","ss")]

# SEX & AGE
demoG <- read.csv(paste(pathWorkingGSOEP, "clean demoWide.csv", sep=""))
demoG[1:5,]

# SELF-REPORTED HEALTH
healthG <- read.csv(paste(pathWorkingGSOEP, "healthLong.csv", sep=""))
healthG[1:5,]
table(healthG$health, useNA="ifany")
healthG$health <- recode(healthG$health, "lo:0=NA; 1=5; 2=4; 3=3; 4=2; 5=1; 6:hi=NA")
healthG <- healthG[!is.na(healthG$health),]
healthG <- aggregate(healthG, by=list(healthG$pid), FUN=mean)
healthG <- healthG[,c("pid","health")]

# COMBINE AND CORRELATE
ssAllG <- merge(ssG, demoG, by="pid", all.x=T)
ssAllG <- merge(ssAllG, healthG, by="pid", all.x=T)
ssAllG[1:5,]
cor(ssAllG[,c("ss","sex","yrBirth","health")], use="pairwise.complete.obs")

# ===================================================================================
# -------------------------   HILDA   ------------------------------------------------
# ===================================================================================

# SOCIAL SUPPORT
ssH <- read.csv(paste(pathWorkingHILDA, "ssLong.csv", sep=""))
ssH[1:5,]
table(ssH$ss1, useNA="ifany")
ssvarsH <- paste("ss",1:10,sep="")
# recode all NAs
ssH[, ssvarsH] <- lapply (ssH[, ssvarsH], iRecode, "lo:0=NA; 8:hi=NA")
# then, reverse score variables 1, 4, 5, 7, 10
ssvarsHr <- c("ss1","ss4","ss5","ss7","ss10")
ssH[, ssvarsHr] <- lapply (ssH[, ssvarsHr], iRecode, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
ssH$ss <- rowMeans(ssH[, ssvarsH], na.rm=T)
ssH <- ssH[,c("pid","wave","ss")]
table(ssH$ss, useNA="ifany")
ssH <- ssH[!is.na(ssH$ss),]
ssH <- aggregate(ssH, by=list(ssH$pid), FUN=mean)
ssH <- ssH[,c("pid","ss")]

# SEX & AGE
demoH <- read.csv(paste(pathWorkingHILDA, "clean demoWide.csv", sep=""))
demoH[1:5,]

# SELF-REPORTED HEALTH
healthH <- read.csv(paste(pathWorkingHILDA, "healthLong.csv", sep=""))
healthH[1:5,]
table(healthH$health, useNA="ifany")
healthH$health <- recode(healthH$health, "lo:0=NA; 1=5; 2=4; 3=3; 4=2; 5=1; 6:hi=NA")
healthH <- healthH[!is.na(healthH$health),]
healthH <- aggregate(healthH, by=list(healthH$pid), FUN=mean)
healthH <- healthH[,c("pid","health")]

# RELATIONSHIP SATISFACTION
relH <- read.csv(paste(pathWorkingHILDA, "relsatLong.csv", sep=""))
relH[1:5,]
table(relH$relsat, useNA="ifany")
relH$relsat <- recode(relH$relsat, "lo:-1=NA; 11:hi=NA")
relH <- relH[!is.na(relH$relsat),]
relH <- aggregate(relH, by=list(relH$pid), FUN=mean)
relH <- relH[,c("pid","relsat")]
  
# COMBINE AND CORRELATE
ssAllH <- merge(ssH, demoH, by="pid", all.x=T)
ssAllH <- merge(ssAllH, healthH, by="pid", all.x=T)
ssAllH <- merge(ssAllH, relH, by="pid", all.x=T)
ssAllH[1:5,]
cor(ssAllH[,c("ss","sex","yrBirth","health","relsat")], use="pairwise.complete.obs")


