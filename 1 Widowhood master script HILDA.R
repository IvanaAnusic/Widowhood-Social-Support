library(psych)
library(foreign)
library(lme4)
library(Hmisc)
library(car)
library(reshape2)

# ---------------------------- load functions ------------------------------- #
source(".../My R Functions/LifeEventsFunctions.r")
source(".../My R Functions/LifeEventsFunctionsHILDa.r")

# ------------------ THESE ARE CONSTANT IN A PROJECT ------------------ #
pathOriginalData <- ".../Panel Data/HILDA10/STATA/"     # path of original panel data files
pathWorking <- ".../datafilesHILDA/"

firstYearOfStudy <- 2001         # first year of study 
lastYearOfStudy <- 2010          # last year of study
lastWaveOfStudy <- 10             # last wave of study (i.e., wave number in last year of study) (first wave of study is always 1)
firstYearOfLS <- 2001   # first year that life satisfaction is collected (e.g., for BHPS life satisfaction, this is 1996)
lastYearOfLS <- lastYearOfStudy  # last year that life satisfaction is collected
firstWaveOfLS <- 1               # first wave that life satisfaction is collected (e.g., for BHPS life satisfaction, this is 6)
lastWaveOfLS <- lastWaveOfStudy  # last wave that life satisfaction is collected

# these are necessary for iGetVariables()
originalDataFile <- "Rperson_$100c.dta"  # name of original data file (from which to pull data)
oldID <- "xwaveid"                      # cross-wave variable, indicates person id
betaFileName <- ""                  # this is used for BETA waves of SHP; set this to "" if there is no beta file
betaYearNumber <- ""                # this is the two digit year of the beta file, in character format
charsToSub <- "\\$"                 # placeholder for wave or year number in file/variable names (e.g., $$ in nchild$$)
# wFile = number or character that indicates wave/year in file names
# wVar = number or character that indicates wave/year in variable names
# these have to be in character format
wFile <- paste(letters[1:lastWaveOfStudy]) 
wVar <- wFile           

# these are necessary for iSelectSample()
# define constants
married <- 10
widowed <- 10000
other <- 888
noresp <- 0
# change constants

same <- 0
NA_married <- married - noresp
married_NA <- noresp - married
NA_widowed <- widowed - noresp
married_widowed <- widowed - married
widowed_NA <- noresp - widowed
widowed_married <- married - widowed

# these are the original values (in the raw datafiles)
origMarried <- 1
origWidowed <- 5

# recoding syntax (recode NA to 0):
widowhoodRecode <- paste("-999:0=", noresp, "; ", origMarried, "=", married, "; ", origWidowed, "=", widowed, "; NA=", noresp, "; else=", other, sep="")
# ----------------------------------------------------------------------------- #


# ---------------- Pull up all variables from original data files --------------------------------------------------- #
ssVarsToPull <- c("$lssupac","$lssupcd","$lssuplf","$lssuplt","$lssupnh","$lssuppi","$lssuppv","$lssupsh","$lssuptp","$lssupvl")
ssVarsToSave <- c("ss1","ss2","ss3","ss4","ss5","ss6","ss7","ss8","ss9","ss10")

iGetVariables(varsToPull="$mrcurr", varsToSave="marstat", nDataFile="$mar.csv", longfilename="marLong.csv", widefilename="marWide.csv", firstWaveV=1)  # 1=legally married, 2=de facto, 3=separated, 4=divorced, 5=widowed, 6=never married and not de facto
iGetVariables(varsToPull="$losat", varsToSave="ls", nDataFile="$ls.csv", longfilename="lsLong.csv", widefilename="lsWide.csv", firstWaveV=firstWaveOfLS)  
iGetVariables(varsToPull=ssVarsToPull, varsToSave=ssVarsToSave, nDataFile="$ss.csv", longfilename="ssLong.csv", widefilename="ssWide.csv", firstWaveV=firstWaveOfLS)
iGetVariables(varsToPull="$hgage", varsToSave="age", nDataFile="$age.csv", longfilename="ageLong.csv", widefilename="ageWide.csv", firstWaveV=1)

# to correlate with social support
iGetVariables(varsToPull="$gh1", varsToSave="health", nDataFile="$health.csv", longfilename="healthLong.csv", widefilename="healthWide.csv", firstWaveV=1) # self-reported health
iGetVariables(varsToPull="$lsrelsp", varsToSave="relsat", nDataFile="$relsat.csv", longfilename="relsatLong.csv", widefilename="relsatWide.csv", firstWaveV=1) # satisfaction with partner


# ---------------- select each sample and figure out year of the event --------------------------------------------------- #

iSelectSample (sample="widowhood", firstWaveV=1, phase1="married", phase2="widowed", varStem="marstat_", originalFile="marWide.csv", saveFile="WidowhoodYear.csv")


# ---------------- clean up and recode social support and life satisfaction data; compute social support variables  --------------------------------------------------- #
iCleanLifeSatHILDA(lsFile="lsWide.csv")

age <- read.csv(paste(pathWorking, "ageLong.csv", sep=""))
age$age <- recode(age$age, "lo:-1=NA")
age <- age[!is.na(age$age),]
agAge <- aggregate(age, by=list(age$pid), FUN=min)
agAge$yrBirth <- firstYearOfStudy-1 + agAge$wave - agAge$age
agAge <- agAge[,c("pid","yrBirth")]

demo <- read.dta(paste(pathOriginalData, "Master_j100c.dta", sep=""), convert.factors=F)
names(demo) <- tolower(names(demo))
demo <- demo[,c("xwaveid", "sex")]
table(demo$sex, useNA="ifany")
# sex: 1=male, 2=female; change to -1=female, 1=male
demo$sex <- recode(demo$sex, "1=1; 2=-1; else=NA")
names(demo) <- c("pid", "sex")
demo$pid <- as.integer(demo$pid)
# combine sex and age
demo <- merge(demo, agAge, by="pid", all=T)
demo <- demo[,c("pid","sex","yrBirth")]
write.csv(demo, file=paste(pathWorking, "clean demoWide.csv", sep=""), row.names=F)

# SOCIAL SUPPORT
d <- read.csv(paste(pathWorking, "ssWide.csv", sep=""))
d[1:5,]
w1ssVars <- paste("ss", 1:10, "_1", sep="")
w2ssVars <- paste("ss", 1:10, "_2", sep="")
w3ssVars <- paste("ss", 1:10, "_3", sep="")
w4ssVars <- paste("ss", 1:10, "_4", sep="")
w5ssVars <- paste("ss", 1:10, "_5", sep="")
w6ssVars <- paste("ss", 1:10, "_6", sep="")
w7ssVars <- paste("ss", 1:10, "_7", sep="")
w8ssVars <- paste("ss", 1:10, "_8", sep="")
w9ssVars <- paste("ss", 1:10, "_9", sep="")
w10ssVars <- paste("ss", 1:10, "_10", sep="")
ssVars <- c(w1ssVars, w2ssVars, w3ssVars, w4ssVars, w5ssVars, w6ssVars, w7ssVars, w8ssVars, w9ssVars, w10ssVars)
length(ssVars)
for (i in 1:length(ssVars)) {
  print(table(d[, ssVars[i]], useNA="always"))
}
# first, recode all negative responses to NA (valid responses are 1-7)
d[, ssVars] <- lapply (d[, ssVars], iRecode, "lo:0=NA; 8:hi=NA")
# then, reverse score variables 1, 4, 5, 7, 10
varsToRecode <- c(paste("ss1_",1:10,sep=""), paste("ss4_",1:10,sep=""), paste("ss5_",1:10,sep=""), paste("ss7_",1:10,sep=""),paste("ss10_",1:10,sep=""))
d[, varsToRecode] <- lapply (d[, varsToRecode], iRecode, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
# internal consistency
a1 <- alpha(d[, w1ssVars])
a2 <- alpha(d[, w2ssVars])
a3 <- alpha(d[, w3ssVars])
a4 <- alpha(d[, w4ssVars])
a5 <- alpha(d[, w5ssVars])
a6 <- alpha(d[, w6ssVars])
a7 <- alpha(d[, w7ssVars])
a8 <- alpha(d[, w8ssVars])
a9 <- alpha(d[, w9ssVars])
a10 <- alpha(d[, w10ssVars])
alphas <- c(a1$total$raw_alpha, 
            a2$total$raw_alpha,
            a3$total$raw_alpha,
            a4$total$raw_alpha,
            a5$total$raw_alpha,
            a6$total$raw_alpha,
            a7$total$raw_alpha,
            a8$total$raw_alpha,
            a9$total$raw_alpha,
            a10$total$raw_alpha)
alphas
# compute mean score at each wave
d$ss_1 <- rowMeans(d[, w1ssVars], na.rm=T)
d$ss_2 <- rowMeans(d[, w2ssVars], na.rm=T)
d$ss_3 <- rowMeans(d[, w3ssVars], na.rm=T)
d$ss_4 <- rowMeans(d[, w4ssVars], na.rm=T)
d$ss_5 <- rowMeans(d[, w5ssVars], na.rm=T)
d$ss_6 <- rowMeans(d[, w6ssVars], na.rm=T)
d$ss_7 <- rowMeans(d[, w7ssVars], na.rm=T)
d$ss_8 <- rowMeans(d[, w8ssVars], na.rm=T)
d$ss_9 <- rowMeans(d[, w9ssVars], na.rm=T)
d$ss_10 <- rowMeans(d[, w10ssVars], na.rm=T)
d <- d[,c("pid",paste("ss_",1:10,sep=""))]
# save a file with only mean scores
write.csv(d, paste(pathWorking,"clean ssWide.csv",sep=""), row.names=F)

# ---------------- limit sample only to people who have at least 1 wave of LS before the event and 1 after the event --------------------------------------------------- #
iBeforeAfterLS(eventFile="WidowhoodYear.csv", lsFile="clean lsWide.csv")

# ---------------- make files with all codes  --------------------------------------------------- #
iAllCodesLSLong ("WidowhoodYear (with 2 waves of LS).csv", "clean lsWide.csv", "w", fileToSave="finalWidowhoodData.csv")


# add sex and age info
allEvents <- read.csv(paste(pathWorking,"finalWidowhoodData.csv",sep=""))
allEvents[1:5,]
nrow(allEvents)
demoData <- read.csv(paste(pathWorking,"clean DemoWide.csv", sep=""))
#ssData <- read.csv(paste(pathWorking,"ssBefAft.csv", sep=""))
ssData <- read.csv(paste(pathWorking,"clean ssWide.csv", sep=""))
allData <- allEvents
allData <- merge(allEvents,demoData,all.x=T)
allData <- merge(allData, ssData)  # only keep those from who have data from ssData
# now compute age at all events....
allData$ageWid <- allData$wyear - allData$yrBirth
nrow(aggregate(allData, by=list(allData$pid), FUN=mean))
write.csv(allData, paste(pathWorking,"allData.csv",sep=""), row.names=F)

