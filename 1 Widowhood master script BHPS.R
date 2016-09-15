library(psych)
library(foreign)
library(lme4)
library(Hmisc)
library(car)
library(reshape2)

# ---------------------------- load functions ------------------------------- #
source(".../My R Functions/LifeEventsFunctions.r")
source(".../My R Functions/LifeEventsFunctionsBHPS.r")


# ------------------ THESE ARE CONSTANT IN A PROJECT ------------------ #
pathOriginalData <- ".../Panel Data/BHPS18/"     # path of original panel data files
pathWorking <- ".../datafilesBHPS/"

firstYearOfStudy <- 1991         # first year of study 
lastYearOfStudy <- 2008          # last year of study
lastWaveOfStudy <- 18            # last wave of study (i.e., wave number in last year of study) (first wave of study is always 1)
firstYearOfLS <- 1996            # first year that life satisfaction is collected (e.g., for BHPS life satisfaction, this is 1996)
lastYearOfLS <- lastYearOfStudy  # last year that life satisfaction is collected
firstWaveOfLS <- 6               # first wave that life satisfaction is collected (e.g., for BHPS life satisfaction, this is 6)
lastWaveOfLS <- lastWaveOfStudy  # last wave that life satisfaction is collected

# these are necessary for iGetVariables()
originalDataFile <- "$indresp.sav"  # name of original data file (from which to pull data)
oldID <- "PID"                      # cross-wave variable, indicates person id
betaFileName <- ""                  # this is used for BETA waves of SHP; set this to "" if there is no beta file
betaYearNumber <- ""                # this is the two digit year of the beta file, in character format
charsToSub <- "\\$"                 # placeholder for wave or year number in file/variable names (e.g., $$ in nchild$$)
# wFile = number or character that indicates wave/year in file names
# wVar = number or character that indicates wave/year in variable names
# these have to be in character format
wFile <- paste(letters[1:lastWaveOfStudy]) 
wVar <- toupper(wFile)           

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
origWidowed <- 4

# recoding syntax (recode NA to 0):
widowhoodRecode <- paste("-999:0=", noresp, "; ", origMarried, "=", married, "; ", origWidowed, "=", widowed, "; NA=", noresp, "; else=", other, sep="")


# ----------------------------------------------------------------------------- #


# ---------------- Pull up all variables from original data files --------------------------------------------------- #
ssVarsToPull <- c("$SSUPA","$SSUPB","$SSUPC","$SSUPD","$SSUPE")
ssVarsToSave <- c("ss1","ss2","ss3","ss4","ss5")

iGetVariables(varsToPull="$MLSTAT", varsToSave="mlstat", nDataFile="$mar.csv", longfilename="marLong.csv", widefilename="marWide.csv")
iGetVariables(varsToPull="$LFSATO", varsToSave="ls", nDataFile="$ls.csv", longfilename="lsLong.csv", widefilename="lsWide.csv", firstWaveV=firstWaveOfLS, removeNA=F)  # ls wasn't collected in wave 11, want to keep an empty column (of NA values)
iGetVariables(varsToPull=ssVarsToPull, varsToSave=ssVarsToSave, nDataFile="$ss.csv", longfilename="ssLong.csv", widefilename="ssWide.csv", firstWaveV=1, lastWaveV=17)

# to correlate with social support
iGetVariables(varsToPull="$HLSTAT", varsToSave="health", nDataFile="$health.csv", longfilename="healthLong.csv", widefilename="healthWide.csv", removeNA=F)  # self-reported health satisfaction wasn't collected in wave 11, want to keep an empty column (of NA values)
iGetVariables(varsToPull="$LFSAT4", varsToSave="relsat", nDataFile="$relsat.csv", longfilename="relsatLong.csv", widefilename="relsatWide.csv", firstWaveV=firstWaveOfLS, removeNA=F)  # satisfaction with spouse/partner wasn't collected in wave 11, want to keep an empty column (of NA values)



# ---------------- select each sample and figure out year of the event --------------------------------------------------- #

# for widowhood, they could have been single, divorced, separated, or widowed prior to ls collection (not looking at first occurrence)
# so make startwave = 6 (first wave of ls selection - that should leave people who had "other" responses prior to ls collection)
iSelectSample (sample="widowhood", firstWaveV=6, phase1="married", phase2="widowed", varStem="mlstat_", originalFile="marWide.csv", saveFile="WidowhoodYear.csv")


# ---------------- clean up and recode social support and life satisfaction data; compute social support variables  --------------------------------------------------- #
iCleanLifeSatBHPS(lsFile="lsWide.csv")

# SOCIAL SUPPORT
d <- read.csv(paste(pathWorking, "ssWide.csv", sep=""))
d[1:5,]
w1ssVars <- paste("ss", 1:5, "_1", sep="")
w3ssVars <- paste("ss", 1:5, "_3", sep="")
w5ssVars <- paste("ss", 1:5, "_5", sep="")
w7ssVars <- paste("ss", 1:5, "_7", sep="")
w13ssVars <- paste("ss", 1:5, "_13", sep="")
w15ssVars <- paste("ss", 1:5, "_15", sep="")
w17ssVars <- paste("ss", 1:5, "_17", sep="")
ssVars <- c(w1ssVars, w3ssVars, w5ssVars, w7ssVars, w13ssVars, w15ssVars, w17ssVars)
# don't use items 9 and 11 - they use different response options
names(d)
d <- d[, c("pid",ssVars)]
# recode variables (valid response options are 1, 2, 3)
# also recode so that 0 = no one, 1 = one person, 2 = more than one (currently 1= one person, 2 = more than one, 3 = no one)
for (i in 1:length(ssVars)) {
  print(table(d[, ssVars[i]], useNA="always"))
}
d[, ssVars] <- lapply (d[, ssVars], iRecode, "lo:0=NA; 3=0; 1=1; 2=2; 4:hi=NA")
# internal consistency
a1 <- alpha(d[, w1ssVars])
a3 <- alpha(d[, w3ssVars])
a5 <- alpha(d[, w5ssVars])
a7 <- alpha(d[, w7ssVars])
a13 <- alpha(d[, w13ssVars])
a15 <- alpha(d[, w15ssVars])
a17 <- alpha(d[, w17ssVars])
alphas <- c(a1$total$raw_alpha, a3$total$raw_alpha, a5$total$raw_alpha, a7$total$raw_alpha, a13$total$raw_alpha, a15$total$raw_alpha, a17$total$raw_alpha)
alphas
# compute mean score at each wave
d$ss_1 <- rowMeans(d[, w1ssVars], na.rm=T)
d$ss_3 <- rowMeans(d[, w3ssVars], na.rm=T)
d$ss_5 <- rowMeans(d[, w5ssVars], na.rm=T)
d$ss_7 <- rowMeans(d[, w7ssVars], na.rm=T)
d$ss_13 <- rowMeans(d[, w13ssVars], na.rm=T)
d$ss_15 <- rowMeans(d[, w15ssVars], na.rm=T)
d$ss_17 <- rowMeans(d[, w17ssVars], na.rm=T)
# save a file with only mean scores
write.csv(d, paste(pathWorking,"clean ssWide.csv",sep=""), row.names=F)

demo <- read.spss(paste(pathOriginalData, "xwaveid.sav", sep=""), to.data.fram=T, use.value.labels=F)
names(demo) <- tolower(names(demo))
demo <- demo[,c("pid", "sex", "doby")]
demo[1:5,]
table(demo$sex, useNA="ifany")
table(demo$doby, useNA="ifany")
# sex: 1=male, 2=female; change to -1=female, 1=male
demo$sex <- recode(demo$sex, "1=1; 2=-1; else=NA")
demo$doby <- recode(demo$doby, "lo:0=NA; 2020:hi=NA")
names(demo) <- c("pid", "sex", "yrBirth")
write.csv(demo, file=paste(pathWorking, "clean demoWide.csv", sep=""), row.names=F)


# ---------------- limit sample only to people who have at least 1 wave of LS before the event and 1 after the event --------------------------------------------------- #
iBeforeAfterLS(eventFile="WidowhoodYear.csv", lsFile="clean lsWide.csv")

# ---------------- make files with all codes  --------------------------------------------------- #
iAllCodesLSLong ("WidowhoodYear (with 2 waves of LS).csv", "clean lsWide.csv", "w", fileToSave="finalWidowhoodData.csv")


# add sex and age info
allEvents <- read.csv(paste(pathWorking,"finalWidowhoodData.csv",sep=""))
demoData <- read.csv(paste(pathWorking,"clean DemoWide.csv", sep=""))
# ssData <- read.csv(paste(pathWorking,"ssBefAft.csv", sep=""))
ssData <- read.csv(paste(pathWorking, "clean ssWide.csv", sep=""))
allData <- merge(allEvents,demoData,all.x=T)
allData <- merge(allData, ssData)  # only keep those from who have data from ssData
# now compute age at all events....
allData$ageWid <- allData$wyear - allData$yrBirth
nrow(aggregate(allData, by=list(allData$pid), FUN=mean))
write.csv(allData, paste(pathWorking,"allData.csv",sep=""), row.names=F)

