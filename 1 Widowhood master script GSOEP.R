library(foreign)
library(lme4)
library(Hmisc)
library(car)
library(reshape2)

# ---------------------------- load functions ------------------------------- #
source(".../My R Functions/LifeEventsFunctions.r")
source(".../My R Functions/LifeEventsFunctionsGSOEP.r")

# ------------------ THESE ARE CONSTANT IN A PROJECT ------------------ #
pathOriginalData <- ".../Panel Data/SOEP26/"     # path of original panel data files
pathWorking <- ".../datafilesGSOEP/"

# the recode function is:
iRecode <- function (x, rstring) { recode (x, rstring) }      # recode NAs to be 0

firstYearOfStudy <- 1984         # first year of study 
lastYearOfStudy <- 2009          # last year of study
lastWaveOfStudy <- 26            # last wave of study (i.e., wave number in last year of study) (first wave of study is always 1)
firstYearOfLS <- 1984            # first year that life satisfaction is collected (e.g., for BHPS life satisfaction, this is 1996)
lastYearOfLS <- lastYearOfStudy  # last year that life satisfaction is collected
firstWaveOfLS <- 1               # first wave that life satisfaction is collected (e.g., for BHPS life satisfaction, this is 6)
lastWaveOfLS <- lastWaveOfStudy  # last wave that life satisfaction is collected

# these are necessary for iGetVariables()
originalDataFile <- "$p.sav"  # name of original data file (from which to pull data)
oldID <- "PERSNR"                      # cross-wave variable, indicates person id
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
origWidowed <- 5

# recoding syntax (recode NA to 0):
widowhoodRecode <- paste("-999:0=", noresp, "; ", origMarried, "=", married, "; ", origWidowed, "=", widowed, "; NA=", noresp, "; else=", other, sep="")


# ----------------------------------------------------------------------------- #


# ---------------- Pull up all variables from original data files --------------------------------------------------- #

mstatVars <- c("AFAMSTD","BFAMSTD","CFAMSTD","DFAMSTD","EFAMSTD","FFAMSTD","GFAMSTD","HFAMSTD","IFAMSTD","JFAMSTD",
               "KFAMSTD","LFAMSTD","MFAMSTD","NFAMSTD","OFAMSTD","PFAMSTD","QFAMSTD","RFAMSTD","SFAMSTD","TFAMSTD",
               "UFAMSTD","VFAMSTD","WFAMSTD","XFAMSTD","YFAMSTD","ZFAMSTD")
mstatWaves <- c(1:26)

lsVars <- c("AP6801","BP9301","CP9601","DP9801","EP89","FP108","GP109","HP10901",
            "IP10901","JP10901","KP10401","LP10401","MP11001","NP11701","OP12301",
            "PP13501","QP14301","RP13501","SP13501","TP14201","UP14501","VP154",
            "WP142","XP149","YP15501","ZP15701")
lsWaves <- c(1:26)

ssVars <- c("BP0704","CP0904","EP0904","IP0704","KP1204","MP0504","NP0304","PP0304","RP0305","VP0305","XP0305","ZP0305")
ssWaves <- c(2,3,5,9,11,13,14,16,18,22,24,26)
# ALSO GET GP0204E FROM GPOST FILE

iGetVariablesGSOEP(varsToPull=mstatVars, wavesToPull=mstatWaves, varsToSave="mstat", ,oDataFile="$pgen.sav", nDataFile="$mstat.csv", longfilename="mstatLong.csv", widefilename="mstatWide.csv")
iGetVariablesGSOEP(varsToPull=lsVars, wavesToPull=lsWaves, varsToSave="ls", nDataFile="$ls.csv", longfilename="lsLong.csv", widefilename="lsWide.csv")
iGetVariablesGSOEP(varsToPull="GP6401E", wavesToPull=7, varsToSave="lsEast", oDataFile="gpost.sav", nDataFile="lsEast.csv", longfilename="lsEastLong.csv", widefilename="lsEastWide.csv")   # East and West Germany have different variables in this wave
iGetVariablesGSOEP(varsToPull=ssVars, wavesToPull=ssWaves, varsToSave="ss", nDataFile="$ss.csv", longfilename="ssLong.csv", widefilename="ssWide.csv")
iGetVariablesGSOEP(varsToPull="GP0204E", wavesToPull=7, varsToSave="ssEast", oDataFile="gpost.sav", nDataFile="ssEast.csv", longfilename="ssEastLong.csv", widefilename="ssEastWide.csv")   # only East Germany file has this variable in this wave

# to correlate with social support
healthVars <- c("M1112684","M1112685","M1112686","M1112687","M1112688",
                "M1112689","M1112690","M1112691","M1112692","M1112693",
                "M1112694","M1112695","M1112696","M1112697","M1112698",
                "M1112699","M1112600","M1112601","M1112602","M1112603",
                "M1112604","M1112605","M1112606","M1112607","M1112608","M1112609")
healthWaves <- c(1:26)
iGetVariablesGSOEP(varsToPull=healthVars, wavesToPull=healthWaves, varsToSave="health", ,oDataFile="$pequiv.sav", nDataFile="$health.csv", longfilename="healthLong.csv", widefilename="healthWide.csv")

# ---------------- select each sample and figure out year of the event --------------------------------------------------- #
iSelectSample (sample="widowhood", firstWaveV=1, phase1="married", phase2="widowed", varStem="mstat_", originalFile="mstatWide.csv", saveFile="WidowhoodYear.csv")

# ---------------- clean up and recode life satisfaction, income, education, etc., figure out first wave stuff --------------------------------------------------- #

# combine life satisfaction data from two files
ls1 <- read.csv(paste(pathWorking,"lsWide.csv",sep=""))
ls2 <- read.csv(paste(pathWorking,"lsEastWide.csv",sep=""))
ls <- merge(ls1, ls2, by="pid", all=T)
ls$ls_7 <- ifelse(!is.na(ls$ls_7), ls$ls_7, ls$lsEast_7)
ls$lsEast_7 <- NULL
ls[1:5,]
# clean up ls
lsV <- paste("ls_",seq(from=firstWaveOfLS, to=lastWaveOfStudy),sep="")
ls[,lsV] <- lapply(ls[,lsV], iRecode, "lo:-1=NA; 11:hi=NA")
# delete cases with all missing
ls$missing <- rowSums(is.na(ls[,lsV]))
table(ls$missing, useNA="ifany")
ls <- ls[ls$missing < lastWaveOfStudy,]
ls$missing <- NULL
ls[1:10,]
# add .03 for each wave of participation
# figure out first wave of life satisfaction
names(ls) <- c("pid", seq(from=firstWaveOfLS, to=lastWaveOfLS))
ls <- melt(ls, id="pid")
names(ls) <- c("pid", "wave", "ls")
ls <- ls[!is.na(ls$ls),]
ls$wave <- as.integer(ls$wave)
agls <- aggregate(ls, by=list(ls$pid), FUN=min)
agls[1:5,]
agls <- agls[,c("pid", "wave")]
names(agls) <- c("pid", "firstWave")
ls <- merge(agls, ls, by="pid", all=T)
ls$ls <- ls$ls + (ls$wave - ls$firstWave)*.03
ls[1:5,]
ls <- melt(ls, id=c("pid","wave","firstWave"))
ls <- dcast(ls, pid + firstWave ~ variable + wave)
ls[1:5,]
ls$firstWave<-NULL
# save clean file
write.csv(ls, file=paste(pathWorking, "clean lsWide.csv", sep=""), row.names=F)

# combine social support data from two files
ss1 <- read.csv(paste(pathWorking, "ssWide.csv", sep=""))
ss2 <- read.csv(paste(pathWorking, "ssEastWide.csv", sep=""))
names(ss1)
names(ss2)
names(ss2) <- c("pid","ss_7")
ss <- merge(ss1, ss2, by="pid", all=T)
ss <- ss[,c("pid","ss_2","ss_3","ss_5","ss_7","ss_9","ss_11","ss_13","ss_14","ss_16","ss_18","ss_22","ss_24","ss_26")]
ss[1:5,]
for(i in c(2, 3, 5, 7, 9, 11, 13, 14, 16, 18, 22, 24, 26)) {
  print(table(ss[,paste("ss_",i,sep="")], useNA="always"))
}
# recode so that 1 = never, 2 = less frequently, 3 = every month, 4 = every week
ssV <- c("ss_2","ss_3","ss_5","ss_7","ss_9","ss_11","ss_13","ss_14","ss_16","ss_18","ss_22","ss_24","ss_26")
ss[,ssV] <- lapply(ss[,ssV], iRecode, "lo:0=NA; 1=4; 2=3; 3=2; 4=1; 5:hi=NA")   
# delete cases with all missing
ss$missing <- rowSums(is.na(ss[,ssV]))
table(ss$missing,useNA="always")
ss <- ss[ss$missing < length(ssV),]
ss$missing <- NULL
ss[1:5,]
# save clean file
write.csv(ss, file=paste(pathWorking, "clean ssWide.csv", sep=""), row.names=F)

# get sex and year of birth
demo <- read.spss(paste(pathOriginalData, "ppfad.sav", sep=""), to.data.fram=T, use.value.labels=F)
demo[1:5,]
names(demo) <- tolower(names(demo))
demo <- demo[,c("persnr", "sex", "gebjahr")]
names(demo) <- c("pid", "sex", "yrBirth")   
# sex: 1=male, 2=female; change to -1=female, 1=male
table(demo$sex,useNA="ifany")
demo$sex <- recode(demo$sex, "1=1; 2=-1; else=NA")
table(demo$yrBirth,useNA="ifany")
demo$yrBirth <- recode(demo$yrBirth, "lo:0=NA") 
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

