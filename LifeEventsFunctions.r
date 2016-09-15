iGetVariables <- function(varsToPull, varsToSave, oDataFile=originalDataFile, nDataFile, 
longfilename, widefilename, oidvar=oldID, type="person", oPath=pathOriginalData, nPath=pathWorking, 
firstWaveV=1, lastWaveV=lastWaveOfStudy, firstYearS=firstStudyYear, betaYear=betaYearNumber, 
betaFile=betaFileName, substring=charsToSub, removeNA=T) {
# this function pulls up longitudinal variables from the panel files, and creates a long and a wide data file with those variables
# varsToPull = character vector of the variable names to pull up - it should be one variable name per longitudinal variable, with substring standing in for wave or year number (e.g., "P$$C44")
# varsToSave = character vector of new variable names - with substring standing in for wave or year number
# oDataFile = original data file name format (e.g., 'SHP$$_P_USER.sav', where $$ stands in for wave numbers
# nDataFile = new data file name format (e.g., lsdata$$.csv', where $$ stands in for wave number
# longfilename = name for the long data file
# widefilename = name for the wide data file
# oidvar = original id variable name (e.g., IDPERS, or IDHOUS$$)
# type = "person" or "house": person or household level variables? (this determines whether pid or hid is used)
# oPath = directory path where original files are stored
# nPath = directory path where new files are to be stored
# firstWaveV = first wave in which variable is collected
# lastWaveV = last wave in which variable is collected
# firstYearS = first year of study (NOT first year in which variable is collected)
# betaYear = two digit year for which only beta file is available
# betaFile = beta waves are named differently in SHP - this tells the name for the beta wave file, with substring in place of wave or year
# removeNA = should cases with all missing (NA) values be removed from long and wide files?

  # get number of variables to pull
  numvars <- length(varsToPull)           

  # create individual, wave-specific files, with all wave-specific variables in each of the files
  for (i in firstWaveV:lastWaveV) {
    getFile <- paste( oPath, sub(substring, wFile[i], oDataFile), sep="")     # replace $$ with wave number
    if ((betaFile != "") & (betaYear == wFile[i])) { 
      getFile <- paste( oPath, sub(substring, wFile[i], betaFile), sep="") 
    }     # if there is a beta file, then user specifies name for it
    saveFile <- paste( nPath, sub(substring, wFile[i], nDataFile), sep="")    # file to which the pulled variables are to be saved
    getVars <- character(length(varsToPull))
    saveVars <- character(length(varsToSave))
    for (j in 1:numvars) {
      getVars[j] <- sub(substring, wVar[i], varsToPull[j])     # names of variables to be pulled   
      saveVars[j] <- sub(substring, wVar[i], varsToSave[j])    # names of variables to be saved
    }
    print(paste("Pulling data from:", getFile))
    if (strsplit(originalDataFile,"\\.")[[1]][2] == "sav") { 
      data <- read.spss(getFile, to.data.fram=T, use.value.labels=F) 
    } else if (strsplit(originalDataFile,"\\.")[[1]][2] == "dta") {
      data <- read.dta(getFile, convert.factors=F)
    }
    data$wave <- i
    for (j in 1:numvars) {
      if (is.na(match(getVars[j], names(data)))) {                # if variable doesn't exist just enter an empty column with that name
        print( paste( "Variable", getVars[j], "does not exist")) 
        print ("Setting an empty column with that variable name")
        data[, getVars[j]] <- NA
      }
    }
    # if old id variable contains "$$" it means it changes with wave, so have to replace $$
    if (length(grep(substring, oidvar)) > 0) { idToUse <- sub(substring, wVar[i], oidvar) }
    else { idToUse <- oidvar }
    # set data to have only id, wave, and variables from varsToPull
    data <- data[c(idToUse, "wave", getVars)]   
    if (type == "person") { names(data) <- c("pid", "wave", saveVars) }
    else if (type == "house") { names(data) <- c("hid", "wave", saveVars) }
    else { print("INCORRECT TYPE: Type must be either person or house") }
    print(paste("Saving data to:", saveFile))
    write.csv(data, file = saveFile, row.names=F)
    print("-------------------------------------------------------------------------------------------------")
  }
  
  # merge individuals files into one long file
  if (lastWaveV > firstWaveV) {
    print("Merging data")
    for (i in (firstWaveV+1):lastWaveV) {
      if (i == (firstWaveV + 1)) {
        data1name <- paste(nPath, sub(substring, wFile[firstWaveV], nDataFile), sep="")
        data1 <- read.csv(data1name)
        print(paste("Merging: start with", data1name))
      }
      else { data1 <- alldata }
      data2name <- paste(nPath, sub(substring, wFile[i], nDataFile), sep="")
      data2 <- read.csv(data2name)
      print(paste("Merging: adding", data2name))
      alldata <- merge(data1, data2, all.x=T, all.y=T)
    }
  }
  else { 
    data1name <- paste(nPath, sub(substring, wFile[firstWaveV], nDataFile), sep="")
    data1 <- read.csv(data1name)
    alldata <- data1
  }
  # remove waves with all NA responses
  if (removeNA==T) {
    print("Removing cases with all missing")
    alldata$missing <- rowSums(is.na(alldata))
    alldata <- alldata[alldata$missing < numvars,]
    alldata$missing <- NULL
  }
  print("Writing long file")
  print(paste(nPath, longfilename, sep=""))
  write.csv(alldata, file=paste(nPath, longfilename, sep=""), row.names=F)
  print("-------------------------------------------------------------------------------------------------")

  # erase individual files
  for (i in firstWaveV:lastWaveV) {
    filename <- paste(nPath, sub(substring, wFile[i], nDataFile), sep="")
    print(paste("Deleting file", filename))
    unlink (filename)
  }
  print("-------------------------------------------------------------------------------------------------")

  # write wide file
  print("Reading long data")
  print(paste(nPath, longfilename, sep=""))
  data <- read.csv(paste(nPath, longfilename, sep=""))
  print("Melting data")
  if (type == "person") { meltdata <- melt(data, id.vars=c("pid", "wave")) }
  else if (type == "house") { meltdata <- melt(data, id.vars=c("hid", "wave")) }
  else { print("INCORRECT TYPE: Type must be either person or house") }
  print("Transforming into wide file")
  if (type == "person") { widedata <- dcast(meltdata, pid ~ variable + wave) }
  else if (type == "house") { widedata <- dcast(meltdata, hid ~ variable + wave) }
  print("Saving wide file")
  print(paste(nPath, widefilename, sep=""))
  write.csv(widedata, file = paste(nPath, widefilename, sep=""), row.names=F)

}

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# the recode function is:
iRecode <- function (x, rstring) { recode (x, rstring) }      # recode NAs to be 0

iSelectSample <- function (sample, varStem, phase1, phase2, phase3="", originalFile, saveFile, 
firstWaveV=firstWaveOfLS, lastWaveV=lastWaveOfLS, firstYearS=firstYearOfStudy, 
NAresp=noresp, remainPhase2=T, keepOriginalData=F, validAfter="", type="person", nPath=pathWorking) {
# select sample function
# this select a sample that changed from phase1 to phase2 (e.g., single to married)
# it figures out the year of the event
# and then saves the file with only the id variable and the yar of the event
# remainPhase2 = (only necessary if phase3="") if T, then only people who stay in phase2 are kept (e.g., get married & stay married)
#   if F, then it doesn't matter if they go back to phase1 after being in phase2 (e.g., after divorce, they don't necessarily have to stay divorced)
# validAfter = other responses that are valid AFTER the event (but not before the event) - e.g., "more than one child" is ok after they have their first one, but not before
#   these responses will be ignored after start of phase2, but cases that have them before phase2 will be deleted
# type = "person" or "house": is this a person-level or household-level file? this affects whether the id variable is "pid" or "hid"

  ERROR <- -333
  if (type == "person") { idVar <- "pid" }
  else if (type == "house") { idVar <- "hid" }
  else { print ("ERROR: Type must be either person or house") }
  print (paste (sample, "sample"))
  # establish recoding syntax
  recodeString <- get(paste(sample,"Recode",sep=""))
  # longitudinal variable set from which to get year of the event
  mainVars <- paste(varStem, firstWaveV:lastWaveV, sep="")
  changeVars <- paste("change_", firstWaveV:lastWaveV, sep="")
  # read in full data
  data <- read.csv (paste (nPath, originalFile, sep=""))
  print(paste("start with",nrow(data)))
  data <- data[,c(idVar, mainVars)]             
  # recode all responses that are not one of the ones we're interested in (e.g., single & married) to 888 (other)
  data[, mainVars] <- lapply (data[, mainVars], iRecode, recodeString)
  
  # select only those that have some responses
  data$cValid <- rowSums(data[,mainVars] != noresp) > 0
  data <- data[data$cValid==T,]
  print(paste("cases with some non-NA responsess", nrow(data)))
  # select only those who don't have any 'other' responses
  data$cValid <- rowSums(data[,mainVars] == other) == 0
  data <- data[data$cValid==T,]
  print(paste("cases without any 'other' responses", nrow(data)))
  # select only those who have both responses of interest (e.g., single & married)
  if (phase3=="") { 
    data$cValid <- (rowSums(data[,mainVars] == get(phase1)) > 0) & (rowSums(data[,mainVars] == get(phase2)) > 0) }            # at least one single and one married
  else if (phase3==phase1) { 
    data$cValid <- (rowSums(data[,mainVars] == get(phase1)) > 1) & (rowSums(data[,mainVars] == get(phase2)) > 0) }  # at least two employed and one unemployed
  else { print("ERROR: Cannot handle phase3 that's not the same as phase 1") }
  data <- data[data$cValid==T,]
  if (phase3=="") { print(paste("cases with both", phase1, "and", phase2, nrow(data))) }
  else if (phase3==phase1) { print(paste("cases with two", phase1, "and one", phase2, nrow(data))) }
  data$cValid <- NULL
  # select those whose first non-NA response is not phase2
  data$tempflag <- 0
  for (i in 1:length(mainVars)) {
    # if they're unemployed, but haven't been flagged as employed before, then flag them as ERROR
    test <- (data[,mainVars[i]] == get(phase2)) & (data$tempflag == 0)    
    data$tempflag <- ifelse (test, ERROR, data$tempflag)
    # if they are employed, and haven't been flagged as ERROR before, then flag them as OK (phase1)
    test <- (data[,mainVars[i]] == get(phase1)) & (data$tempflag != ERROR)
    data$tempflag <- ifelse (test, get(phase1), data$tempflag)
  }
  data <- data[data$tempflag != ERROR,]
  data$tempflag <- NULL
  print(paste("cases which don't start as", phase2, nrow(data)))

  # select only those who don't have a "validAfter" response before phase 2
  if (validAfter != "") {
    data$tempflag <- 0
    for (i in 1:length(mainVars)) {
      # if they have a "validAfter" response, but haven't been flagged as in phase 2, then flag them as ERROR
      test <- (data[,mainVars[i]] == validAfter) & (data$tempflag == 0) 
      data$tempflag <- ifelse (test, ERROR, data$tempflag)
      # if they are in phase2, and haven't been flagged as ERROR before, then flag them as OK (phase2)
      test <- (data[,mainVars[i]] == get(phase2)) & (data$tempflag != ERROR)
      data$tempflag <- ifelse (test, get(phase2), data$tempflag)
    }
    data <- data[data$tempflag != ERROR,]
    data$tempflag <- NULL
    print(paste("cases that don't have afterValid responses before", phase2, nrow(data)))
  }

  # compute change between two consecutive waves 
  # change_2 means change from wave 1 to wave 2
  for (i in 2:(lastWaveV-firstWaveV+1)) {
    data[,changeVars[i]] <- data[,mainVars[i]] - data[,mainVars[i-1]]
  }
  # select only those who go from (phase1 to phase2) OR (NA to phase2 if they also went from phase1 to NA)
  # e.g., those who go from (single to married) OR (NA to married if they also went from single to NA)
  phase1_phase2 <- paste(phase1, "_", phase2, sep="")
  phase1_NA <- paste(phase1, "_NA", sep="")
  NA_phase2 <- paste("NA_", phase2, sep="")
  data$cValid1 <-(rowSums(data[,changeVars[2:(lastWaveV-firstWaveV+1)]] == get(phase1_phase2)) > 0)
  data$cValid2 <- (rowSums(data[,changeVars[2:(lastWaveV-firstWaveV+1)]] == get(NA_phase2)) > 0) & (rowSums(data[,changeVars[2:(lastWaveV-firstWaveV+1)]] == get(phase1_NA)) > 0)
  data <- data[(data$cValid1 | data$cValid2) == T,] 
  print(paste("cases that go from",phase1,"to",phase2,"or NA to",phase2,"(if also from",phase1,"to NA)",nrow(data)))
  data$cValid1 <- NULL
  data$cValid2 <- NULL
  if (phase3==phase1) {
    # also select those who go (from phase2 to phase3) OR (NA to phase3 if they also went from phase2 to NA)
    phase2_phase3 <- paste(phase2, "_", phase3, sep="")
    phase2_NA <- paste(phase2, "_NA", sep="")
    NA_phase3 <- paste("NA_", phase3, sep="")
    data$cValid3 <-(rowSums(data[,changeVars[2:(lastWaveV-firstWaveV+1)]] == get(phase2_phase3)) > 0)
    data$cValid4 <- (rowSums(data[,changeVars[2:(lastWaveV-firstWaveV+1)]] == get(NA_phase3)) > 0) & (rowSums(data[,changeVars[2:(lastWaveV-firstWaveV+1)]] == get(phase2_NA)) > 0)
    data <- data[(data$cValid3 | data$cValid4) == T,]
    data$cValid3 <- NULL
    data$cValid4 <- NULL
    # the above may include people who have "NA employed unemployed NA"
    # so then select out people who don't have any employed waves after being unemployed
    data$tempflag <- 0
    for (i in 1:length(mainVars)) {
      # if they are employed, haven't been flagged as unemployed before, then flag them as employed
      test <- (data[,mainVars[i]] == get(phase1)) & (data$tempflag == 0)
      data$tempflag <- ifelse (test, get(phase1), data$tempflag)
      # if they are employed, and have been flagged as unemployed before, then flag them as OK (9999)
      test <- (data[,mainVars[i]] == get(phase1)) & (data$tempflag == get(phase2))
      data$tempflag <- ifelse (test, 9999, data$tempflag)      
      # if they are unemployed, and haven't already been tagged as pass, tag them as unemployed
      test <- (data[,mainVars[i]] == get(phase2)) & (data$tempflag != 9999)
      data$tempflag <- ifelse (test, get(phase2), data$tempflag)
    }
    # select only those who were flagged as OK (9999) - e.g., who became reemployed
    data <- data[data$tempflag == 9999,]
    print(paste("cases that also go from",phase2,"to",phase3,"or NA to",phase3,"(if also from",phase2,"to NA)",nrow(data)))
  }
  
  
  if (phase3!=phase1 & remainPhase2==T) {
    # find people who say they are in phase1 AFTER they said they were in phase2 (e.g., single AFTER they say they are married)
    # if they're married in the first wave, flag them as married
    data$tempflag <- 0
    for (i in 1:length(mainVars)) {
      # if they're married, and haven't yet been flagged, then flag them as married (1)
      test1 <- ((data[,mainVars[i]] == get(phase2)) & (data$tempflag == 0))
      data$tempflag <- ifelse (test1, 1, data$tempflag)
      # if they're single, but have been flagged as married before, then flag them as ERROR
      test2 <- ((data[,mainVars[i]] == get(phase1)) & (data$tempflag == 1))
      data$tempflag <- ifelse (test2, ERROR, data$tempflag)
    }
    data <- data[data$tempflag==1,]
    data$tempflag <- NULL
    print(paste("cases that did not go back to",phase1,"after saying they are",phase2,nrow(data)))
  }

  if (phase3=="") {
    # set eventyear as the first year they report they're in phase2 (e.g., married (it can follow married, even if has one blank between them))
    data$eventyear <- 0
    k <- 0
    for (i in (lastWaveV-firstWaveV+1):2) {
      k <- k + 1
      year <- firstYearS + lastWaveV - k
      # if change is from single to married, set the wave as the year of marriage
      test <- (data[,changeVars[i]] == get(phase1_phase2))
      data$eventyear <- ifelse (test, year, data$eventyear)  
      if (i > 2) {
        # if change is from NA to married, and the change before was from single to NA, set the wave as the year of marriage
        test <- (data[,changeVars[i]] == get(NA_phase2)) & (data[,changeVars[i-1]] == get(phase1_NA))
        data$eventyear <- ifelse (test, year, data$eventyear)
      }
    }
    # select only those for whom we can estimate eventyear
    data <- data[data$eventyear > 0,]
    data <- data[order(data[,idVar]),]
    if (keepOriginalData == F) { data <- data[,c(idVar, "eventyear")] }
    else { data <- data[,c(idVar, "eventyear", mainVars)] }
    print(table(data$eventyear))

  }
  else if (phase3==phase1) {
    # find out the year of switch from phase1 to phase2 (e.g., year of first unemployment) - it can be phase1_phase2 or phase1_NA_phase2 - event1year
    # find out the year of switch from phase2 to phase3 (e.g., year of reemployment) - it can be phase2_phase3 or phase2_NA_phase3 - event2year
    # if they have two blanks between first occurrence of phase1 to phase2 OR phase2 to phase3, set year to 333 (error)
    # only set event2year if event1year exists
    data$event1year <- 0
    data$event2year <- 0
    for (i in 2 : (lastWaveV-firstWaveV+1)) {
      year <- firstYearS + firstWaveV + i - 2
      # if employed to unemployed, and event1year has not been set: event1year=year
      test <- (data[,changeVars[i]] == get(phase1_phase2)) & (data$event1year == 0)
      data$event1year <- ifelse(test, year, data$event1year)
      # if unemployed to employed, and event2year has not been set, but event1year has been set: event2year=year
      test <- (data[,changeVars[i]] == get(phase2_phase3)) & (data$event2year == 0) & (data$event1year > 0)
      data$event2year <- ifelse(test, year, data$event2year)
      if (i > 2) {
        # if NA to unemployed, and previous was from employed to NA, and event1year has not yet been set (=0): event1year=year
        test <- (data[,changeVars[i]] == get(NA_phase2)) & (data[,changeVars[i-1]] == get(phase1_NA)) & (data$event1year == 0)
        data$event1year <- ifelse(test, year, data$event1year)
        # if NA to employed, and previous was from unemployed to NA, and event2year has not been set (=0), but even1year has been set: event2year=year
        test <-(data[,changeVars[i]] == get(NA_phase3)) & (data[,changeVars[i-1]] == get(phase2_NA)) & (data$event2year == 0) & (data$event1year > 0)
        data$event2year <- ifelse(test, year, data$event2year)
        # if NA to unemployed and previous was NA to NA, and event1year has not been set (=0): event1year=ERROR
        test <- (data[,changeVars[i]] == get(NA_phase2)) & (data[,mainVars[i-2]] == NAresp) & (data$event1year == 0)
        data$event1year <- ifelse(test, ERROR, data$event1year)
        # if NA to employed and previous was NA to NA, and event2 year has not been set yet (=0), but event1year has been set(<0 ): event2year=ERROR
        test <- (data[,changeVars[i]] == get(NA_phase3)) & (data[,mainVars[i-2]] == NAresp) & (data$event2year == 0) & (data$event1year > 0 )
        data$event2year <- ifelse(test, ERROR, data$event2year)
      }
    }
    # select only those for whom we can estimate year of unemployment and year of reemployment
    data <- data[(data$event1year > 0 & data$event2year > 0),]
    data <- data[order(data[,idVar]),]
    if (keepOriginalData == F) { data <- data[,c(idVar, "event1year", "event2year")] }
    else { data <- data[,c(idVar, "event1year", "event2year", mainVars)] }
    print(table(data[c("event1year","event2year")]))
  }
  
  print(paste("total N",nrow(data)))
  print("========================================================================================")
  write.csv(data, file=paste(nPath, saveFile, sep=""), row.names=F)
}

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

iBeforeAfterLS <- function(eventFile, lsFile, firstYearS=firstYearOfStudy, 
firstYearV=firstYearOfLS, firstWaveV=firstWaveOfLS, lastWaveV=lastWaveOfLS, 
lsPrefix="ls_", newFile=paste(strsplit(eventFile, "\\.")[[1]][1],"(with 2 waves of LS).csv"), nPath=pathWorking, 
type="person", phase3=F) {
# eventFile = file that contains pid and eventyear
# lsFile = file that contains cleaned up life satisfaction data 
# firstYearS = first year of study 
# firstYearV = the year in which life satisfaction data are available (e.g., 1996 in BHPS)
# firstWaveV = first wave in which life satisfaction data are available (e.g., 6 in BHPS)
# lastWaveV = last wave for which life satisfaction data are available
# lsPrefix = prefix for life satisfaction variables (part before the wave number)
# newFile = filename under which new data is to be saved
# nPath = path where working files are read from and saved to
# phase3 = if F, there is no phase3 (e.g., single to married; or married to divorced) (so there is one eventyear)
#    if T, there is phase 3 (e.g., employed to unemployed, to employed) (so there are two eventyears)

  eventData <- merge(read.csv (paste (nPath, eventFile, sep="")), read.csv( paste (nPath, lsFile, sep="")), by="pid", all=T) 

  if (phase3 == F) {
    # select only those who have eventyear
    eventData <- eventData[!is.na(eventData$eventyear),]
    # select only those whose eventyear is after ls data collection began
    eventData <- eventData[eventData$eventyear > firstYearV,]
    # count the number of ls waves before and after event year
    eventData$eventWave <- eventData$eventyear - firstYearS + 1
    for (i in 1:nrow(eventData)) {
      befVars <- paste(lsPrefix, firstWaveV:(eventData$eventWave[i]-1), sep="")
      aftVars <- paste(lsPrefix, eventData$eventWave[i]:lastWaveV, sep="")
      if (length(befVars) > 1) { eventData$lsBef[i] <- rowSums(!is.na(eventData[i, befVars])) }
      else { eventData$lsBef[i] <- ifelse(!is.na(eventData[i, befVars]), 1, 0) }
      if (length(aftVars) > 1) { eventData$lsAft[i] <- rowSums(!is.na(eventData[i, aftVars])) }
      else { eventData$lsAft[i] <- ifelse(!is.na(eventData[i, aftVars]), 1, 0) }
    }
    # select only those who have at least 1 ls wave before and 1 ls wave after the event
    print(paste("For", eventFile))
    print(paste("Original number of cases", nrow(eventData)))
    eventData <- eventData[eventData$lsBef > 0 & eventData$lsAft > 0,]
    print(paste("Number of cases with 1 wave of LS before and 1 after the event:", nrow(eventData)))
    eventData <- eventData[,c("pid", "eventyear")]
  }
  else if (phase3 == T) {
    # select only those who have both event1year and event2year
    eventData <- eventData[(!is.na(eventData$event1year) & !is.na(eventData$event2year)),]
    # select only those whose event1year is after ls data collection began
    eventData <- eventData[eventData$event1year > firstYearV,]
    # count the number of ls waves before event1year, between event1year and event2year, and after event2year
    eventData$event1Wave <- eventData$event1year - firstYearS + 1
    eventData$event2Wave <- eventData$event2year - firstYearS + 1
    for (i in 1:nrow(eventData)) {
      befVars <- paste(lsPrefix, firstWaveV:(eventData$event1Wave[i]-1), sep="")
      midVars <- paste(lsPrefix, eventData$event1Wave[i]:(eventData$event2Wave[i]-1), sep="")
      aftVars <- paste(lsPrefix, eventData$event2Wave[i]:lastWaveV, sep="")
      if (length(befVars) > 1) { eventData$lsBef[i] <- rowSums(!is.na(eventData[i, befVars])) }
      else { eventData$lsBef[i] <- ifelse(!is.na(eventData[i, befVars]), 1, 0) }
      if (length(midVars) > 1) { eventData$lsMid[i] <- rowSums(!is.na(eventData[i, midVars])) }
      else { eventData$lsMid[i] <- ifelse(!is.na(eventData[i, midVars]), 1, 0) }
      if (length(aftVars) > 1) { eventData$lsAft[i] <- rowSums(!is.na(eventData[i, aftVars])) }
      else { eventData$lsAft[i] <- ifelse(!is.na(eventData[i, aftVars]), 1, 0) }
    }
    # select only those who have at least 1 ls wave before event1year, 1 ls wave between event1year and event2year, and 1 ls wave after event2year
    print(paste("For", eventFile))
    print(paste("Original number of cases", nrow(eventData)))
    eventData <- eventData[eventData$lsBef > 0 & eventData$lsMid > 0 & eventData$lsAft > 0,]
    print(paste("Number of cases with 1 wave of LS before, between, and after the events:", nrow(eventData)))
    eventData <- eventData[,c("pid", "event1year", "event2year")]
  }
  else { print("ERROR: phase3 must be either T or F (default is F)") }
  # save new data file
  write.csv(eventData, paste(nPath, newFile, sep=""), row.names=F)
  print("----------------------------------------------------------------------------------")
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

iMergeEventLSLong <- function (eventYearFile, lsFile, eventPrefix, lsPrefix="ls_", splitChar="_", nPath=pathWorking, phase3=F) {
# this function is called by other functions that do the recoding (e.g., iReactAdapt, iYearBeforeOfAfterRest, etc.)
# combine event data with ls data, turn it into long format
# eventYearFile = file that contains pid and event year
# lsFile = file that contains life satisfaction data
# lsPrefix = prefix for life satisfaction variables (part before the wave number)
# splitChar = character before wave number in a variable (e.g., if variable names are ls_6, ls_7, etc, then splitChar is "_"
# nPath = directory where working files are read from and saved to
# phase3 = if F, there is no phase3 (e.g., single to married; or married to divorced) 
#   if T, there is phase 3 (e.g., employed to unemployed, to employed)

  # rename eventyear to wyear, myear, cyear, etc. (for widowhood, marriage, childbirth)
  eventData <- read.csv( paste(nPath, eventYearFile, sep=""))
  
  if (phase3 == F) {
    eyear <- paste(eventPrefix, "year", sep="")
    print(paste("Renaming eventyear to", eyear))
    names(eventData) <- c("pid", eyear)
    # merge event data with life satisfaction data
    eventData <- merge(eventData, read.csv( paste (nPath, lsFile, sep="")), by="pid", all=T)
    eventData <- eventData[!is.na(eventData[,eyear]),]    # select only cases for which eventyear exists
    # write wide file
    print("Melting data")
    longData <- melt(eventData, id.vars=c("pid", eyear), na.rm=T)
  }
  else if (phase3 == T) {
    e1year <- paste (eventPrefix, "year1", sep="")
    e2year <- paste (eventPrefix, "year2", sep="")
    print(paste("Renaming event1year to", e1year, "and event2year to", e2year))
    names(eventData) <- c("pid", e1year, e2year)
    # merge event data with life satisfaction data
    eventData <- merge(eventData, read.csv( paste(nPath, lsFile, sep="")), by="pid", all=T)
    eventData <- eventData[(!is.na(eventData[,e1year]) & !is.na(eventData[,e2year])),]   # select only cases for which both event1year and event2year exist
    # write wide file
    print("Melting data")
    longData <- melt(eventData, id.vars=c("pid", e1year, e2year), na.rm=T)
  }
  else { print("ERROR: phase3 must be either TRUE or FALSE") }

  varLevels <- levels(longData$variable)
  for (i in 1:length(varLevels)) {
    varLevels[i] <- strsplit(varLevels[i], splitChar)[[1]][2]
  }
  levels(longData$variable) <- varLevels
  print("Renaming variables")
  names(longData)[names(longData)=="variable"] <- "wave"
  names(longData)[names(longData)=="value"] <- strsplit(lsPrefix, splitChar)[[1]][1]
  longData$wave <- as.numeric(levels(longData$wave))[longData$wave]   # can't do just as.numeric(longData$wave) - it starts the wave at 1 instead of 6
  longData <- longData[order(longData$pid, longData$wave),]
  print("Created long file")
  return(longData)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
  
iAllCodesLSLong <- function (eventYearFile, lsFile, eventPrefix, fileToSave, lsPrefix="ls_", reaction="-1:1", splitChar="_", nPath=pathWorking, firstYearS=firstYearOfStudy, phase3=F) {
  longData <- iMergeEventLSLong (eventYearFile, lsFile, eventPrefix, lsPrefix, splitChar, nPath=pathWorking, phase3) 
  # creates a long file with codes for reaction, adaptation, year before, year after, year of event, sequence variable (year from event)
  eyear <- paste(eventPrefix, "year", sep="")
  e1year <- paste(eventPrefix, "year1", sep="")
  e2year <- paste(eventPrefix, "year2", sep="")

  seqVar <- paste(eventPrefix, "Seq", sep="")
  if (phase3 == F) {
    # compute wave of event
    longData$eventWave <- longData[,eyear] - firstYearS + 1  
    # compute sequence variable - years from event (i.e., 0 = year of the event)
    longData[,seqVar] <- longData$wave - longData$eventWave
  } 
  else if (phase3 == T) {
    # compute wave of event1 and even2
    longData$event1Wave <- longData[,e1year] - firstYearS + 1
    longData$event2Wave <- longData[,e2year] - firstYearS + 1
    # compute sequence variable - year from unemployment bout
    longData[,seqVar] <- ifelse(longData$wave<longData$event1Wave, longData$wave-longData$event1Wave, 
      ifelse(longData$wave<(longData$event2Wave-1), 0, longData$wave - (longData$event2Wave-1)))
  }
  
  # compute bef and aft variables
  print("Coding bef and aft variables")
  befVar <- paste(eventPrefix, "Bef", sep="")
  aftVar <- paste(eventPrefix, "Aft", sep="")
  longData[,befVar] <- ifelse(longData[,seqVar] < 0, 1, 0)
  longData[,aftVar] <- ifelse(longData[,seqVar] >= 0, 1, 0)
  
  # make reaction and adaptation variables
  print("Coding reaction and adaptation variables")
  reactVar <- paste(eventPrefix, "React", sep="")
  adaptVar <- paste(eventPrefix, "Adapt", sep="")
  reactRange <- strsplit(reaction, ":")
  reactRecode <- paste("-999:", as.numeric(reactRange[[1]][1])-1, "=0; ", reaction, "=1; ",as.numeric(reactRange[[1]][2])+1, ":999=0", sep="")
  adaptRecode <- paste("-999:", as.numeric(reactRange[[1]][2]), "=0; ", as.numeric(reactRange[[1]][2])+1, ":999=1", sep="")
  longData[,reactVar] <- recode(longData[,seqVar], reactRecode)
  longData[,adaptVar] <- recode(longData[,seqVar], adaptRecode)
  
  # make year before, year of, year after variables
  yrBefVar <- paste(eventPrefix, "YrBef", sep="")
  yrOfVar <- paste(eventPrefix, "YrOf", sep="")
  yrAftVar <- paste(eventPrefix, "YrAft", sep="")
  yrAdaptVar <- paste(eventPrefix, "Adapt", sep="")
  print("Coding year before, year or, year after the event, and adaptatio years variables")
  longData[,yrBefVar] <- ifelse(longData[,seqVar] == -1, 1, 0)
  longData[,yrOfVar] <- ifelse(longData[,seqVar] == 0, 1, 0)
  longData[,yrAftVar] <- ifelse(longData[,seqVar] == 1, 1, 0)
  
  # delete variables that aren't needed
  if (phase3 == F) { longData$eventWave <- NULL }
  else if (phase3 == T) { 
    longData$event1Wave <- NULL
    longData$event2Wave <- NULL
  }
  # save new file
  print(paste("Saving data to", fileToSave))
  write.csv (longData, paste(nPath, fileToSave, sep=""), row.names=F)
  print("----------------------------------------------------------------------------------")

}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

iMergeAllData <- function (filesToMerge, fileToSave="allData.csv", nPath=pathWorking, type="person") {
# this merges all the data in filesToMerge into a single data frame
# and then orders it by pid (if type = "person") or hid (if type = "house)
# filesToMerge = list of filenames that contain data to merge
# nPath = path where files are stored
# type = person or house - indicates whether pid or hid is the ID variable
  print("Merging:")
  print(paste("Start with", filesToMerge[1]))
  all <- read.csv(paste(nPath, filesToMerge[1], sep=""))
  if (length(filesToMerge > 1)) {
    for (i in 2:length(filesToMerge)) {
      print(paste("Adding", filesToMerge[i]))
      toadd <- read.csv(paste(nPath, filesToMerge[i], sep=""))
      all <- merge(all, toadd, all=T)
    }
  }
  if (type=="person") { all <- all[order(all$pid, all$wave),] }
  else if (type=="house") { all <- all[order(all$hid, all$wave),] }
  else { print("ERROR: type must be either person or house") }
  write.csv(all, paste(nPath, fileToSave, sep=""), row.names=F)
  return (all)
}



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

iSelectControls <- function (controls, varStem, conVar, originalFile, saveFile, 
firstWaveV=firstWaveOfLS, lastWaveV=lastWaveOfLS, firstYearS=firstYearOfStudy, 
NAresp=noresp, keepOriginalData=F, type="person", nPath=pathWorking) {
# this funtion selects a control sample that does not have any "other" responses (based on the recode syntax)
# conVar = variable name to identify what sample this is a control for (e.g., marCon = controls for the marriage sample) - it will be set to 1
# type = "person" or "house": is this a person-level or household-level file? this affects whether the id variable is "pid" or "hid"

  ERROR <- -333
  if (type == "person") { idVar <- "pid" }
  else if (type == "house") { idVar <- "hid" }
  else { print ("ERROR: Type must be either person or house") }
  print (paste ("Selecting control sample:", controls))
  # establish recoding syntax
  recodeString <- get(paste(controls,"ControlsRecode",sep=""))
  # longitudinal variable set from which to get year of the event
  mainVars <- paste(varStem, firstWaveV:lastWaveV, sep="")
  changeVars <- paste("change_", firstWaveV:lastWaveV, sep="")
  # read in full data
  data <- read.csv (paste (nPath, originalFile, sep=""))
  print(paste("start with",nrow(data)))
  data <- data[,c(idVar, mainVars)]             
  # recode all responses that are not one of the ones we're interested in (e.g., single & married) to 888 (other)
  data[, mainVars] <- lapply (data[, mainVars], iRecode, recodeString)
  
  # select only those that have some responses
  data$cValid <- rowSums(data[,mainVars] != noresp) > 0
  data <- data[data$cValid==T,]
  print(paste("cases with some non-NA responsess", nrow(data)))
  # select only those who don't have any 'other' responses
  data$cValid <- rowSums(data[,mainVars] == other) == 0
  data <- data[data$cValid==T,]
  print(paste("cases without any 'other' responses", nrow(data)))

  
  # set conVar to 1
  data[,conVar] <- 1
  
  if (keepOriginalData == F) { 
    data <- data[,c(idVar, conVar)] 
    }
  else { data <- data[,c(idVar, conVar, mainVars)] }

#  data <- data[,idVar]
  print(paste("total N",nrow(data)))
  print("========================================================================================")
  write.csv(data, file=paste(nPath, saveFile, sep=""), row.names=F)
}


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

iControlsDemoLSLong <- function(conFile, lsFile, demoFile, newFile, conVar, npath=pathWorking, lsPrefix="ls_", splitChar="_", type="person") {
# combines data on controls and life satisfaction data
# conVar = variable name to identify what sample this is a control for (e.g., marCon = controls for the marriage sample) - set in iSelectControls
# lsFile = final (clean) life satisfaction file
# newFile = filename of the file to which to save the combined data
# conVar = variable that identifies which control (
  print("Merging controls file with LS file")
  data <- merge(read.csv(paste(npath,conFile,sep="")), read.csv(paste(npath,lsFile,sep="")), by="pid", all=T)

  # create long File with a wave variable
  print("Creating long file")
  if (type == "person") { meltdata <- melt(data, id.vars=c("pid", conVar)) }
  else if (type == "house") { meltdata <- melt(data, id.vars=c("hid", conVar)) }
  varLevels <- levels(meltdata$variable)
  for (i in 1:length(varLevels)) {
    varLevels[i] <- strsplit(varLevels[i], splitChar)[[1]][2]
  }
  levels(meltdata$variable) <- varLevels
  names(meltdata)[names(meltdata)=="variable"] <- "wave"
  names(meltdata)[names(meltdata)=="value"] <- strsplit(lsPrefix, splitChar)[[1]][1]
  meltdata$wave <- as.numeric(levels(meltdata$wave))[meltdata$wave]   # can't do just as.numeric(longData$wave) - it starts the wave at 1 instead of 6
  meltdata <- meltdata[order(meltdata$pid, meltdata$wave),]
  
  print("Deleting NA responses")
  meltdata <- meltdata[!is.na(meltdata$ls),]
  meltdata <- meltdata[!is.na(meltdata[,conVar]),]
  
  print("Adding demographics data")
  meltdata <- merge(meltdata,read.csv(paste(pathWorking, demoFile, sep="")), all.x=T)
  
  print("Adding data from first wave (age, education, income)")
  # get first wave (so you can calculate age at first wave), and first reported education and income
  firstWave <- read.csv(paste(pathWorking, "firstWave.csv", sep=""))
  education <- read.csv(paste(pathWorking, "eduFirst.csv", sep=""))
  income <- read.csv(paste(pathWorking, "incFirst.csv", sep=""))
  # merge this information with meltdata
  meltdata <- merge(meltdata, firstWave, by="pid", all.x=T)
  meltdata <- merge(meltdata, education, by="pid", all.x=T)
  meltdata <- merge(meltdata, income, by="pid", all.x=T)
  # compute age at first wave
  meltdata$firstAge <- 1998 + meltdata$firstWave - meltdata$yrBirth
  
  print(paste("Saving",newFile))
  write.csv(meltdata, paste(npath, newFile, sep=""), row.names=F)
  
}

######################################
# additional functions 09/13/12
######################################

iRemovePrefix <- function(prefix, variables, data, keep="", remove=F) {
  # function to remove all prefixes from variables
  # prefix = prefix to remove
  # variables = variables from which to remove prefix
  # data = data in which to look
  # keep = additional variables to keep
  # remove = if T then only keep variables from which you are removing prefix and and what's passed to keep
  for (i in 1:length(variables)) {
    n <- names(data)
    norig <- paste(prefix, variables[i], sep="")
    nnew <- variables[i] 
    n <- sub(norig, nnew, n)
    names(data) <- n
  }
  if (remove==T) { data <- data[,c(keep, variables)] }
  return(data)
}

iData <- function(name, prefix="", agename="", varsToRename=toRemovePrefix, varsToKeep=toKeep) {
  # creates a data frame with data from only a single event
  # name = event name (e.g., sight)
  # prefix = default is the first three letters of event (e.g., sigyear, sigReact, sigAdapt)
  # agename = name of age at event variable (default is ageName, e.g., ageSight)
  if (prefix=="") { prefix <- substr(name,1,3) }
  nameyear <- paste(prefix, "year", sep="")
  if (agename=="") { ageName <- paste("age",toupper(substr(name,1,1)), substr(name,2,nchar(name)),sep="") }
  else { ageName <- agename }
  print(paste("Selecting only data for event", name))
  d <- allData[!is.na(allData[,nameyear]),]
  print("Centering age at event")
  agD <- aggregate(d, by=list(d$pid), FUN=mean)
  agD$cAge <- scale(agD[,ageName], scale=F)
  d <- merge(d, agD[,c("pid","cAge")], by="pid", all=T)
  print(paste("Removing prefix", prefix, "from:"))
  print(varsToRename)
  d <- iRemovePrefix(prefix, varsToRename, d, keep=varsToKeep, T)
  return(d)
}

iMatch2 <- function (c, e, controlVar, matchList, transList="", transNames="", toCenter="", f) {
  # matches control and event groups on a list of variables provided in "matchList"
  # matchList <- list of variables on which to match the groups
  # transList <- list of variables to be transformed using natural logarith
  # transNames <- list of new names for the transformed variables
  # toCenter <- variables to be centered
  # f <- formula for matching (for calculating propensity scores)
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
  agBoth <- both[,c("pid", matchList, "event", "control")]
  print("Aggregating to find means for each person, to plot and transform any variables that need to be transformed")
  agBoth <- aggregate(agBoth, by=list(agBoth$pid), FUN=mean)
  agBoth$Group.1 <- NULL
  for (i in 1:length(matchList)) {
    print(paste("Event people that are missing info on ", matchList[i], ": ",nrow(agBoth[is.na(agBoth[,matchList[i]]) & agBoth$event==1,]), sep=""))
  }
  # log transform any variables in transList
  if (transList != "") {
    if (length(transList) != length(transNames)) { print("ERROR: must provide names for all variables to be transformed") }
    for (i in 1:length(transList)) {
      print(paste("plotting distribution for",transList[i]))
      plot(density(agBoth[!is.na(agBoth[,transList[i]]), transList[i]]))
      agBoth[,transNames[i]] <- log(agBoth[,transList[i]] + 1)
      print(paste("plotting distribution of ln(", transList[i], ")", sep=""))
      plot(density(agBoth[!is.na(agBoth[,transNames[i]]),transNames[i]]))
    }
  }
  if (length(toCenter)>1 | toCenter[1] != "") {
    cList <- ""
    for (i in 1:length(toCenter)) {
      cName <- paste("c", toupper(substr(toCenter[i],1,1)), substr(toCenter[i],2,nchar(toCenter[i])),sep="")
      agBoth[,cName] <- scale(agBoth[,toCenter[i]], scale=F)
      cList[i] <- cName
    }
  } 
  print("Selecting only complete cases")
  comp <- agBoth[complete.cases(agBoth),]   # select only complete cases
  # matching
  print("Matching...")
  fFor <- as.formula(f)
  reg <- glm(fFor, family=binomial(link="logit"), data=comp)
  pscores <- predict(reg, type="link")
  outVar <- strsplit(strsplit(f, "~")[[1]][1]," ")[[1]][1]
  matches <- matching(z=comp[,outVar], score=pscores)
  matched <- comp[matches$matched,]
  # tag these cases as ones to use in the analyses
  print("Tagging matches")
  matched$use <- 1
  matched2 <- matched[,c("pid", "use", cList)]
  # combine with original dataset, so  you'll know which cases to use
  print("Combining original & matched datasets")
  EventControl <- merge(both, matched2, all.x=T, by="pid")
  EventControl <- EventControl[!is.na(EventControl$use),]
  # make linear trend start at the first wave that they are in the study
  print("Creating a linear trend variable")
  EventControl$lin <- EventControl$wave - EventControl$firstWave
  return(EventControl)
}

iConRA2 <- function (eventData, controlData, controlVar, prefix="", matchList, transList="", transNames="", toCenter="", fMatch, fRA, fRAnocovar, dv="ls") {
  # matchList <- list of variables on which to match the groups
  # transList <- list of variables to be transformed using natural logarith
  # transNames <- list of new names for the transformed variables
  # toCenter <- variables to be centered
  # f <- formula for matching (for calculating propensity scores)
  EventControl <- iMatch2(c=controlData, e=eventData, controlVar=controlVar, matchList=matchList, transList=transList, transNames=transNames, toCenter=toCenter, f=fMatch)
  # set react and adapt to 0 for control group, leave it as is for event group
  EventControl$react <- ifelse(EventControl$event==1, EventControl[,paste(prefix,"React",sep="")], 0)
  EventControl$adapt <- ifelse(EventControl$event==1, EventControl[,paste(prefix,"Adapt",sep="")], 0)
  # analyses
  print("Running final regression on matched cases")
  fRA <- as.formula(fRA)
  RA <- lmer( fRA, data=EventControl)
  fRAnocovar <- as.formula(fRAnocovar)
  RAnocovar <- lmer( fRAnocovar, data=EventControl)
  print("Plotting the means")
  meansAll <- aggregate(EventControl, by=list(EventControl$control, EventControl$lin), FUN=mean)
  meansPlot <- qplot(x=lin, y=get(dv), colour=control, data=meansAll, main=prefix)
  # return results: complete cases, matched cases, matching regression, final regression
  results <- list(EventControl=EventControl, RA=RA, RAnocovar=RAnocovar,meansPlot=meansPlot)
  return(results)
}

# to plot control groups
iSubPlot_old <- function (d, dataRaw, xRaw, eventTitle, xrange, yrange) {
  dEvent <- d[d$event==1,c(xRaw,"lin","predLSevent")]
  dControl <- d[d$event==0,c(xRaw,"lin","predLScontrol")]
  dIfNoEvent <- d[d$event==1,c(xRaw,"lin","predLSifnoevent")]
  dEvent1 <- aggregate(dEvent, by=list(dEvent$lin), FUN=mean)
  dControl1 <- aggregate(dControl, by=list(dControl$lin), FUN=mean)
  dIfNoEvent1 <- aggregate(dIfNoEvent, by=list(dIfNoEvent$lin), FUN=mean)
  dEvent1
  dControl1
  dIfNoEvent1
  dEvent2 <- aggregate(dEvent, by=list(dEvent[,xRaw]), FUN=mean)
  dIfNoEvent2 <- aggregate(dIfNoEvent, by=list(dIfNoEvent[,xRaw]), FUN=mean)
  dEvent2
  dIfNoEvent2
  seqlin <- dEvent2[,c(xRaw,"lin")]
  linseq <- dEvent1[,c(xRaw,"lin")]
  dControl[,xRaw] <- NULL
  dControl2 <- merge(dControl, linseq, by="lin", all=T)
  dControl2 <- aggregate(dControl2, by=list(dControl2[,xRaw]), FUN=mean)
  # in this case, for both dControl2 and dRawSeq, you use the average cSeq for each lin (from the event group) for the control group
  dRawLin <- aggregate(dataRaw[,c("lin","event","ls")], by=list(dataRaw$lin, dataRaw$event), FUN=mean)
  dRawSeq <- merge(dataRaw, linseq, by="lin", all=T)
  dRawSeq <- dRawSeq[,c(paste(xRaw,".x",sep=""), paste(xRaw,".y",sep=""),"event","ls")]
  dRawSeq[,xRaw] <- ifelse(!is.na(dRawSeq[,paste(xRaw,".x",sep="")]), dRawSeq[,paste(xRaw,".x",sep="")], dRawSeq[,paste(xRaw,".y",sep="")])
  dRawSeq <- aggregate(dRawSeq[,c(xRaw,"event","ls")], by=list(dRawSeq[,xRaw], dRawSeq$event), FUN=mean)
  
  plotLin <- ggplot(data=d, aes_string(x=xRaw, y="predLSevent")) + 
    geom_line(data=dIfNoEvent1, aes(x=lin, y=predLSifnoevent), linetype="solid", colour="grey") +
    geom_line(data=dEvent1, aes(x=lin, y=predLSevent), linetype="solid") +
    geom_line(data=dControl1, aes(x=lin, y=predLScontrol), linetype="longdash") +
    geom_point(data=dRawLin[dRawLin$event==1,], aes_string(x="lin", y="ls")) +
    geom_point(data=dRawLin[dRawLin$event==0,], aes_string(x="lin", y="ls", shape=4)) +
    coord_cartesian(ylim=yrange,xlim=c(0,xrange[2])) +
    scale_x_continuous("Year", breaks=seq(0,xrange[2],by=1)) +
    scale_y_continuous("Life Satisfaction", breaks=seq(yrange[1],yrange[2],by=.5)) +
    opts(
      panel.grid.major=theme_blank(),
      panel.grid.minor=theme_blank(),
      panel.background = theme_blank(),
      axis.line = theme_segment(),
      title=eventTitle)
  
  # with year from event at x-axis
  plotSeq <- ggplot(data=d, aes_string(x=xRaw, y="predLSevent")) + 
    geom_line(data=dIfNoEvent2, aes_string(x=xRaw, y="predLSifnoevent"), linetype="solid", colour="grey") +
    geom_line(data=dEvent2, aes_string(x=xRaw, y="predLSevent"), linetype="solid") +
    geom_line(data=dControl2, aes_string(x=xRaw, y="predLScontrol"), linetype="longdash") +
    geom_point(data=dRawSeq[dRawSeq$event==1,], aes_string(x=xRaw, y="ls")) +
    geom_point(data=dRawSeq[dRawSeq$event==0,], aes_string(x=xRaw, y="ls", shape=4)) +
    coord_cartesian(ylim=yrange,xlim=xrange) +
    scale_x_continuous("Year", breaks=seq(xrange[1],xrange[2],by=1)) +
    scale_y_continuous("Life Satisfaction", breaks=seq(yrange[1],yrange[2],by=.5)) +
    opts(
      panel.grid.major=theme_blank(),
      panel.grid.minor=theme_blank(),
      panel.background = theme_blank(),
      axis.line = theme_segment(),
      title=eventTitle)
  
  plot <- list(plotLin=plotLin, plotSeq=plotSeq)
  
  return (plot)
}

iSubPlot <- function (d, dataRaw, xRaw, eventTitle, xrange, yrange, dv="ls", dvLabel="Life Satisfaction") {
  x1 <- ifelse(xrange[1]<0, ceiling(xrange[1]), floor(xrange[1]))
  x2 <- ifelse(xrange[2]<0, ceiling(xrange[2]), floor(xrange[2]))
  dEvent <- d[d$event==1,c(xRaw,"lin","predLSevent")]
  dControl <- d[d$event==0,c(xRaw,"lin","predLScontrol")]
  dIfNoEvent <- d[d$event==1,c(xRaw,"lin","predLSifnoevent")]
  dEvent1 <- aggregate(dEvent, by=list(dEvent$lin), FUN=mean)
  dControl1 <- aggregate(dControl, by=list(dControl$lin), FUN=mean)
  dIfNoEvent1 <- aggregate(dIfNoEvent, by=list(dIfNoEvent$lin), FUN=mean)
  dEvent1
  dControl1
  dIfNoEvent1
  dEvent2 <- aggregate(dEvent, by=list(dEvent[,xRaw]), FUN=mean)
  dIfNoEvent2 <- aggregate(dIfNoEvent, by=list(dIfNoEvent[,xRaw]), FUN=mean)
  dEvent2
  dIfNoEvent2
  seqlin <- dEvent2[,c(xRaw,"lin")]
  linseq <- dEvent1[,c(xRaw,"lin")]
  dControl[,xRaw] <- NULL
  dControl2 <- merge(dControl, linseq, by="lin", all=T)
  dControl2 <- aggregate(dControl2, by=list(dControl2[,xRaw]), FUN=mean)
  # in this case, for both dControl2 and dRawSeq, you use the average cSeq for each lin (from the event group) for the control group
  dRawLin <- aggregate(dataRaw[,c("lin","event",dv)], by=list(dataRaw$lin, dataRaw$event), FUN=mean)
  dRawSeq <- merge(dataRaw, linseq, by="lin", all=T)
  dRawSeq <- dRawSeq[,c(paste(xRaw,".x",sep=""), paste(xRaw,".y",sep=""),"event",dv)]
  dRawSeq[,xRaw] <- ifelse(!is.na(dRawSeq[,paste(xRaw,".x",sep="")]), dRawSeq[,paste(xRaw,".x",sep="")], dRawSeq[,paste(xRaw,".y",sep="")])
  dRawSeq <- aggregate(dRawSeq[,c(xRaw,"event",dv)], by=list(dRawSeq[,xRaw], dRawSeq$event), FUN=mean)
  
  plotLin <- ggplot(data=d, aes_string(x=xRaw, y="predLSevent")) + 
    geom_line(data=dIfNoEvent1, aes(x=lin, y=predLSifnoevent), linetype="solid", colour="grey") +
    geom_line(data=dEvent1, aes(x=lin, y=predLSevent), linetype="solid") +
    geom_line(data=dControl1, aes(x=lin, y=predLScontrol), linetype="longdash") +
    geom_point(data=dRawLin[dRawLin$event==1,], aes_string(x="lin", y=dv)) +
    geom_point(data=dRawLin[dRawLin$event==0,], aes_string(x="lin", y=dv, shape=4)) +
    coord_cartesian(ylim=yrange,xlim=c(0,xrange[2])) +
    scale_x_continuous("Year", breaks=seq(0,x2,by=1)) +
    scale_y_continuous(dvLabel, breaks=seq(yrange[1],yrange[2],by=.5)) +
    plotformat(eventTitle)
  
  # with year from event at x-axis
  plotSeq <- ggplot(data=d, aes_string(x=xRaw, y="predLSevent")) + 
    geom_line(data=dIfNoEvent2, aes_string(x=xRaw, y="predLSifnoevent"), linetype="solid", colour="grey") +
    geom_line(data=dEvent2, aes_string(x=xRaw, y="predLSevent"), linetype="solid") +
    geom_line(data=dControl2, aes_string(x=xRaw, y="predLScontrol"), linetype="longdash") +
    geom_point(data=dRawSeq[dRawSeq$event==1,], aes_string(x=xRaw, y=dv)) +
    geom_point(data=dRawSeq[dRawSeq$event==0,], aes_string(x=xRaw, y=dv, shape=4)) +
    coord_cartesian(ylim=yrange,xlim=xrange) +
    scale_x_continuous("Year", breaks=seq(x1,x2,by=1)) +
    scale_y_continuous(dvLabel, breaks=seq(yrange[1],yrange[2],by=.5)) +
    plotformat(eventTitle)
  
  plot <- list(plotLin=plotLin, plotSeq=plotSeq)
  
  return (plot)
}

iPlotControlsRAnew <- function(res, eventTitle, dataRaw, xRaw="Seq", xrange=c(-10,10), yrange=c(6.5, 8.5), dv="ls", dvLabel="Life Satisfaction") {
  # make plots, for a hypothetical case where first year is 4 years before the event, and all covariates are 0
  # res = where results are stored
  # eventTitle = event: "Marriage", "Childbirth", "Widowhood", "Unemployment", or "Disability"
  int <- fixef(res)[1]
  lin <- fixef(res)[2]
  control <- fixef(res)[3]
  react <- fixef(res)[4]
  adapt <- fixef(res)[5]
  
  d <- dataRaw
  d$predLSevent <- int + lin*d$lin + react*d$react + adapt*d$adapt
  d$predLScontrol <- int + lin*d$lin + control
  d$predLSifnoevent <- int + lin*d$lin
  
  plot <- iSubPlot(d, dataRaw, xRaw, eventTitle, xrange, yrange, dv=dv, dvLabel=dvLabel)
  return(plot)
}

######################################
# additional functions 09/18/12
######################################

iControlsDemoLSLong2 <- function(conFile, lsFile, demoFile, newFile, conVar, npath=pathWorking, lsPrefix="ls_", splitChar="_", type="person", conList, dv="ls") {
  # combines data on controls and life satisfaction data
  # NEW: list of control variables is passed in by user (other than sex and age, they're always included)
  # conList must be a list of file names that contain control variables (e.g., eduFirst.csv)
  # conVar = variable name to identify what sample this is a control for (e.g., marCon = controls for the marriage sample) - set in iSelectControls
  # lsFile = final (clean) life satisfaction file
  # newFile = filename of the file to which to save the combined data
  # conVar = variable that identifies which control (
  print("Merging controls file with LS file")
  data <- merge(read.csv(paste(npath,conFile,sep="")), read.csv(paste(npath,lsFile,sep="")), by="pid", all=T)
  
  # create long File with a wave variable
  print("Creating long file")
  if (type == "person") { meltdata <- melt(data, id.vars=c("pid", conVar)) }
  else if (type == "house") { meltdata <- melt(data, id.vars=c("hid", conVar)) }
  varLevels <- levels(meltdata$variable)
  for (i in 1:length(varLevels)) {
    varLevels[i] <- strsplit(varLevels[i], splitChar)[[1]][2]
  }
  levels(meltdata$variable) <- varLevels
  names(meltdata)[names(meltdata)=="variable"] <- "wave"
  names(meltdata)[names(meltdata)=="value"] <- strsplit(lsPrefix, splitChar)[[1]][1]
  meltdata$wave <- as.numeric(levels(meltdata$wave))[meltdata$wave]   # can't do just as.numeric(longData$wave) - it starts the wave at 1 instead of 6
  meltdata <- meltdata[order(meltdata$pid, meltdata$wave),]
  
  print("Deleting NA responses")
  meltdata <- meltdata[!is.na(meltdata[,dv]),]
  meltdata <- meltdata[!is.na(meltdata[,conVar]),]
  
  print("Adding demographics data")
  meltdata <- merge(meltdata,read.csv(paste(pathWorking, demoFile, sep="")), all.x=T)
  
  print("Adding data from first wave (age and control variables)")
  # get first wave (so you can calculate age at first wave), and first reported education and income
  firstWave <- read.csv(paste(pathWorking, "firstWave.csv", sep=""))
  meltdata <- merge(meltdata, firstWave, by="pid", all.x=T)
  for (i in 1:length(conList)) {
    conData <- read.csv(paste(pathWorking, conList[i], sep=""))
    # merge this information with meltdata
    meltdata <- merge(meltdata, conData, by="pid", all.x=T)
  }
  # compute age at first wave
  meltdata$firstAge <- (firstYearOfStudy-1) + meltdata$firstWave - meltdata$yrBirth
  print(paste("Saving",newFile))
  write.csv(meltdata, paste(npath, newFile, sep=""), row.names=F)
}


plotformat <- function(title="") {
  f <- opts(
    panel.grid.major=theme_blank(),
    panel.grid.minor=theme_blank(),
    panel.background = theme_blank(),
    axis.line = theme_segment(),
    axis.text.x=theme_text(colour='black'),
    axis.text.y=theme_text(colour='black'),
    title=title)
  return(f)
}

# ------------------------ NONLINEAR MODEL FUNCTIONS ---------------------------- #
# --------------- FUNCTIONS ------------------------------------ #
# asympBef = baseline asymptote (before the event)
# changeBef = rate of change before the event
# reactDiff = reaction in the year of the event (difference from pre-event baseline)
# asympDiff = adaptation baseline (difference from pre-even baseline)
# changeAft = rate of change after the event
iAsympDiff <- function(seq, bef, aft, asympBef, changeBef, reactDiff, asympDiff, changeAft) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * (asympBef + (reactDiff)*(1/(1-changeBef))^seq) +
    aft * ((asympBef+asympDiff) + (reactDiff-asympDiff)*(1-changeAft)^seq)
}
# gradient
iAsympDiffG <- deriv(body(iAsympDiff)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft"), function.arg=iAsympDiff)

# with control groups
iNonlinControls <- function(seq, lin, bef, aft, event, control, linear, asympBef, changeBef, reactDiff, asympDiff, changeAft, group) {
  # seq is a sequence variable
  # linear is the linear trend over time (e.g., age trend)
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  lin * linear +
    event *
    (bef * (asympBef + (reactDiff)*(1/(1-changeBef))^seq) +
    aft * ((asympBef+asympDiff) + (reactDiff-asympDiff)*(1-changeAft)^seq)) +
    control *
    (asympBef+group)
}
# gradient
iNonlinControlsG <- deriv(body(iNonlinControls)[[2]], namevec=c("linear", "asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft", "group"), function.arg=iNonlinControls)


iConNL2 <- function (eventData, controlData, controlVar, prefix="", matchList, transList="", transNames="", toCenter="", fMatch, fNL, dv="ls", svals) {
  # matchList <- list of variables on which to match the groups
  # transList <- list of variables to be transformed using natural logarith
  # transNames <- list of new names for the transformed variables
  # toCenter <- variables to be centered
  # f <- formula for matching (for calculating propensity scores)
  EventControl <- iMatch2(c=controlData, e=eventData, controlVar=controlVar, matchList=matchList, transList=transList, transNames=transNames, toCenter=toCenter, f=fMatch)
  # set before, after, and sequence and adapt to 0 for control group, leave it as is for event group
  EventControl$bef <- ifelse(EventControl$event==1, EventControl[,paste(prefix,"Bef",sep="")], 0)
  EventControl$aft <- ifelse(EventControl$event==1, EventControl[,paste(prefix,"Aft",sep="")], 0)
  EventControl$seq <- ifelse(EventControl$event==1, EventControl[,paste(prefix,"Seq",sep="")], 0)
  # analyses
  print("Running final regression on matched cases")
  forNL <- as.formula(fNL)
  NL <- nlmer( forNL, data=EventControl, start=svals, verbose=T)
  # return results: complete cases, matched cases, matching regression, final regression
  results <- list(EventControl=EventControl, NL=NL)
  return(results)
}

iPlotControlsNL <- function(res, eventTitle, dataRaw, xRaw, ylim=c(4.5,8.5), dvName="Life Satisfaction") {
  # res = where results are stored
  # eventTitle = event: "Marriage", "Childbirth", "Widowhood", "Unemployment", or "Disability"
  lin <- fixef(res)[1]
  aB <- fixef(res)[2]
  cB <- fixef(res)[3]
  rD <- fixef(res)[4]
  aD <- fixef(res)[5]
  cA <- fixef(res)[6]
  g <- fixef(res)[7]
  
  d <- data.frame(seq=seq(-10,10, by=1), yr=seq(0,20, by=1))
  d$bef <- ifelse(d$seq<0, 1, 0)
  d$aft <- ifelse(d$seq>=0, 1, 0)
  d$event <- lin*d$yr +
    d$bef* (aB + (rD)*(1/(1-cB))^d$seq) +
    d$aft* ((aB+aD) + (rD-aD)*(1-cA)^d$seq)
  d$control <- lin*d$yr + aB+g
  d$ifnoevent <- lin*d$yr + aB
  
  plot <- ggplot(data=d, aes(x=seq, y=event)) +
    geom_line(data=d, aes(x=seq, y=ifnoevent), linetype="solid", colour="grey") +
    geom_line(data=d, aes(x=seq, y=event), linetype="solid") +
    geom_line(data=d, aes(x=seq, y=control), linetype="longdash") +
    coord_cartesian(ylim=ylim,xlim=c(-10,10)) +
    scale_x_continuous("Year", breaks=seq(-10,10,by=1)) +
    scale_y_continuous(dvName, breaks=seq(ylim[1],ylim[2],by=.5)) +
    plotformat(eventTitle)
  return (plot)
}

iPlotEvent <- function(dataRaw, dataPred, xRaw, title="", xlim=c(-10,10), ylim=c(6.5,8.5), dv="ls") {
  x1 <- ifelse(xlim[1]<0, ceiling(xlim[1]), floor(xlim[1]))
  x2 <- ifelse(xlim[2]<0, ceiling(xlim[2]), floor(xlim[2]))
  ggplot(data=dataRaw, aes_string(x=xRaw, y=dv)) +
    stat_summary(fun.y=mean, geom="point") +
    geom_line(data=dataPred, aes(x=seq, y=predLS)) +
    coord_cartesian(ylim=ylim,xlim=xlim) +
    scale_x_continuous("Year", breaks=seq(x1,x2,by=1)) +
    scale_y_continuous("Life Satisfaction", breaks=seq(ylim[1],ylim[2],by=.5)) +
    plotformat(title)
}
iPredNonlin <- function (a) {
  d <- data.frame(seq=seq(-10,10, by=.5))
  d$bef <- ifelse(d$seq<0, 1, 0)
  d$aft <- ifelse(d$seq>=0, 1, 0)
  d$aB <- a[1]
  d$cB <- a[2]
  d$rD <- a[3]
  d$aD <- a[4]
  d$cA <- a[5]
  d$predLS <-
    d$bef * (d$aB + (d$rD)*(1/(1-d$cB))^d$seq) +
    d$aft * ((d$aB+d$aD) + (d$rD-d$aD)*(1-d$cA)^d$seq)
  return(d)
}

iPlotControlsNLnew <- function(res, eventTitle, dataRaw, xRaw="Seq", xrange=c(-10,10), yrange=c(6.5, 8.5), dv="ls", dvLabel="Life Satisfaction") {
  lin <- fixef(res)[1]
  aB <- fixef(res)[2]
  cB <- fixef(res)[3]
  rD <- fixef(res)[4]
  aD <- fixef(res)[5]
  cA <- fixef(res)[6]
  g <- fixef(res)[7]
  
  d <- dataRaw
  d$predLSevent <- lin*d$lin +
    d$bef* (aB + (rD)*(1/(1-cB))^d$seq) +
    d$aft* ((aB+aD) + (rD-aD)*(1-cA)^d$seq)
  d$predLScontrol <- lin*d$lin + aB+g
  d$predLSifnoevent <- lin*d$lin + aB
  
  plot <- iSubPlot(d, dataRaw, xRaw, eventTitle, xrange, yrange, dv=dv, dvLabel=dvLabel)
  return(plot)
}
# ------------------------ END OF NONLINEAR MODEL FUNCTIONS ---------------------------- #


