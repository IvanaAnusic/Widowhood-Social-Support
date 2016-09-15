iRunGetVariablesHILDA <- function(varName, newName) {
  # runs the iGetVariables function for newName
  nDataFile <- paste("$",newName,".csv",sep="")
  longfilename <- paste(newName,"Long.csv",sep="")
  widefilename <- paste(newName,"Wide.csv",sep="")
  iGetVariables(varsToPull=varName, varsToSave=newName, nDataFile=nDataFile, longfilename=longfilename, firstWaveV=3, widefilename=widefilename)
}

iRunSelectSampleHILDA <- function(name, f=3) {
  # runs the iSelectSample function for name
  varStem <- paste(name,"_",sep="")
  originalFile <- paste(name,"Wide.csv",sep="")
  capName <- paste(toupper(substr(name, 1, 1)),substr(name, 2, nchar(name)),sep="")
  saveFile <- paste(capName, "Year.csv", sep="")
  iSelectSample (sample=name, firstWaveV=f, phase1="no", phase2="yes", varStem=varStem, originalFile=originalFile, saveFile=saveFile)
}

iRunBefAftCodesHILDA <- function(name){
  # runs the iBeforeAfterLS function (to select only people with 1 wave before and 1 wave after event)
  # runs the iAllCodesLSLong fuction to create long file with all dummy codes
  capName <- paste(toupper(substr(name, 1, 1)),substr(name, 2, nchar(name)),sep="")
  eventFile <- paste(capName, "Year.csv", sep="")
  twoWavesFile <- paste(capName, "Year (with 2 waves of LS).csv", sep="")
  fileToSave <- paste("final", capName, "Data.csv", sep="")
  prefix <- substr(name, 1, 3)
  iBeforeAfterLS(eventFile=eventFile, lsFile="clean lsWide.csv")
  iAllCodesLSLong (twoWavesFile, "clean lsWide.csv", prefix, fileToSave=fileToSave)
}

iCleanLifeSatHILDA <- function(lsFile, newFile=paste("clean",lsFile), nPath=pathWorking) {
  # cleans up life satisfaction data
  lsData <- read.csv (paste(nPath,lsFile,sep=""))
  lsVars <- paste("ls_", firstWaveOfLS:lastWaveOfLS, sep="")
  lsData[,lsVars] <- lapply(lsData[,lsVars], iRecode, "-999:-1=NA; 11:999=NA")
  write.csv(lsData, paste(nPath,newFile,sep=""), row.names=F)
}
