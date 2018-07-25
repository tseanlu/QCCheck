
ARMcheck <- function(indat){
  setwd("//cdars.pfizer.com/cdars/prod/prjA409/internship/A4091056/saseng/cdisc3_0/data/rawdata")
  if (file.exists("final.sas7bdat")==TRUE) {
    final <- read_sas("final.sas7bdat")
    final <- final[which(final$FINSTAT==11),]
    new <- indat[indat$USUBJID %in% final$PID,]
    if (all(new$ACTARM != "Not Assigned") & all(new$ACTARMCD != "NOTASSGN")) {
      finding <- "ACTARM and ACTARMCD are correct."
      conclusion <- "Passed"
    } else if (all(new$ACTARM != "Not Assigned") == FALSE & all(new$ACTARMCD != "NOTASSGN")) {
      finding <- "ACTARM is incorrect. ACTARMCD is correct."
      conclusion <- "Failed"
    } else if (all(new$ACTARM != "Not Assigned") & all(new$ACTARMCD != "NOTASSGN")==FALSE) {
      finding <- "ACTARM is correct. ACTARMCD is incorrect."
      conclusion <- "Failed"
    } else {
      finding <- "Both ACTARM and ACTARMCD are incorrect."
      conclusion <- "Failed"
    }
  } else {
    finding <- "PDS.final dataset does not exist, could not check the correctness of ACTARM and ACTARMCD."
    conclusion <- "Not applicable"
  }
  result <- list(Finding = finding,
                 Conclusion = conclusion)
  return(result)
}

DYCHECK <- function(indat,datname){
  name <- toupper(gsub(pattern = "\\..*", replacement = "", x = datname))
  var <- colnames(indat)
  RF <- RF()
  full <- merge(x = indat, y = RF, by = "USUBJID", all.x = TRUE)
  full$RFSTT <- as.Date(full$RFST, format = "%Y-%m-%d")
  finding <- c()
  conclusion <- c()
  E <- c()
  R <- c()
  #If --DTC in dataset
  if(paste(name, "DY", sep = "") %in% var){
    #Check 0 in --DY
    if (0 %in% full[paste(name, "DY", sep = "")]){
      finding[1] <- paste("0 (s) found in ", name, "DY.", sep = "")
    } else {
      finding[1] <- paste("There is no 0 in ", name, "DY.", sep = "")
    }
    full$DTC <- as.Date(full[[paste(name, "DTC", sep = "")]],format = "%Y-%m-%d")
    full <- full[!is.na(full$DTC) & !is.na(full$RFSTT),]
    #Generate --DY
    full$DY[full$DTC >= full$RFSTT] <- difftime(time1 = full$DTC[full$DTC >= full$RFSTT], time2 = full$RFSTT[full$DTC >= full$RFSTT], units = "days") + 1
    full$DY[full$DTC < full$RFSTT] <- difftime(time1 = full$DTC[full$DTC < full$RFSTT], time2 = full$RFSTT[full$DTC < full$RFSTT], units = "days")
    #Check if --DY is correctly generated
    if (all(full$DY == full[paste(name, "DY", sep = "")])) {
      finding[2] <- paste(name, "DY is correctly generated in the dataset.", sep = "")
      conclusion <- "Passed"
    } else {
      SUBJID <- full$USUBJID[full$DY != full[paste(name, "DY", sep = "")]]
      DY <- full$DY[full$DY != full[paste(name, "DY", sep = "")]]
      ODY <- full[paste(name, "DY", sep = "")][full$DY != full[paste(name, "DY", sep = "")]]
      finding[2] <- paste(name, "DY is not correctly generated in the dataset.", sep = "")
      conclusion <- "Failed"
      for (i in 1:length(SUBJID)) {
        E[i] <- paste(name, "DY of subject ", SUBJID[i], " is", ODY[i], ", but should be ", DY[i], ".")
      }
    }
    # If --STDY in dataset
  } else if (paste(name, "STDY", sep = "") %in% var){
    #Check 0 in --STDY
    if (0 %in% full[paste(name, "STDY", sep = "")]){
      finding[1] <- paste("0 (s) found in ", name, "STDY.", sep = "")
    } else {
      finding[1] <- paste("There is no 0 in ", name, "STDY.", sep = "")
    }
    full$DTC <- as.Date(full[[paste(name, "STDTC", sep = "")]],format = "%Y-%m-%d")
    full <- full[!is.na(full$DTC) & !is.na(full$RFSTT),]
    #Generate --DY
    full$DY[full$DTC >= full$RFSTT] <- difftime(time1 = full$DTC[full$DTC >= full$RFSTT], time2 = full$RFSTT[full$DTC >= full$RFSTT], units = "days") + 1
    full$DY[full$DTC < full$RFSTT] <- difftime(time1 = full$DTC[full$DTC < full$RFSTT], time2 = full$RFSTT[full$DTC < full$RFSTT], units = "days")
    #Check if --STDY is correctly generated
    if (all(full$DY == full[paste(name, "STDY", sep = "")])) {
      finding[2] <- paste(name, "STDY is correctly generated in the dataset.", sep = "")
      conclusion <- "Passed"
    } else {
      SUBJID <- full$USUBJID[full$DY != full[paste(name, "STDY", sep = "")]]
      DY <- full$DY[full$DY != full[paste(name, "STDY", sep = "")]]
      ODY <- full[paste(name, "STDY", sep = "")][full$DY != full[paste(name, "STDY", sep = "")]]
      finding[2] <- paste(name, "STDY is not correctly generated in the dataset.", sep = "")
      conclusion[1] <- "Failed"
      for (i in 1:length(SUBJID)) {
        E[i] <- paste(name, "STDY of subject ", SUBJID[i], " is", ODY[i], ", but should be ", DY[i], ".")
      }
    }
    # If --ENDTC in dataset
    if (paste(name, "ENDY", sep = "") %in% var) {
      #Check 0 in --STDY
      if (0 %in% full[paste(name, "ENDY", sep = "")]){
        finding[3] <- paste("0 (s) found in ", name, "ENDY.", sep = "")
      } else {
        finding[3] <- paste("There is no 0 in ", name, "ENDY.", sep = "")
      }
      full$DTC <- as.Date(full[[paste(name, "ENDTC", sep = "")]],format = "%Y-%m-%d")
      full <- full[!is.na(full$DTC) & !is.na(full$RFSTT),]
      #Generate --DY
      full$DY[full$DTC >= full$RFSTT] <- difftime(time1 = full$DTC[full$DTC >= full$RFSTT], time2 = full$RFSTT[full$DTC >= full$RFSTT], units = "days") + 1
      full$DY[full$DTC < full$RFSTT] <- difftime(time1 = full$DTC[full$DTC < full$RFSTT], time2 = full$RFSTT[full$DTC < full$RFSTT], units = "days")
      #Check if --STDY is correctly generated
      if (all(full$DY == full[paste(name, "ENDY", sep = "")])) {
        finding[4] <- paste(name, "ENDY is correctly generated in the dataset.", sep = "")
        conclusion[2] <- "Passed"
      } else {
        SUBJID <- full$USUBJID[full$DY != full[paste(name, "ENDY", sep = "")]]
        DY <- full$DY[full$DY != full[paste(name, "ENDY", sep = "")]]
        ODY <- full[paste(name, "ENDY", sep = "")][full$DY != full[paste(name, "ENDY", sep = "")]]
        finding[4] <- paste(name, "ENDY is not correctly generated in the dataset.", sep = "")
        for (i in 1:length(SUBJID)) {
          R <- paste(name, "ENDY of subject ", SUBJID[i], " is", ODY[i], ", but should be ", DY[i], ".")
        }
      }
    }
  } else {
    finding <- paste("No DY variable in ", name,  "dataset", sep = "")
    conclusion <- "Not applicable"
  }
  Finding <- paste(finding, E, R, collapse = " ")
  Conclusion <- paste(conclusion, collapse = " ")
  results <- list(Finding = Finding,
                  Conclusion = Conclusion)
  return(results)
}

DTCfmt <- function(indat,datname) {
  name <- toupper(gsub(pattern = "\\..*", replacement = "", x = datname))
  var <- colnames(indat)
  DTCvar <- var[grepl("DTC",var)]
  datDTC <- indat[,DTCvar]
  a <- c()
  finding <- c()
  conclusion <- c()
  badDTC <- c()
  for (i in 1:ncol(datDTC)) {
    DTCcol <- na.omit(datDTC[[i]])
    year <- substr(DTCcol,1,4)
    month <- substr(DTCcol,6,7)
    day <- substr(DTCcol,9,10)
    hour <- substr(DTCcol,12,13)
    minute <- substr(DTCcol,15,16)
    second <- substr(DTCcol,18,19)
    if (all(grepl("[0-9]",year[year!=""]))==TRUE & all(grepl("[0-9]",month[month!=""]))==TRUE 
        & all(grepl("[0-9]",day[day!=""]))==TRUE & all(grepl("[0-9]",hour[hour!=""]))==TRUE
        & all(grepl("[0-9]",minute[minute!=""]))==TRUE & all(grepl("[0-9]",second[second!=""]))==TRUE) {
      a[i] <- "TRUE"
      badDTC[i] <- ""
    } else {
      a[i] <- "FALSE"
      badDTC[i] <- DTCvar[i]
    }
  }
  if (all(a=="TRUE")) {
    finding <- paste("All DTC variables in ",name,"dataset are in ISO format.")
    conclusion <- "Passed"
  } else {
    finding <- paste(badDTC[badDTC!=""], "in", name, "dataset are not in ISO format.",collapse = " ")
    conclusion <- "Failed"
  }
  result <- list(Finding = finding,
                 Conclusion = conclusion
  )
  return(result)
}

EPOCHcheck <- function(indat, datname) {
  name <- toupper(gsub(pattern = "\\..*", replacement = "", x = datname))
  var <- colnames(indat)
  RF <- RF()
  full <- merge(x = indat, y = RF, by = "USUBJID", all.x = TRUE)
  if("EPOCH" %in% var){
    if(paste(name, "DTC", sep = "") %in% var){
      full[[paste(name, "DTC", sep = "")]][nchar(full[[paste(name, "DTC", sep = "")]])==10] <- paste(full[[paste(name, "DTC", sep = "")]][nchar(full[[paste(name, "DTC", sep = "")]])==10], "T00:00:00", sep = "")
      full$DTC <- strptime(full[[paste(name, "DTC", sep = "")]],format = "%Y-%m-%dT%H:%M:%S")
    } else if (paste(name, "STDTC", sep = "") %in% var){
      full[[paste(name, "STDTC", sep = "")]][nchar(full[[paste(name, "STDTC", sep = "")]])==10] <- paste(full[[paste(name, "STDTC", sep = "")]][nchar(full[[paste(name, "STDTC", sep = "")]])==10], "T00:00:00", sep = "")
      full$DTC <- strptime(full[[paste(name, "STDTC", sep = "")]],format = "%Y-%m-%dT%H:%M:%S")
    } else {
      finding <- paste("EPOCH exists in dataset, but no ", name, "DTC variable in dataset, could not check the correctness of EPOCH", sep = "")
      conclusion <- "Failed"
    }
    full$RFSTT <-  strptime(full$RFST,format="%Y-%m-%dT%H:%M:%S")
    full$RFXENN <- strptime(full$RFXEN,format="%Y-%m-%dT%H:%M:%S")
    full$EPOC[is.na(full$DTC)] <- ""
    full$EPOC[full$DTC < full$RFSTT] <- "SCREENING"
    full$EPOC[full$RFSTT <= full$DTC & full$DTC <= full$RFXENN] <- "TREATMENT"
    full$EPOC[full$RFXENN < full$DTC] <- "FOLLOW-UP"
    
    if (all(full$EPOC == full$EPOCH)) {
      finding <- paste("EPOCH exists, and is correct", sep = " ")
      conclusion <- "Passed"
    } else {
      SUBJID <- full$USUBJID[full$EPOC != full$EPOCH]
      REPOCH <- full$EPOC[full$EPOC != full$EPOCH]
      OEPOCH <- full$EPOCH[full$EPOC != full$EPOCH]
      Finding <- c()
      for (i in 1:length(SUBJID)) {
        Finding[i] <- (paste("EPOCH of subject ", SUBJID[i], " is", OEPOCH[i], ", but should be ", REPOCH[i], "."))
      }
      conclusion <- "Failed"
    }
  } else {
    finding <- paste("EPOCH does not exist in dataset. Please generate it.", sep = " ")
    conclusion <- "Failed"
    Error <- ""
  }
  result <- list(Finding = finding,
                 Conclusion = conclusion)
  return(result)
}

ndupk <- function(indat,datname){
  name <- toupper(gsub(pattern = "\\..*", replacement = "", x = datname))
  if (sum(duplicated(indat))==0) {
    finding <-paste("No duplicate records in the ", name, "dataset.") 
    conclusion<-"Passed"
  } else {
    SUBJID <- indat$USUBJID[duplicated(indat)]
    finding <- paste("There are duplicate records in the dataset:", SUBJID, collapse = " ") 
    conclusion <- "Failed"
  }
  result <- list(
    Finding = finding,
    Conclusion = conclusion
  )
  return(result)
}

RF <- function() {
  setwd("//cdars.pfizer.com/cdars/prod/prjA409/internship/A4091056/saseng/cdisc3_0/data/rawdata")
  #check the existence of TESTDRUG dataset
  #print("check if TESTDRUG dataset exists")
  if (file.exists("testdrug.sas7bdat")==TRUE) {
    TESTDRUG <- read_sas("testdrug.sas7bdat")
    #print("TESTDRUG dataset exists, generate RFSTDTC and RFXENDTC from TESTDRUG dataset")
    #take out columuns PID FROMDATE FROMTIME DRGDATE DOSETIME TODATE STOPTIME DOSETIME
    TESTDRUGsub <- TESTDRUG[,c("PID", "FROMDATE", "FROMTIME", "DRGDATE", "DOSETIME", "TODATE", "STOPTIME")]
    #impute FROMDATE FROMTIME TODATE STOPTIME if missing
    TESTDRUGsub$FROMDATE[is.na(TESTDRUGsub$FROMDATE)] <- TESTDRUGsub$DRGDATE[is.na(TESTDRUGsub$FROMDATE)]
    TESTDRUGsub$FROMTIME[is.na(TESTDRUGsub$FROMTIME)] <- TESTDRUGsub$DOSETIME[is.na(TESTDRUGsub$FROMTIME)]
    TESTDRUGsub$TODATE[is.na(TESTDRUGsub$TODATE)] <- TESTDRUGsub$FROMDATE[is.na(TESTDRUGsub$TODATE)]
    TESTDRUGsub$STOPTIME[is.na(TESTDRUGsub$STOPTIME)] <- TESTDRUGsub$DOSETIME[is.na(TESTDRUGsub$STOPTIME)]
    #generate EXSTDTC EXENDTC
    TESTDRUGsub$EXSTDTC <- paste(TESTDRUGsub$FROMDATE, TESTDRUGsub$FROMTIME, sep = "T")
    TESTDRUGsub$EXENDTC <- paste(TESTDRUGsub$TODATE, TESTDRUGsub$STOPTIME, sep = "T")
    #generate RFSTDTC RFXENDTC
    TESTDRUGsub$EXSTDTC[TESTDRUGsub$EXSTDTC== "NATNA"] <- " "
    TESTDRUGsub$EXENDTC[TESTDRUGsub$EXENDTC== "NATNA"] <- " "
    ST <- TESTDRUGsub %>% arrange(PID, EXSTDTC) 
    uST <- ST[!duplicated(ST$PID),]
    uST <- uST[,c("PID", "EXSTDTC")]
    EN <- TESTDRUGsub %>% group_by(PID) %>% top_n(1, EXENDTC)
    uEN <- EN[,c("PID", "EXENDTC")]
    RF <- merge(uST,uEN,by = "PID")
    RF$USUBJID <- RF$PID
    RF$RFST <- RF$EXSTDTC
    RF$RFXEN <- RF$EXENDTC
    RF <- RF[,c("USUBJID", "RFST", "RFXEN")]
  } else {
    #print("TESTDRUG dataset does not exist, check if EX dataset exists")
    setwd("//cdars.pfizer.com/cdars/prod/prjA409/internship/A4091056/saseng/cdisc3_0/data")
    if (file.exists("ex.sas7bdat")==TRUE) {
      print("EX dataset exists, generate RFSTDTC AND RFXENDTC from EX dataset")
      EX <- read_sas("ex.sas7bdat")
      ST <- EX %>% arrange(USUBJID, EXSTDTC) 
      uST <- ST[!duplicated(ST$USUBJID),]
      uST <- uST[,c("USUBJID", "EXSTDTC")]
      EN <- EX %>% group_by(USUBJID) %>% top_n(1, EXENDTC)
      uEN <- EN[,c("USUBJID", "EXENDTC")]
      RF <- merge(uST,uEN,by = "USUBJID")
      RF$RFST <- RF$EXSTDTC
      RF$RFXEN <- RF$EXENDTC
      RF <- RF[,c("USUBJID", "RFST", "RFXEN")]
    } else {
      #print("TESTDRUG and EX datasets are all not exist, use DM dataset instead")
      DM <- read_sas("dm.sas7bdat")
      RF <- DM[,c("USUBJID", "RFSTDTC", "RFXENDTC")]
      RF$RFST <- RF$RFSTDTC
      RF$RFXEN <- RF$RFXENDTC
      RF <- RF[,c("USUBJID", "RFST", "RFXEN")]
    }
  }
  return(RF)
}

