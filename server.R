library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(knitr)
library(tinytex)
library(gridExtra)
library(haven)
library(DT)
library(grid)
library(tableone)
library(ggplot2)
library(reshape)
library(lme4)
library(QCfuns)
library(parsedate)
library(dplyr)
library(parsedate)
adwnall.sas7bdat=read_sas("adwnall.sas7bdat")
adae.sas7bdat=read_sas("adae.sas7bdat")

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


shinyServer(function(input, output,session) {
  specdata <- reactive({
    req(input$file1)
    data_set<-read.csv(input$file1$datapath)
    return (data_set)
  })

  data_setr <- eventReactive(input$updater,{
    setwd("\\\\cdars.pfizer.com/cdars/prod/prjA409/internship/A4091056/saseng/cdisc3_0/data/rawdata")
    switch(input$datasetr,
           "_ce_ldm.sas7bdat"=read_sas("_ce_ldm.sas7bdat"),
           "_co_ldm.sas7bdat"=read_sas("_co_ldm.sas7bdat"),
           "_dcdlist.sas7bdat"=read_sas("_dcdlist.sas7bdat"),
           "_de_ldm.sas7bdat"=read_sas("_de_ldm.sas7bdat"),
           "_design.sas7bdat"=read_sas("_design.sas7bdat"),
           "_fa_ldm.sas7bdat" =read_sas("_fa_ldm.sas7bdat" ),
           "_ie_ldm.sas7bdat"=read_sas("_ie_ldm.sas7bdat"),
           "_map_codelists.sas7bdat" =read_sas("_map_codelists.sas7bdat"),
           "_map_codelists_23nov.sas7bdat"=read_sas("_map_codelists_23nov.sas7bdat"),
           "_map_codelists_bkp.sas7bdat"=read_sas("_map_codelists_bkp.sas7bdat"),
           "_map_codelists_copy.sas7bdat"=read_sas("_map_codelists_copy.sas7bdat"),
           "_map_variables.sas7bdat"=read_sas("_map_variables.sas7bdat"),
           "_map_variables_23nov.sas7bdat"=read_sas("_map_variables_23nov.sas7bdat"),
           "_map_variables_3jul17.sas7bdat"=read_sas("_map_variables_3jul17.sas7bdat"),
           "_map_variables_bkp.sas7bdat"=read_sas("_map_variables_bkp.sas7bdat"),
           "_mo_ldm.sas7bdat" =read_sas("_mo_ldm.sas7bdat"),
           "_qs_ldm.sas7bdat"=read_sas("_qs_ldm.sas7bdat"),
           "_relrec_ldm.sas7bdat"=read_sas("_relrec_ldm.sas7bdat"),
           "_rs_ldm.sas7bdat"=read_sas("_rs_ldm.sas7bdat"),
           "_sc_ldm.sas7bdat"=read_sas("_sc_ldm.sas7bdat"),
           "_xi_ldm.sas7bdat"=read_sas("_xi_ldm.sas7bdat"),
           "adverse.sas7bdat" =read_sas("adverse.sas7bdat" ),
           "allergy.sas7bdat"=read_sas("allergy.sas7bdat"),
           "car.sas7bdat"=read_sas("car.sas7bdat"),
           "cd_b_p.sas7bdat"=read_sas("cd_b_p.sas7bdat"),
           "child.sas7bdat"=read_sas("child.sas7bdat"),
           "codelist.sas7bdat"=read_sas("codelist.sas7bdat"),
           "comments.sas7bdat"=read_sas("comments.sas7bdat"),
           "condrug.sas7bdat"=read_sas("condrug.sas7bdat"),
           "contrt.sas7bdat"=read_sas("contrt.sas7bdat"),
           "convunit.sas7bdat"=read_sas("convunit.sas7bdat"),
           "demog.sas7bdat"=read_sas("demog.sas7bdat"),
           "dev.sas7bdat"=read_sas("dev.sas7bdat"),
           "dof.sas7bdat"=read_sas("dof.sas7bdat"),
           "drcpdrg.sas7bdat"=read_sas("drcpdrg.sas7bdat"),
           "drub.sas7bdat"=read_sas("drub.sas7bdat"),
           "dt_ce.sas7bdat"=read_sas("dt_ce.sas7bdat"),
           "dt_co.sas7bdat"=read_sas("dt_co.sas7bdat"),
           "dt_de.sas7bdat"=read_sas("dt_de.sas7bdat"),
           "dt_fa.sas7bdat"=read_sas( "dt_fa.sas7bdat"),
           "dt_fa_23nov.sas7bdat"=read_sas("dt_fa_23nov.sas7bdat"),
           "dt_fa1.sas7bdat"=read_sas("dt_fa1.sas7bdat"),
           "dt_ie.sas7bdat"=read_sas("dt_ie.sas7bdat"),
           "dt_ie_copy.sas7bdat"=read_sas("dt_ie_copy.sas7bdat"),
           "dt_ie_copy1.sas7bdat" =read_sas("dt_ie_copy1.sas7bdat"),
           "dt_mo.sas7bdat"=read_sas("dt_mo.sas7bdat"),
           "dt_mo_copy.sas7bdat"=read_sas("dt_mo_copy.sas7bdat"),
           "dt_qs.sas7bdat"=read_sas("dt_qs.sas7bdat"),
           "dt_relrec.sas7bdat"=read_sas("dt_relrec.sas7bdat"),
           "dt_rs.sas7bdat"=read_sas( "dt_rs.sas7bdat"),
           "dt_sc.sas7bdat"=read_sas("dt_sc.sas7bdat"),
           "dt_xi.sas7bdat"=read_sas("dt_xi.sas7bdat"),
           "dt_xi_copy.sas7bdat"=read_sas("dt_xi_copy.sas7bdat"),
           "e5d.sas7bdat"=read_sas("e5d.sas7bdat"),
           "ecg.sas7bdat"=read_sas("ecg.sas7bdat"),
           "fa.sas7bdat"=read_sas("fa.sas7bdat"),
           "fa_ne.sas7bdat"=read_sas("fa_ne.sas7bdat"),
           "final.sas7bdat" = read_sas("final.sas7bdat"),
           "fxa.sas7bdat"=read_sas("fxa.sas7bdat"),
           "gen_r003f.sas7bdat"=read_sas("gen_r003f.sas7bdat"),
           "hcr.sas7bdat"=read_sas( "hcr.sas7bdat"),
           "inex.sas7bdat"=read_sas("inex.sas7bdat"),
           "jme.sas7bdat"=read_sas("jme.sas7bdat"),
           "lab_safe.sas7bdat"=read_sas("lab_safe.sas7bdat"),
           "lab_sdw.sas7bdat"=read_sas("lab_sdw.sas7bdat"),
           "labctc.sas7bdat"=read_sas("labctc.sas7bdat"),
           "leas.sas7bdat"=read_sas("leas.sas7bdat"),
           "map_codelists.sas7bdat"=read_sas("map_codelists.sas7bdat"),
           "map_codelists_29jan18.sas7bdat"=read_sas("map_codelists_29jan18.sas7bdat"),
           "map_variables.sas7bdat"=read_sas("map_variables.sas7bdat"),
           "map_variables_29jan18.sas7bdat"=read_sas("map_variables_29jan18.sas7bdat"),
           "me.sas7bdat"=read_sas("me.sas7bdat"),
           "mprt.sas7bdat"=read_sas("mprt.sas7bdat"),
           "mri.sas7bdat"=read_sas("mri.sas7bdat"),
           "mse.sas7bdat"=read_sas("mse.sas7bdat"),
           "msh.sas7bdat"=read_sas("msh.sas7bdat"),
           "ne.sas7bdat"=read_sas("ne.sas7bdat"),
           "nis.sas7bdat"=read_sas("nis.sas7bdat"),
           "normrang.sas7bdat"=read_sas("normrang.sas7bdat"),
           "nrs.sas7bdat"=read_sas("nrs.sas7bdat"),
           "om1.sas7bdat"=read_sas("om1.sas7bdat"),
           "phyexam.sas7bdat"=read_sas("phyexam.sas7bdat"),
           "pqm.sas7bdat"=read_sas("pqm.sas7bdat"),
           "prevdis.sas7bdat"=read_sas("prevdis.sas7bdat"),
           "primdiag.sas7bdat"=read_sas("primdiag.sas7bdat"),
           "qtci.sas7bdat"=read_sas("qtci.sas7bdat"),
           "random.sas7bdat"=read_sas("random.sas7bdat"),
           "rm.sas7bdat"=read_sas("rm.sas7bdat"),
           "sae.sas7bdat"=read_sas("sae.sas7bdat"),
           "sae_narr.sas7bdat"=read_sas("sae_narr.sas7bdat"),
           "sas.sas7bdat"=read_sas("sas.sas7bdat"),
           "sau.sas7bdat"=read_sas("sau.sas7bdat"),
           "sbs.sas7bdat"=read_sas("sbs.sas7bdat"),
           "scdg.sas7bdat"=read_sas("scdg.sas7bdat"),
           "sbs.sas7bdat"=read_sas("sbs.sas7bdat"),
           "scdg.sas7bdat"=read_sas("scdg.sas7bdat"),
           "sga.sas7bdat"=read_sas("sga.sas7bdat"),
           "srv_p.sas7bdat"=read_sas("srv_p.sas7bdat"),
           "stc.sas7bdat"=read_sas("stc.sas7bdat"),
           "sum_group.sas7bdat"=read_sas("sum_group.sas7bdat"),
           "suppfa_ne.sas7bdat"=read_sas("suppfa_ne.sas7bdat"),
           "testdrug.sas7bdat"=read_sas("testdrug.sas7bdat"),
           "tpc.sas7bdat"=read_sas("tpc.sas7bdat"),
           "tpddatum.sas7bdat"=read_sas("tpddatum.sas7bdat"),
           "tsm2.sas7bdat"=read_sas("tsm2.sas7bdat"),
           "vitals.sas7bdat"=read_sas("vitals.sas7bdat"),
           "wn.sas7bdat"=read_sas("wn.sas7bdat"),
           "wpi.sas7bdat"=read_sas("wpi.sas7bdat")
    )
  },ignoreNULL = FALSE)
  data_sets <- eventReactive(input$updates,{
    setwd("\\\\cdars.pfizer.com/cdars/prod/prjA409/internship/A4091056/saseng/cdisc3_0/data")
    switch(input$datasets,
           "adsae.sas7bdat"=read_sas("adsae.sas7bdat"),
           "adsae_narr.sas7bdat"=read_sas("adsae_narr.sas7bdat"),
           "ae.sas7bdat"=read_sas("ae.sas7bdat"),
           "ae191.sas7bdat"=read_sas("ae191.sas7bdat"),
           "caselist.sas7bdat"=read_sas("caselist.sas7bdat"),
           "ce.sas7bdat"=read_sas("ce.sas7bdat"),
           "cm.sas7bdat"=read_sas("cm.sas7bdat"),
           "co.sas7bdat"=read_sas("co.sas7bdat"),
           "cocr.sas7bdat"=read_sas("cocr.sas7bdat"),
           "de.sas7bdat"=read_sas("de.sas7bdat"),
           "dm.sas7bdat"=read_sas("dm.sas7bdat"),
           "ds.sas7bdat"=read_sas("ds.sas7bdat"),
           "dv.sas7bdat"=read_sas("dv.sas7bdat"),
           "ec.sas7bdat"=read_sas("ec.sas7bdat"),
           "eg.sas7bdat"=read_sas("eg.sas7bdat"),
           "ex.sas7bdat"=read_sas("ex.sas7bdat"),
           "fa.sas7bdat"=read_sas("fa.sas7bdat"),
           "fahohc.sas7bdat"=read_sas("fahohc.sas7bdat"),
           "faje.sas7bdat"=read_sas("faje.sas7bdat"),
           "faprne.sas7bdat"=read_sas("faprne.sas7bdat"),
           "hohc.sas7bdat"=read_sas("hohc.sas7bdat"),
           "ie.sas7bdat"=read_sas("ie.sas7bdat"),
           "lab_sdw.sas7bdat"=read_sas("lab_sdw.sas7bdat"),
           "lb.sas7bdat"=read_sas("lb.sas7bdat"),
           "mh.sas7bdat"=read_sas("mh.sas7bdat"),
           "mkjm.sas7bdat"=read_sas("mkjm.sas7bdat"),
           "mo.sas7bdat"=read_sas("mo.sas7bdat"),
           "mocr.sas7bdat"=read_sas("mocr.sas7bdat"),
           "mons.sas7bdat"=read_sas("mons.sas7bdat"),
           "nv.sas7bdat"=read_sas("nv.sas7bdat"),
           "nvne.sas7bdat"=read_sas("nvne.sas7bdat"),
           "pe.sas7bdat"=read_sas("pe.sas7bdat"),
           "pecopy.sas7bdat"=read_sas("pecopy.sas7bdat"),
           "pidlist.sas7bdat"=read_sas("pidlist.sas7bdat"),
           "pr.sas7bdat"=read_sas("pr.sas7bdat"),
           "prne.sas7bdat"=read_sas("prne.sas7bdat"),
           "qs.sas7bdat"=read_sas("qs.sas7bdat"),
           "qse5.sas7bdat"=read_sas("qse5.sas7bdat"),
           "qshc.sas7bdat"=read_sas("qshc.sas7bdat"),
           "qsnr.sas7bdat"=read_sas("qsnr.sas7bdat"),
           "qssg.sas7bdat"=read_sas("qssg.sas7bdat"),
           "qsst.sas7bdat"=read_sas("qsst.sas7bdat"),
           "qswn.sas7bdat"=read_sas("qswn.sas7bdat"),
           "qswp.sas7bdat"=read_sas("qswp.sas7bdat"),
           "relrec.sas7bdat"=read_sas("relrec.sas7bdat"),
           "relrechc.sas7bdat"=read_sas("relrechc.sas7bdat"),
           "relrecne.sas7bdat"=read_sas("relrecne.sas7bdat"),
           "rp.sas7bdat"=read_sas("rp.sas7bdat"),
           "rs.sas7bdat"=read_sas("rs.sas7bdat"),
           "rscr.sas7bdat"=read_sas("rscr.sas7bdat"),
           "rsns.sas7bdat"=read_sas("rsns.sas7bdat"),
           "sae.sas7bdat"=read_sas("sae.sas7bdat"),
           "sae_narr.sas7bdat"=read_sas("sae_narr.sas7bdat"),
           "sc.sas7bdat"=read_sas("sc.sas7bdat"),
           "schc.sas7bdat"=read_sas("schc.sas7bdat"),
           "stdm_dm.sas7bdat"=read_sas("stdm_dm.sas7bdat"),
           "su.sas7bdat"=read_sas("su.sas7bdat"),
           "suppae.sas7bdat"=read_sas("suppae.sas7bdat"),
           "suppae191.sas7bdat"=read_sas("suppae191.sas7bdat"),
           "suppcm.sas7bdat"=read_sas("suppcm.sas7bdat"),
           "suppdm.sas7bdat"=read_sas("suppdm.sas7bdat"),
           "suppds.sas7bdat"=read_sas("suppds.sas7bdat"),
           "suppdv.sas7bdat"=read_sas("suppdv.sas7bdat"),
           "suppec.sas7bdat"=read_sas("suppec.sas7bdat"),
           "suppeg.sas7bdat"=read_sas("suppeg.sas7bdat"),
           "suppex.sas7bdat"=read_sas("suppex.sas7bdat"),
           "suppfa.sas7bdat"=read_sas("suppfa.sas7bdat"),
           "suppfaje.sas7bdat"=read_sas("suppfaje.sas7bdat"),
           "suppfaprne.sas7bdat"=read_sas("suppfaprne.sas7bdat"),
           "suppie.sas7bdat"=read_sas("suppie.sas7bdat"),
           "supplb.sas7bdat"=read_sas( "supplb.sas7bdat"),
           "suppmh.sas7bdat"=read_sas("suppmh.sas7bdat"),
           "suppmkjm.sas7bdat"=read_sas("suppmkjm.sas7bdat"),
           "suppmo.sas7bdat"=read_sas("suppmo.sas7bdat"),
           "suppmocr.sas7bdat"=read_sas("suppmocr.sas7bdat"),
           "suppmons.sas7bdat"=read_sas("suppmons.sas7bdat"),
           "suppnv.sas7bdat"=read_sas("suppnv.sas7bdat"),
           "supppe.sas7bdat"=read_sas("supppe.sas7bdat"),
           "supppr.sas7bdat"=read_sas("supppr.sas7bdat"),
           "suppqs.sas7bdat"=read_sas("suppqs.sas7bdat"),
           "suppqse5.sas7bdat"=read_sas("suppqse5.sas7bdat"),
           "suppqshc.sas7bdat"=read_sas("suppqshc.sas7bdat"),
           "suppqsnr.sas7bdat"=read_sas("suppqsnr.sas7bdat"),
           "suppqssg.sas7bdat"=read_sas("suppqssg.sas7bdat"),
           "suppqsst.sas7bdat"=read_sas("suppqsst.sas7bdat"),
           "suppqswn.sas7bdat"=read_sas("suppqswn.sas7bdat"),
           "suppqswp.sas7bdat"=read_sas("suppqswp.sas7bdat"),
           "supprp.sas7bdat"=read_sas("supprp.sas7bdat"),
           "supprsns.sas7bdat"=read_sas("supprsns.sas7bdat"),
           "suppsc.sas7bdat"=read_sas("suppsc.sas7bdat"),
           "suppvs.sas7bdat"=read_sas("suppvs.sas7bdat"),
           "suppvs_01MAY2018.sas7bdat"=read_sas("suppvs_01MAY2018.sas7bdat"),
           "suppxi.sas7bdat"=read_sas("suppxi.sas7bdat"),
           "vs.sas7bdat"=read_sas("vs.sas7bdat"),
           "vs_01MAY2018.sas7bdat"=read_sas("vs_01MAY2018.sas7bdat"),
           "xi.sas7bdat"=read_sas("xi.sas7bdat")
    )
  },ignoreNULL = FALSE)
  data_seta <- eventReactive(input$updatea,{
    setwd("\\\\cdars.pfizer.com/cdars/prod/prjA409/internship/A4091056/saseng/cdisc3_0/data_vai")
    switch(input$dataseta,
           "adadj.sas7bdat"=read_sas("adadj.sas7bdat"),
           "adae.sas7bdat"=read_sas("adae.sas7bdat"),
           "adaetest.sas7bdat"=read_sas("adaetest.sas7bdat"),
           "adbase.sas7bdat"=read_sas("adbase.sas7bdat"),
           "adcm.sas7bdat"=read_sas("adcm.sas7bdat"),
           "adcy.sas7bdat"=read_sas("adcy.sas7bdat"),
           "adds.sas7bdat"=read_sas("adds.sas7bdat"),
           "addthc.sas7bdat"=read_sas("addthc.sas7bdat"),
           "ade5.sas7bdat"=read_sas("ade5.sas7bdat"),
           "adeg.sas7bdat"=read_sas("adeg.sas7bdat"),
           "adeg_16mar2018.sas7bdat"=read_sas("adeg_16mar2018.sas7bdat"),
           "adeg_19mar2018.sas7bdat"=read_sas("adeg_19mar2018.sas7bdat"),
           "adeg_old.sas7bdat"=read_sas("adeg_old.sas7bdat"),
           "adex.sas7bdat"=read_sas("adex.sas7bdat"),
           "adie.sas7bdat"=read_sas("adie.sas7bdat"),
           "adie_old.sas7bdat"=read_sas("adie_old.sas7bdat"),
           "adjm.sas7bdat"=read_sas("adjm.sas7bdat"),
           "adlb.sas7bdat"=read_sas("adlb.sas7bdat"),
           "adlb_std.sas7bdat"=read_sas("adlb_std.sas7bdat"),
           "admh.sas7bdat"=read_sas("admh.sas7bdat"),
           "admh_old.sas7bdat"=read_sas("admh_old.sas7bdat"),
           "adni.sas7bdat"=read_sas("adni.sas7bdat"),
           "adnr.sas7bdat"=read_sas("adnr.sas7bdat"),
           "adnrall.sas7bdat"=read_sas("adnrall.sas7bdat"),
           "adnrmi.sas7bdat"=read_sas("adnrmi.sas7bdat"),
           "adnrtest.sas7bdat"=read_sas("adnrtest.sas7bdat"),
           "adns.sas7bdat"=read_sas("adns.sas7bdat"),
           "adnsaids.sas7bdat"=read_sas("adnsaids.sas7bdat"),
           "adomeract.sas7bdat"=read_sas("adomeract.sas7bdat"),
           "adpe.sas7bdat"=read_sas("adpe.sas7bdat"),
           "adpr.sas7bdat"=read_sas("adpr.sas7bdat"),
           "adrm.sas7bdat"=read_sas("adrm.sas7bdat"),
           "adrmall.sas7bdat"=read_sas("adrmall.sas7bdat"),
           "adrp.sas7bdat"=read_sas("adrp.sas7bdat"),
           "adsaec.sas7bdat"=read_sas("adsaec.sas7bdat"),
           "adsaee.sas7bdat"=read_sas("adsaee.sas7bdat"),
           "adsg.sas7bdat"=read_sas("adsg.sas7bdat"),
           "adsgall.sas7bdat"=read_sas("adsgall.sas7bdat"),
           "adsgcat.sas7bdat"=read_sas("adsgcat.sas7bdat"),
           "adsgmi.sas7bdat"=read_sas("adsgmi.sas7bdat"),
           "adsl.sas7bdat"=read_sas("adsl.sas7bdat"),
           "adsl_0.sas7bdat"=read_sas("adsl_0.sas7bdat"),
           "adst.sas7bdat"=read_sas("adst.sas7bdat"),
           "adsu.sas7bdat"=read_sas("adsu.sas7bdat"),
           "adttd.sas7bdat"=read_sas("adttd.sas7bdat"),
           "advs.sas7bdat"=read_sas("advs.sas7bdat"),
           "advs_01.sas7bdat"=read_sas("advs_01.sas7bdat"),
           "advs_01MAY2018.sas7bdat"=read_sas("advs_01MAY2018.sas7bdat"),
           "advs_04DEC2017.sas7bdat"=read_sas("advs_04DEC2017.sas7bdat"),
           "advs_06FEB2018.sas7bdat"=read_sas("advs_06FEB2018.sas7bdat"),
           "advs_10AUG2017.sas7bdat"=read_sas("advs_10AUG2017.sas7bdat"),
           "advs_16mar2018.sas7bdat"=read_sas("advs_16mar2018.sas7bdat"),
           "advs_keeping_rec_with_postdiff_derived.sas7bdat"=
             read_sas("advs_keeping_rec_with_postdiff_derived.sas7bdat"),
           "advs_with_unsched_dataisssue_visit_ort.sas7bdat"=
             read_sas("advs_with_unsched_dataisssue_visit_ort.sas7bdat"),
           "adwn.sas7bdat"=read_sas("adwn.sas7bdat"),
           "adwnall.sas7bdat"=read_sas("adwnall.sas7bdat"),
           "adwnmi.sas7bdat"=read_sas("adwnmi.sas7bdat"),
           "adwntest.sas7bdat"=read_sas("adwntest.sas7bdat"),
           "adwntest1.sas7bdat"=read_sas("adwntest1.sas7bdat"),
           "adwp.sas7bdat"=read_sas("adwp.sas7bdat"),
           "faje.sas7bdat"=read_sas("faje.sas7bdat")
    )
  },ignoreNULL = FALSE)
  #############freq table###########
  fq_data <-eventReactive(input$updatefqdomain,{
    safadsl<-adsl.sas7bdat[adsl.sas7bdat$ACTARMCD=="A"|adsl.sas7bdat$ACTARMCD=="B"|adsl.sas7bdat$ACTARMCD=="C",]
    safadae=adae.sas7bdat[adae.sas7bdat$ACTARMCD=="A"|adae.sas7bdat$ACTARMCD=="B"|adae.sas7bdat$ACTARMCD=="C",]
    return(
      switch(input$fqdomain,
             "ADSL"=CreateCatTable(vars = c("AGEGR1","RACE","ETHNIC"), data = safadsl ,strata = c("ACTARM"))
             ,"ADAE"=CreateCatTable(vars = c("AECAT"), data = safadae ,strata = c("ACTARM")))
    )
  },ignoreNULL = FALSE)
  table_DC <- eventReactive(input$updatestrata,{
    if(input$subset=="SAFETY")
    {sadae.sas7bdat=adae.sas7bdat[adae.sas7bdat$ACTARMCD=="A"|adae.sas7bdat$ACTARMCD=="B"|adae.sas7bdat$ACTARMCD=="C",]
    sadsl.sas7bdat=adsl.sas7bdat[adsl.sas7bdat$ACTARMCD=="A"|adsl.sas7bdat$ACTARMCD=="B"|adsl.sas7bdat$ACTARMCD=="C",]
    }
    else{
      sadae.sas7bdat=adae.sas7bdat
      sadsl.sas7bdat=adsl.sas7bdat
    }
    if(input$fqdomain=="ADSL")
    {listVar= c("AGEGR1","RACE","ETHNIC")
    dataf=sadsl.sas7bdat }
    else if (input$fqdomain=="ADAE")
    {listVar= c("AECAT")
    dataf=sadae.sas7bdat
    }
    return(switch(input$strata,
                  "OVERALL"=CreateCatTable(vars = listVar, data = dataf ,strata = c("ACTARM"))
                  ,"SEX"=CreateCatTable(vars = listVar, data = dataf ,strata = c("SEX","ACTARM")))
    )},ignoreNULL = FALSE)
  output$table<- DT::renderDataTable({

    return (DT::datatable(print(table_DC()),
                          editable = TRUE))
  })
  ##################################
  #############box plot#############
  score_data <- eventReactive(input$updatescore,
                              {
                                week16=adwnall.sas7bdat[adwnall.sas7bdat$AVISIT=="WEEK 16",]
                                painSC=week16[week16$PARAM=="WOMAC2-WOMAC Pain Subscale Score",]
                                phyFC=week16[week16$PARAM=="WOMAC2-WOMAC Physical Function Subscale Score",]
                                return(
                                  switch(input$score,
                                         "Pain subscale"=painSC,
                                         "Physical Function"=phyFC)
                                )
                              }
                              ,ignoreNULL = FALSE    )
  output$boxplot <-renderPlot({
    ggplot(score_data(), aes(x = TRTA, y = CHG, fill = TRTA)) +
      geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2)

  })
  output$anova <-renderPrint({
    model1<-lm(score_data()$CHG~score_data()$TRTA)
    return(summary(model1))
  })
  ############statistics############
  output$stat <- renderPrint({
    effi_stat()
  })
  effi_stat <-reactive({

    switch(input$cstat,"ANOVA"=effi_anova(),"Summary"=summary(effi_anova()))
  })
  effi_anova <-reactive({


    # subset: change of pain subscale in week 16
    pnsubscr <- subset(wnall, PARAM == "WOMAC2-WOMAC Pain Subscale Score" & AVISIT == "WEEK 16" & TYPEC == "LOCF")
    pnsubscr$pain <- pnsubscr$CHG
    pnsubscr2 <- pnsubscr[,c("USUBJID", "PARAM", "pain", "TRTA")]


    #subset: change of physical function subscale in week 16
    pfsubscr <- subset(wnall, PARAM == "WOMAC2-WOMAC Physical Function Subscale Score" & AVISIT == "WEEK 16" & TYPEC == "LOCF")
    pfsubscr$funct <- pfsubscr$CHG
    pfsubscr2 <- pfsubscr[,c("USUBJID", "PARAM", "funct", "TRTA")]

    #baseline
    bs <- sl[,c("USUBJID", "KLGRDJIN", "JINDOVER", "PAINBASE", "DIRYPAIN", "SITEID")]

    # primary endpoints: pain subscale score
    DATASET1 <- merge(x=pnsubscr2, y=bs, by="USUBJID", all.x = TRUE)

    # primary endpoints: physical function score
    DATASET2 <- merge(x=pfsubscr2, y=bs, by="USUBJID", all.x = TRUE)
    model1 <- lmer(pain ~ TRTA + KLGRDJIN + JINDOVER + PAINBASE + DIRYPAIN + (1|SITEID), data = DATASET1, REML = FALSE)
    model2 <- lmer(funct ~ TRTA + KLGRDJIN + JINDOVER + PAINBASE + DIRYPAIN + (1|SITEID), data = DATASET2, REML = FALSE)
    return(
      switch(input$cscore,"Pain subscale"=model1,"Physical Function"=model2)
    )
  })
  ##################################
  ##################################
  ############line plot#############
  n<-reactive({
    n<-input$n
  })
  painSCn<-reactive({
    painSC1<-painSC[painSC$TYPEC=="LOCF",]
    return(
      painSCn<-painSC1[1:n(),]
    )
  })
  output$lineplot <-renderPlot({
    ggplot(painSCn(),aes(AVISITN,AVAL,colour=TRTA))+geom_smooth()+geom_point()
  })
  ##################################
  ############bar,pie plot##############
  data_ae <-reactive({
    setwd("\\\\cdars.pfizer.com/cdars/prod/prjA409/internship/A4091056/saseng/cdisc3_0/data_vai")
    adae.sas7bdat<-read_sas("adae.sas7bdat")
    aelist <- c("Bradycardia", "Dizziness", "Heart rate decreased", "Orthostatic hypotension", "Sinus bradycardia", "Synscope", "Hypolidrosis", "Abdominal discomfort", "Diarrhoea", "Nausea", "Vomiting", "Urinary incomtimence")
    adaes <- subset(adae.sas7bdat, adae.sas7bdat$AEDECOD %in% aelist)
    return(switch(input$subset,
                  "Fulldata"=adae.sas7bdat,"Based on SAP"=adaes))
  })
  piechart <-reactive({
    AEFreq0=as.data.frame(table(data_ae()$AEBODSYS))
    bp<- ggplot(AEFreq0, aes(x="", y=Freq, fill=Var1))+ geom_bar(width = 1, stat = "identity")+theme(legend.position = "bottom")
    pie <- bp + coord_polar("y", start=0)
    return(pie)
  })
  barplot <- reactive({
    AEFreq=as.data.frame(table(data_ae()$TRTA,data_ae()$AEBODSYS))
    AEFreq2=AEFreq[AEFreq$Var1!="",]
    bp<- ggplot(AEFreq2, aes(x=Var1, y=Freq, fill=Var2))+ geom_bar(width = 1, stat = "identity")+theme(legend.position = "bottom")
    return(bp)
  })
  ALL<- reactive(
    table(data_ae()$TRTA)
  )
  percentplot <-reactive({
    trt1 <- data_ae()[data_ae()$TRTA == "Tanezumab 2.5 mg",]
    trt2 <- data_ae()[data_ae()$TRTA == "Tanezumab 2.5/5 mg",]
    plc <- data_ae()[data_ae()$TRTA == "Placebo",]

    table1 <- as.data.frame(table(trt1$AEBODSYS))
    table2 <- as.data.frame(table(trt2$AEBODSYS))
    table3 <- as.data.frame(table(plc$AEBODSYS))

    table1$trt1 <- table1$Freq
    table2$trt2 <- table2$Freq
    table3$plc <- table3$Freq

    table1 <- table1[,c(1,3)]
    table2 <- table2[,c(1,3)]
    table3 <- table3[,c(1,3)]

    Freq <- merge(x=table1, y=table2, by = "Var1", all = TRUE)
    Freq2 <- merge(x=table3, y=Freq, by = "Var1", all = TRUE)

    Freq3 <- t(Freq2)

    row.names(Freq3) <- c("var", "Placebo", "Tanezumab 2.5mg", "Tanezumab 2.5/5mg")
    colnames(Freq3) <- Freq3[1,]
    Freq4 <- Freq3[2:4,]

    Freq4[is.na(Freq4)] <- 0

    melted <- melt(Freq2, id.vars = "Var1")

    melted$Var1 <- as.character(melted$Var1)

    melted$value[is.na(melted$value)] <- 0

    melted$variable <- as.character(melted$variable)

    melted$variable[melted$variable=="plc"] <- "Placebo"

    melted$variable[melted$variable=="trt1"] <- "Tanezumab 2.5mg"

    melted$variable[melted$variable=="trt2"] <- "Tanezumab 2.5/5mg"

    colnames(melted) <- c("Var1", "Treatment Group", "value")

    ggplot(melted, aes( y=value, x=Var1, fill= `Treatment Group`))+geom_bar(stat="identity", position = "dodge")+
      labs(x="Adverse Event", y="Frequency")
    table1$trt1 <- table1$trt1/ALL()[3]*100
    table2$trt2 <- table2$trt2/ALL()[4]*100
    table3$plc <- table3$plc/ALL()[2]*100

    Freq5 <- merge(x=table1, y=table2, by = "Var1", all = TRUE)
    Freq6 <- merge(x=table3, y=Freq5, by = "Var1", all = TRUE)

    meltedp <- melt(Freq6, id.vars = "Var1")

    meltedp$Var1 <- as.character(meltedp$Var1)

    meltedp$value[is.na(meltedp$value)] <- 0

    meltedp$variable <- as.character(meltedp$variable)

    meltedp$variable[meltedp$variable=="plc"] <- "Placebo"

    meltedp$variable[meltedp$variable=="trt1"] <- "Tanezumab 2.5mg"

    meltedp$variable[meltedp$variable=="trt2"] <- "Tanezumab 2.5/5mg"

    colnames(meltedp) <- c("Var1", "Treatment Group", "value")

    return(ggplot(meltedp, aes( y=value, x=Var1, fill= `Treatment Group`))+geom_bar(stat="identity", position = "dodge")+
             labs(x="Adverse Event", y="Percentage * 100"))
  })
  aefigure <- reactive({
    switch(input$barpie,"barplot"=barplot(),"piechart"=piechart(),"percentplot"=percentplot())
  })
  output$chart <-renderPlot({
    aefigure()
  })
  ##################################
  output$summaryr <- renderTable({
    req(data_setr())
    srr<-as.data.frame(summary(data_setr()))
    srr1<-srr[,c(2,3)]
    colnames(srr1)<-c("Variable name","Descriptions")
    return (srr1)
  })
  output$summarys <- renderTable({
    req(data_sets())
    srs<-as.data.frame(summary(data_sets()))
    srs1<-srs[,c(2,3)]
    colnames(srs1)<-c("Variable name","Descriptions")
    return (srs1)
  })
  output$summarya <- renderTable({
    req(data_seta())
    sra<-as.data.frame(summary(data_seta()))
    sra1<-sra[,c(2,3)]
    colnames(sra1)<-c("Variable name","Descriptions")
    return (sra1)
  })

  output$viewr <- DT::renderDataTable(
    DT::datatable(data_setr())
  )
  output$views <- DT::renderDataTable(
    DT::datatable(data_sets())
  )
  output$viewa <- DT::renderDataTable(
    DT::datatable(data_seta())
  )

  observeEvent(input$QCsdtm,
               {
                 output$Message<-renderText(
                   print("<b>Your QC report for SDTM datasets has been updated.
                         Please download the report by clicking the 'Reports' tab.</b>"))

                 Item1="General 1"
                 ndup <- ndupk(data_sets(),input$datasets)
                 message1 <- ndup$Finding
                 conclusion1 <- ndup$Conclusion
                 itemh1<-cbind(Item1,message1,conclusion1)

                 Item2="General 2"
                 epoch <- EPOCHcheck(data_sets(),input$datasets)
                 message2 <- epoch$Finding
                 conclusion2 <- epoch$Conclusion
                 itemh2<-cbind(Item2,message2,conclusion2)

                 Item3="General 3"
                 dy <- DYCHECK(data_sets(),input$datasets)
                 message3 <- dy$Finding
                 conclusion3 <- dy$Conclusion
                 itemh3<-cbind(Item3,message3,conclusion3)

                 Item4="General 4"
                 DTC <- DTCfmt(data_sets(),input$datasets)
                 message4 <- DTC$Finding
                 conclusion4 <- DTC$Conclusion
                 itemh4 <- cbind(Item4,message4,conclusion4)

                 Item5="General 5"
                 if (input$datasets=="ae.sas7bdat" |
                     input$datasets=="mh.sas7bdat" |
                     input$datasets=="pr.sas7bdat")
                 {
                   if (input$datasets=="ae.sas7bdat")
                   {
                     aevars<-c("AEHLT","AEHLGT","AELLT")
                     aevars<-as.data.frame(aevars)
                     colnames(aevars)<-c("shaevars")
                     sdtmvarnames<-names(data_sets())
                     sdtmvarnames<-as.data.frame(sdtmvarnames)
                     colnames(sdtmvarnames)<-c("varsinsdtm")
                     if (any((aevars$shaevars %in% sdtmvarnames$varsinsdtm)==FALSE))
                     {
                       aenotsdtm<-aevars[((aevars$shaevars %in% sdtmvarnames$varsinsdtm)==FALSE),]
                       aenotsdtm1<-as.data.frame(aenotsdtm)
                       aenotsdtm2<-apply(aenotsdtm1,2,paste,collapse=", ")
                       message5<-paste("The below required legacy terms are not in the AE dataset:",
                                       aenotsdtm2, ".")
                       conclusion5<-"Failed"
                     }
                     else {
                       message5<-"All hierachical legacy terms are included in the AE dataset."
                       conclusion5<-"Passed"
                     }
                   }
                   else if (input$datasets=="mh.sas7bdat")
                   {
                     mhvars<-c("MHHLT","MHHLGT","MHLLT")
                     mhvars<-as.data.frame(mhvars)
                     colnames(mhvars)<-c("shmhvars")
                     sdtmvarnames<-names(data_sets())
                     sdtmvarnames<-as.data.frame(sdtmvarnames)
                     colnames(sdtmvarnames)<-c("varsinsdtm")
                     if (any((mhvars$shmhvars %in% sdtmvarnames$varsinsdtm)==FALSE))
                     {
                       mhnotsdtm<-mhvars[((mhvars$shmhvars %in% sdtmvarnames$varsinsdtm)==FALSE),]
                       mhnotsdtm1<-as.data.frame(mhnotsdtm)
                       mhnotsdtm2<-apply(mhnotsdtm1,2,paste,collapse=", ")
                       message5<-paste("The below required legacy terms are not in the MH dataset:",
                                       mhnotsdtm2, ".")
                       conclusion5<-"Failed"
                     }
                     else {
                       message5<-"All hierachical legacy terms are included in the MH dataset."
                       conclusion5<-"Passed"
                     }
                   }
                   else if (input$datasets=="pr.sas7bdat")
                   {
                     prvars<-c("PRHLT","PRHLGT","PRLLT")
                     prvars<-as.data.frame(prvars)
                     colnames(prvars)<-c("shprvars")
                     sdtmvarnames<-names(data_sets())
                     sdtmvarnames<-as.data.frame(sdtmvarnames)
                     colnames(sdtmvarnames)<-c("varsinsdtm")
                     if (any((prvars$shprvars %in% sdtmvarnames$varsinsdtm)==FALSE))
                     {
                       prnotsdtm<-prvars[((prvars$shprvars %in% sdtmvarnames$varsinsdtm)==FALSE),]
                       prnotsdtm1<-as.data.frame(prnotsdtm)
                       prnotsdtm2<-apply(prnotsdtm1,2,paste,collapse=", ")
                       message5<-paste("The below required legacy terms are not in the PR dataset:",
                                       prnotsdtm2, ".")
                       conclusion5<-"Failed"
                     }
                     else {
                       message5<-"All hierachical legacy terms are included in the PR dataset."
                       conclusion5<-"Passed"
                     }
                   }
                 }
                 else {message5<-"This item is only applicable to AE, MH or PR domain."
                 conclusion5<-"Not applicable"
                 }
                 message5<-sapply(lapply(message5, strwrap, width=50), paste, collapse="\n")
                 itemh5<-cbind(Item5,message5,conclusion5)

                 ## QC check for -ENRF, -ENRTPT, -ENTPT for CM and AE domain
                 Item6="General 6"
                 if (input$datasets=="cm.sas7bdat" |
                     input$datasets=="ae.sas7bdat")
                 {
                   if (input$datasets=="cm.sas7bdat" &
                       CRFspec1=="Collection date")
                   {
                     prvars<-c("CMENRTPT","CMENTPT")
                     prvars<-as.data.frame(prvars)
                     colnames(prvars)<-c("shprvars")
                     sdtmvarnames<-names(data_sets())
                     sdtmvarnames<-as.data.frame(sdtmvarnames)
                     colnames(sdtmvarnames)<-c("varsinsdtm")
                     if (any((prvars$shprvars %in% sdtmvarnames$varsinsdtm)==FALSE))
                     {
                       prnotsdtm<-prvars[((prvars$shprvars %in% sdtmvarnames$varsinsdtm)==FALSE),]
                       prnotsdtm1<-as.data.frame(prnotsdtm)
                       prnotsdtm2<-apply(prnotsdtm1,2,paste,collapse=", ")
                       message6<-paste("The below variables are not mapped in the CM dataset:",
                                       prnotsdtm2, ".")
                       conclusion6<-"Failed"
                     }
                     else {
                       message6<-"The variables are mapped in the CM domain."
                       conclusion6<-"Passed"
                     }
                   }
                   else if (input$datasets=="ae.sas7bdat" &
                            CRFspec1=="Collection date")
                   {
                     prvars<-c("AEENRTPT","AEENTPT")
                     prvars<-as.data.frame(prvars)
                     colnames(prvars)<-c("shprvars")
                     sdtmvarnames<-names(data_sets())
                     sdtmvarnames<-as.data.frame(sdtmvarnames)
                     colnames(sdtmvarnames)<-c("varsinsdtm")
                     if (any((prvars$shprvars %in% sdtmvarnames$varsinsdtm)==FALSE))
                     {
                       prnotsdtm<-prvars[((prvars$shprvars %in% sdtmvarnames$varsinsdtm)==FALSE),]
                       prnotsdtm1<-as.data.frame(prnotsdtm)
                       prnotsdtm2<-apply(prnotsdtm1,2,paste,collapse=", ")
                       message6<-paste("The below variables are not mapped in the AE dataset:",
                                       prnotsdtm2, ".")
                       conclusion6<-"Failed"
                     }
                     else {
                       message6<-"The variables are mapped in the AE domain."
                       conclusion6<-"Passed"
                     }
                   }
                   else if (input$datasets=="cm.sas7bdat" &
                            CRFspec1=="First dosing date")
                   {
                     prvars<-c("CMENRF")
                     prvars<-as.data.frame(prvars)
                     colnames(prvars)<-c("shprvars")
                     sdtmvarnames<-names(data_sets())
                     sdtmvarnames<-as.data.frame(sdtmvarnames)
                     colnames(sdtmvarnames)<-c("varsinsdtm")
                     if  (any((prvars$shprvars %in% sdtmvarnames$varsinsdtm)==FALSE))
                     {
                       prnotsdtm<-prvars[((prvars$shprvars %in% sdtmvarnames$varsinsdtm)==FALSE),]
                       prnotsdtm1<-as.data.frame(prnotsdtm)
                       prnotsdtm2<-apply(prnotsdtm1,2,paste,collapse=", ")
                       message6<-paste("The below variables are not mapped in the CM dataset:",
                                       prnotsdtm2, ".")
                       conclusion6<-"Failed"
                     }
                     else {
                       message6<-"The variables are mapped in the CM domain."
                       conclusion6<-"Passed"
                     }
                   }
                   else if (input$datasets=="ae.sas7bdat" &
                            CRFspec1=="First dosing date")
                   {
                     prvars<-c("AEENRF")
                     prvars<-as.data.frame(prvars)
                     colnames(prvars)<-c("shprvars")
                     sdtmvarnames<-names(data_sets())
                     sdtmvarnames<-as.data.frame(sdtmvarnames)
                     colnames(sdtmvarnames)<-c("varsinsdtm")
                     if  (any((prvars$shprvars %in% sdtmvarnames$varsinsdtm)==FALSE))
                     {
                       prnotsdtm<-prvars[((prvars$shprvars %in% sdtmvarnames$varsinsdtm)==FALSE),]
                       prnotsdtm1<-as.data.frame(prnotsdtm)
                       prnotsdtm2<-apply(prnotsdtm1,2,paste,collapse=", ")
                       message6<-paste("The below variables are not mapped in the AE dataset:",
                                       prnotsdtm2, ".")
                       conclusion6<-"Failed"
                     }
                     else {
                       message6<-"The variables are mapped in the AE domain."
                       conclusion6<-"Passed"
                     }
                   }
                 }
                 else {
                   message6<-"This item is only applicable to CM or AE domain."
                   conclusion6<-"Not applicable"
                 }
                 itemh6<-cbind(Item6,message6,conclusion6)

                 Item7="DM 1"
                 if (input$datasets=="dm.sas7bdat")
                 {
                   df1<-data_sets()[which(data_sets()$DTHDTC!=''),]
                   if (df1$DTHFL=='Y')
                   {message7<-"DTHFL has been mapped."
                   conclusion7<-"Passed"}
                   else {message7<-"DTHFL has not been mapped."
                   conclusion7<-"Failed"}
                 }
                 else {message7<-"This item is only applicable to DM domain."
                 conclusion7<-"Not applicable"}
                 itemh7<-cbind(Item7,message7,conclusion7)

                 Item8="DM 2"
                 if (input$datasets=="dm.sas7bdat")
                 {ACT <- ARMcheck(data_sets())
                 message8 <- ACT$Finding
                 conclusion8 <- ACT$Conclusion}
                 else {message8<-"This item is only applicalbe to DM domain."
                 conclusion8<-"Not applicable"}
                 itemh8<-cbind(Item8,message8,conclusion8)

                 Item9="DM 3"
                 if (input$datasets=="dm.sas7bdat")
                 {
                   res=any(colnames(data_sets())=="RACEOTH")
                   if (res=="FALSE")
                   {message9<-"No other race in the dataset."
                   conclusion9<-"Passed"}
                   else {message9<-"There is other race in the dataset."
                   conclusion9<-"Failed"}
                 }
                 else {message9<-"This item is only applicable to DM domain."
                 conclusion9<-"Not applicable"}
                 itemh9<-cbind(Item9,message9,conclusion9)

                 ## QC(DM) check for other race
                 Item10="DM 3"
                 if (input$datasets=="dm.sas7bdat")
                 {
                   res=any(colnames(data_sets())=="RACEOTH")
                   if (res=="FALSE")
                   {message10<-"No other race in the dataset."
                   conclusion10<-"Passed"}
                   else {message10<-"There is other race in the dataset."
                   conclusion10<-"Failed"}
                 }
                 else {message10<-"This item is only applicable to DM domain."
                 conclusion10<-"Not applicable"}
                 itemh10<-cbind(Item10,message10,conclusion10)

                 ## QC(DM) check for multiple race
                 Item11="SUPPDM 1"
                 if (input$datasets=="suppdm.sas7bdat")
                 {
                   mul=any(data_sets()$QNAM=="RACE1")
                   if (mul=="FALSE")
                   {message11<-"No multiple race in the dataset."
                   conclusion11<-"Passed"}
                   else {message11<-"There is multiple race in the dataset."
                   conclusion11<-"Failed"}
                 }
                 else {message11<-"This item is only applicable to SUPPDM domain."
                 conclusion11<-"Not applicable"}
                 itemh11<-cbind(Item11,message11,conclusion11)

                 ## QC(EX) check for whodrug standard
                 Item12="EX 1"
                 if (input$datasets=="ex.sas7bdat")
                 {
                   who=any(data_sets()$EXTRT=="Tanezumab 2.5/5 mg"
                           |data_sets()$EXTRT== "Tanezumab 2.5 mg")
                   if (who=="FALSE")
                   {message12<-"It meets whodrug standard: PF-04383119."
                   conclusion12<-"Passed"}
                   else {message12<-"It didn't pass the QC check."
                   conclusion12<-"Failed"}
                 }
                 else {message12<-"This item is only applicable to EX domain."
                 conclusion12<-"Not applicable"}
                 itemh12<-cbind(Item12,message12,conclusion12)

                 ## QC check for LB (%)
                 Item13="LB 1"
                 if (input$datasets=="lb.sas7bdat")
                 {
                   if (any(data_sets()$LBORRESU=="%"))
                   {
                     ## choose the %
                     per=data_sets()[data_sets()$LBORRESU=="%",]
                     ## except for sth dont need percentage
                     per1=per[per$LBTEST!="Hematocrit" &
                                per$LBTEST!="Hemoglobin A1C" &
                                per$LBTEST!="Erythrocytes Distribution Width" &
                                per$LBTEST!="Neutrophils Band Form" &
                                per$LBTEST!="Nucleated Erythrocytes",]
                     LE=any(grepl("LE",per1$LBTESTCD)=="FALSE")
                     LE1=any(grepl("/",per1$LBTEST)=="FALSE")
                     if (LE=="FALSE"& LE1=="FALSE")
                     {message13<-"It meets % rule: LBTESTCD comtain LE."
                     conclusion13<-"Passed"}
                     else {message13<-"Omg, check for LBTESTCD and LBTEST."
                     conclusion13<-"Failed"}
                   }
                 }
                 else {message13<-"This item in only applicable to LB domain."
                 conclusion13<-"Not applicable"}
                 itemh13<-cbind(Item13,message13,conclusion13)

                 messagesdtm<-as.data.frame(rbind(itemh1,itemh2,itemh3,itemh4,itemh5,
                                                  itemh6,itemh7,itemh8,itemh9,itemh10,
                                                  itemh11,itemh12,itemh13
                 ))
                 colnames(messagesdtm)<-c("Item #","Findings","Conclusions")
                 setwd("~")
                 table <- tableGrob(messagesdtm)
                 grid.newpage()
                 h <- grobHeight(table)
                 w <- grobWidth(table)
                 title <- textGrob(paste("QC check results for SDTM datasets:",
                                         input$datasets),
                                   y=unit(0.5,"npc") + 1.5*h,
                                   vjust=0, gp=gpar(fontsize=10))
                 footnote <- textGrob(paste("PFIZER CONFIDENTIAL\nTime of report generation:",
                                            Sys.time()),
                                      x=unit(0.5,"npc") - 1.5*w,
                                      y=unit(0.5,"npc") - 1.5*h,
                                      vjust=1,hjust=0, gp=gpar( fontface="italic"))
                 gt <- gTree(children=gList(table, title, footnote))
                 pdf("QCresults.pdf",height=11,width=8.5)
                 grid.draw(gt)
                 dev.off()
                 output$QC_Reports<-downloadHandler(
                   filename = "QC_report.pdf",
                   content = function(file){
                     file.copy("QCresults.pdf",file)
                   }
                 )
               }
  )
  observeEvent(input$QCdataset,
               {output$Message<-renderText(
                 print("<b>Your QC report for Dataset has been updated.
                       Please download the report by clicking the 'Reports' tab.</b>")
                 )
               Item1="D 11"
               specs<-specdata()[,7]
               specs1<-as.data.frame(specs)
               colnames(specs1)<-c("varsinspec")
               adamvarnames<-names(data_seta())
               adamvarnames<-as.data.frame(adamvarnames)
               colnames(adamvarnames)<-c("varsinadam")

               if (any((specs1$varsinspec %in% adamvarnames$varsinadam)==FALSE)
                   & !any((adamvarnames$varsinadam %in% specs1$varsinspec)==FALSE))
               {
                 specnotadam<-specs1[((specs1$varsinspec %in% adamvarnames$varsinadam)==FALSE),]
                 specnotadam1<-as.data.frame(specnotadam)
                 specnotadam2<-apply(specnotadam1,2,paste,collapse=", ")
                 message1<-paste("The below variables in the specs
                                 but not in the ADaM dataset:",
                                 specnotadam2)
                 conclusion1<-"Failed"
               }
               else if (!any((specs1$varsinspec %in% adamvarnames$varsinadam)==FALSE)
                        & any((adamvarnames$varsinadam %in% specs1$varsinspec)==FALSE))
               {
                 adamnotspec<-adamvarnames[((adamvarnames$varsinadam %in% specs1$varsinspec)==FALSE),]
                 adamnotspec1<-as.data.frame(adamnotspec)
                 adamnotspec2<-apply(adamnotspec1,2,paste,collapse=", ")
                 message1<-paste("The below variables in the ADaM dataset
                                 but not in the specs:",
                                 adamnotspec2)
                 conclusion1<-"Failed"
               }
               else if (any((specs1$varsinspec %in% adamvarnames$varsinadam)==FALSE)
                        & any((adamvarnames$varsinadam %in% specs1$varsinspec)==FALSE))
               {
                 adamnotspec<-adamvarnames[((adamvarnames$varsinadam %in% specs1$varsinspec)==FALSE),]
                 adamnotspec1<-as.data.frame(adamnotspec)
                 adamnotspec2<-apply(adamnotspec1,2,paste,collapse=", ")

                 specnotadam<-specs1[((specs1$varsinspec %in% adamvarnames$varsinadam)==FALSE),]
                 specnotadam1<-as.data.frame(specnotadam)
                 specnotadam2<-apply(specnotadam1,2,paste,collapse=", ")

                 message1<-paste("The below variables in the specs but
                                 not in the ADaM dataset:",
                                 specnotadam2,
                                 ". ",
                                 "The below variables in the ADaM dataset but
                                 not in the specs:",
                                 adamnotspec2,
                                 ".")
                 conclusion1<-"Failed"}
               else {message1<-"The variables in the ADaM dataset are mapped with those in specs."
               conclusion1<-"Passed"}
               message1<-sapply(lapply(message1, strwrap, width=50), paste, collapse="\n")
               itemh1<-cbind(Item1,message1,conclusion1)

               messagesdtm<-as.data.frame(rbind(itemh1))
               colnames(messagesdtm)<-c("Item #","Findings","Conclusions")

               setwd("~")
               table <- tableGrob(messagesdtm)
               grid.newpage()
               h <- grobHeight(table)
               w <- grobWidth(table)
               title <- textGrob("QC check results for Dataset",
                                 y=unit(0.5,"npc") + 12.0*h,
                                 vjust=0, gp=gpar(fontsize=10))
               footnote <- textGrob(paste("PFIZER CONFIDENTIAL\nTime of report generation:",
                                          Sys.time()),
                                    x=unit(0.5,"npc") - 3.0*w,
                                    y=unit(0.5,"npc") - 12.0*h,
                                    vjust=1,hjust=0,gp=gpar( fontface="italic"))
               gt <- gTree(children=gList(table, title, footnote))

               pdf("QCresults.pdf",height=11,width=8.5)
               grid.draw(gt)
               dev.off()
               output$QC_Reports<-downloadHandler(
                 filename = "QC_report.pdf",
                 content = function(file){
                   file.copy("QCresults.pdf",file)
                 }
               )
               }
  )
  observeEvent(input$QCgeneral,
               {output$Message<-renderText(
                 print("<b>Your QC report for General Items has been updated.
                       Please download the report by clicking the 'Reports' tab.</b>")
                 )
               Item1="G29"
               if (input$dataseta=="adsl.sas7bdat")
                 #We can other applicable datasets in the above condition.
               {
                 aimvars<-c("USUBJID","FASFL","SAFFL","ITTFL","PPROTFL","COMPLFL","RANDFL","ENRLFL")
                 aimvars<-as.data.frame(aimvars)
                 colnames(aimvars)<-c("shvars")
                 adamvarnames<-names(data_seta())
                 adamvarnames<-as.data.frame(adamvarnames)
                 colnames(adamvarnames)<-c("varsinadam")

                 if (any((aimvars$shvars %in% adamvarnames$varsinadam)==FALSE))
                 {
                   aimnotadam<-aimvars[((aimvars$shvars %in% adamvarnames$varsinadam)==FALSE),]
                   aimnotadam1<-as.data.frame(aimnotadam)
                   aimnotadam2<-apply(aimnotadam1,2,paste,collapse=", ")
                   message1<-paste("The below required variables/flags are not in the ADaM dataset:",
                                   aimnotadam2, ".")
                   conclusion1<-"Failed"
                 }
                 else {message1<-"All required variables and flags are in the dataset."
                 conclusion1<-"Passed"}
               }
               else {message1<-"This item is only applicable to patient level datasets."
               conclusion1<-"Not applicable"}
               message1<-sapply(lapply(message1, strwrap, width=50), paste, collapse="\n")
               itemh1<-cbind(Item1,message1,conclusion1)

               messagesdtm<-as.data.frame(rbind(itemh1))
               colnames(messagesdtm)<-c("Item #","Findings","Conclusions")
               setwd("~")
               pdf("QCresults.pdf",height=11,width=8.5)
               grid.table(messagesdtm)
               dev.off()
               output$QC_Reports<-downloadHandler(
                 filename = "QC_report.pdf",
                 content = function(file){
                   file.copy("QCresults.pdf",file)
                 }
               )
               }
  )
  observeEvent(input$QCtable,
               {output$Message<-renderText(
                 print("<b>Your QC report for Tables has been updated.
                       Please download the report by clicking the 'Reports' tab.</b>")
                 )
               Item1="General 1"
               if (sum(duplicated(data_sets()))==0 &
                   !is.na(sum(duplicated(data_sets()))==0))
               {message1<-"No duplicate records in the dataset."
               conclusion1<-"Passed"}
               else {message1<-"There are duplicate records in the dataset."
               conclusion1<-"Failed"}
               itemh1<-cbind(Item1,message1,conclusion1)

               messagesdtm<-as.data.frame(rbind(itemh1))
               colnames(messagesdtm)<-c("Item #","Findings","Conclusions")
               setwd("~")
               pdf("QCresults.pdf",height=11,width=8.5)
               grid.table(messagesdtm)
               dev.off()
               output$QC_Reports<-downloadHandler(
                 filename = "QC_report.pdf",
                 content = function(file){
                   file.copy("QCresults.pdf",file)
                 }
               )
               }
  )
  observeEvent(input$QClisting,
               {output$Message<-renderText(
                 print("<b>Your QC report for Listings has been updated.
                       Please download the report by clicking the 'Reports' tab.</b>")
                 )
               Item1="General 1"
               if (sum(duplicated(data_sets()))==0 &
                   !is.na(sum(duplicated(data_sets()))==0))
               {message1<-"No duplicate records in the dataset."
               conclusion1<-"Passed"}
               else {message1<-"There are duplicate records in the dataset."
               conclusion1<-"Failed"}
               itemh1<-cbind(Item1,message1,conclusion1)

               messagesdtm<-as.data.frame(rbind(itemh1))
               colnames(messagesdtm)<-c("Item #","Findings","Conclusions")
               setwd("~")
               pdf("QCresults.pdf",height=11,width=8.5)
               grid.table(messagesdtm)
               dev.off()
               output$QC_Reports<-downloadHandler(
                 filename = "QC_report.pdf",
                 content = function(file){
                   file.copy("QCresults.pdf",file)
                 }
               )
               }
  )
  observeEvent(input$QCfigure,
               {output$Message<-renderText(
                 print("<b>Your QC report for Figures has been updated.
                       Please download the report by clicking the 'Reports' tab.</b>")
                 )
               Item1="General 1"
               if (sum(duplicated(data_sets()))==0 &
                   !is.na(sum(duplicated(data_sets()))==0))
               {message1<-"No duplicate records in the dataset."
               conclusion1<-"Passed"}
               else {message1<-"There are duplicate records in the dataset."
               conclusion1<-"Failed"}
               itemh1<-cbind(Item1,message1,conclusion1)

               messagesdtm<-as.data.frame(rbind(itemh1))
               colnames(messagesdtm)<-c("Item #","Findings","Conclusions")
               setwd("~")
               pdf("QCresults.pdf",height=11,width=8.5)
               grid.table(messagesdtm)
               dev.off()
               output$QC_Reports<-downloadHandler(
                 filename = "QC_report.pdf",
                 content = function(file){
                   file.copy("QCresults.pdf",file)
                 }
               )
               }
  )
               }
)
