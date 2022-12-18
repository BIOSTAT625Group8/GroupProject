library(data.table)
library(dplyr)
setwd("D:/Onedrive/OneDrive - Umich/Coursework/Year1 Fall/BIOSTAT 625/Project/Data")
# Read data using fread instead of readcsv
Beneficary_2008 <- fread("DE1_0_2008_Beneficiary_Summary_File_Sample_1.csv")
Beneficary_2009 <- fread("DE1_0_2009_Beneficiary_Summary_File_Sample_1.csv")
Beneficary_2010 <- fread("DE1_0_2010_Beneficiary_Summary_File_Sample_1.csv")
Inpatient <- fread("DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv")

# covert into beneficiary
Beneficary_2008 <- as.data.frame(Beneficary_2008)
Beneficary_2009 <- as.data.frame(Beneficary_2009)
Beneficary_2010 <- as.data.frame(Beneficary_2010)
Inpatient <- as.data.frame(Inpatient)


# Data Cleaning & Combination
data_preprocess <- function(Beneficary_2008,Beneficary_2009,Beneficary_2010,Inpatient){
  
  # variables of interest
  Beneficary_char <- c("DESYNPUF_ID","BENE_BIRTH_DT","BENE_SEX_IDENT_CD",
                      "BENE_RACE_CD","BENE_ESRD_IND","SP_ALZHDMTA","SP_CHF",
                      "SP_CHRNKIDN","SP_CNCR","SP_COPD","SP_DEPRESSN",
                      "SP_DIABETES","SP_ISCHMCHT","SP_OSTEOPRS","SP_RA_OA","SP_STRKETIA")
  Inpatient_char <- c("DESYNPUF_ID","CLM_ID","SEGMENT","CLM_FROM_DT","CLM_THRU_DT","PRVDR_NUM","CLM_PMT_AMT")
  Outpatient_char <- c("DESYNPUF_ID","CLM_ID","SEGMENT","CLM_FROM_DT","CLM_THRU_DT","PRVDR_NUM","CLM_PMT_AMT")
  
  # select from original tables
  Beneficary_2008 <- Beneficary_2008[,Beneficary_char]
  Beneficary_2009 <- Beneficary_2009[,Beneficary_char]
  Beneficary_2010 <- Beneficary_2010[,Beneficary_char]
  
  # set year, combine tables and define age
  Beneficary_2008$YEAR <- 2008
  Beneficary_2009$YEAR <- 2009
  Beneficary_2010$YEAR <- 2010
  Beneficary <- rbind(Beneficary_2008,Beneficary_2009,Beneficary_2010)
  Beneficary$AGE <- Beneficary$YEAR - as.numeric(substr(Beneficary[,"BENE_BIRTH_DT"],1,4))

  ## set all binary data to 1 and 2, categorical to 1,2,3,4
  Beneficary$BENE_RACE_CD[which(Beneficary$BENE_RACE_CD==5)]<-4
  Beneficary$BENE_RACE_CD <- Beneficary$BENE_RACE_CD-1
  Beneficary$BENE_ESRD_IND <- 1* (Beneficary$BENE_ESRD_IND=="Y")
  for(i in Beneficary_char[-c(1,2,4,5)]){
    Beneficary[,i][which(Beneficary[,i]==2)]<-0
  }
  
  ## Select variables of interest for Inpatient
  Inpatient <- Inpatient[,Inpatient_char]

  ## Consider those claims with multiple segments
  max(Inpatient$SEGMENT)

  # Update those NAs in Inpatient claims which have multiple segments
  for(i in which(duplicated(Inpatient$CLM_ID))){
    ids <- which(Inpatient$CLM_ID == Inpatient$CLM_ID[i])
    if(is.na(Inpatient$CLM_FROM_DT[ids[1]])){
      Inpatient[ids[1],"CLM_FROM_DT"] <- Inpatient[ids[2],"CLM_FROM_DT"]
      Inpatient[ids[1],"CLM_THRU_DT"] <- Inpatient[ids[2],"CLM_THRU_DT"]
    } else{
      Inpatient[ids[2],"CLM_FROM_DT"] <- Inpatient[ids[1],"CLM_FROM_DT"]
      Inpatient[ids[2],"CLM_THRU_DT"] <- Inpatient[ids[1],"CLM_THRU_DT"]
    }
  }
  
  # set year for each claim
  Inpatient$YEAR <- as.numeric(substr(Inpatient[,"CLM_FROM_DT"],1,4))
  
  # combine inpatient and beneficiary
  Inpatient <- left_join(Inpatient,Beneficary,by = c("DESYNPUF_ID","YEAR"))
  
  # delete all the missing value cases
  Inpatient <- Inpatient[complete.cases(Inpatient),]
  
  # create a binary response to test whether the patient reclaimed in Medicare in the following three months
  Inpatient <- Inpatient[order(Inpatient[,"CLM_FROM_DT"]),]
  Inpatient$RECLAIM_3MONTHS <- 0
  for(i in which(duplicated(Inpatient$DESYNPUF_ID))){
    idx <- which(Inpatient$DESYNPUF_ID==Inpatient$DESYNPUF_ID[i])
    for(j in 1:(length(idx)-1)){
      Inpatient$RECLAIM_3MONTHS[idx[j]] <- 1*(as.numeric(substr(Inpatient[j+1,"CLM_FROM_DT"]-Inpatient[j,"CLM_THRU_DT"],5,8)<=300))
   }
  }
  
  # order the data based on provider, screen those providers without more than 5 claims
  Inpatient <- Inpatient[order(factor(Inpatient[,"PRVDR_NUM"])),]
  prov_size <- as.integer(table(Inpatient[,"PRVDR_NUM"]))
  prov_size_patient <- rep(prov_size,prov_size)
  Inpatient$included <- 1 * (prov_size_patient > 5)
  
  return(Inpatient)
}