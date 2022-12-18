

fe_prov_linear <- function(data, xchar, ychar, provchar, maxiter = 1e4, tol=1e-5){
  data <- data[which(data$included==1),]
  
  prov_response_len <- sapply(split(data[, ychar], data[, provchar]), length)
  prov_response_sum <- sapply(split(data[, ychar], data[, provchar]), sum)
  
  X <- as.matrix(data[,xchar])
  gamma_prov <- rep(mean(data[,ychar]),length(prov_response_len))
  beta <- rep(0,dim(X)[2])
  iter <- 0; diff <- 10
  
  while (iter<=maxiter & diff>=tol) {
    iter <- iter + 1
    cat(paste0("\n Iter ",iter,":"))
    # provider effect update
    gamma <- rep(gamma_prov, prov_response_len)
    eta <- X%*%beta
    mu <- c(gamma+eta)
    gamma_prov <- sapply(split(data[,ychar]-mu, data[,provchar]), sum) /
      sapply(split(rep(1,dim(X)[1]), data[,provchar]), sum) + gamma_prov
    gamma <- rep(gamma_prov, prov_response_len)
    
    # regression parameter update
    mu <- c(gamma+eta)
    score <- t(X)%*%(data[,ychar]-mu)
    hessian <- t(X)%*%(X)
    beta_new <- beta + as.numeric(solve(hessian)%*%score)
    diff <- norm(matrix(beta-beta_new),"I") # stopping criterion
    beta <- beta_new
    cat(paste0("The diff criterion is ",formatC(diff,digits=3,format="e"),";"))
  }
  
  fitted <- c(gamma+ X%*%beta)
  
  return(list(gamma = gamma, beta = beta, fitted = fitted))
}

fe_prov_logistic <- function(data, xchar, ychar, provchar, maxiter = 1e4, tol=1e-5){
  data <- data[which(data$included==1),]
  
  prov_response_len <- sapply(split(data[, ychar], data[, provchar]), length)
  prov_response_sum <- sapply(split(data[, ychar], data[, provchar]), sum)
  
  X <- as.matrix(data[,xchar])
  gamma_prov <- rep(mean(data[,ychar]),length(prov_response_len))
  beta <- rep(0,dim(X)[2])
  iter <- 0; diff <- 10
  
  while (iter<=maxiter & diff>=tol) {
    iter <- iter + 1
    cat(paste0("\n Iter ",iter,":"))
    # provider effect update
    gamma <- rep(gamma_prov, prov_response_len)
    eta <- X%*%beta
    p <- c(plogis(gamma+eta))
    gamma_prov <- sapply(split(data[,ychar]-p, data[,provchar]), sum) /
      sapply(split(c(p*(1-p)), data[,provchar]), sum) + gamma_prov
    gamma_prov <- pmin(pmax(gamma_prov, median(gamma_prov)-10), median(gamma_prov)+10)
    gamma <- rep(gamma_prov, prov_response_len)
    
    # regression parameter update
    p <- c(plogis(gamma+eta))
    score <- t(X)%*%(data[,ychar]-p)
    hessian <- t(X)%*%(c(p*(1-p))*X)
    beta_new <- beta + as.numeric(solve(hessian)%*%score)
    diff <- norm(matrix(beta-beta_new),"I") # stopping criterion
    beta <- beta_new
    cat(paste0("The diff criterion is ",formatC(diff,digits=3,format="e"),";"))
  }
  
  fitted <- c(plogis(gamma+X%*%beta))
  
  return(list(gamma = gamma, beta = beta, fitted = fitted))
}


x_char <- c("BENE_SEX_IDENT_CD",
            "BENE_RACE_CD","BENE_ESRD_IND","SP_ALZHDMTA","SP_CHF",
            "SP_CHRNKIDN","SP_CNCR","SP_COPD","SP_DEPRESSN",
            "SP_DIABETES","SP_ISCHMCHT","SP_OSTEOPRS","SP_RA_OA","SP_STRKETIA","AGE")
provchar <- "PRVDR_NUM"

Beneficary_2008 <- fread("DE1_0_2008_Beneficiary_Summary_File_Sample_1.csv")
Beneficary_2009 <- fread("DE1_0_2009_Beneficiary_Summary_File_Sample_1.csv")
Beneficary_2010 <- fread("DE1_0_2010_Beneficiary_Summary_File_Sample_1.csv")
Inpatient <- fread("DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv")

Beneficary_2008 <- as.data.frame(Beneficary_2008)
Beneficary_2009 <- as.data.frame(Beneficary_2009)
Beneficary_2010 <- as.data.frame(Beneficary_2010)
Inpatient <- as.data.frame(Inpatient)

Inpatient <- data_preprocess(Beneficary_2008,Beneficary_2009,Beneficary_2010,Inpatient)

Beneficary_2008_2 <- fread("DE1_0_2008_Beneficiary_Summary_File_Sample_2.csv")
Beneficary_2009_2 <- fread("DE1_0_2009_Beneficiary_Summary_File_Sample_2.csv")
Beneficary_2010_2 <- fread("DE1_0_2010_Beneficiary_Summary_File_Sample_2.csv")
Inpatient_2 <- fread("DE1_0_2008_to_2010_Inpatient_Claims_Sample_2.csv")

Beneficary_2008_2 <- as.data.frame(Beneficary_2008_2)
Beneficary_2009_2 <- as.data.frame(Beneficary_2009_2)
Beneficary_2010_2 <- as.data.frame(Beneficary_2010_2)
Inpatient_2 <- as.data.frame(Inpatient_2)

Inpatient_2 <- data_preprocess(Beneficary_2008_2,Beneficary_2009_2,Beneficary_2010_2,Inpatient_2)

res1 <- fe_prov_linear(data = Inpatient, xchar = x_char, ychar = "CLM_PMT_AMT", provchar = "PRVDR_NUM")
res2 <- fe_prov_logistic(data = Inpatient, xchar = x_char, ychar = "RECLAIM_3MONTHS", provchar = "PRVDR_NUM")

## training and predict error for linear case
# training error
ychar <- "CLM_PMT_AMT"
sqrt(mean((res1$fitted-Inpatient[which(Inpatient$included==1),ychar])^2)) # with provider effect

fit1 <- lm(CLM_PMT_AMT~.,data = Inpatient[,c(7,10:24)])
sqrt(mean((fitted.values(fit1)-Inpatient[,ychar])^2)) # without provider effect

# testing error
Inpatient_2_new <- Inpatient_2[which(Inpatient_2$included==1),]
PRV_Diff <- setdiff(unique(Inpatient_2_new$PRVDR_NUM),unique(names(res1$gamma)))# all included

idx2 <- which((unique(names(res1$gamma))) %in% setdiff(unique(names(res1$gamma)),Inpatient_2_new$PRVDR_NUM))
gamma_prov <- res1$gamma[unique(names(res$gamma))]
gamma_prov <- gamma_prov[-idx2]

prov_size <- sapply(split(Inpatient_2_new[, "CLM_PMT_AMT"], Inpatient_2_new[, "PRVDR_NUM"]), length)
gamma_pred <- rep(gamma_prov,prov_size)
x_pred <- Inpatient_2_new[,xchar]
y_pred <- c(gamma_pred+as.matrix(x_pred) %*% as.vector(res$beta))
y_real <- Inpatient_2_new[,"CLM_PMT_AMT"]
sqrt(mean((y_pred-y_real)^2)) # with provider effect

y_pred2 <- predict.lm(fit1, Inpatient_2[,xchar])
sqrt(mean((y_pred2-Inpatient_2[,"CLM_PMT_AMT"])^2)) # without provider effect

## training and predict error for logistic case
# training error
ychar <- "RECLAIM_3MONTHS"
sqrt(mean((res2$fitted-Inpatient[which(Inpatient$included==1),ychar])^2)) # with provider effect

fit2 <- glm(RECLAIM_3MONTHS~.,data = Inpatient[,c(10:25)],family = "binomial")
sqrt(mean((fitted.values(fit2)-Inpatient[,ychar])^2)) # without provider effect

# testing error

idx2 <- which((unique(names(res2$gamma))) %in% setdiff(unique(names(res2$gamma)),Inpatient_2_new$PRVDR_NUM))
gamma_prov <- res2$gamma[unique(names(res$gamma))]
gamma_prov <- gamma_prov[-idx2]

prov_size <- sapply(split(Inpatient_2_new[, ychar], Inpatient_2_new[, "PRVDR_NUM"]), length)
gamma_pred <- rep(gamma_prov,prov_size)
x_pred <- Inpatient_2_new[,xchar]
y_pred <- c(plogis(gamma_pred+as.matrix(x_pred) %*% as.vector(res2$beta)))
y_real <- Inpatient_2_new[,ychar]
sqrt(mean((y_pred-y_real)^2)) # with provider effect

y_pred2 <- c(plogis(fit2$coefficients[1]+as.matrix(Inpatient_2[,xchar])%*%fit2$coefficients[-1]))
sqrt(mean((y_pred2-Inpatient_2[,ychar])^2)) # without provider effect
