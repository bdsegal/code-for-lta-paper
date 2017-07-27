# ACL Latent Transition Analysis
# Make plots for bootstrap

# for making matches
perms <- permutations(n = 6, r = 6, v = 1:6)

# for recording any bootstap samples with label switching problems
problemMale <- NULL
problemFemale <- NULL

# 1) get point estimates from model fit to overall data
# files <- list.files("groupFit/")
files <- list.files("singleFitOut/")

fitFiles <- files[grep("fit", files)]

# get corresponding file of parameters
paramFiles <- files[grep("param", files)]
est <- read.csv(file.path("singleFitOut", paramFiles), na.strings=".")

# item response probabilities (irpSing) ---------------------------------------
irpSing <- est[which(est$PARAM == "RHO" & est$TIME==1),]
irpSing <- irpSing[order(irpSing$GROUP, irpSing$VARIABLE),
                   c(2, 3, 6, 7, 8, 9, 10, 11, 12)]

irpSing$variable <- factor(c(rep("BMI", 4), rep("Dead", 2), rep("Drinking", 4), 
                             rep("Current smoker", 3)),
                           levels = c("BMI", "Drinking", "Current smoker", "Dead"))

irpSing$response <- factor(c("Normal", "Overweight", "Obese", "Dead (BMI)", 
                             "Dead", "Alive", 
                             "None", "Moderate", "Heavy", "Dead (drinking)",
                             "Yes","No", "Dead (smoking)"), 
                          levels = c("Normal", "Overweight", "Obese", "Dead (BMI)",
                                     "Alive", "Dead",
                                     "None", "Moderate", "Heavy", "Dead (drinking)",
                                     "No", "Yes", "Dead (smoking)"))

irpSingM <- melt(irpSing[, -(2:3)], id = c("GROUP", "variable","response"),
                 variable.name = "status")
irpSingM$status <- as.numeric(gsub("ESTLS", "", irpSingM$status))

irpSingMale <- irpSingM[which(irpSingM$GROUP == 1), ]
irpSingFemale <- irpSingM[which(irpSingM$GROUP == 2), ]

# swap labels male
swapM <- rep(NA, 6)
for (s in 1:6) {
sub <- irpSingMale[which(irpSingMale$status == s), ]
if (sub[with(sub, which(variable == "Dead" & 
                        response == "Dead")), "value"] > 0.9) {
    swapM[s] <- 6
  } else if (sub[with(sub, which(variable == "Current smoker" & 
                                 response == "Yes")), "value"] > 0.9) {
    swapM[s] <- 3
  } else if (sub[with(sub, which(variable == "BMI" & 
                                 response == "Obese")), "value"] > 0.6) {
    swapM[s] <- 4
  } else if (sub[with(sub, which(variable == "Drinking" & 
                                 response == "None")), "value"] > 0.8) {
    swapM[s] <- 5
   } else if (sub[with(sub, which(variable == "BMI" & 
                                 response == "Normal")), "value"] > 0.8) {
    swapM[s] <- 1
  } else {
    swapM[s] <- 2
  }  
}

# swap labels female
swapF <- rep(NA, 6)
for (s in 1:6) {
sub <- irpSingFemale[which(irpSingFemale$status == s), ]
if (sub[with(sub, which(variable == "Dead" & 
                        response == "Dead")), "value"] > 0.9) {
    swapF[s] <- 6
  } else if (sub[with(sub, which(variable == "Current smoker" & 
                                 response == "Yes")), "value"] > 0.9) {
    swapF[s] <- 3
  } else if (sub[with(sub, which(variable == "BMI" & 
                                 response == "Obese")), "value"] > 0.6) {
    swapF[s] <- 4
  } else if (sub[with(sub, which(variable == "Drinking" & 
                                 response == "None")), "value"] > 0.8) {
    swapF[s] <- 5
   } else if (sub[with(sub, which(variable == "BMI" & 
                                 response == "Normal")), "value"] > 0.8) {
    swapF[s] <- 1
  } else {
    swapF[s] <- 2
  }  
}

irpSingMale$status <- sapply(irpSingMale$status, function(x) {swapM[x]})
irpSingFemale$status <- sapply(irpSingFemale$status, function(x) {swapF[x]})

irpSingMale <- irpSingMale[with(irpSingMale,
                                order(status, variable, response)), ]
irpSingFemale <- irpSingFemale[with(irpSingFemale,
                                    order(status, variable, response)), ]
    
# transition probabilities --------------------------------------------------
tpSing <- est[which(est$PARAM == "TAU"), ]
tpSingMale <- tpSing[which(tpSing$GROUP == 1), ]
tpSingFemale <- tpSing[which(tpSing$GROUP == 2), ]

# swap status lables
tpSingMale$STATUS <- sapply(tpSingMale$STATUS, function(x) {swapM[x]}) 
tpSingFemale$STATUS <- sapply(tpSingFemale$STATUS, function(x) {swapF[x]}) 

tpSingMaleNew <- tpSingMale[, c("TIME", "STATUS")]
statCols <- grep("ESTLS", colnames(tpSingMale))
for (j in 1:length(statCols)) {
  col <- which(swapM == j)
  tpSingMaleNew <- cbind(tpSingMaleNew, tpSingMale[, statCols[col]])
}
colnames(tpSingMaleNew) <- c("wave", "status", 1:6)

tpSingFemaleNew <- tpSingFemale[, c("TIME", "STATUS")]
statCols <- grep("ESTLS", colnames(tpSingFemale))
for (j in 1:length(statCols)) {
  col <- which(swapF == j)
  tpSingFemaleNew <- cbind(tpSingFemaleNew, tpSingFemale[, statCols[col]])
}
colnames(tpSingFemaleNew) <- c("wave", "status", 1:6)

tpSingMaleNew <- tpSingMaleNew[with(tpSingMaleNew, order(wave, status)), ]
tpSingMaleNewM <- melt(tpSingMaleNew, id.vars = c("wave", "status"),
                       variable.name = "to")
colnames(tpSingMaleNewM) <- c("wave", "from", "to", "value")
tpSingMaleNewM$from <- ordered(tpSingMaleNewM$from, levels = 6:1)
tpSingMaleNewM$to <- ordered(tpSingMaleNewM$to, levels = 1:6)
tpSingMaleNewM$facet <- paste("Wave", tpSingMaleNewM$wave, "to",
                              tpSingMaleNewM$wave + 1)

tpSingFemaleNew <- tpSingFemaleNew[with(tpSingFemaleNew, order(wave, status)), ]
tpSingFemaleNewM <- melt(tpSingFemaleNew, id.vars = c("wave", "status"),
                         variable.name = "to")
colnames(tpSingFemaleNewM) <- c("wave", "from", "to", "value")
tpSingFemaleNewM$from <- ordered(tpSingFemaleNewM$from, levels = 6:1)
tpSingFemaleNewM$to <- ordered(tpSingFemaleNewM$to, levels = 1:6)
tpSingFemaleNewM$facet <- paste("Wave", tpSingFemaleNewM$wave, "to", 
                                tpSingFemaleNewM$wave + 1) 

# initial probabilities -------------------------------------------------------
initSing <- est[grep("DELTA", est$PARAM),]
initSingMale <- initSing[which(initSing$GROUP == 1), ]
initSingFemale <- initSing[which(initSing$GROUP == 2), ]

# swap status labels
initSingMaleNew <- initSingMale[, c("PARAM", "VARIABLE")]
statCols <- grep("ESTLS", colnames(initSingMale))
for (j in 1:length(statCols)) {
  col <- which(swapM == j)
  initSingMaleNew <- cbind(initSingMaleNew, initSingMale[, statCols[col]])
}
colnames(initSingMaleNew) <- c("param", "covariate", 1:6)

initSingFemaleNew <- initSingFemale[, c("PARAM", "VARIABLE")]
statCols <- grep("ESTLS", colnames(initSingFemale))
for (j in 1:length(statCols)) {
  col <- which(swapF == j)
  initSingFemaleNew <- cbind(initSingFemaleNew, initSingFemale[, statCols[col]])
}
colnames(initSingFemaleNew) <- c("param", "covariate", 1:6)

avgM <- as.matrix(initSingMaleNew[1, -(1:2)])
avgF <- as.matrix(initSingFemaleNew[1, -(1:2)])

# covariates
beta0SingMale <- initSingMaleNew[-1, ]

betaSingMale  <- cbind(covariate = c("reference", 
                       as.character(beta0SingMale$covariate[-1])), 
                       beta0SingMale[, -(1:2)])
                  
betaSingMaleM <- melt(betaSingMale, id.vars = "covariate", 
                      variable.name = "status")
betaSingMaleM$value[which(is.na(betaSingMaleM$value))] <- 0

betaSingMaleM$covariate <- ordered(betaSingMaleM$covariate,
                                   levels = c("reference", "male1", "educ1", 
                                              "black1", "age1Centered"))
betaSingMaleM <- betaSingMaleM[with(betaSingMaleM, order(covariate, status)), ]

beta0SingFemale <- initSingFemaleNew[-1, ]

betaSingFemale <- cbind(covariate = c("reference", 
                                      as.character(beta0SingFemale$covariate[-1])),
                        beta0SingFemale[, -(1:2)])
                  
betaSingFemaleM <- melt(betaSingFemale, id.vars = "covariate", 
                        variable.name = "status")
betaSingFemaleM$value[which(is.na(betaSingFemaleM$value))] <- 0

betaSingFemaleM$covariate <- ordered(betaSingFemaleM$covariate,
                                     levels = c("reference", "male1", "educ1",
                                                "black1", "age1Centered"))
betaSingFemaleM <- betaSingFemaleM[with(betaSingFemaleM, 
                                        order(covariate, status)), ]

# get bootstrap results -------------------------------------------------------
files <- list.files("groupBootOut/")

nBoot <- 1000
logLik <- rep(NA, nBoot)
itemRespMaleBoot <- array(NA, dim = c(78, nBoot))
itemRespFemaleBoot <- array(NA, dim = c(78, nBoot))
tranProbMaleBoot <- array(NA, dim = c(144, nBoot))
tranProbFemaleBoot <- array(NA, dim = c(144, nBoot))
initProbMaleBoot <- array(NA, dim = c(6, nBoot))
initProbFemaleBoot <- array(NA, dim = c(6, nBoot))
initBetaMaleBoot <- array(NA, dim = c(24, nBoot))
initBetaFemaleBoot <- array(NA, dim = c(24, nBoot))

for (b in 1:nBoot) {
  print(b)
  bootFiles <- files[grep(paste("boot", b, "_", sep = ""), files)]
  fitFiles <- bootFiles[grep("fit", bootFiles)]

  # read in files with log likelihoods
  fitList <- list()
  for (i in 1:length(fitFiles)) {
    fitList[[i]] <- read.csv(file.path("groupBootOut", 
                             fitFiles[i]), skip = 1, header = FALSE)
  }
  fitMat <- do.call(rbind, fitList)

  # find model with largest log likelihood
  logLik[b] <- max(fitMat[, 1310])
  fitMax <- which.max(fitMat[, 1310])
  iterKeep <- strsplit(strsplit(fitFiles[fitMax], "iter")[[1]][2], "\\.")[[1]][1]
  
  # get corresponding file of parameters
  paramFiles <- bootFiles[grep("PARAM", bootFiles)]
  paramKeep <- grep(paste("iter", iterKeep, sep = ""), paramFiles)
  est <- read.csv(file.path("groupBootOut", paramFiles[paramKeep]), na.strings=".")
  
  # item response probabilities (irp) -----------------------------------------
  irp <- est[which(est$PARAM == "RHO" & est$TIME==1),]
  irp <- irp[order(irp$GROUP, irp$VARIABLE), c(2, 3, 6, 7, 8, 9, 10, 11, 12)]

  irp$variable <- factor(c(rep("BMI", 4), rep("Dead", 2), rep("Drinking", 4), 
                           rep("Current smoker", 3)),
                         levels = c("BMI", "Drinking", "Current smoker", "Dead"))

  irp$response <- factor(c("Normal", "Overweight", "Obese", "Dead (BMI)", 
                            "Dead", "Alive", 
                            "None", "Moderate", "Heavy", "Dead (drinking)",
                            "Yes","No", "Dead (smoking)"), 
                          levels = c("Normal", "Overweight", "Obese", "Dead (BMI)",
                                     "Alive", "Dead",
                                     "None", "Moderate", "Heavy", "Dead (drinking)",
                                     "No", "Yes", "Dead (smoking)"))

  irpM <- melt(irp[, -(2:3)], id = c("GROUP", "variable","response"),
               variable.name = "status")
  irpM$status <- as.numeric(gsub("ESTLS", "", irpM$status))
  
  irpMale <- irpM[which(irpM$GROUP == 1), ]
  irpFemale <- irpM[which(irpM$GROUP == 2), ]
  
  irpMale <- irpMale[with(irpMale, order(status, variable, response)), ]
  irpFemale <- irpFemale[with(irpFemale, order(status, variable, response)), ]
  
  Wm <- matrix(nrow = 6, ncol = 6)
  for (l in 1:6) {
    for (m in 1:6) {
     subMale <- irpMale[which(irpMale$status == l), ]
     subSingMale <- irpSingMale[which(irpSingMale$status == m), ]
     Wm[l, m] <- sqrt(sum((subSingMale$value - subMale$value)^2))
    }
  }
  
  Wf <- matrix(nrow = 6, ncol = 6)
  for (l in 1:6) {
    for (m in 1:6) {
     subFemale <- irpFemale[which(irpMale$status == l), ]
     subSingFemale <- irpSingFemale[which(irpSingFemale$status == m), ]
     Wf[l, m] <- sqrt(sum((subSingFemale$value - subFemale$value)^2))
    }
  }

  # brute force matching -- ok with only 6! = 720 permutations
  distM <- rep(NA, nrow(perms))
  for (u in 1:nrow(perms)) {
    distM[u] <- sum(Wm[cbind(1:6, perms[u, ])])
  }
  minDistM <- which(distM == min(distM))
  swapM <- perms[minDistM[1], ]
  
  distF <- rep(NA, nrow(perms))
  for (u in 1:nrow(perms)) {
    distF[u] <- sum(Wf[cbind(1:6, perms[u, ])])
  }
  minDistF <- which(distF == min(distF))
  swapF <- perms[minDistF[1], ]

  if(length(minDistM) > 1) {
    problemMale <- c(problemMale, b)
  }
  if(length(minDistF) > 1) {
    problemFemale <- c(problemFemale, b)
  }
   
  irpMale$status <- sapply(irpMale$status, function(x) {swapM[x]})
  irpFemale$status <- sapply(irpFemale$status, function(x) {swapF[x]})
  
  irpMale <- irpMale[with(irpMale, order(status, variable, response)), ]
  irpFemale <- irpFemale[with(irpFemale, order(status, variable, response)), ]
  
  itemRespMaleBoot[, b] <- irpMale$value
  itemRespFemaleBoot[, b] <- irpFemale$value
    
  # transition probabilities --------------------------------------------------
  tp <- est[which(est$PARAM == "TAU"), ]
  tpMale <- tp[which(tp$GROUP == 1), ]
  tpFemale <- tp[which(tp$GROUP == 2), ]
  
  # swap status lables
  tpMale$STATUS <- sapply(tpMale$STATUS, function(x) {swapM[x]}) 
  tpFemale$STATUS <- sapply(tpFemale$STATUS, function(x) {swapF[x]}) 
  
  tpMaleNew <- tpMale[, c("TIME", "STATUS")]
  statCols <- grep("ESTLS", colnames(tpMale))
  for (j in 1:length(statCols)) {
    col <- which(swapM == j)
    tpMaleNew <- cbind(tpMaleNew, tpMale[, statCols[col]])
  }
  colnames(tpMaleNew) <- c("wave", "status", 1:6)

  tpFemaleNew <- tpFemale[, c("TIME", "STATUS")]
  statCols <- grep("ESTLS", colnames(tpFemale))
  for (j in 1:length(statCols)) {
    col <- which(swapF == j)
    tpFemaleNew <- cbind(tpFemaleNew, tpFemale[, statCols[col]])
  }
  colnames(tpFemaleNew) <- c("wave", "status", 1:6)
  
  tpMaleNew <- tpMaleNew[with(tpMaleNew, order(wave, status)), ]
  tpMaleNewM <- melt(tpMaleNew, id.vars = c("wave", "status"),
                     variable.name = "to")
  colnames(tpMaleNewM) <- c("wave", "from", "to", "value")
  tpMaleNewM$from <- ordered(tpMaleNewM$from, levels = 6:1)
  tpMaleNewM$to <- ordered(tpMaleNewM$to, levels = 1:6)
  tpMaleNewM$facet <- paste("Wave", tpMaleNewM$wave, "to", tpMaleNewM$wave + 1)

  tpFemaleNew <- tpFemaleNew[with(tpFemaleNew, order(wave, status)), ]
  tpFemaleNewM <- melt(tpFemaleNew, id.vars = c("wave", "status"),
                       variable.name = "to")
  colnames(tpFemaleNewM) <- c("wave", "from", "to", "value")
  tpFemaleNewM$from <- ordered(tpFemaleNewM$from, levels = 6:1)
  tpFemaleNewM$to <- ordered(tpFemaleNewM$to, levels = 1:6)
  tpFemaleNewM$facet <- paste("Wave", tpFemaleNewM$wave, "to",
                              tpFemaleNewM$wave + 1)

  tranProbMaleBoot[, b] <- tpMaleNewM$value
  tranProbFemaleBoot[, b] <- tpFemaleNewM$value
  
  # initial probabilities -----------------------------------------------------
  
  init <- est[grep("DELTA", est$PARAM),]
  initMale <- init[which(init$GROUP == 1), ]
  initFemale <- init[which(init$GROUP == 2), ]
  
  # swap status labels
  initMaleNew <- initMale[, c("PARAM", "VARIABLE")]
  statCols <- grep("ESTLS", colnames(initMale))
  for (j in 1:length(statCols)) {
    col <- which(swapM == j)
    initMaleNew <- cbind(initMaleNew, initMale[, statCols[col]])
  }
  colnames(initMaleNew) <- c("param", "covariate", 1:6)
  
  initFemaleNew <- initFemale[, c("PARAM", "VARIABLE")]
  statCols <- grep("ESTLS", colnames(initFemale))
  for (j in 1:length(statCols)) {
    col <- which(swapF == j)
    initFemaleNew <- cbind(initFemaleNew, initFemale[, statCols[col]])
  }
  colnames(initFemaleNew) <- c("param", "covariate", 1:6)

  avgM <- as.matrix(initMaleNew[1, -(1:2)])
  avgF <- as.matrix(initFemaleNew[1, -(1:2)])
  
  initProbMaleBoot[, b] <- avgM
  initProbFemaleBoot[, b] <- avgF
  
  # covariates
  beta0Male <- initMaleNew[-1, ]
  
  betaMale  <- cbind(covariate = c("reference", as.character(beta0Male$covariate[-1])),
                     beta0Male[, -(1:2)])
                    
  betaMaleM <- melt(betaMale, id.vars = "covariate", variable.name = "status")
  betaMaleM$value[which(is.na(betaMaleM$value))] <- 0
  
  betaMaleM$covariate <- ordered(betaMaleM$covariate,
                                 levels = c("reference", "male1", "educ1",
                                            "black1", "age1Centered"))
  betaMaleM <- betaMaleM[with(betaMaleM, order(covariate, status)), ]
  
  initBetaMaleBoot[, b] <- betaMaleM$value
    
  beta0Female <- initFemaleNew[-1, ]
  
  betaFemale  <- cbind(covariate = c("reference", as.character(beta0Female$covariate[-1])), 
                       beta0Female[, -(1:2)])
                    
  betaFemaleM <- melt(betaFemale, id.vars = "covariate", variable.name = "status")
  betaFemaleM$value[which(is.na(betaFemaleM$value))] <- 0
  
  betaFemaleM$covariate <- ordered(betaFemaleM$covariate,
                                   levels = c("reference", "male1", "educ1",
                                              "black1", "age1Centered"))
  betaFemaleM <- betaFemaleM[with(betaFemaleM, order(covariate, status)), ]
  
  initBetaFemaleBoot[, b] <- betaFemaleM$value

}

problemMale
problemFemale

# save workplace image for `plot_results.R`
save.image(file = "post_process_final.RData")
