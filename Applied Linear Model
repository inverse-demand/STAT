# Data used: http://archive.ics.uci.edu/ml/datasets/Communities+and+Crime
library(dplyr)
library(car)
library(readxl)
library(perturb)


communities <- read.csv("~/communities.csv.data", header=FALSE)

#### Add in column names: ####
colnames(communities) <- c("state",
                           "county",
                           "community",
                           "communityname",
                           "fold",
                           "population",
                           "householdsize",
                           "racepctblack",
                           "racePctWhite",
                           "racePctAsian",
                           "racePctHisp",
                           "agePct12t21",
                           "agePct12t29",
                           "agePct16t24",
                           "agePct65up",
                           "numbUrban",
                           "pctUrban",
                           "medIncome",
                           "pctWWage",
                           "pctWFarmSelf",
                           "pctWInvInc",
                           "pctWSocSec",
                           "pctWPubAsst",
                           "pctWRetire",
                           "medFamInc",
                           "perCapInc",
                           "whitePerCap",
                           "blackPerCap",
                           "indianPerCap",
                           "AsianPerCap",
                           "OtherPerCap",
                           "HispPerCap",
                           "NumUnderPov",
                           "PctPopUnderPov",
                           "PctLess9thGrade",
                           "PctNotHSGrad",
                           "PctBSorMore",
                           "PctUnemployed",
                           "PctEmploy",
                           "PctEmplManu",
                           "PctEmplProfServ",
                           "PctOccupManu",
                           "PctOccupMgmtProf",
                           "MalePctDivorce",
                           "MalePctNevMarr",
                           "FemalePctDiv",
                           "TotalPctDiv",
                           "PersPerFam",
                           "PctFam2Par",
                           "PctKids2Par",
                           "PctYoungKids2Par",
                           "PctTeen2Par",
                           "PctWorkMomYoungKids",
                           "PctWorkMom",
                           "NumIlleg",
                           "PctIlleg",
                           "NumImmig",
                           "PctImmigRecent",
                           "PctImmigRec5",
                           "PctImmigRec8",
                           "PctImmigRec10",
                           "PctRecentImmig",
                           "PctRecImmig5",
                           "PctRecImmig8",
                           "PctRecImmig10",
                           "PctSpeakEnglOnly",
                           "PctNotSpeakEnglWell",
                           "PctLargHouseFam",
                           "PctLargHouseOccup",
                           "PersPerOccupHous",
                           "PersPerOwnOccHous",
                           "PersPerRentOccHous",
                           "PctPersOwnOccup",
                           "PctPersDenseHous",
                           "PctHousLess3BR",
                           "MedNumBR",
                           "HousVacant",
                           "PctHousOccup",
                           "PctHousOwnOcc",
                           "PctVacantBoarded",
                           "PctVacMore6Mos",
                           "MedYrHousBuilt",
                           "PctHousNoPhone",
                           "PctWOFullPlumb",
                           "OwnOccLowQuart",
                           "OwnOccMedVal",
                           "OwnOccHiQuart",
                           "RentLowQ",
                           "RentMedian",
                           "RentHighQ",
                           "MedRent",
                           "MedRentPctHousInc",
                           "MedOwnCostPctInc",
                           "MedOwnCostPctIncNoMtg",
                           "NumInShelters",
                           "NumStreet",
                           "PctForeignBorn",
                           "PctBornSameState",
                           "PctSameHouse85",
                           "PctSameCity85",
                           "PctSameState85",
                           "LemasSwornFT",
                           "LemasSwFTPerPop",
                           "LemasSwFTFieldOps",
                           "LemasSwFTFieldPerPop",
                           "LemasTotalReq",
                           "LemasTotReqPerPop",
                           "PolicReqPerOffic",
                           "PolicPerPop",
                           "RacialMatchCommPol",
                           "PctPolicWhite",
                           "PctPolicBlack",
                           "PctPolicHisp",
                           "PctPolicAsian",
                           "PctPolicMinor",
                           "OfficAssgnDrugUnits",
                           "NumKindsDrugsSeiz",
                           "PolicAveOTWorked",
                           "LandArea",
                           "PopDens",
                           "PctUsePubTrans",
                           "PolicCars",
                           "PolicOperBudg",
                           "LemasPctPolicOnPatr",
                           "LemasGangUnitDeploy",
                           "LemasPctOfficDrugUn",
                           "PolicBudgPerPop",
                           "ViolentCrimesPerPop")

#### End Column name ####

communities[communities == '?'] <- NA

# 1. Describe data (summary stats, scatter plots, histograms, box plots, distribution plots, etc)
# 2. Perform statistical analysis
#    test for significance and overall significance of the model
#    make individual inferences about each parameter in the model
#    Obtain simultaneous confidence intervals and/or prediction intervals where appropriate
# 3. Check the validity of the model
#    Test for lack of fit
#    Obtain residual plots and test to verify assumptions
#    Check for multicollinearity, outliers, and influential observations
#    Take appropriate remedial measures

#### 1. ####

#### Keep ####
crimes <- select(communities,
                "ViolentCrimesPerPop",
                "population",
                "medIncome",
                "pctWInvInc",
                "pctWSocSec",
                "pctWPubAsst",
                "medFamInc",
                "perCapInc",
                "PctLess9thGrade",
                "PctNotHSGrad",
                "PctBSorMore",
                "PctPopUnderPov",
                "TotalPctDiv",
                "PersPerFam",
                "PctWorkMomYoungKids",
                "PctLargHouseFam",
                "MedRentPctHousInc",
                "NumInShelters")
#####

crimes_filter <- select(communities,
                        "ViolentCrimesPerPop",
                        "population",
                        "pctWPubAsst",
                        "PctNotHSGrad",
                        "PctPopUnderPov",
                        "TotalPctDiv",
                        "PersPerFam",
                        "MedRentPctHousInc",
                        "NumInShelters"
)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

apply(crimes_filter, 2, median)
apply(crimes_filter, 2, Mode)
apply(crimes_filter, 2, mean)
apply(crimes_filter, 2, sd)

#### Create anova alt calculation ####
anova_alt = function (object, reg_collapse=TRUE,...) 
{
  if (length(list(object, ...)) > 1L) 
    return(anova.lmlist(object, ...))
  if (!inherits(object, "lm")) 
    warning("calling anova.lm(<fake-lm-object>) ...")
  w <- object$weights
  ssr <- sum(if (is.null(w)) object$residuals^2 else w * object$residuals^2)
  mss <- sum(if (is.null(w)) object$fitted.values^2 else w * 
               object$fitted.values^2)
  if (ssr < 1e-10 * mss) 
    warning("ANOVA F-tests on an essentially perfect fit are unreliable")
  dfr <- df.residual(object)
  p <- object$rank
  if (p > 0L) {
    p1 <- 1L:p
    comp <- object$effects[p1]
    asgn <- object$assign[stats:::qr.lm(object)$pivot][p1]
    nmeffects <- c("(Intercept)", attr(object$terms, "term.labels"))
    tlabels <- nmeffects[1 + unique(asgn)]
    ss <- c(vapply(split(comp^2, asgn), sum, 1), ssr)
    df <- c(lengths(split(asgn, asgn)), dfr)
    if(reg_collapse){
      if(attr(object$terms, "intercept")){
        collapse_p<-2:(length(ss)-1)
        ss<-c(ss[1],sum(ss[collapse_p]),ss[length(ss)])
        df<-c(df[1],sum(df[collapse_p]),df[length(df)])
        tlabels<-c(tlabels[1],"Source")
      } else{
        collapse_p<-1:(length(ss)-1)
        ss<-c(sum(ss[collapse_p]),ss[length(ss)])
        df<-c(df[1],sum(df[collapse_p]),df[length(df)])
        tlabels<-c("Regression")
      }
    }
  }else {
    ss <- ssr
    df <- dfr
    tlabels <- character()
    if(reg_collapse){
      collapse_p<-1:(length(ss)-1)
      ss<-c(sum(ss[collapse_p]),ss[length(ss)])
      df<-c(df[1],sum(df[collapse_p]),df[length(df)])
    }
  }
  
  ms <- ss/df
  f <- ms/(ssr/dfr)
  P <- pf(f, df, dfr, lower.tail = FALSE)
  table <- data.frame(df, ss, ms, f, P)
  table <- rbind(table, 
                 colSums(table))
  if (attr(object$terms, "intercept")){
    table$ss[nrow(table)]<- table$ss[nrow(table)] - table$ss[1]
  }
  table$ms[nrow(table)]<-table$ss[nrow(table)]/table$df[nrow(table)]
  table[length(P):(length(P)+1), 4:5] <- NA
  dimnames(table) <- list(c(tlabels, "Error","Total"), 
                          c("Df","SS", "MS", "F", 
                            "P"))
  if (attr(object$terms, "intercept")){
    table <- table[-1, ]
    table$MS[nrow(table)]<-table$MS[nrow(table)]*(table$Df[nrow(table)])/(table$Df[nrow(table)]-1)
    table$Df[nrow(table)]<-table$Df[nrow(table)]-1
  }
  structure(table, heading = c("Analysis of Variance Table\n"), 
            class = c("anova", "data.frame"))
}

# Graph out data

pairs(ViolentCrimesPerPop ~ ., data = crimes_filter)

# if too many and want to check predictors against outcome variable...

graph = function(){
  for (i in 1:(length(crimes_filter)-1)){
    
    plot(crimes_filter[,i+1],
         crimes_filter$ViolentCrimesPerPop,
         xlab = colnames(crimes_filter[i+1]),
         ylab = "Violent Crimes Per 100k")
    
  }
}

graph()

# Check skew
hist(crimes_filter$ViolentCrimesPerPop, breaks = 100)
# Looks like violent crimes per pop is postively skewed
# Make sure to check residual distribution


#### End Step 1. ####

#### Start Step 2. ####
# 2. Perform statistical analysis
#    test for significance and overall significance of the model
#    make individual inferences about each parameter in the model
#    Obtain simultaneous confidence intervals and/or prediction intervals where appropriate

# Add confidence intervals in here

# Check confidence interval

crimes_filter <- select(communities,
                        "ViolentCrimesPerPop",
                        "population",
                        "pctWPubAsst",
                        "PctNotHSGrad",
                        "PctPopUnderPov",
                        "TotalPctDiv",
                        "PersPerFam",
                        "MedRentPctHousInc",
                        "NumInShelters"
                        )
crimes_filter.lm <- lm(ViolentCrimesPerPop ~ ., data = crimes_filter)
summary(crimes_filter.lm)
anova(crimes_filter.lm)
anova_alt(crimes_filter.lm)
pairs(ViolentCrimesPerPop ~ ., data = crimes_filter)
round(cor(crimes_filter),2)

# Check confidence interval
confint(crimes_filter.lm, level = .95)


#### End Step 2. ####

#### Start Step 3. ####
# 3. Check the validity of the model
#    Test for lack of fit
#    Obtain residual plots and test to verify assumptions
#    Check for multicollinearity, outliers, and influential observations
#    Take appropriate remedial measures

# Lack of fit
reduced <- lm(ViolentCrimesPerPop ~ ., data = crimes_filter)
full <- lm(ViolentCrimesPerPop ~ (population * pctWPubAsst) +
                                 PctNotHSGrad +
                                 PctPopUnderPov +
                                 TotalPctDiv +
                                 PersPerFam +
                                 MedRentPctHousInc +
                                 NumInShelters, data = crimes_filter)
# reduced <- crimes.lm
# full <- crimes_filter.lm

anova(reduced)
anova(reduced, full)

# Fail to reject that there is no lack of fit, and that the current model is adequate compared to the more complex model


# Residual plots and test to verify assumptions
res <- resid(crimes_filter.lm)
qqnorm(res, ylab = "residuals")
qqline(res)
plot(density(res))

resid_graph = function(){
  for (i in 1:(length(crimes_filter)-1)){
    
    plot(crimes_filter[,i+1], check.lm$residuals)
    
  }
}

resid_graph()

# Check if multicollinearity exists
check.lm <- lm(ViolentCrimesPerPop ~ .,data = crimes_filter)
summary(check.lm)

vif(crimes_filter.lm)
alias(crimes_filter.lm)
colldiag(crimes_filter.lm)

# Check model
summary(check.lm)
anova(check.lm)
