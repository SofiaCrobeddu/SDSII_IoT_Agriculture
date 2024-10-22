library(nnet)
library(car)
library(caret)
library(R2jags)
library(corrplot)

data <- read.csv("C:\\Users\\sofyc\\OneDrive\\Desktop\\SDS II\\project\\Advanced_IoT_Dataset.csv\\Advanced_IoT_Dataset.csv")

#Exploratory data analysis
str(data) #30000x14
colnames(data) <- c("Random", 
                    "Average of chlorophyll in the plant (ACHP)",
                    "Plant height  rate (PHR)",
                    "Average wet weight of the growth vegetative (AWWGV)",
                    "Average leaf area of the plant (ALAP)",
                    "Average number of plant leaves (ANPL)",
                    "Average root diameter (ARD)",
                    "Average dry weight of the root (ADWR)",
                    "Percentage of dry matter for vegetative growth (PDMVG)",
                    "Average root length (ARL)",
                    "Average wet weight of the root (AWWR)",
                    "Average dry weight of vegetative plants (ADWV)",
                    "Percentage of dry matter for root growth (PDMRG)",
                    "Class")
#colnames(data) <- c("Random", "ACHP", "PHR", "AWWGV", "ALAP", "ANPL", "ARD",
                    #"ADWR", "PDMVG", "ARL", "AWWR", "ADWV", "PDMRG", "Class")
#Transforming the classes in numbers
data$Class.chr <- data$Class
data$Class <- as.factor(data$Class)

#Dividing in S=1 (smart greenhouse) and T=0 (traditional greenhouse) classes
data$Class.binary <- rep(NA, nrow(data))
data$Class.binary <- ifelse(data$Class.chr %in% c("SA", "SB", "SC"), 1, 0)

#Checking the NA
sum(is.na(data)) #0

#Removing the columns 'Random' and 'Class'
predictors <- data[, -which(names(data) == "Class")]
predictors <- predictors[, c(2:13)]

#Correlation matrix
cor_matrix <- cor(predictors, use = "complete.obs")
#Plot
corrplot(cor_matrix, method = "circle")

#Variances
apply(predictors, 2, var)

#Distributions of predictors
par(mfrow = c(3, 4))
for (i in 1:ncol(predictors)) {
  hist(predictors[, i], main = colnames(predictors)[i], 
       xlab = colnames(predictors)[i], col = "lightblue", border = "black")
}
par(mfrow = c(1, 1))
#Very different distributions.
#Distribution of Class (target variable)
plot(as.factor(data$Class), main= "Classes distributions", col="lightblue", 
     border = "black")

#Standardization of data
preProcValues <- preProcess(predictors, method = c("center", "scale"))
transformed_standardized_predictors <- predict(preProcValues, predictors)
par(mfrow = c(3, 4))
for (i in 1:ncol(transformed_standardized_predictors)) {
  hist(transformed_standardized_predictors[, i], 
       main = colnames(transformed_standardized_predictors)[i], 
       xlab = colnames(transformed_standardized_predictors)[i], 
       col = "lightblue", border = "black")
}
par(mfrow = c(1, 1))

#----------------------------------------------------------------------------
#Selection of a sample to apply the models
size <- 150 #Sample size for each class
idx <- createDataPartition(data$Class, p = size/5000, 
                           list = FALSE, times = 1) #index of stratification
#Stratified sample
data.sampled <- data[idx, ]

#Check of sizes
table(data.sampled$Class) #150 for each one --> OK
str(data.sampled) #900x16

#Standardization of data sampled
sampled_standardized_predictors <- predict(preProcValues, data.sampled[, c(2:13)])
par(mfrow = c(3, 4))
for (i in 1:ncol(sampled_standardized_predictors)) {
  hist(sampled_standardized_predictors[, i], 
       main = colnames(sampled_standardized_predictors)[i], 
       xlab = colnames(sampled_standardized_predictors)[i], 
       col = "lightblue", border = "black")
}
par(mfrow = c(1, 1))

#-----------------------------------------------------------------------------
#First Bayesian model: Multinomial logistic model with interactions
y1 <- as.numeric(data.sampled$Class)
K <- length(unique(y1)) #6, number of classes

#MCMC 
S <- 20000
burn_in <- 5000

data.prepared <- list(
  y=y1,
  ACHP = sampled_standardized_predictors$`Average of chlorophyll in the plant (ACHP)`,
  PHR = sampled_standardized_predictors$`Plant height  rate (PHR)`, 
  AWWGV = sampled_standardized_predictors$`Average wet weight of the growth vegetative (AWWGV)`, 
  ALAP = sampled_standardized_predictors$`Average leaf area of the plant (ALAP)`, 
  ANPL = sampled_standardized_predictors$`Average number of plant leaves (ANPL)`,
  ARD = sampled_standardized_predictors$`Average root diameter (ARD)`,
  ADWR = sampled_standardized_predictors$`Average dry weight of the root (ADWR)`, 
  PDMVG = sampled_standardized_predictors$`Percentage of dry matter for vegetative growth (PDMVG)`,
  ARL = sampled_standardized_predictors$`Average root length (ARL)`, 
  AWWR = sampled_standardized_predictors$`Average wet weight of the root (AWWR)`, 
  ADWV = sampled_standardized_predictors$`Average dry weight of vegetative plants (ADWV)`, 
  PDMRG = sampled_standardized_predictors$`Percentage of dry matter for root growth (PDMRG)`,
  N = nrow(sampled_standardized_predictors),
  K = K
)

params <- c('beta0', 'beta', 'gamma')

inits <- list(
  inits1=list('beta0' = rep(0, K), 'beta' = matrix(0, K, 9), 'gamma' = matrix(0, K, 3)),
  inits2=list('beta0' = rep(1, K), 'beta' = matrix(1, K, 9), 'gamma' = matrix(1, K, 3)),
  inits3=list('beta0' = rep(-0.5, K), 'beta' = matrix(-0.5, K, 9), 'gamma' = matrix(-0.5, K, 3))
)

jags.1 <- jags(data=data.prepared,
               inits=inits,
               parameters.to.save=params,
               model.file="C:\\Users\\sofyc\\OneDrive\\Desktop\\SDS II\\project\\mod_agr.txt",
               n.chains=3,
               n.iter=S,
               n.burnin=burn_in,
               n.thin=5)
               
plot(jags.1$BUGSoutput$sims.array[,1,1], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,1]))

plot(jags.1$BUGSoutput$sims.array[,1,2], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,2]))

plot(jags.1$BUGSoutput$sims.array[,1,3], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,3]))

plot(jags.1$BUGSoutput$sims.array[,1,4], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,4]))

plot(jags.1$BUGSoutput$sims.array[,1,5], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,5]))

plot(jags.1$BUGSoutput$sims.array[,1,6], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,6]))

plot(jags.1$BUGSoutput$sims.array[,1,7], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,7]))

plot(jags.1$BUGSoutput$sims.array[,1,8], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,8]))

plot(jags.1$BUGSoutput$sims.array[,1,9], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,9]))

plot(jags.1$BUGSoutput$sims.array[,1,10], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,10]))

plot(jags.1$BUGSoutput$sims.array[,1,11], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,11]))

plot(jags.1$BUGSoutput$sims.array[,1,12], type='l')
plot(acf(jags.1$BUGSoutput$sims.array[,1,12]))

#Summary plot time series coefficients
par(mfrow = c(3, 4))
for (i in 1:12) {
  plot(jags.1$BUGSoutput$sims.array[,1,i], type='l', 
       main = paste("Coefficient TS for ", i))
}
par(mfrow = c(1, 1))

#Summary plot acf
par(mfrow = c(3, 4))
for (i in 1:12) {
  acf(jags.1$BUGSoutput$sims.array[,1,i], main = paste("ACF for Variable ", i))
}
par(mfrow = c(1, 1))

#Point estimates
MCMC1.beta.0 <- mean(jags.1$BUGSoutput$sims.list$beta0) #intercept
MCMC1.beta.1 <- mean(jags.1$BUGSoutput$sims.array[,1,1])
MCMC1.beta.2 <- mean(jags.1$BUGSoutput$sims.array[,1,2])
MCMC1.beta.3 <- mean(jags.1$BUGSoutput$sims.array[,1,3])
MCMC1.beta.4 <- mean(jags.1$BUGSoutput$sims.array[,1,4])
MCMC1.beta.5 <- mean(jags.1$BUGSoutput$sims.array[,1,5])
MCMC1.beta.6 <- mean(jags.1$BUGSoutput$sims.array[,1,6])
MCMC1.beta.7 <- mean(jags.1$BUGSoutput$sims.array[,1,7])
MCMC1.beta.8 <- mean(jags.1$BUGSoutput$sims.array[,1,8])
MCMC1.beta.9 <- mean(jags.1$BUGSoutput$sims.array[,1,9])
MCMC1.gamma.1 <- mean(jags.1$BUGSoutput$sims.array[,1,10])
MCMC1.gamma.2 <- mean(jags.1$BUGSoutput$sims.array[,1,11])
MCMC1.gamma.3 <- mean(jags.1$BUGSoutput$sims.array[,1,12])

Bayesian.regression1 <- c(
  "beta0" = MCMC1.beta.0,
  "ACHP" = MCMC1.beta.1,
  "PHR" = MCMC1.beta.2,
  "AWWGV" = MCMC1.beta.3,
  "ALAP" = MCMC1.beta.4,
  "ANPL" = MCMC1.beta.5,
  "ARD" = MCMC1.beta.6,
  "PDMVG" = MCMC1.beta.7,
  "ARL" = MCMC1.beta.8,
  "PDMRG" = MCMC1.beta.9,
  "ARL * ADWR" = MCMC1.gamma.1,
  "ARD * AWWR" = MCMC1.gamma.2,
  "PDMVG * ADWV" = MCMC1.gamma.3
)
Bayesian.regression1

#Interval estimates 95%
int.jags1 <- matrix(NA, nrow=13, ncol=2)
rownames(int.jags1[1:10,]) <- paste0("beta", 0:9)
rownames(int.jags1[11:13,]) <- paste0("gamma", 1:3)
colnames(int.jags1) <- c("lower", "upper")
for(i in 1:13){
  int.jags1[i,] <- round(as.numeric(Bayesian.regression1[i]) + c(-1, 1)*qnorm(1-0.05/2)*sd(jags.1$BUGSoutput$sims.array[,1,i]), 3)
}
int.jags1

#-------------------------------------------------------------------------------
#Second Bayesian model: Multilevel model with Hierarchical Random Terms
#Putting S=1 and T=2 
data.sampled$Env <- ifelse(data.sampled$Class.chr %in% c("SA", "SB", "SC"), 1, 2)
#Putting 1 for variety A, 2 for B, 3 for C
data.sampled$Variety <- ifelse(data.sampled$Class.chr %in% c("SA", "TA"), 1,
                               ifelse(data.sampled$Class.chr %in% c("SB", "TB"), 2, 3))
#Second MCMC 
S <- 10000
burn_in <- 2000

data.prepared <- list(
  y=y1,
  ACHP = sampled_standardized_predictors$`Average of chlorophyll in the plant (ACHP)`,
  PHR = sampled_standardized_predictors$`Plant height  rate (PHR)`, 
  AWWGV = sampled_standardized_predictors$`Average wet weight of the growth vegetative (AWWGV)`, 
  ALAP = sampled_standardized_predictors$`Average leaf area of the plant (ALAP)`, 
  ANPL = sampled_standardized_predictors$`Average number of plant leaves (ANPL)`,
  ARD = sampled_standardized_predictors$`Average root diameter (ARD)`,
  PDMVG = sampled_standardized_predictors$`Percentage of dry matter for vegetative growth (PDMVG)`,
  ARL = sampled_standardized_predictors$`Average root length (ARL)`,
  PDMRG = sampled_standardized_predictors$`Percentage of dry matter for root growth (PDMRG)`,
  N = nrow(sampled_standardized_predictors),
  K = K,
  environment = as.numeric(data.sampled$Env),
  variety = as.numeric(data.sampled$Variety)
)

params <- c('beta', 'u_env', 'u_variety')

#inits <- list(
#  inits1=list('beta' = array(0, dim = c(9, K)), 'u_env' = array(0, dim = c(2, K)),
#              'u_variety' = array(0, dim = c(3, K)),'tau_env' = 1, 'tau_variety' = 1),
#  inits2=list('beta' = array(10, dim = c(9, K)), 'u_env' = array(0.5, dim = c(2, K)),
#              'u_variety' = array(0.5, dim = c(3, K)), 'tau_env' = 1, 'tau_variety' = 1),
#  inits3=list('beta' = array(-10, dim = c(9, K)), 'u_env' = array(-0.5, dim = c(2, K)),
#              'u_variety' = array(-0.5, dim = c(3, K)), 'tau_env' = 1, 'tau_variety' = 1)
#)

inits <- list(
  inits1=list('beta' = array(0, dim = c(9, K)), 'u_env' = array(0, dim = c(2, K)),
              'u_variety' = array(0, dim = c(3, K))),
  inits2=list('beta' = array(10, dim = c(9, K)), 'u_env' = array(0.5, dim = c(2, K)),
              'u_variety' = array(0.5, dim = c(3, K))),
  inits3=list('beta' = array(-10, dim = c(9, K)), 'u_env' = array(-0.5, dim = c(2, K)),
              'u_variety' = array(-0.5, dim = c(3, K)))
)

jags.2 <- jags(data=data.prepared,
               inits=inits,
               parameters.to.save=params,
               model.file="C:\\Users\\sofyc\\OneDrive\\Desktop\\SDS II\\project\\mod_agr_multilevel_random.txt",
               n.chains=3,
               n.iter=S,
               n.burnin=burn_in)

plot(jags.2$BUGSoutput$sims.array[,1,1], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,1]))

plot(jags.2$BUGSoutput$sims.array[,1,2], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,2]))

plot(jags.2$BUGSoutput$sims.array[,1,3], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,3]))

plot(jags.2$BUGSoutput$sims.array[,1,4], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,4]))

plot(jags.2$BUGSoutput$sims.array[,1,5], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,5]))

plot(jags.2$BUGSoutput$sims.array[,1,6], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,6]))

plot(jags.2$BUGSoutput$sims.array[,1,7], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,7]))

plot(jags.2$BUGSoutput$sims.array[,1,8], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,8]))

plot(jags.2$BUGSoutput$sims.array[,1,9], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,9]))

plot(jags.2$BUGSoutput$sims.array[,1,10], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,10]))

plot(jags.2$BUGSoutput$sims.array[,1,11], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,11]))

plot(jags.2$BUGSoutput$sims.array[,1,12], type='l')
plot(acf(jags.2$BUGSoutput$sims.array[,1,12]))

#Summary plot acf
par(mfrow = c(3, 4))
for (i in 1:12) {
  acf(jags.2$BUGSoutput$sims.array[,1,i], main = paste("ACF for Variable", i))
}
par(mfrow = c(1, 1))

#Point estimates
MCMC2.beta.0 <- mean(jags.2$BUGSoutput$sims.list$beta0) #intercept
MCMC2.beta.1 <- mean(jags.2$BUGSoutput$sims.array[,1,1])
MCMC2.beta.2 <- mean(jags.2$BUGSoutput$sims.array[,1,2])
MCMC2.beta.3 <- mean(jags.2$BUGSoutput$sims.array[,1,3])
MCMC2.beta.4 <- mean(jags.2$BUGSoutput$sims.array[,1,4])
MCMC2.beta.5 <- mean(jags.2$BUGSoutput$sims.array[,1,5])
MCMC2.beta.6 <- mean(jags.2$BUGSoutput$sims.array[,1,6])
MCMC2.beta.7 <- mean(jags.2$BUGSoutput$sims.array[,1,7])
MCMC2.beta.8 <- mean(jags.2$BUGSoutput$sims.array[,1,8])
MCMC2.beta.9 <- mean(jags.2$BUGSoutput$sims.array[,1,9])
MCMC2.beta.10 <- mean(jags.2$BUGSoutput$sims.array[,1,10])
MCMC2.beta.11 <- mean(jags.2$BUGSoutput$sims.array[,1,11])
MCMC2.beta.12 <- mean(jags.2$BUGSoutput$sims.array[,1,12])

Bayesian.regression2 <- c(
  "beta0" = MCMC2.beta.0,
  "ACHP" = MCMC2.beta.1,
  "PHR" = MCMC2.beta.2,
  "AWWGV" = MCMC2.beta.3,
  "ALAP" = MCMC2.beta.4,
  "ANPL" = MCMC2.beta.5,
  "ARD" = MCMC2.beta.6,
  "ADWR" = MCMC2.beta.7,
  "PDMVG" = MCMC2.beta.8,
  "ARL" = MCMC2.beta.9,
  "AWWR" = MCMC2.beta.10,
  "ADWV" = MCMC2.beta.11,
  "PDMRG" = MCMC2.beta.12
)
Bayesian.regression2

#Interval estimates 95%
int.jags2 <- matrix(NA, nrow=13, ncol=2)
rownames(int.jags2) <- paste0("beta", 0:12)
colnames(int.jags2) <- c("lower", "upper")
for(i in 1:13){
  int.jags2[i,] <- round(as.numeric(Bayesian.regression2[i]) + c(-1, 1)*qnorm(1-0.05/2)*sd(jags.2$BUGSoutput$sims.array[,1,i]), 3)
}
int.jags2

#-------------------------------------------------------------------------------
#Third Bayesian model: Hierarchical model
y1 <- as.numeric(data.sampled$Class)
K <- length(unique(y1))

#First MCMC 
S <- 10000
burn_in <- 2000

data.prepared <- list(
  y=y1,
  ACHP = sampled_standardized_predictors$`Average of chlorophyll in the plant (ACHP)`,
  PHR = sampled_standardized_predictors$`Plant height  rate (PHR)`, 
  AWWGV = sampled_standardized_predictors$`Average wet weight of the growth vegetative (AWWGV)`, 
  ALAP = sampled_standardized_predictors$`Average leaf area of the plant (ALAP)`, 
  ANPL = sampled_standardized_predictors$`Average number of plant leaves (ANPL)`,
  ARD = sampled_standardized_predictors$`Average root diameter (ARD)`,
  #ADWR = sampled_standardized_predictors$`Average dry weight of the root (ADWR)`, 
  PDMVG = sampled_standardized_predictors$`Percentage of dry matter for vegetative growth (PDMVG)`,
  ARL = sampled_standardized_predictors$`Average root length (ARL)`, 
  #AWWR = sampled_standardized_predictors$`Average wet weight of the root (AWWR)`, 
  #ADWV = sampled_standardized_predictors$`Average dry weight of vegetative plants (ADWV)`, 
  PDMRG = sampled_standardized_predictors$`Percentage of dry matter for root growth (PDMRG)`,
  N = nrow(sampled_standardized_predictors),
  K = K,
  environment = as.numeric(data.sampled$Env),
  variety = as.numeric(data.sampled$Variety)
)

params <- c('beta', 'u_env', 'u_variety')

#inits <- list(
#  inits1=list('beta' = array(0, dim = c(9, K)), 'u_env' = array(0, dim = c(2, K)),
#              'mu_beta' = array(0, dim = c(9, K)), 'tau_beta' = array(1, dim = c(9, K)),
#              'u_variety' = array(0, dim = c(3, K)),'tau_env' = 1, 'tau_variety' = 1),
#  inits2=list('beta' = array(10, dim = c(9, K)), 'u_env' = array(0.5, dim = c(2, K)),
#              'mu_beta' = array(1, dim = c(9, K)), 'tau_beta' = array(0.5, dim = c(9, K)),
#              'u_variety' = array(0.5, dim = c(3, K)), 'tau_env' = 1, 'tau_variety' = 1),
#  inits3=list('beta' = array(-10, dim = c(9, K)), 'u_env' = array(-0.5, dim = c(2, K)),
#              'mu_beta' = array(-1, dim = c(9, K)), 'tau_beta' = array(2, dim = c(9, K)),
#              'u_variety' = array(-0.5, dim = c(3, K)), 'tau_env' = 1, 'tau_variety' = 1)
#)

inits <- list(
  inits1=list('beta' = matrix(0, K, 9), 'u_env' = matrix(0, 2, K),
              'u_variety' = matrix(0, 3, K)),
  inits2=list('beta' = matrix(10, K, 9), 'u_env' = matrix(0.5, 2, K),
              'u_variety' = matrix(0.5, 3, K)),
  inits3=list('beta' = matrix(-10, K, 9), 'u_env' = matrix(-0.5, 2, K),
              'u_variety' = matrix(-0.5, 3, K))
)

jags.3 <- jags(data=data.prepared,
               inits=inits,
               parameters.to.save=params,
               model.file="C:\\Users\\sofyc\\OneDrive\\Desktop\\SDS II\\project\\mod_agr_hierarchical.txt",
               n.chains=3,
               n.iter=S,
               n.burnin=burn_in)

plot(jags.3$BUGSoutput$sims.array[,1,1], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,1]))

plot(jags.3$BUGSoutput$sims.array[,1,2], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,2]))

plot(jags.3$BUGSoutput$sims.array[,1,3], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,3]))

plot(jags.3$BUGSoutput$sims.array[,1,4], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,4]))

plot(jags.3$BUGSoutput$sims.array[,1,5], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,5]))

plot(jags.3$BUGSoutput$sims.array[,1,6], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,6]))

plot(jags.3$BUGSoutput$sims.array[,1,7], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,7]))

plot(jags.3$BUGSoutput$sims.array[,1,8], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,8]))

plot(jags.3$BUGSoutput$sims.array[,1,9], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,9]))

plot(jags.3$BUGSoutput$sims.array[,1,10], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,10]))

plot(jags.3$BUGSoutput$sims.array[,1,11], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,11]))

plot(jags.3$BUGSoutput$sims.array[,1,12], type='l')
plot(acf(jags.3$BUGSoutput$sims.array[,1,12]))

#Summary plot acf
par(mfrow = c(3, 4))
for (i in 1:12) {
  acf(jags.3$BUGSoutput$sims.array[,1,i], main = paste("ACF for Variable", i))
}
par(mfrow = c(1, 1))

#Point estimates
MCMC3.beta.0 <- mean(jags.3$BUGSoutput$sims.list$beta0) #intercept
MCMC3.beta.1 <- mean(jags.3$BUGSoutput$sims.array[,1,1])
MCMC3.beta.2 <- mean(jags.3$BUGSoutput$sims.array[,1,2])
MCMC3.beta.3 <- mean(jags.3$BUGSoutput$sims.array[,1,3])
MCMC3.beta.4 <- mean(jags.3$BUGSoutput$sims.array[,1,4])
MCMC3.beta.5 <- mean(jags.3$BUGSoutput$sims.array[,1,5])
MCMC3.beta.6 <- mean(jags.3$BUGSoutput$sims.array[,1,6])
MCMC3.beta.7 <- mean(jags.3$BUGSoutput$sims.array[,1,7])
MCMC3.beta.8 <- mean(jags.3$BUGSoutput$sims.array[,1,8])
MCMC3.beta.9 <- mean(jags.3$BUGSoutput$sims.array[,1,9])
MCMC3.beta.10 <- mean(jags.3$BUGSoutput$sims.array[,1,10])
MCMC3.beta.11 <- mean(jags.3$BUGSoutput$sims.array[,1,11])
MCMC3.beta.12 <- mean(jags.3$BUGSoutput$sims.array[,1,12])

Bayesian.regression3 <- c(
  "beta0" = MCMC3.beta.0,
  "ACHP" = MCMC3.beta.1,
  "PHR" = MCMC3.beta.2,
  "AWWGV" = MCMC3.beta.3,
  "ALAP" = MCMC3.beta.4,
  "ANPL" = MCMC3.beta.5,
  "ARD" = MCMC3.beta.6,
  "ADWR" = MCMC3.beta.7,
  "PDMVG" = MCMC3.beta.8,
  "ARL" = MCMC3.beta.9,
  "AWWR" = MCMC3.beta.10,
  "ADWV" = MCMC3.beta.11,
  "PDMRG" = MCMC3.beta.12
)
Bayesian.regression3

#Interval estimates 95%
int.jags3 <- matrix(NA, nrow=13, ncol=2)
rownames(int.jags3) <- paste0("beta", 0:12)
colnames(int.jags3) <- c("lower", "upper")
for(i in 1:13){
  int.jags3[i,] <- round(as.numeric(Bayesian.regression3[i]) + c(-1, 1)*qnorm(1-0.05/2)*sd(jags.3$BUGSoutput$sims.array[,1,i]), 3)
}
int.jags3

#-------------------------------------------------------------------------------
#Comparison
jags.1$BUGSoutput$DIC #2.091237
jags.2$BUGSoutput$DIC #3.736012
jags.3$BUGSoutput$DIC #1.674792
#The third model is the most adapted to our data, considering also the 
#penalization for the complexity.

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Simulation data from the third model:
set.seed(154)
#Estimates of beta0 for each Environment
beta0.est <- apply(jags.3$BUGSoutput$sims.list$beta0, 2, mean)
#Estimates of beta for (Environment, Variety, predictor)
beta.est <- apply(jags.3$BUGSoutput$sims.list$beta, c(2, 3, 4), mean)
#Estimates of sigma and mu of every beta coefficients
sigma.beta.est <- apply(jags.3$BUGSoutput$sims.list$sigma_beta, 2, mean)
mu.beta.est <- apply(jags.3$BUGSoutput$sims.list$mu_beta, 2, mean)

#Simulation
J <- ncol(X)
M <- nrow(X) #size of simulation
y.sim <- rep(NA, M)
for (i in 1:M) {
  s <- data.sampled$Env[i] #Environment (1 or 2)
  v <- data.sampled$Variety[i] #Variety (1, 2 or 3)
  
  #eta based on estimates of beta coefficients
  eta.sim <- beta0.est[s] + sum(X[i, ] * (mu.beta.est + rnorm(J, 0, sigma.beta.est)))
  
  #simulated probability
  p.sim <- exp(eta.sim)/(1+exp(eta.sim))
  
  #simulated y[i] with probability p.sim
  y.sim[i] <- rbinom(1, 1, p.sim)
}

#Application of the Hierarchical Bayesian model
S <- 10000
burn_in <- 2000

data.sim.prep <- list(
  y=y.sim,
  X=X,
  J = J,
  N = nrow(X),
  environment = as.numeric(data.sampled$Env),
  variety = as.numeric(data.sampled$Variety)
)

params <- c('beta0', 'beta', 'mu_beta', 'sigma_beta')
b0.0 <- rep(0, 2)
z.0 <- array(0, dim = c(2, 3, J))

inits <- list(inits1=list('beta0' = b0.0, 'z_beta' = z.0))

jags.sim <- jags(data=data.sim.prep,
               inits=inits,
               parameters.to.save=params,
               model.file="C:\\Users\\sofyc\\OneDrive\\Desktop\\SDS II\\project\\mod_agr_hierarchical.txt",
               n.chains=1,
               n.iter=S,
               n.burnin=burn_in,
               n.thin = 20)

plot(jags.sim$BUGSoutput$sims.array[,1,1], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,1]))

plot(jags.sim$BUGSoutput$sims.array[,1,2], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,2]))

plot(jags.sim$BUGSoutput$sims.array[,1,3], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,3]))

plot(jags.sim$BUGSoutput$sims.array[,1,4], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,4]))

plot(jags.sim$BUGSoutput$sims.array[,1,5], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,5]))

plot(jags.sim$BUGSoutput$sims.array[,1,6], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,6]))

plot(jags.sim$BUGSoutput$sims.array[,1,7], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,7]))

plot(jags.sim$BUGSoutput$sims.array[,1,8], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,8]))

plot(jags.sim$BUGSoutput$sims.array[,1,9], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,9]))

plot(jags.sim$BUGSoutput$sims.array[,1,10], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,10]))

plot(jags.sim$BUGSoutput$sims.array[,1,11], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,11]))

plot(jags.sim$BUGSoutput$sims.array[,1,12], type='l')
plot(acf(jags.sim$BUGSoutput$sims.array[,1,12]))

#Summary plot acf
par(mfrow = c(3, 4))
for (i in 1:12) {
  acf(jags.sim$BUGSoutput$sims.array[,1,i], main = paste("ACF for Variable", i))
}
par(mfrow = c(1, 1))

#DIC
jags.sim$BUGSoutput$DIC #963.8862

#Point estimates
MCMCsim.beta.0 <- mean(jags.sim$BUGSoutput$sims.list$beta0) #intercept
MCMCsim.beta.1 <- mean(jags.sim$BUGSoutput$sims.array[,1,1])
MCMCsim.beta.2 <- mean(jags.sim$BUGSoutput$sims.array[,1,2])
MCMCsim.beta.3 <- mean(jags.sim$BUGSoutput$sims.array[,1,3])
MCMCsim.beta.4 <- mean(jags.sim$BUGSoutput$sims.array[,1,4])
MCMCsim.beta.5 <- mean(jags.sim$BUGSoutput$sims.array[,1,5])
MCMCsim.beta.6 <- mean(jags.sim$BUGSoutput$sims.array[,1,6])
MCMCsim.beta.7 <- mean(jags.sim$BUGSoutput$sims.array[,1,7])
MCMCsim.beta.8 <- mean(jags.sim$BUGSoutput$sims.array[,1,8])
MCMCsim.beta.9 <- mean(jags.sim$BUGSoutput$sims.array[,1,9])
MCMCsim.beta.10 <- mean(jags.sim$BUGSoutput$sims.array[,1,10])
MCMCsim.beta.11 <- mean(jags.sim$BUGSoutput$sims.array[,1,11])
MCMCsim.beta.12 <- mean(jags.sim$BUGSoutput$sims.array[,1,12])

Bayesian.regression.sim <- c(
  "beta0" = MCMCsim.beta.0,
  "ACHP" = MCMCsim.beta.1,
  "PHR" = MCMCsim.beta.2,
  "AWWGV" = MCMCsim.beta.3,
  "ALAP" = MCMCsim.beta.4,
  "ANPL" = MCMCsim.beta.5,
  "ARD" = MCMCsim.beta.6,
  "ADWR" = MCMCsim.beta.7,
  "PDMVG" = MCMCsim.beta.8,
  "ARL" = MCMCsim.beta.9,
  "AWWR" = MCMCsim.beta.10,
  "ADWV" = MCMCsim.beta.11,
  "PDMRG" = MCMCsim.beta.12
)
Bayesian.regression.sim

#Interval estimates 95%
int.jags.sim <- matrix(NA, nrow=13, ncol=2)
rownames(int.jags.sim) <- paste0("beta", 0:12)
colnames(int.jags.sim) <- c("lower", "upper")
for(i in 1:13){
  int.jags.sim[i,] <- round(as.numeric(Bayesian.regression.sim[i]) + c(-1, 1)*qnorm(1-0.05/2)*sd(jags.sim$BUGSoutput$sims.array[,1,i]), 3)
}
int.jags.sim

#-------------------------------------------------------------------------------
#Frequentist models
#1) Multinomial case
mod.multinom <- multinom(data.sampled$Class ~ ., 
                         data = sampled_standardized_predictors)
summary(mod.multinom)

#2) Logistic with Ridge Penalty case VERIFICO
library(glmnet)
mod.ridge <- cv.glmnet(X.interaction, y2, alpha = 0, family = "binomial")
summary(mod.ridge)


#------------------------------------------------------------------------
#In teoria cose inutili
#Point estimates for each class
MCMC1.beta0 <- rep(NA, K)
for(k in 1:K){
  MCMC1.beta0[k] <- mean(jags.1$BUGSoutput$sims.list$beta0[,k])
  names(MCMC1.beta0) <- paste0("k=", 1:6)
}

MCMC1.beta <- matrix(NA, nrow = 9, ncol=K)
for(k in 1:K){
  for(p in 1:9){
    MCMC1.beta[p,k] <- mean(jags.1$BUGSoutput$sims.list$beta[,p,k])
    rownames(MCMC1.beta) <- paste0("beta", 1:9)
    colnames(MCMC1.beta) <- paste0("k=", 1:6)
  }
}

MCMC1.gamma <- matrix(NA, nrow = 3, ncol=K)
for(k in 1:K){
  for(p in 1:3){
    MCMC1.gamma[p,k] <- mean(jags.1$BUGSoutput$sims.list$gamma[,p,k])
    rownames(MCMC1.gamma) <- paste0("gamma", 1:3)
    colnames(MCMC1.gamma) <- paste0("k=", 1:6)
  }
}

#Point estimates
MCMC1.beta.0 <- mean(MCMC1.beta0)
MCMC1.beta.1 <- mean(MCMC1.beta[1,])
MCMC1.beta.2 <- mean(MCMC1.beta[2,])
MCMC1.beta.3 <- mean(MCMC1.beta[3,])
MCMC1.beta.4 <- mean(MCMC1.beta[4,])
MCMC1.beta.5 <- mean(MCMC1.beta[5,])
MCMC1.beta.6 <- mean(MCMC1.beta[6,])
MCMC1.beta.7 <- mean(MCMC1.beta[7,])
MCMC1.beta.8 <- mean(MCMC1.beta[8,])
MCMC1.beta.9 <- mean(MCMC1.beta[9,])
MCMC1.gamma.1 <- mean(MCMC1.gamma[1,])
MCMC1.gamma.2 <- mean(MCMC1.gamma[2,])
MCMC1.gamma.3 <- mean(MCMC1.gamma[3,])

Bayesian.regression1 <- c(
  "beta0" = MCMC1.beta.0,
  "beta.ACHP" = MCMC1.beta.1,
  "beta.PHR" = MCMC1.beta.2,
  "beta.AWWGV" = MCMC1.beta.3,
  "beta.ALAP" = MCMC1.beta.4,
  "beta.ANPL" = MCMC1.beta.5,
  "beta.ARD" = MCMC1.beta.6,
  "beta.PDMVG" = MCMC1.beta.7,
  "beta.ARL" = MCMC1.beta.8,
  "beta.PDMRG" = MCMC1.beta.9,
  "gamma.ARL*ADWR" = MCMC1.gamma.1,
  "gamma.ARD*AWWR" = MCMC1.gamma.2,
  "gamma.PDMVG*ADWV" = MCMC1.gamma.3
)

#Calculating the standard deviation of coefficients for each class
MCMC1.beta0.sd <- sd(MCMC1.beta0)
MCMC1.beta.sd <- apply(MCMC1.beta, 1, sd)
MCMC1.gamma.sd <- apply(MCMC1.gamma, 1, sd)

#sd for each coefficient
Bayesian.regression1.df <- as.data.frame(Bayesian.regression1)
Bayesian.regression1.df$sd <- NA
Bayesian.regression1.df$sd[1] <- MCMC1.beta0.sd
Bayesian.regression1.df$sd[2:10] <- MCMC1.beta.sd
Bayesian.regression1.df$sd[11:13] <- MCMC1.gamma.sd

#Interval estimates 95%
int.jags1 <- matrix(NA, nrow=13, ncol=2)
rownames(int.jags1) <- c(paste("beta0"), 
                         paste0("beta", 1:9), 
                         paste0("gamma", 1:3))
colnames(int.jags1) <- c("lower", "upper")
for(i in 1:13){
  int.jags1[i,] <- round(as.numeric(Bayesian.regression1[i]) + c(-1, 1)*qnorm(1-0.05/2)*Bayesian.regression1.df$sd[i], 3)
}

#Completing the dataframe to show the results
Bayesian.regression1.df$lower <- int.jags1[,1]
Bayesian.regression1.df$upper <- int.jags1[,2]
colnames(Bayesian.regression1.df) <- c("Estimate", "Std.Dev.", "95% CI Lower", "95% CI Upper")

#Table
kable(Bayesian.regression1.df, caption = "First Bayesian model analysis and inference", align = "c")