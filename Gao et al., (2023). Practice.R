# Practice the data in Gao et al. (2023). 10.1016/j.jhydrol.2023.130228

setwd('D:\\Practice Gao et al., (2023)')

# define function
Element.grp <- function(data){
  
  element.F <- NULL
  repeat{
    
    element <- data[1]
    
    # create a index from 1 to n
    n <- length(data);vector <- 1:n
    
    # get the index of the elements that are same as the first one
    index <- vector[data==element]
    
    # remove the same elements, and formed a new data list
    data <- data[-index]
    
    # record the elements
    element.F <- c(element.F,element)
    
    if (length(data)==0)break
  }
  return(element.F)
}

#1. Data input ####
data.src <- read.csv('env+spe.csv')
col.title <- c(rep('SITE',7),rep('HYDRO',8),rep('PHENO',8),rep('BIRD',5))
site <- data.src[,col.title=='SITE']

hydro <- data.src[,col.title=='HYDRO'];hydro.scale <- scale(hydro,center = TRUE,scale = TRUE)
pheno <- data.src[,col.title=='PHENO'];pheno.scale <- scale(pheno,center = TRUE,scale = TRUE)

bird <- data.src[,col.title=='BIRD']

data.used <- cbind(site,hydro.scale,pheno.scale,bird)

#2. Data description ####
Data.description.plot <- function(data){
  par(mfrow=c(3,3))
  plot(rowSums(data)~site$Year,main='Total')
  for (i in 1:ncol(data)){
    plot(data[,i]~site$Year,main=colnames(data)[i])
  }
  par(mfrow=c(1,1))
}
Data.description.plot(hydro)
Data.description.plot(pheno)
Data.description.plot(bird)

#3. GLMM ####

#3.1 Check for data distribution ####
Check.for.dispersion <- function(vector){
  mean = mean(vector)  # 例如：856.2
  var = var(vector)   # 例如：125000 
  ratio = var/mean
  if(ratio < 0.5 ) Judgement <- 'central tendency'; Recommended <- 'unknown'
  if (ratio <= 2 & ratio >0.5) Judgement <- 'not dispersion'; Recommended <- 'Poisson'
  if (ratio >2) Judgement <- 'Over dispersion'; Recommended <- 'Negaive binominal'
  return (c(ratio,Judgement, Recommended))
}

for (i in 1:ncol(bird)){
  print(colnames(bird)[i])
  print(Check.for.dispersion(bird[,i]))
  print(' ')
}
# if var >> mean, choose negative binominal, Over dispersion ==>  glmer
# if var ~= mean, choose Poisson, not over dispersion ==> glmer.nb
# if Gaussian distribution ==> lmer


#3.2 GLMM model ####

#Hydro.formula
paste0(colnames(hydro),collapse =' + ')
#Pheno.formula
paste0(colnames(pheno),collapse =' + ')

#3.2.1 VIF collinearity

title <- 'SedgeEaters'

library(car)
linear.model <- lm(SedgeEaters  ~  SB30 + WA30 
                    + VEOS
                   ,data=data.used)
colinearity <- vif(linear.model)
colinearity
max(colinearity)

colnames(bird)[1]

#3.2.2 Generalized linear mixed model (GLMM) using Template Model Builder (TMB)
library(lme4)
library(glmmTMB)

model <- glmer.nb(                        # glmer.nb() cannot be used in this case
  SeedEaters ~ SB30 + WA30
             + VEOS
             + (1 | Year),
  data = data.used,
)

# glmmTMB for more complex situations
model.1 <- glmmTMB(
  SedgeEaters  ~  SB30 + WA30 
                 + VEOS
                 + (1 | Year),
  data = data.used,
  family = nbinom2()
)

#3.2.3 model validation
library(DHARMa)

# residuals
simulation_output <- simulateResiduals(fittedModel = model.1)

## plot
# (1) QQ plot; (2) residual vs. Predicted deviations
plot(simulation_output)

# #only QQ plot
# plotQQunif(simulationOutput = simulation_output, 
#            testDispersion = FALSE,
#            testUniformity = FALSE,
#            testOutliers = FALSE)
# 
# # Only residual vs. Predicted deviations
# plotResiduals(simulation_output, rank = TRUE, quantreg = FALSE,smoothScatter = TRUE)

#3.2.4. Model selection
library(MuMIn)

# Global model
global_model <- glmmTMB(
                        SedgeEaters  ~  SB30 + WA30 
                        + VEOS
                        + (1 | Year),
                        data = data.used,
                        family = nbinom2()
                )

# dredge all possible submodels
submodels <- dredge(global_model)

# 查看模型选择结果
print(submodels)

# 选择最优模型
best_model <- get.models(submodels, subset = delta < 2)

# 模型平均
model_avg <- model.avg(submodels)

# 查看模型平均结果
summary(model_avg)

# 提取重要变量
importance <- importance(submodels)
print(importance)

final_model <- glmmTMB(
                        SedgeEaters  ~  SB30 + WA30 
                        + VEOS
                        + (1 | Year),
                        data = data.used,
                        family = nbinom2()
                      )
#3.2.5. Effect size
library(effectsize)
summary(final_model)
standardized_coef <- standardize_parameters(final_model)
print("standardized coefficient:")
print(standardized_coef)

#3.2.6. Hierarchical partition
library(glmm.hp)
variable_importance <- glmm.hp(final_model)
variable_importance
summary(variable_importance)

#4. HSI calculation ####

# do not have entire data, just using one SedgeEaters and its drivers
title
HSI <- predict(final_model,data.used)

data.plus.HSI <- cbind(data.used,HSI)

#4.1 using geodetector to check the 
library(geodetector)
interaction_detector('HSI', c('SB30','WA30','VEOS'), data.plus.HSI) # the interactive effect between variables

#4.2 Sen (2012) analysis

Drought.year <- rbind(data.plus.HSI[data.plus.HSI$Year=='2006',], data.plus.HSI[data.plus.HSI$Year=='2015',])
Nondrought.year <- data.plus.HSI[data.plus.HSI$Year!='2006' & data.plus.HSI$Year!='2015',]

Drought.data <- Drought.year[c('SiteID','HSI')]
Nondrought.data <- Nondrought.year[c('SiteID','HSI')]

Merge.labels <- function(metrix,HSI,SiteID){
  HSI.values <- metrix$HSI
  label <- Element.grp(metrix$SiteID)
  values<- NULL
  for(i in 1:length(label)){
    value <- median(HSI.values[label==label[i]])
    values <- c(values,value)
  }
  names(values) <- label
  return(values)
}

Drought.HSI <- Merge.labels(Drought.data,HSI,SiteID)
Nondrought.HSI <- Merge.labels(Nondrought.data,HSI,SiteID)

# sen test
HSI.delta <- HSI.nondrought - HSI.drought
ratio <- HSI.delta/HSI.nondrought

# if changes exceed 95% of the original value, then it is significant (Kundzewicz et al., 2005) 
ratio.2 <- ratio^2
significance <- (ratio.2 > 0.95^2)
rbind(HSI.delta,significance)

# data(maxau)
# sens.slope(maxau[,"s"])
# mk.test(maxau[,"s"])
# plot(maxau[,"s"])
