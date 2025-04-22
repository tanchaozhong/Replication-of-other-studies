# A practice to perform analysis in Chang et al.,(2025)

# Define functions
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
Reorder <- function(to.be.ordered.vector.matrix,type='matrix',against.vector.name=NULL,reference.vector){
  
  # if it is a matrix, but no name of the against vector
  if(type == 'matrix' && is.null(against.vector.name)) print('this is a matrix, where is the NAME of the against vector? what do you want me to do?')
  
  # Define the vector
  vector <- names(to.be.ordered.vector.matrix)  # using the name of this vector
  if(type == 'matrix') vector <- to.be.ordered.vector.matrix[[against.vector.name]] # using one column of the matrix
  
  # Re-order the vector
  
  ordered.matrix.vector <- NULL
  
  reference.vector <- Element.grp(reference.vector)
  
  for (name in reference.vector){
    
    # the matrix, and there is a match
    if(type == 'matrix' && sum(vector == name)>0) {
      new.matrix <- to.be.ordered.vector.matrix[vector == name,]
      ordered.matrix.vector <- rbind(ordered.matrix.vector,new.matrix)
      
    }
    
    # the vector, and there is a match
    if(type == 'vector' && sum(vector == name)>0) {
      new.vector <- to.be.ordered.vector.matrix[vector == name]
      ordered.matrix.vector <- c(ordered.matrix.vector,new.vector)
    }
  }
  return(ordered.matrix.vector)
  
}


#1. input data ####
setwd('D:\\Practice Chang et al., (2025) Water Research')
env <- read.csv('env.csv',row.names = 1)
ben <- read.csv('spe.csv')

env.title <- c(rep('Geography',3), rep('Climate',2), 'Human.footprint', rep('Hydro.morphology',6)
               ,rep('Land.use',6),rep('Water.quality',11),rep('Biotic.factors',4))

richness <- rowSums(ben)
#formula
paste0(colnames(env),collapse = ' + ')

#2. Linear regression ####

Check.for.dispersion <- function(vector){
  mean = mean(vector)  # 例如：856.2
  var = var(vector)   # 例如：125000 
  ratio = var/mean
  if(ratio < 0.5 ) Judgement <- 'central tendency'; Recommended <- 'unknown'
  if (ratio <= 2 & ratio >0.5) Judgement <- 'not dispersion'; Recommended <- 'Poisson'
  if (ratio >2) Judgement <- 'Over dispersion'; Recommended <- 'Negaive binominal'
  return (c(ratio,Judgement, Recommended))
}
Check.for.dispersion(richness)

linear.model <- lm(richness~
                     Lat + Lon + Elevation + 
                     Percipitation + Temp + 
                     Human.index + 
                     Current.velocity + Catchment.area + Perimeter + Longest.distance + Slope + Cumulative.detric.distance + 
                     Urban. + Agri. + Plant. + Water. + Forest. + Bare.land. + 
                     TN + TP + NH4 + NO3 + TOC + WT + EC + DO + pH + ORP + Chl.a + 
                     Archaea + Bacteria + Fungi + Protist
                   , data=env)

summary(linear.model)

## Cannot delete those parameters
# library(car)
# VIF <- vif(linear.model)
# max(VIF)
# VIF[max(VIF) == VIF]


#2.1 Model selection by stepAIC ####
# select the best model

# (1) when > 10 variables
library(MASS)
step_model <- stepAIC(linear.model, direction = "both") 

final.select <- step_model

# # (2) when 1-10 independent variables, time-consuming
# library(MuMIn)
# 
# options(na.action = 'na.fail')
# model.select <- dredge(linear.model)
# subset(model.select,delta<2)
# model.avg<-model.avg(dd, subset = delta < 2)#对这些优质模型进行平均，可以结合多个模型的优势，减少单个模型可能存在的偏差，提高预测的稳定性和准确性
# summary(model.avg)
# final.select <- get.models(fit_select, 1) # the first model, normally

# final model from the selection
summary(final.select)

#2.2 Plot estimates####
library(effectsize)
summary(final.select)
standardized_coef <- standardize_parameters(final.select)
print("standardized coefficient:")
print(standardized_coef)

# estimates +- se of the parameters

estimate.plot <- data.frame(Parameter = standardized_coef$Parameter,
                            Coef = standardized_coef$Std_Coefficient,
                            CI_low = standardized_coef$CI_low,
                            CI_high = standardized_coef$CI_high)

#2.2.1 order the parameter based on the order####
reference.matrix <- data.frame(env.group = env.title, env = colnames(env))

# order the parameter
ordered.estimate.plot <- NULL
group.f <-NULL
for (i in 1:nrow(reference.matrix)){
  name = reference.matrix$env # the parameter name from the templete
  
  judge <- estimate.plot$Parameter == name[i] # where there is a paramter fit the templete
  
  if(sum(judge)== 1) { # if there is 
    ordered.estimate.plot <- rbind(ordered.estimate.plot,estimate.plot[judge,])
    group <- reference.matrix$env.group[i]
    group.f <- c(group.f,group)
    
  }
}
ordered.estimate.plot <- cbind(group.f,ordered.estimate.plot)
  
# Color
{
"#98D9CC" # Geography
"#CCCCCC" # Climate
"#4F2683" # Human footprint
"#FDB813" # Hydro-morphology
"#3C3C3B" # Land use
"#00A2E8" # Water quality
"#EA526F" # Biotic factors
}
col.title <- c(rep("#98D9CC",3), rep("#CCCCCC",2),"#4F2683", rep("#FDB813",6)
               ,rep("#3C3C3B",6),rep("#00A2E8",11),rep("#EA526F",4))

env.title <- c(rep('Geography',3), rep('Climate',2), 'Human.footprint', rep('Hydro.morphology',6)
               ,rep('Land.use',6),rep('Water.quality',11),rep('Biotic.factors',4))


# create this one-on-one matrix to connect "env group","env", and "color"
color.metrix <- data.frame(col = col.title, env.group = env.title, env = colnames(env))

col.select <- function(parameter){
  juduge = color.metrix$env==parameter
  col = color.metrix$col[juduge]
  return(col)
}

#2.2.2 Plot ####
Estimate.plot<-function(){
  # Create a blank
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
       xlim = c(-4, 4), ylim = c(1, nrow(ordered.estimate.plot)), yaxt = "n")
  
  # Creat y axis
  axis(4, at = 1:nrow(ordered.estimate.plot)
       , labels = ordered.estimate.plot$Parameter
       , las = 1, cex.axis = 0.7)
  
  # Create x axis
  axis(1, at = -4:4,las = 1, cex.axis = 0.7)
  
  # plot Estimates +- SE
  for (i in 1:nrow(ordered.estimate.plot)) {
    points(ordered.estimate.plot$Coef[i], i, pch = 16
           , col = col.select(ordered.estimate.plot$Parameter[i])
           )
    segments(ordered.estimate.plot$CI_low[i], i, ordered.estimate.plot$CI_high[i], i
             ,col=col.select(ordered.estimate.plot$Parameter[i]),lty=1,lwd=3
             ) # the same line, from CI_low to CI_high
  }
  
  # 添加零值线
  abline(v = 0, lty = 2)
}
Estimate.plot()

# 添加标题
mtext("Parameter estimates", side = 1, line = 3, cex = 1.2)
title(main = "Genera richness")

#3. Variation partitioning analysis by glmm.hp ####


# split the matrix based on the group factor, and store into a list
ordered.estimate.plot
factor.list <- split(ordered.estimate.plot,ordered.estimate.plot$group.f)

# for each matrix in the list, takeout the name of the variables
list.f <- NULL
for(i in 1:length(factor.list)){
  list.variables  <- (factor.list[[i]])$Parameter
  name.title <- (factor.list[[i]])$group.f[1]
  
  list <- list(name.title, list.variables)
  list.f <- list(list.f,list)
  
}

# model parameters that are ready to copy!!
list.f

iv <- list(
  "Biotic.factors" = c("Archaea", "Bacteria", "Fungi", "Protist"),
  
  "Geography" = c("Lat", "Lon", "Elevation"),
  "Climate" = c("Percipitation"), 
  "Hydro.morphology" = c("Current.velocity", "Catchment.area", "Perimeter", "Longest.distance", "Slope", "Cumulative.detric.distance"),
  
  "Land.use" = c("Urban.", "Plant.", "Water.", "Forest.", "Bare.land."),
  
  "Water.quality" = c("TN", "TP", "NO3", "TOC", "WT", "DO", "pH", "ORP", "Chl.a")
)

# calculate the explained variance based on group of factors
final.select <- lm(richness ~ Lat + Lon + Elevation + 
                     Percipitation + 
                     Current.velocity + Catchment.area + Perimeter + Longest.distance + 
                     Slope + Cumulative.detric.distance + 
                     Urban. + Plant. + Water. + Bare.land. + Forest. +
                     TN + TP + NO3 + TOC + WT + DO + pH + ORP + Chl.a + 
                     Archaea + Bacteria + Fungi + Protist, 
                   data = cbind(richness,env)
                   )
#glmm.hp(final.select)

# variance explained by each group
library(glmm.hp)
variance <- glmm.hp(final.select,iv,type = 'adjR2')
variance

variance.partition <- variance$hierarchical.partitioning
variance.percentage <- variance.partition[,4]
variance.percentage

# what about the color - variable connection
color.metrix
col=c('#98D9CC','#CCCCCC','#FDB813','#3C3C3B','#00A2E8')

"#98D9CC" # Geography
"#CCCCCC" # Climate
"#4F2683" # Human footprint
"#FDB813" # Hydro-morphology
"#3C3C3B" # Land use
"#00A2E8" # Water quality
"#EA526F" # Biotic factors

col.group <- c('Geography'="#98D9CC",
               'Climate'="#CCCCCC",
               'Human.footprint'="#4F2683",
               'Hydro.morphology'="#FDB813",
               'Land.use'="#3C3C3B",
               'Water.quality'="#00A2E8",
               'Biotic.factors'="#EA526F")

# Order the variance.percentage and color
variance.percentage

ordered.variance.percentage <- Reorder(to.be.ordered.vector.matrix = variance.percentage,type = 'vector',reference.vector =  color.metrix$env.group)

ordered.col <- Reorder(col.group,type='vector',reference.vector = names(ordered.variance.percentage) )

# Plot estimate and pearson ####
Barplot.accumulate <- function(){
  barplot(cbind(ordered.variance.percentage,rep(1,6))
        ,col=ordered.col
        ,ylab = 'Relative importance of vairables%'
        )
  legend(
    "topright",
    legend = rev(names(ordered.variance.percentage)),
    fill = rev(ordered.col),
    bty = "n"
  )
}

par(mfrow=c(1,2))
Barplot.accumulate()
Estimate.plot()
par(mfrow=c(1,1))

#4. Random forest + Pearson ####
#4.1 data preparation
library(ranger)

#4.1 Full model
spe.env <- cbind(richness,env)

#Normalize the explanatory variables using min-max scaling?
var.scale <- (var-min)/(max-min)


fr.model <- ranger(richness ~ .,data=spe.env, importance = "impurity")
fr.model$predictions
fr.model$variable.importance
fr.model$r.squared

#4.2 Model cross validation with 10 replications
fr.model.cross.validation <- function(spe.env){
  rf.model.result.f <- NULL
  for (i in 1:10){
  
    # split the data into train 80% and test part 20%
    train_index <- sample(1:nrow(spe.env), 0.8 * nrow(spe.env))
    train_data <- spe.env[train_index, ]
    test_data <- spe.env[-train_index, ]
    
    # train data to generate the model
    fr.model.train <- ranger(richness ~ .,data=train_data,importance = "impurity")
  
    # use the generated model to obtain predictions 
    predictions <- predict(fr.model.train, data = test_data)$predictions
    
    # calculate the "Root mean square error" 均方根误差 RMSE
    rmse <- sqrt(mean((predictions - test_data$richness)^2))
    Var.importance <- fr.model.train$variable.importance
    R.square <- fr.model.train$r.squared
    RMSE <- rmse
    
    rf.model.result <- c(Var.importance,R.square,RMSE)
    rf.model.result.f <- rbind(rf.model.result.f,rf.model.result)
  }
  colnames(rf.model.result.f) <- c(names(Var.importance),'R.square', "Root.mean.square.error")
  rownames(rf.model.result.f) <- 1:nrow(rf.model.result.f)
  rf.model.result.f <- as.data.frame(rf.model.result.f)
  # choose the model results with the lowest RMSE
  RMSE <- rf.model.result.f['Root.mean.square.error']
  rf.model.f <- rf.model.result.f[RMSE == min(RMSE),]
  return(rf.model.f)
}

fr.model.f <- fr.model.cross.validation(spe.env)


#5. Spearman correlation ####

Pearson.test <- function(spe.env){
  correlation.f <- NULL
  for(i in 2:ncol(spe.env)){  # starts from 2, do not consider the richness - richness relationship
    correlation <- cor.test(spe.env$richness,spe.env[,i],method='pearson')
    correlation.f <- c(correlation.f,correlation$estimate)
  }
  names(correlation.f) <- colnames(env)
  return(correlation.f)
}
Pearson.test(spe.env)
cor.f <- Pearson.test(spe.env)

#6. split the spe.env into different matrix####
dentric.distance <- spe.env$Cumulative.detric.distance

percentile.75th <- quantile(dentric.distance,probs = 0.75)
percentile.50th <- quantile(dentric.distance,probs = 0.5)
percentile.25th <- quantile(dentric.distance,probs = 0.25)

#create a blank
network.size <- c()

# set a standard
network.size[dentric.distance>percentile.75th] <- 'Large'
network.size[dentric.distance<percentile.75th] <- 'Mid.large'
network.size[dentric.distance<percentile.50th] <- 'Mid.small'
network.size[dentric.distance<percentile.25th] <- 'Small'

network.size

# split the data into four parts, based on network size
spe.env.data.list <- split(spe.env,network.size)

#7. calculate the random forest and pearson relationship, based on the 4 splited data ####
cor.f <- NULL
fr.model.f <- NULL
for (data in spe.env.data.list){
  cor <- Pearson.test(data)
  fr.model <- fr.model.cross.validation(data)
  
  cor.f <- rbind(cor.f,cor)
  fr.model.f <- rbind(fr.model.f,fr.model)
}

rownames(cor.f) <- names(spe.env.data.list)
rownames(fr.model.f) <- names(spe.env.data.list)

# results
cor.f
fr.model.f

#8. Plot the rf results and add pearson correlation ####
#8.1 heatmap plot ####

# create a color band
color_map <- colorRampPalette(c("#0072B2", "white", "#EA526F"))

cor.f.re <- apply(cor.f, 2, rev) # turn the last row to the first row, turn the last col to the first col
cor_data <- t(cor.f.re)
#Plot the correlation

# # Heatmap
# heatmap(
#   t(cor.f.re),
#   Rowv = NA,  # do not cluster on row
#   Colv = NA,  # do not cluster on col
#   col = color_map(100),  
#   scale = "none",  # can be adjust based on row, col or none. default is "row"
#   margins = c(5, 5),  # 设置边距
#   main = "Heatmap of Data Values"  # 图标题
# )
# 
# #check the data for heatmap -- reverse twice
# cor.f
# cor.f.re2 <- t(apply(cor.f, 1, rev)) # apply(x,2,rev)
# t(apply(cor.f.re2, 2, rev))


#image
image(
  x = 1:ncol(cor_data),
  y = 1:nrow(cor_data),
  z = t(cor_data),
  col = color_map(100),
  axes = FALSE,
  xlab = "",
  ylab = "",
  main = "Combined Visualization",
  zlim = c(-1, 1) # 确保颜色范围正确
)

# create the x-axis
axis(1, at = 1:ncol(cor_data), labels = colnames(cor_data), 
     las = 2, cex.axis = 0.8, tick = FALSE)

# create the y-axis
axis(2, at = 1:nrow(cor_data), labels = rownames(cor_data), 
     las = 1, cex.axis = 0.7, tick = FALSE)

# Check the data
apply(cor_data,2,rev)

#8.2 Plot the importance####
# the last two variables : R square + RMSE
no.need <- c(ncol(fr.model.f)-1,ncol(fr.model.f))
Importance <- fr.model.f[,-no.need]

Importance.re <- apply(Importance,2,rev)
Importance.re.t <- t(Importance.re)
Importance.re.t

imp_data <- Importance.re.t

# Standardize the size of the circle
max_imp <- max(imp_data, na.rm = TRUE)
imp_scaled <- imp_data / max_imp * 3

# Add circles on the Heatmap
for (i in 1:nrow(imp_data)) {
  for (j in 1:ncol(imp_data)) {
    if (!is.na(imp_data[i, j])) {
      points(
        x = j,
        y = i,
        cex = imp_scaled[i, j],
        pch = 21,
        #bg = adjustcolor("black", alpha.f = 0.6),
        col = "black"
      )
    }
  }
}
# Check the data
apply(Importance.re.t,2,rev)

legend(
  "topright",
  legend = c(round(max_imp/4), round(max_imp/2), round(3*max_imp/4), round(max_imp)), # arrange based on the 1/4, 2/4, 3/4, and 4/4
  pch = 21,
  pt.cex = c(1, 2, 3, 4),
  title = "Variable Importance",
  cex = 0.7,
  bty = "n"
)

# # create a blank
# plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
#      xlim = c(1, 4), ylim = c(1, nrow(Importance.re.t))
# )
# 
# # Create y axis
# axis(4, at = 1:nrow(Importance.re.t)
#      , labels = rownames(Importance.re.t)
#      , las = 1, cex.axis = 0.7)
# 
# # Create x axis
# axis(1, at = 1:4,labels = colnames(Importance.re.t),las = 1, cex.axis = 1)
# 
# # Plot the importance
#   for (j in 1:nrow(Importance.re.t)) {
#     for (k in 1:ncol(Importance.re.t)) {
#       points(k, nrow(Importance.re.t) - j + 1, cex = Importance.re.t[j, k] / 100, pch = 21, bg = "white")
#     }
#   }

#9. Structure Equation Model ####
colnames(spe.env)
paste0(colnames(spe.env),collapse = ' + ')
env.title

library(piecewiseSEM) # official website: https://jslefche.github.io/piecewiseSEM/

# Geography = Lat + Lon + Elevation
# Climate = Percipitation + Temp
# Human.footprint = Human.footprint
# Hydro.morphology = Current.velocity + Catchment.area + Perimeter + Longest.distance + Slope + Cumulative.detric.distance
# Land.use = Urban. + Agri. + Plant. + Water. + Forest. + Bare.land.
# Water.quality = TN + TP + NH4 + NO3 + TOC + WT + EC + DO + pH + ORP + Chl.a
# Biotic.factors = Archaea + Bacteria + Fungi + Protist

# Using PCA to generate the main component
pca <- prcomp(spe.env[c("Lat" , "Lon" ,"Elevation" )], scale = TRUE);Geography <- pca$x[,1] # PC1 axis
pca <- prcomp(spe.env[c("Percipitation" , "Temp" )], scale = TRUE);Climate <- pca$x[,1] # PC1 axis
Human.footprint <- spe.env$Human.index
pca <- prcomp(spe.env[c("Current.velocity" ,"Catchment.area" ,"Perimeter","Longest.distance" ,"Slope" ,"Cumulative.detric.distance"  )], scale = TRUE);Hydro.morphology <- pca$x[,1] # PC1 axis
pca <- prcomp(spe.env[c("Urban."  ,"Agri."  ,"Plant.", "Water." , "Forest.","Bare.land." )], scale = TRUE);Land.use <- pca$x[,1] # PC1 axis
pca <- prcomp(spe.env[c("TN" ,"TP","NH4" , "NO3" , "TOC" ,"WT" , "EC", "DO","pH", "ORP", "Chl.a" )], scale = TRUE);Water.quality <- pca$x[,1] # PC1 axis
pca <- prcomp(spe.env[c("Archaea" ,"Bacteria","Fungi" ,"Protist" )], scale = TRUE);Biotic.factors <- pca$x[,1] # PC1 axis

# new data frame
sem.data <- cbind(richness, Geography, Climate, Human.footprint, Hydro.morphology, Land.use, Water.quality, Biotic.factors)
sem.data=as.data.frame(sem.data)
paste0(colnames(sem.data),collapse = ' + ')

piece.sem <- psem(
  lm(richness ~ Geography + Climate + Human.footprint + Hydro.morphology + Land.use + Water.quality + Biotic.factors,data=sem.data),
  lm(Climate ~ Geography,data=sem.data),
  data=sem.data
)

summary(piece.sem)
coefs(piece.sem)

plot(piece.sem)
AIC(piece.sem)
anova(piece.sem)
coefs(piece.sem)
LLchisq(piece.sem)
residuals(piece.sem)
rsquared(sem)
fisherC(piece.sem)

# # 间接效应 = a * b
# indirect_effect <- a * b
# print(indirect_effect)
# 
# # 使用自助法（Bootstrap）计算置信区间
# set.seed(123)
# boot_effects <- replicate(1000, {
#   # 重抽样
#   boot_dat <- dat[sample(nrow(dat), replace = TRUE), ]
#   
#   # 重新拟合模型
#   boot_model1 <- lm(read ~ Visual_Score, data = boot_dat)
#   boot_model2 <- lm(math ~ read, data = boot_dat)
#   
#   # 提取系数
#   a_boot <- coef(boot_model1)["Visual_Score"]
#   b_boot <- coef(boot_model2)["read"]
#   
#   a_boot * b_boot
# })
# 
# # 计算95%置信区间
# quantile(boot_effects, c(0.025, 0.975))


# sem <- psem(
#   # lm(richness ~ Lat + Lon + Elevation +    # full model
#   #      Percipitation + Temp + Human.index +
#   #      Current.velocity + Catchment.area +
#   #      Perimeter + Longest.distance + Slope +
#   #      Cumulative.detric.distance +
#   #      Urban. + Agri. + Plant. + Water. +
#   #      Forest. + Bare.land. +
#   #      TN + TP + NH4 + NO3 + TOC +
#   #      WT + EC + DO + pH + ORP + Chl.a +
#   #      Archaea + Bacteria + Fungi + Protist,
#   #    data=spe.env),
#   # lm(Percipitation + Temp ~ Lat + Lon + Elevation, data=spe.env),  # Geo --> Climate
#   # lm(TN + TP + NH4 + NO3 + TOC + WT + EC + DO + pH + ORP + Chl.a ~ Lat + Lon + Elevation, data=spe.env), # Geo --> Water.Q
#   data=spe.env
# )



# Traditional way of SEM

# library(lavaan)
# sem.model.overall <-'
#         Geography =~ Lat + Lon + Elevation
#         Climate =~ Percipitation + Temp
#         
#         Hydro.morphology =~Current.velocity + Catchment.area + Perimeter + Longest.distance + Slope + Cumulative.detric.distance
#         
#         Land.use =~  Urban. + Agri. + Plant. + Water. + Forest. + Bare.land.
#         Water.quality =~ TN + TP + NH4 + NO3 + TOC + WT + EC + DO + pH + ORP + Chl.a
#         Biotic.factors =~ Archaea + Bacteria + Fungi + Protist
#         
#         richness ~ Geography + Climate + Hydro.morphology + Land.use + Water.quality + Biotic.factors
#         
#     '
# fit.overall <- sem(sem.model.overall, data=spe.env)
# varTable(fit.overall)
# summary(fit.overall)
