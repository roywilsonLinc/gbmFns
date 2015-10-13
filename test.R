
library(devtools)
load_all()

?gbm.effectplot

?use_build_ignore

library(gbm)
# A least squares regression example # create some data
N <- 1000
X1 <- runif(N)
X2 <- 2*runif(N)
X3 <- ordered(sample(letters[1:4],N,replace=TRUE),levels=letters[4:1])
X4 <- factor(sample(letters[1:6],N,replace=TRUE))
X5 <- factor(sample(letters[1:3],N,replace=TRUE))
X6 <- 3*runif(N)
mu <- c(-1,0,1,2)[as.numeric(X3)]
SNR <- 10 # signal-to-noise ratio
Y <- X1**1.5 + 2 * (X2**.5) + mu
sigma <- sqrt(var(Y)/SNR)
Y <- Y + rnorm(N,0,sigma)
# introduce some missing values
X1[sample(1:N,size=500)] <- NA
X4[sample(1:N,size=300)] <- NA
data <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)
# fit initial model
gbm1 <-
  gbm(Y~X1+X2+X3+X4+X5+X6, # formula
      data=data, # dataset
      var.monotone=c(0,0,0,0,0,0), # -1: monotone decrease,
      # +1: monotone increase,
      # 0: no monotone restrictions
      distribution="gaussian", # see the help for other choices
      n.trees=1000, # number of trees
      shrinkage=0.05, # shrinkage or learning rate,
      # 0.001 to 0.1 usually work
      interaction.depth=3, # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
      train.fraction = 0.5, # fraction of data for training,
      # first train.fraction*N used for training
      n.minobsinnode = 10, # minimum total weight needed in each node
      cv.folds = 3, # do 3-fold cross-validation
      keep.data=TRUE, # keep a copy of the dataset with the object
      verbose=FALSE, # don't print out progress
      n.cores=1)

library(ggplot2)
library(cowplot)
library(reshape2)


which(sapply(gbm1$var.levels,class)=="character")

?plot_grid

gbm.effectplot(gbm.model=gbm1,relimp.range=c(10,100),y.label="y",num.row=2,num.col=2)

gbm.plot.NA(gbm.model=gbm1,relimp.range=c(1,100))

?gbm.plot.NA

?gbm.effectplot


gbm.effectplot <- function(gbm.model,relimp.range,y.label,num.row,num.col){
  
  gbm.model=gbm1
  relimp.range=c(1,100)
  y.label="y"
  num.row=2
  num.col=2
  
  gbm.sum.all <- summary(gbm.model,plotit=F)
  
  #reduce gbm.sum based on relimp.range and re-order
  gbm.sum <- gbm.sum.all[((gbm.sum.all$rel.inf>relimp.range[1])&(gbm.sum.all$rel.inf<=relimp.range[2])),]
  
  name.summary <- as.vector(gbm.sum$var)
  
  vec.pos <- rep(NA,times=length(name.summary))
  
  for(i in 1:length(name.summary)){
    vec.pos[i] <- which(gbm.model$var.names==name.summary[i])
  }
  
  #create dataframe from gbm input############################################
  x.mat <- matrix(gbm.model$data$x,length(gbm.model$data$y),length(gbm.model$data$x)/length(gbm.model$data$y))
  colnames(x.mat) <- gbm.model$var.names
  
  df <- as.data.frame(x.mat)
  
  #Assign factor class to factor inputs
  factor.vec <- which(sapply(gbm1$var.levels,class)=="character")

  df[,factor.vec] <- lapply(df[,factor.vec],function(x){as.factor(x)})
  
  #character vector of variable classes
  df2 <- df[,name.summary,drop=FALSE]
  var.classes <- unlist(lapply(df2,class))
  
  
  #create marginal mappings
  plot.dat.ls <- list()
  
  for(i in 1:length(vec.pos)){
    plot.dat.ls[[i]] <-  	
      plot(gbm.model,i.var=vec.pos[i],return.grid=T)
  }
  
  #kernel density estimation and rescaling of density to fit plots
  #define numeric vs factor components of dataframe
  var.classes.num <- ifelse(var.classes=="factor",0,1)
  
  plot.kernel.ls <- list()
  
  for(i in 1:length(vec.pos)){
    
    if(var.classes[i]=="factor"){plot.kernel.ls[[i]] <- NA}else
      
    {
      #create kernel density estimate for binary classifier
      categorical.dist <- c("bernoulli","huberized","multinomial","adaboost")
      
      if(gbm.model$distribution%in%categorical.dist){
        
        df.kern.ls <- list()
        unique.y <- sort(unique(gbm.model$data$y))
        
        for(j in 1:length(unique.y)){
          df.kern.tmp <- data.frame(density(df2[which(gbm.model$data$y==unique.y[j]),names(plot.dat.ls[[i]])[1]],na.rm=T)[c("x","y")])
          label.m <- length(gbm.model$data$y[gbm.model$data$y==unique.y[j]])/length(gbm.model$data$y) 
          df.y.scale <- (max(plot.dat.ls[[i]][,2])-min(plot.dat.ls[[i]][,2]))/(max(df.kern.tmp$y)-min(df.kern.tmp$y))
          df.kern.tmp$y.scaled <- df.kern.tmp$y*df.y.scale*label.m + (min(plot.dat.ls[[i]][,2])-min(df.kern.tmp$y))
          df.kern.tmp$category <- unique.y[j]
          df.kern.ls[[j]] <- df.kern.tmp
        }
        df.kern <- do.call("rbind",df.kern.ls)
        plot.kernel.ls[[i]] <- df.kern
      }else
        
      {
        df.kern <- data.frame(density(df2[,names(plot.dat.ls[[i]])[1]],na.rm=T)[c("x","y")])
        df.y.scale <- (max(plot.dat.ls[[i]][,2])-min(plot.dat.ls[[i]][,2]))/(max(df.kern$y)-min(df.kern$y))
        df.kern$y.scaled <- df.kern$y*df.y.scale + (min(plot.dat.ls[[i]][,2])-min(df.kern$y))
        plot.kernel.ls[[i]] <- df.kern
      }
      
    }
    
  }
  
  plot.ls <- list()
  
  for(i in 1:length(vec.pos)){
    plot.ls[[i]] <-
      if(var.classes[i]=="factor")
      {
        ggplot(plot.dat.ls[[i]], aes_string(x=names(plot.dat.ls[[i]])[1], 
                                            y="y"))+geom_boxplot()+ylab(y.label)+coord_flip()
      }else
        
      {
        
        if(gbm.model$distribution%in%categorical.dist){
          
          plot.kernel.ls[[i]]$category <- as.factor(plot.kernel.ls[[i]]$category)
          
          ggplot()+
            geom_line(data=plot.kernel.ls[[i]],lty=2,
                      aes_string(x=names(plot.kernel.ls[[i]])[1], 
                                 y=names(plot.kernel.ls[[i]])[3],colour="category"))+
            geom_line(data=plot.dat.ls[[i]],aes_string(x=names(plot.dat.ls[[i]])[1], 
                                                       y="y"))+
            ylab(y.label)+xlab(names(plot.dat.ls[[i]])[1])+
            xlim(quantile(df2[,names(plot.dat.ls[[i]])[1]],probs=c(0.02,0.98),na.rm=T))
          
        }else{
          
          ggplot(plot.dat.ls[[i]], aes_string(x=names(plot.dat.ls[[i]])[1], 
                                              y="y"))+ylab(y.label)+geom_line()+
            xlim(quantile(df2[,names(plot.dat.ls[[i]])[1]],probs=c(0.02,0.98),na.rm=T))+
            geom_line(data=plot.kernel.ls[[i]],col=4,lty=2,aes_string(x=names(plot.kernel.ls[[i]])[1],y=names(plot.kernel.ls[[i]])[3]))
        }
      }
  }

  return(plot_grid(plotlist = plot.ls, nrow=num.row, ncol=num.col))
  
}



