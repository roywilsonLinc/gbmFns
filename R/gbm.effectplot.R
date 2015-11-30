#'GBM plot of main effects with data distribution overlay.
#'
#'\code{gbm.effectplot} plots marginal effect for important variables.
#'
#'@param gbm.model A gbm model to be plotted.
#'@param relimp.range A vector of length 2 giving upper and lower
#'	relative importance cut-offs
#'@param y.label name of response variable  
#'@param num.row Number of rows in output plot
#'@param num.col Number of columns in output plot

#'@return plot of main effects of gbm model.
#'@examples
#'\dontrun{
#'gbm.effectplot(gbm.model=gbm.model,
#'relimp.range=c(1,100),num.row=4,num.col=4)
#'}

#'@import gbm
#'@import reshape2
#'@import cowplot
#'@export gbm.effectplot
########################################

gbm.effectplot <- function(gbm.model,relimp.range,y.label,num.row,num.col){
  
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
  factor.vec <- which(sapply(gbm.model$var.levels,class)=="character")
  
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



