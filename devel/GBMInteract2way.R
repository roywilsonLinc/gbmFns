#'GBM function for assessing 2-way input interaction
#'
#'\code{GBMInt2} Calculates all 2-way interaction metrics for inputs with relative importances within a defined range
#'
#'@param gbm.model A gbm model to be plotted.
#'@param df A dataframe used to produce the gbm model.
#'@param relimp.range A vector of length 2 giving upper and lower
#'  relative importance cut-offs
#'@param n.trees number of trees for interaction estimation 

#'@return Matrix of interactions between inputs.
#'@examples
#'\dontrun{
#'GBMInt2(gbm.model=gbm1,
#'df=data1,relimp.range=c(1,100),n.trees=100)
#'}

#'@import gbm
#'@import foreach
#'@import reshape2
#'@export GBMInt2

GBMInt2 <- function(gbm.model,relimp.range,n.trees,plotit){
  
  gbm.sum.all <- summary(gbm.model,plotit=F)
  
  #reduce gbm.sum based on relimp.range and re-order
  gbm.sum <- gbm.sum.all[((gbm.sum.all$rel.inf>relimp.range[1])&(gbm.sum.all$rel.inf<=relimp.range[2])),]
  
  name.summary <- as.vector(gbm.sum$var)
  
  ###################################################
  #create dataframe from gbm object
  x.mat <- matrix(gbm1$data$x,length(gbm1$data$y),length(gbm1$data$x)/length(gbm1$data$y))

  y.mat <- matrix(gbm1$data$y,length(gbm1$data$y),1)
  
  df.mat <- cbind(y.mat,x.mat)

  colnames(df.mat) <- c("y.mat",gbm1$var.names)  
  df <- as.data.frame(df.mat)
  
  ##################################################
  
  vec.pos <- rep(NA,times=length(name.summary))
  
  for(i in 1:length(name.summary)){
    vec.pos[i] <- which(gbm.model$var.names==name.summary[i])
  }
  vec.pos.m <- expand.grid(vec.pos,vec.pos)
  
  interact.v <- 
    foreach(i=1:dim(vec.pos.m)[1], .combine=c,.packages=c("gbm")) %dopar%
    (interact.gbm(gbm.model,data=df,
                  i.var=c(vec.pos.m[i,1],vec.pos.m[i,2]),n.trees=n.trees))

  
  interact.m <- matrix(interact.v,nrow=length(vec.pos),ncol=length(vec.pos))
  
  rownames(interact.m) <- name.summary
  colnames(interact.m) <- name.summary
  
  return(interact.m)
  
  if(plotit==TRUE){
    
      p1 <- ggplot(interact.m, aes(Var1, Var2, fill = value)) + geom_tile()+
        scale_fill_gradient(low = "blue",  high = "red") +labs(x="",y="")
      
      return(p1)
    
  }


}



