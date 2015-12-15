#'GBM function for assessing 2-way input interaction
#'
#'\code{gbm.int2} Calculates all 2-way interaction metrics for inputs with relative importances within a defined range
#'
#'@param gbm.model A gbm model to be plotted.
#'@param relimp.range A vector of length 2 giving upper and lower
#'  relative importance cut-offs
#'@param n.trees number of trees for interaction estimation
#'@param n.cores number of cores to utilise if NULL then no parallelisation
#'@param plot.it to plot or not to plot that is the question 

#'@return Matrix of interactions between inputs.
#'@examples
#'\dontrun{
#'gbm.int2(gbm.model=gbm1,
#'relimp.range=c(1,100),n.trees=100,n.cores=NULL)
#'}

#'@import gbm
#'@import foreach
#'@import doParallel
#'@export gbm.int2

gbm.int2 <- function(gbm.model,relimp.range,n.trees,n.cores=NULL){
  
  gbm.sum.all <- summary(gbm.model,plotit=F)
  
  #reduce gbm.sum based on relimp.range and re-order
  gbm.sum <- gbm.sum.all[((gbm.sum.all$rel.inf>relimp.range[1])&(gbm.sum.all$rel.inf<=relimp.range[2])),]
  
  name.summary <- as.vector(gbm.sum$var)
  
  ###################################################
  #create dataframe from gbm object
  x.mat <- matrix(gbm.model$data$x,length(gbm.model$data$y),length(gbm.model$data$x)/length(gbm.model$data$y))

  y.mat <- matrix(gbm.model$data$y,length(gbm.model$data$y),1)
  
  df.mat <- cbind(y.mat,x.mat)

  colnames(df.mat) <- c("y.mat",gbm.model$var.names)  
  df <- as.data.frame(df.mat)
  
  ##################################################
  
  vec.pos <- rep(NA,times=length(name.summary))
  
  for(i in 1:length(name.summary)){
    vec.pos[i] <- which(gbm.model$var.names==name.summary[i])
  }
  vec.pos.m <- expand.grid(vec.pos,vec.pos)
  
  #register parallel backend
  cl <- makeCluster(n.cores,type="FORK")
  registerDoParallel(cl)
  
  interact.v <- 
    foreach(i=1:dim(vec.pos.m)[1], .combine=c,.packages=c("gbm")) %dopar%
    (interact.gbm(gbm.model,data=df,
                  i.var=c(vec.pos.m[i,1],vec.pos.m[i,2]),n.trees=n.trees))

  
  stopCluster(cl)
  
  interact.m <- matrix(interact.v,nrow=length(vec.pos),ncol=length(vec.pos))
  
  rownames(interact.m) <- name.summary
  colnames(interact.m) <- name.summary
  
  return(interact.m)

}



