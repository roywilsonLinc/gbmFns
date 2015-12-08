#'Predict survival functions and distributions for gbm cph models
#'
#'\code{gbm.pred.survfns} extracts survival model functions and densities and plots if required
#'
#'@param gbm.model A gbm model to extract survival functions and desities from.
#'@param max.time A numeric scalar giving the time point
#'	to which estimates should be extended
#'@param data.in A dataframe of inputs for prediction if NULL will use the orginal model dataframe
#'@param ntrees The number of trees for predictions
#'@param var.keep A character vector of variables from data.in to keep in the output data.table

#'@return A data.table with hazard function outputs for each row in the prediction data
#'@examples
#'\dontrun{
#'gbm.survfns(gbm.model=gbm.model,max.time=100,ntrees=100)
#'}

#'@import gbm
#'@import cowplot
#'@import data.table
#'@export gbm.pred.survfns
########################################

gbm.pred.survfns <- function(gbm.model,max.time,data.in=NULL,vars.keep,ntrees){
  
  pars.in <- as.list(match.call()[-1]) 
  
  t.eval.v <- 1:max.time
  
  #generate predictions of proportional hazards for the newdata
  
  if(is.null(data.in)){
    pred.new <- predict(gbm.model,n.trees=ntrees)
  }else{
    pred.new <- predict(gbm.model,newdata=data.in,n.trees=ntrees)
  }

  #use internal data from gbm object
  lambda.t <- basehaz.gbm(t = gbm.model$data$y,
                          delta = gbm.model$data$Misc,
                          f.x = gbm.model$fit,
                          smooth = TRUE,
                          t.eval = t.eval.v,
                          cumulative = FALSE)
  
  biglambda.t <- basehaz.gbm(t = gbm.model$data$y,
                             delta = gbm.model$data$Misc,
                             f.x = gbm.model$fit,
                             smooth = TRUE,
                             t.eval = t.eval.v,
                             cumulative = TRUE)
  
  #linear extrapolation of hazard function for missing biglambda.t range####################
  if(any(is.na(biglambda.t))){
    
    extrap.df <- data.frame(t.eval.v,biglambda.t)
    
    na.start <- extrap.df[(extrap.df[is.na(extrap.df$biglambda.t),][1,]$t.eval.v-20):(extrap.df[is.na(extrap.df$biglambda.t),][1,]$t.eval.v-1),]
    
    extrap.df[(extrap.df[is.na(extrap.df$biglambda.t),][1,]$t.eval.v):dim(extrap.df)[1],]$biglambda.t <- 
      predict(lm(biglambda.t~t.eval.v,data=na.start),newdata = extrap.df[(extrap.df[is.na(extrap.df$biglambda.t),][1,]$t.eval.v):dim(extrap.df)[1],])
    
    biglambda.t <- extrap.df$biglambda.t
  }
  
  #linear extrapolation of hazard function for missing lambda.t range####################
  if(any(is.na(lambda.t))){
    
    extrap.df <- data.frame(t.eval.v,lambda.t)
    
    na.start <- extrap.df[(extrap.df[is.na(extrap.df$lambda.t),][1,]$t.eval.v-20):(extrap.df[is.na(extrap.df$lambda.t),][1,]$t.eval.v-1),]
    
    extrap.df[(extrap.df[is.na(extrap.df$lambda.t),][1,]$t.eval.v):dim(extrap.df)[1],]$lambda.t <- 
      predict(lm(lambda.t~t.eval.v,data=na.start),newdata = extrap.df[(extrap.df[is.na(extrap.df$lambda.t),][1,]$t.eval.v):dim(extrap.df)[1],])
    
    lambda.t <- extrap.df$lambda.t
  }
  
  #define portion of data.in to keep
  data.in.keep <- data.in[,vars.keep]
  data.in.keep.expand <- data.in.keep[rep(seq_len(nrow(data.in.keep)),each=length(t.eval.v)),]
  
  #create data.table that holds a function for every case predicted
  t.eval.v.rep <- rep(t.eval.v,times=length(pred.new))
  lambda.t.rep <- rep(lambda.t,times=length(pred.new))
  biglambda.t.rep <- rep(biglambda.t,times=length(pred.new))
  preds.rep <- rep(preds.new,each=length(t.eval.v))
  
  haz.dt <- data.table(t.eval.v.rep,lambda.t.rep,biglambda.t.rep,preds.rep,data.in.keep.expand)
  
  setnames(haz.dt,c("time","lambda.t","biglambda.t","preds",keep.expand))
  
  haz.dt[,lambda.tx := lambda.t*exp(preds)]
  haz.dt[,s.t := (exp(-biglambda.t))^exp(preds)]
  haz.dt[,cdf := 1-s.t]
  haz.dt[,y.hat := log(1-cdf)]
  haz.dt[,log.min.y.hat := log(-y.hat)]
  haz.dt[,log.time := log(time)]
  haz.dt[,pdf := c(NA,diff(cdf))]
  
  #remove NAs
  na.omit(haz.dt)
  
  return(haz.dt)
  
}
