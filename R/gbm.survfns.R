#'GBM extract and plot survival functions and distributions
#'
#'\code{gbm.survfns} extracts survival model functions and densities and plots if required
#'
#'@param gbm.model A gbm model to extract survival functions and desities from.
#'@param t.eval.max A numeric scalar giving the time point
#'	to which estimates and plots should be extended
#'@param plot.it plot the functions. Default is FALSE 

#'@return plot of survival functions from gbm model
#'@examples
#'\dontrun{
#'gbm.survfns(gbm.model=gbm.model,t.evel.max=100,plot.it=TRUE)
#'}

#'@import gbm
#'@import cowplot
#'@export gbm.survfns
########################################

gbm.survfns <- function(gbm.model,t.eval.max,plot.it=FALSE){
  
  t.eval.v <- 1:t.eval.max
  
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
  
  
  haz.df <- data.frame(t.eval.v,lambda.t,biglambda.t)
  
  names(haz.df)[1] <- "time"
  
  #remove NAs
  haz.df <- haz.df[complete.cases(haz.df),]
  
  #hazard calcs
  haz.df$s.t <- exp(-haz.df$biglambda.t)
  haz.df$cdf <- 1-haz.df$s.t
  haz.df$y.hat <- log(1-haz.df$cdf)
  haz.df$log.min.y.hat <- log(-haz.df$y.hat)
  haz.df$log.t.eval.v <- log(haz.df$time)
  haz.df$pdf <- c(NA,diff(haz.df$cdf))
  
  #plots
  
  if(plot.it == TRUE){
  p1 <- ggplot(haz.df,aes(x=time,y=cdf))+geom_line()+
    ggtitle("Estimated CDF of base survival distribution")+
    scale_x_continuous("Time")+
    scale_y_continuous("distribution quantile")
  
  p2 <- ggplot(haz.df,aes(x=time,y=lambda.t))+geom_line()+
    ggtitle("Estimated base hazard function of survival distribution")+
    scale_x_continuous("Time")+
    scale_y_continuous("hazard")
  
  p3 <- ggplot(haz.df,aes(x=time,y=s.t))+geom_line()+
    ggtitle("Estimated base survival function for survival distribution")+
    scale_x_continuous("Time")+
    scale_y_continuous(expression(Pr (T>t)))
  
  p4 <- ggplot(haz.df,aes(x=time,y=pdf))+geom_line()+
    ggtitle("Estimated base pdf for survival distribution")+
    scale_x_continuous("Time")+
    scale_y_continuous(expression(Pr (t)))
  }

  return(haz.df)
  
}
