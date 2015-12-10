#'GBM plot of ordered relative influence and missing values.
#'
#'\code{gbm.relimp} plots relative importances and number of NAs from gbm.
#'
#'@param gbm.model A gbm model to be plotted.
#'@param relimp.range A vector of length 2 giving upper and lower
#' relative importance cut-offs
#'@return plot of relative influence and NAs of gbm model.
#'@examples
#'\dontrun{
#'gbm.relimp(gbm.model=gbm1,df=data1)
#'}

#'@import gbm
#'@import cowplot
#'@export gbm.relimp

#gbm NA and relative influence plots

gbm.relimp <- function(gbm.model,relimp.range){

gbm.sum.all <- summary(gbm.model,plotit=F)

#reduce gbm.sum based on relimp.range and re-order
gbm.sum <- gbm.sum.all[((gbm.sum.all$rel.inf>relimp.range[1])&(gbm.sum.all$rel.inf<=relimp.range[2])),]

name.summary <- as.vector(gbm.sum$var)

vec.pos <- rep(NA,times=length(name.summary))

for(i in 1:length(name.summary)){
  vec.pos[i] <- which(gbm.model$var.names==name.summary[i])
}


#Plot for Relative Importance
gbm.sum$var2 <- factor(gbm.sum$var,levels=rev(as.character(gbm.sum$var)))


#Figure 1
p1.relinf <- 
   ggplot(gbm.sum, aes(x=var2, y=rel.inf)) +
	xlab("Inputs")+ylab("Relative Influence")+
    geom_bar(stat="identity") + coord_flip()

#Find NAs in dataset

na.count <- function(x){
	x.na <- is.na(x)
	length(x.na[x.na==T])
}

#Create dataframe from gbm object##########################################
x.mat <- matrix(gbm.model$data$x,length(gbm.model$data$y),length(gbm.model$data$x)/length(gbm.model$data$y))
colnames(x.mat) <- gbm.model$var.names

x.mat.sub <- x.mat[,rownames(gbm.sum)]

df <- as.data.frame(x.mat.sub)

###########################################################################

na.list <- lapply(df,na.count)	

vars <- names(na.list)
na.num <- unlist(na.list)

na.num.df <- data.frame(vars,na.num,row.names=NULL)

total.cases <- length(df[,1])

na.num.df$per.na <- (na.num.df$na.num/total.cases)*100

na.num.df$vars2 <- factor(na.num.df$vars,levels=rev(as.character(gbm.sum$var)))

#Plot of percentage missing data
p1.perNA <-
ggplot(na.num.df, aes(x=vars2, y=per.na))+
	xlab("Inputs")+ylab("Percentage NAs")+
    geom_bar(stat="identity") + expand_limits(y=0)+coord_flip()

#Figure with relative importances and NAs
return(plot_grid(p1.relinf,p1.perNA,ncol=2))

}








