library(ggplot2)
library(plyr)
library(corrplot)
library(RColorBrewer)



scatterplot <- function(data, xvar, yvar=yvar, color = cvar, shape = svar, size=zvar, 
                        jitter_width=0, jitter_height = 0,
                        regression = FALSE, method="auto", se=TRUE,fullrange=TRUE,
                        facet = FALSE, fvar = NULL) {
  if (color == "NULL"){color = NULL}
  if (shape == "NULL"){shape = NULL}
  if (size == "NULL"){size = NULL}
  
  basic <- ggplot(data, aes_string(x=xvar, y=yvar, color=color, shape = shape, size = size)) +  
    # Jitter the points: Jitter range is 0.1 on the x-axis, .5 on the y-axis
    geom_point(position = position_jitter(width=jitter_width,height=jitter_height))  + 
    labs(title=paste(xvar, "vs.", yvar), x =xvar, y =yvar)+ 
    theme(plot.title = element_text(color = "black", size = 15, face = "bold",hjust = 0.5))
  # Add a loess smoothed fit curve with confidence region (by default includes 95% confidence region)
  if (regression == TRUE){
    basic <- basic+geom_smooth(method = method, se=se,fullrange = fullrange) 
  }  
  
  if (facet == TRUE){
    basic <- basic + facet_grid(. ~ data[, fvar])  
  }
  basic
}


histogram <- function(data, xvar, group="NULL", binwidth=200, alpha=.4, position="identity"){
  if (group == "NULL"){
    group = NULL
  }
  basic <- ggplot(data, aes_string(x=xvar, fill = group)) +
    geom_histogram(binwidth=binwidth, alpha=alpha, position=position, colour="black")+ 
    labs(title=paste("Histogram of", xvar), x =xvar)+ 
    theme(plot.title = element_text(color = "black", size = 15, face = "bold",hjust = 0.5))
  
  if (!is.null(group)){
    # calculate the mean value for each group  
    foo <- function(data, facs, bar) {
      res <- ddply(data, facs, function(dfr, colnm){mean(dfr[,colnm])}, bar)
      res
    }
    mu <- foo(data, group, xvar)
    
    basic <- basic  + geom_vline(data=mu, aes(xintercept=V1, color=mu[,1]),linetype="dashed")+
      labs(color=group)
  } else {
    basic <- basic+
      geom_vline(aes(xintercept=mean(data[,xvar], na.rm=T)),   # Ignore NA values for mean
                 color="red", linetype="dashed", size=1)
  }
  basic
}


density <- function(data, xvar,group = "NULL", alpha=0.2){
  if (group == "NULL"){
    group = NULL
  }
  basic <- ggplot(data, aes_string(x=xvar,fill=group)) + 
    geom_density(alpha=alpha)+  # Overlay with transparent density plot 
    labs(title=paste("Density plot of", xvar), x =xvar)+ 
    theme(plot.title = element_text(color = "black", size = 15, face = "bold",hjust = 0.5))
  
  if (!is.null(group)){
    # calculate the mean value for each group  
    foo <- function(data, facs, bar) {
      res <- ddply(data, facs, function(dfr, colnm){mean(dfr[,colnm])}, bar)
      res
    }
    mu <- foo(data, group, xvar)
    
    basic <- basic  + geom_vline(data=mu, aes(xintercept=V1, color=mu[,1]),linetype="dashed")+
      labs(color=group)
  }   else {
    basic <- basic+
      geom_vline(aes(xintercept=mean(data[,xvar], na.rm=T)),   # Ignore NA values for mean
                 color="red", linetype="dashed", size=1)
  }
  basic
}


barplot <- function(data, xvar, yvar="NULL",fill="NULL",position = "dodge", 
                    width=0.7,alpha=0.2) {     # facet="FALSE", fvar="NULL",
  if (yvar == "NULL"){yvar = NULL} 
  if (fill == "NULL"){fill = NULL}
  # if (fvar == "NULL"){fcar = NULL}
  
  basic <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fill)) +   
    geom_bar(position=position, stat="identity", width = width, alpha=alpha)  + 
    labs(title=paste("Bar plot by", xvar), x =xvar)+ 
    theme(plot.title = element_text(color = "black", size = 15, face = "bold",hjust = 0.5))
  # ,coord_flip ="FALSE"
  # if (coord_flip=="TRUE"){
  #   basic <- basic + coord_flip()
  # } 
  # 
  # if (facet=="TRUE"){
  #   basic <- basic + facet_grid(. ~ data[, fvar])
  # }
  
  basic
}

# barplot_aggregation <- function(data, xvar, yvar, agg.function = mean,
#                                 position = "dodge",width=0.7,coord_flip =FALSE,alpha=0.5) {
#   #calculate mean/max/min/sum value of x for each group
#   aggregation <- function(data, facs, bar, agg.function) {
#     res <- ddply(data, facs, function(dfr, colnm){agg.function(dfr[,colnm])}, bar)
#     res
#   }
#   table <- aggregation(data, xvar, yvar, agg.function=agg.function)
#   basic <-  ggplot(table, aes(x=table[,1], y=table[,2],fill=table[,1])) +
#     geom_bar(position=position, stat="identity", width = width, alpha=alpha)  +
#     labs(title=paste( as.character(substitute(agg.function)), "of", yvar, "by",xvar ),
#          x=xvar, y=paste( as.character(substitute(agg.function)), "of", yvar), fill=xvar)+
#     theme(plot.title = element_text(color = "black", size = 15, face = "bold",hjust = 0.5))
# 
#   if (coord_flip==TRUE){
#     return(basic + coord_flip())
#   } else{
#     return(basic)
#   }
# }


boxplot <- function(data, xvar, yvar, fill="NULL",  alpha=0.5) {  # coord_flip ="FALSE",
  if (fill == "NULL"){fill = NULL}
  basic <- ggplot(data, aes_string(x=xvar, y=yvar, fill=fill)) +   
    geom_boxplot(alpha=alpha)  + 
    labs(title=paste("Box plot of", yvar, "by", xvar), x =xvar, y = yvar)+ 
    theme(plot.title = element_text(color = "black", size = 15, face = "bold",hjust = 0.5))
  
  # if (coord_flip=="TRUE"){
  #   basic <- basic + coord_flip()
  # }
  basic
}

correlation_plot <- function(data, sig.level = 0.05){
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  tp <- Filter(is.numeric, data)
  M<-cor(tp)
  
  
  # mat : is a matrix of data
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(tp)
  
  corrplot(M, method="color", col=col(200),  
           type="upper", order="hclust", 
           addCoef.col = "black", # Add coefficient of correlation
           tl.col="black", tl.srt=45, #Text label color and rotation
           # Combine with significance
           p.mat = p.mat, sig.level = sig.level, insig = "blank", 
           # hide correlation coefficient on the principal diagonal
           diag=FALSE
  )
}
# Source: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

cor_pvalue <- function(data, sig.level = 0.01){
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  tp <- Filter(is.numeric, data)
  M<-cor(tp)
  
  
  # mat : is a matrix of data
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(tp)
  return(p.mat) 

}