# Function Defination -----
radarplot <- function(df=df){
  df1 <- df[, c("Cluster","Description","p.adjust")]
  
  
  library(tidyr)
  df1<-spread(df1, Description, p.adjust)
  
  df1[is.na(df1)] <-  1
  rownames(df1) <- df1[,1]
  df1 <- df1[,-1]
  df1 <- -log(df1)
  
  df1 <- t(df1)
  df1 <- as.data.frame(df1)
  df1 <- arrange(df1,df1$down,df1$up)
  
  df1 <- t(df1)
  df1 <- as.data.frame(df1)
  
  my.data <- matrix( c(rep(max(df1),ncol(df1)), 
                       rep(0, ncol(df1)), 
                       rep(-log(0.05), ncol(df1))), nrow = 3, ncol = ncol(df1), byrow=TRUE)
  
  
  colnames(my.data) <- colnames(df1)
  rownames(my.data) <- c("max", "min", "p")
  
  my.data <- rbind(my.data, df1)
  my.data <- my.data[c(1,2,4,5,3),]
  
  
  library(fmsb)
  radarchart(my.data, 
             pty = c(16,16,32),
             axistype = 1,
             pcol = c("#64299C", "#0439FD","black"), 
             pfcol = c(scales::alpha(c("#64299C", "#0439FD","black"), c(0.5, 0.5, 0.5))),
             plwd = c(3,3,3),
             plty = 1,
             cglcol = "grey60", 
             cglty = 1, 
             cglwd = 1,
             axislabcol = "grey60",
             vlcex = 0.7, 
             vlabels = colnames(colnames(my.data)),
             caxislabels = c(0, 10, 20, 30, 40),
             calcex=0.8
  )
  
  
  legend(x = "bottomright", legend = c("down","up"), horiz = F,
         bty = "n", pch = 15 , col = c("#64299C", "#0439FD"),
         text.col = "black", cex = 1, pt.cex = 1.5)
  
  
  legend(x = "center", legend = c("p<0.05"), horiz = TRUE,
         text.col = "white", cex = 1,bg=NULL,box.lty=0)
  
}

