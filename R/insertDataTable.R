#' Insert table displaying data on plotly graph
#'
#' @param p_ly Plotly graph
#' @param x Biplot object
#'
#' @return Updated plotly graph
#' @noRd
InsertDataTable<-function(p_ly,x){
  p_ly <- p_ly|> add_trace(
    type = 'table',
    meta = 'tableDATA',
    domain=list(x=c(0.53,1),y=c(0,0.2)),
    columnwidth = c(10,rep(20,x$p)),
    columnorder = 1:(x$p+1),
    visible=TRUE,
    header = list(
      values = paste("<b>",c("Obs",colnames(x$x))),
      align = rep("center",x$p+1),
      line = list(width = 1, color = 'black'),
      fill = list(color = c("grey", "grey")),
      font = list(family = "Arial", size = 14, color = "white")
    ),
    cells = list(
      values = t(cbind(rownames(x$x),x$x)),
      align = rep("center",x$p+1),
      line = list(color = "black", width = 1),
      font = list(family = "Arial", size = 14, color = c("black")),
      height=24,
      fill=list(color=t(matrix(rep(c(rep("white",x$p+1),
                                     rep("#ededed",x$p+1)),x$n/2),ncol=x$p+1,
                               byrow=TRUE)))
    ))
  return(p_ly)
}



#' Insert fit measures to plotly graph
#'
#' @param p_ly plotly object
#' @param x object of class biplot
#'
#' @return updated plotly figure
#' @noRd
InsertFitMeasures<-function(p_ly,x){
  AdequacyMat<-matrix(NA,nrow=3,ncol=x$p)
  PredictMat<-matrix(NA,nrow=3,ncol=x$p)
  AdequacyMat[1,]<-diag(x$PCA$v[,1:2]%*%t(x$PCA$v[,1:2]))
  AdequacyMat[2,]<-diag(x$PCA$v[,c(1,3)]%*%t(x$PCA$v[,c(1,3)]))
  AdequacyMat[3,]<-diag(x$PCA$v[,c(2,3)]%*%t(x$PCA$v[,c(2,3)]))
  rownames(AdequacyMat)<-c("PC: 1+2: ","PC: 1+3: ","PC: 2+3: ")
  colnames(AdequacyMat)<-colnames(x$x)
  rownames(PredictMat)<-c("PC: 1+2: ","PC: 1+3: ","PC: 2+3: ")
  colnames(PredictMat)<-colnames(x$x)
  eigval <- x$PCA$d^2
  lambda.mat <- diag(eigval)

  J<-matrix(0,nrow=min(x$p,x$n),ncol=min(x$p,x$n))

  J[c(1,2),c(1,2)]<-diag(2)
  V<-x$PCA$v
  PredictMat[1,]<-diag(diag(diag(V%*%lambda.mat%*%J%*%t(V)))%*%solve(
    diag(diag(V%*%lambda.mat%*%t(V)))))
  J[1:3,1:3]<-diag(c(1,0,1))
  PredictMat[2,]<-diag(diag(diag(V%*%lambda.mat%*%J%*%t(V)))%*%solve(
    diag(diag(V%*%lambda.mat%*%t(V)))))
  J[1:3,1:3]<-diag(c(0,1,1))
  PredictMat[3,]<-diag(diag(diag(V%*%lambda.mat%*%J%*%t(V)))%*%solve(
    diag(diag(V%*%lambda.mat%*%t(V)))))

  AdequacyMat<-round(AdequacyMat,4)
  PredictMat<-round(PredictMat,4)

  for(i in 1:3){

    p_ly <- p_ly|> add_trace(
      type = 'table',
      meta = 'PredTable',
      domain=list(x=c(0.25,0.85),y=c(0,0.2)),
      columnwidth = c(20,rep(20,x$p)),
      columnorder = 1:(x$p+1),
      visible=TRUE,
      header = list(
        values = paste("<b>",c(c("PC: 1 & 2","PC: 1 & 3","PC: 2 & 3")[i],
                               colnames(x$x))),
        align = rep("center",x$p+1),
        line = list(width = 1, color = 'black'),
        fill = list(color = c("grey", "grey")),
        font = list(family = "Arial", size = 14, color = "white")
      ),
      cells = list(
        values =rbind(c("Adequacy","Predictivity"),
                      cbind(AdequacyMat[i,],PredictMat[i,])),
        align = rep("center",x$p+1),
        line = list(color = "black", width = 1),
        font = list(family = "Arial", size = 14, color = c("black")),
        height=24,
        fill=list(color=c("#ededed","white"))
      ))

  }
  return(list(p_ly,AdequacyMat,PredictMat))
}
