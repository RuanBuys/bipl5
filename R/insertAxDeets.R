#' Insert Axis Predictiveness graph
#'
#' @param p_ly Plotly graph
#' @param x Object of class biplot
#'
#' @return Updated plotly object
#'
#' @noRd
InsertAxisDeets<-function(p_ly,x){
  pred_deets<-axis_predictivities(x)
  ColNames<-c(colnames(x$x),"Weighted mean = Quality")
  p<-x$p
  linetipe<-"dashdot"
  lwidth<-2
  for(i in 1:nrow(pred_deets)){
    if(i == nrow(pred_deets)){
      linetipe<-"solid"
      lwidth<-3
    }
    p_ly<-p_ly |>
      add_trace(y=pred_deets[i,],x=1:p,type="scatter", mode="lines+markers",
                line=list(dash=linetipe,width=lwidth),
                xaxis="x",yaxis="y",hoverinfo="skip",showlegend=TRUE,
                name=ColNames[i],visible=FALSE,meta="axis_pred",
                legendgroup="AxPred",
                legendgrouptitle=list(text="<b> Axis Predictivity <b>"))
  }
  return(p_ly)
}
