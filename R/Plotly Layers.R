#' Insert various polygons to the plotly graph
#'
#' @param p_ly plotly graph
#' @param coors named list containing coordinates of polygons
#' @param aes aestetics
#' @param leg_group what legend group it should be. Used for legend group title
#'
#' @noRd
insert_polygon_EZ<-function(p_ly,coors,aes,leg_group="Alpha Bags"){

  leg_name<-names(coors)
  for(i in 1:length(coors)){

    if(leg_group != "Alpha Bags"){
      Elip<-cluster::ellipsoidhull(coors[[i]])
      coors[[i]]<-cluster::predict.ellipsoid(Elip,n.out=101)
    }

    p_ly<-p_ly |> add_trace(x=coors[[i]][,1],
                            y=coors[[i]][,2],
                            mode="lines",
                            type="scatter",
                            line=list(color=aes$col[i],width=aes$lwd[i]),
                            fill="toself",
                            fillcolor=grDevices::adjustcolor(aes$col[i],aes$opacity[i]),
                            legendrank=2000,
                            name=leg_name[i],
                            legendgroup=leg_group,
                            legendgrouptitle=list(text=paste("<b>",leg_group,"</b>",sep="")),
                            visible=TRUE,
                            meta="polygon"
                            )

  }

  return(p_ly)
}
