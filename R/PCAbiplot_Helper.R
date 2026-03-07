
#' Get quadrant of vector loading
#'
#' @param V Matrix of vector loadings from SVD
#' @param m Vector of slopes
#'
#' @return vector of quadrants
#' @noRd
getquad<-function(V,m){
  p<-nrow(V)
  quadrants<-numeric()
  for(i in 1:p){
    if(V[i,1]>0) quadrants[i]<-c(1,4)[(m[i] < 0) + 1]
    else
      quadrants[i]<-c(2,3)[(m[i] < 0) + 1]
  }
  quadrants
}


#' Creates biplot with calibrated axes and vector loadings
#'
#' @param p_ly plotly object with only layout inserted already
#' @param x Object of class bipl5
#' @param symbol Plotting symbols per class group
#' @param color colors per class group
#' @param visible boolean: whether traces visible or not
#'
#' @return a list containing plotly graph, axes details, predicted values,
#'         number annotations, angles of axes
#' @noRd
add_vector_biplot<-function(p_ly,x,symbol,color,visible){

  Z<-x$Z
  p<-x$p
  n<-x$n
  mu<-x$mu
  stddev<-x$stddev
  group<-x$group
  m<-x$m
  quads<-x$quads
  Xhat<-Z%*%t(x$V) |> sweep(MARGIN = 2,STATS=stddev,FUN="*") |>
    sweep(MARGIN=2,STATS=mu,FUN="+")
  p_ly_pch<-symbol

  radius<-max(abs(Z))*1.2
  theta<-seq(0,2*pi,length.out=200)
  elipcoords<-cbind(radius*cos(theta),radius*sin(theta))
  endpoints<-tickmarks(ellip=elipcoords,gradient=m,p=p,
                       V=x$V, mu=mu,stddev=stddev)
  shift<-check_inside_circle(ticks=endpoints,r=radius,thetas=atan(m))

  #-------------------------------PLoTLY----------------------------
  #insert the Z coordinates
  if(is.null(color))
    Col<-colorpal(length(levels(group)))
  else
    Col<-color
  #insert the Z coordinates
  p_ly<-insert_Z_coo(p_ly,x,p_ly_pch,Col,visible)

  # Insert axes with the tick marks
  AnnotCounter<-numeric()
  angles<-list()
  for(i in 1:p){
    AnnotCounter[i]<-length(shift[[i]][,3])*2#peuter-------------------------
    index2<-which(shift[[i]][,3]== max(shift[[i]][,3],na.rm=TRUE))
    angle<-atan(shift[[i]][index2,2]/shift[[i]][index2,1])
    AxName<-paste("<b>",colnames(x$x)[i],"</b>")
    pos<-"right"
    quads<-getquad(x$V,x$m)

    if(quads[i]==3){
      angle<-angle-pi
      pos<-"left"
    }
    if(quads[i]==2){
      angle<-angle-pi
      pos<-"left"
    }

    angles[[i]]<-list(x=-10*sin(atan(x$m[i])),y=10*cos(atan(x$m[i])))
    p_ly<-p_ly |>
      add_trace(x=shift[[i]][,1],y=shift[[i]][,2],
                type="scatter", mode="markers",
                marker=list(color="grey",size=4),
                name=colnames(x$x)[i],legendgroup=paste("Ax",i,sep=""),
                meta='axis',xaxis="x",yaxis="y",customdata=i,
                hoverinfo='name',visible=F,showlegend=FALSE) |>

      add_trace(x=c(radius*cos(atan(m[i])),radius*cos(atan(m[i])-pi)),
                y=c(radius*sin(atan(m[i])),radius*sin(atan(m[i])-pi)),
                type="scatter",
                mode="lines",line = list(color = 'grey',width=1),
                name=colnames(x$x)[i],legendgroup=paste("Ax",i,sep=""),
                meta='axis',xaxis="x",yaxis="y",customdata=i,
                hoverinfo='name',visible=visible)|>

      add_annotations(x=shift[[i]][,1],y=shift[[i]][,2],
                      text=as.character(shift[[i]][,3]),
                      showarrow=FALSE,textangle=-atan(x$m[i])*180/pi,
                      visible=visible,yshift=-12*cos(atan(x$m[i])),
                      xshift=12*sin(atan(x$m[i])),meta='axis',
                      xaxis="x",yaxis="y",customdata=i,font=list(size=10) )|>
      #-----------------------------------------------------------PEUTER------------
      add_annotations(x=shift[[i]][,1],y=shift[[i]][,2],
                      text=" &#124;",
                      showarrow=FALSE,textangle=-atan(x$m[i])*180/pi,
                      visible=visible,meta='axis',
                      xaxis="x",yaxis="y",customdata=i,font=list(size=8) )|>
      #----------------------------------------------------------------------------
      add_trace(x=radius*cos(angle),y=radius*sin(angle),
                text=AxName,type="scatter",mode="text",textposition=pos,
                legendgroup=paste("Ax",i,sep=""),showlegend=FALSE,
                textfont=list(size=12),
                meta='axis',xaxis="x",yaxis="y",visible=visible)



  }
  p_ly<-p_ly|> add_trace(x=elipcoords[,1],y=elipcoords[,2], type="scatter",
                   mode="lines",line = list(color = 'green',width=0.6),
                   name="circle",showlegend=FALSE,
                   meta='circle',xaxis="x",yaxis="y",
                   visible=visible,hoverinfo="none")

  #---------Get equations of shifted axes for prediction lines------------
  slope<-numeric()
  intercept<-numeric()
  for(i in 1:p){
    deets<-equation(shift[[i]][1,-3],shift[[i]][2,-3])
    slope[i]<-deets[1]
    intercept[i]<-deets[2]
  }
  df<-data.frame(m=slope,c=intercept)



  return(list(p_ly,df,Xhat,counter=sum(AnnotCounter),angles))

}




#' Insert Z coordinates to plot
#'
#' @param p_ly Plotly object after scaffolding
#' @param x Biplot object containing Z, group
#' @param p_ly_pch plotting characters
#' @param Col Colors
#' @param visible Show trace or not
#' @noRd
insert_Z_coo<-function(p_ly,x,p_ly_pch,Col,visible=TRUE){
  num_groups<-length(levels(x$group))
  for(i in 1:num_groups){
    p_ly<-p_ly |>
      add_trace(data=x$Z,x=x$Z[x$group==levels(x$group)[i],1],
                y=x$Z[x$group==levels(x$group)[i],2],name=levels(x$group)[i],
                type = "scatter", mode = "markers",
               # hovertext=rownames(x$x)[x$group==levels(x$group)[i]],
                hovertext=hovertext_generator(x,i),
                hoverinfo="text+name",
                customdata=(1:x$n)[x$group==levels(x$group)[i]],
                meta="data",xaxis="x",yaxis="y",visible=visible,
                marker=list(symbol=p_ly_pch[i],color=Col[i],opacity=1),
                legendgroup="data",
                legendgrouptitle=list(text="<b>Data</b>"))
  }
  return(p_ly)
}

#' Format the hovertext of the observations in the biplot
#'
#' @param x An object from biplotEZ package, updated to include Xhat
#' @param i Current class group to be added
#' @param linebreak Character indicating the linebreak to be used.
#'
#' @return Character string containing formated table to be displayd on hover
#' @noRd
format_hover_values <- function(values, digits = 4) {
  values <- as.matrix(values)

  if (nrow(values) == 0) {
    return(matrix(
      character(0),
      nrow = 0,
      ncol = ncol(values),
      dimnames = dimnames(values)
    ))
  }

  out <- t(vapply(seq_len(nrow(values)), function(idx) {
    format(round(values[idx, ], digits = digits), trim = TRUE, scientific = FALSE)
  }, character(ncol(values))))
  dimnames(out) <- dimnames(values)
  out
}

center_pad_strings <- function(values, width) {
  values <- as.character(values)
  widths <- nchar(values, type = "width")
  pad <- pmax(width - widths, 0)
  left <- floor(pad / 2)
  right <- ceiling(pad / 2)
  paste0(strrep(" ", left), values, strrep(" ", right))
}

hovertext_generator<-function(x,i,linebreak="\n"){
  # Prefer the observation-level metric from fit.measures(); keep a legacy fallback.
  sample_pred <- x$sample.predictivity
  if (is.null(sample_pred) && !is.null(x$within.class.sample.predictivity)) {
    sample_pred <- x$within.class.sample.predictivity
  }

  if(is.null(x$XHat))
    return(rownames(x$x)[x$group==levels(x$group)[i]])
  obs<-paste0("Observation: ",rownames(x$x))
  idx <- (1:x$n)[x$group==levels(x$group)[i]]

  if (length(idx) == 0) {
    return(character(0))
  }

  var_names <- colnames(x$x)
  if (is.null(var_names)) {
    var_names <- rep("", ncol(x$x))
  }

  actual_vals <- format_hover_values(as.matrix(x$x[idx, , drop = FALSE]))
  pred_vals <- format_hover_values(as.matrix(x$XHat[idx, , drop = FALSE]))
  name_width <- max(nchar(var_names, type = "width")) + 1

  #iterate over all observations in the group
  longvector <- character(length(idx))
  for(pos in seq_along(idx)){
    j <- idx[pos]
    actual_row <- actual_vals[pos, ]
    pred_row <- pred_vals[pos, ]
    actual_width <- max(nchar(c("Actual", actual_row), type = "width")) + 2
    pred_width <- max(nchar(c("Pred", pred_row), type = "width")) + 2

    table_head <- c(
      paste0(
        "|",
        strrep(" ", name_width),
        "|",
        center_pad_strings("Actual", actual_width),
        "|",
        center_pad_strings("Pred", pred_width),
        "|"
      ),
      paste0(
        "|:",
        strrep("-", name_width - 1),
        "|:",
        strrep("-", actual_width - 2),
        ":|:",
        strrep("-", pred_width - 2),
        ":|"
      )
    )

    table_body <- paste0(
      "|",
      sprintf(paste0("%-", name_width, "s"), var_names),
      "|",
      center_pad_strings(actual_row, actual_width),
      "|",
      center_pad_strings(pred_row, pred_width),
      "|"
    )
    vec <- paste0(c(table_head, table_body), linebreak, collapse = "")
    vec<-paste0(obs[j],linebreak,linebreak,vec)
    # Append the observation's sample predictivity below the Actual/Pred table.
    if (!is.null(sample_pred) && length(sample_pred) >= j) {
      vec <- paste0(
        vec,
        linebreak,
        "Sample predictivity: ",
        formatC(as.numeric(sample_pred[j]), format = "f", digits = 4)
      )
    }
    longvector[pos] <- vec
  }
  return(longvector)
}

#' Check if tick mark is inside bounding circle
#'
#' @param ticks list of tick marks
#' @param r radius of the bounding circle
#' @param thetas the gradients of the axes
#'
#' @return list of tick marks which are inside bounding circle
#' @noRd
check_inside_circle<-function(ticks,r,thetas){
  n<-length(ticks)
  for(i in 1:n){
    inside<-ticks[[i]][,1]^2+ticks[[i]][,2]^2 <= r^2
    #bound1<-c(r*cos(thetas[i]),r*sin(thetas[i]),NA)
    #bound2<-c(r*cos(thetas[i]-pi),r*sin(thetas[i]-pi),NA)
    ticks[[i]]<-ticks[[i]][inside,]

  }
  return(ticks)
}



#' Insert the vector loadings as annotations
#'
#' @param p_ly Plotly graph
#' @param PC12 First two PC's: class bipl5
#' @param PC13 First third PC: class bipl5
#' @param PC23 Second thrid PC: class bipl5
#'
#' @return Updated plotly graph with vector loadings inserted
#' @noRd
insert_vector_annots<-function(p_ly,PC12,PC13,PC23){
  p_ly<- p_ly |>  add_annotations( ax = PC12$V[,1],
                    ay = PC12$V[,2],
                    xref = "x", yref = "y",
                    axref = "x", ayref = "y",
                    text = colnames(PC12$x),
                    showarrow = TRUE,
                    x = rep(0,PC12$p),
                    y = rep(0,PC12$p),
                    arrowside="start",
                    visible=FALSE,
                    meta='vecload'
                    )
    if(!is.null(PC13)){
      p_ly<- p_ly |> add_annotations( ax = PC13$V[,1],
                          ay = PC13$V[,2],
                          xref = "x", yref = "y",
                          axref = "x", ayref = "y",
                          text = colnames(PC12$x),
                          showarrow = TRUE,
                          x = rep(0,PC12$p),
                          y = rep(0,PC12$p),
                          arrowside="start",
                          visible=FALSE
                      )
    }
    if(!is.null(PC23)){
      p_ly<-p_ly |> add_annotations( ax = PC23$V[,1],
                          ay = PC23$V[,2],
                          xref = "x", yref = "y",
                          axref = "x", ayref = "y",
                          text = colnames(PC12$x),
                          showarrow = TRUE,
                          x = rep(0,PC12$p),
                          y = rep(0,PC12$p),
                          arrowside="start",
                          visible=FALSE
                      )
    }
  return(p_ly)
}

