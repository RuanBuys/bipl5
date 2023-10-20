#' Add plotly scatter traces to current plotly object
#'
#' @inheritParams TDA
#' @importFrom cluster ellipsoidhull predict.ellipsoid
#' @return list with plotly object and data needed
#' @noRd
addPlotlyBiplot<-function(p_ly,x,visible,dist,inflate=1,alpha=0.95,
                          alpha_Elip=NULL,swop=FALSE,density.args=NULL,
                          color=NULL,symbol="circle"){
  Z<-x$Z
  p<-x$p
  n<-x$n
  mu<-x$mu
  stddev<-x$stddev
  group<-x$group
  m<-x$m
  quads<-x$quads
  Xhat<-Z%*%t(x$V) |> sweep(MARGIN = 2,STATS=stddev,FUN="*") |> sweep(MARGIN=2,STATS=mu,FUN="+")
  p_ly_pch<-symbol

      #start of by drawing an ellipse over all the data... used to determine how far axes shifted
      bigElip<-cluster::ellipsoidhull(Z)
      bigElipcoords<-cluster::predict.ellipsoid(bigElip,n.out=101)

      #next draw possible smaller ellipse.... used to get length of the axes
      if(!is.null(alpha_Elip)){
        bag<-do.call(alpha_Elip,list(alpha=alpha,x=Z))
        elip<-cluster::ellipsoidhull(bag)
        elipcoords<-cluster::predict.ellipsoid(elip,n.out=101)
      }
      else
        elipcoords<-bigElipcoords
      endpoints<-tickmarks(ellip=elipcoords,gradient=m,p=p,V=x$V, mu=mu,stddev=stddev)
      shift<-MoveLines(elip=bigElipcoords,m=m,quadrant=quads,d=dist,initial_ends=endpoints,swop=swop,cols=colnames(x$x))
      DensCoors<-MoveDensities(Z=Z,m=m,endpoints=shift$ends,dist=shift$ShiftDist,dinflation=inflate,group=group,densityargs=density.args)

  #---------------Get equations of shifted axes for prediction lines---------------
      slope<-numeric()
      intercept<-numeric()
      for(i in 1:p){
        deets<-equation(shift$ends[[i]][1,-3],shift$ends[[i]][2,-3])
        slope[i]<-deets[1]
        intercept[i]<-deets[2]
      }
    df<-data.frame(m=slope,c=intercept)

  #------------PLOTLY-------------------------------------

  if(is.null(color))
    Col<-colorpal(length(levels(group)))
  for(i in 1:length(levels(x$group))){
    p_ly<-p_ly |>
      add_trace(data=Z,x=Z[x$group==levels(x$group)[i],1],y=Z[x$group==levels(x$group)[i],2],name=levels(x$group)[i],
              type = "scatter", mode = "markers",hovertext=rownames(x$x)[x$group==levels(x$group)[i]],hoverinfo="text+name",
              customdata=(1:n)[x$group==levels(x$group)[i]], meta="data",xaxis="x",yaxis="y",visible=visible,
              marker=list(symbol=p_ly_pch[i],color=Col[i]))
  }
  # Insert axes with the tick marks
  AnnotCounter<-numeric()
  angles<-list()
  for(i in 1:p){
    AnnotCounter[i]<-length(shift$ends[[i]][,3])
    index<-which(shift$ends[[i]][,1]== max(shift$ends[[i]][,1]))
    index2<-which(shift$ends[[i]][,3]== max(shift$ends[[i]][,3]))
    if(index==index2){
      AxName<-paste("  ",colnames(x$x)[i])
      pos<-"right"
    }
    else{
      index<-index2
      AxName<-paste(colnames(x$x)[i],"  ")
      pos<-"left"
    }
    angles[[i]]<-list(x=-10*sin(atan(x$m[i])),y=10*cos(atan(x$m[i])))
    p_ly<-p_ly |> add_trace(x=shift$ends[[i]][,1],y=shift$ends[[i]][,2], text=as.character(shift$ends[[i]][,3]),
                            type="scatter", mode="lines+markers",line = list(color = 'grey',width=2.2),
                            marker=list(color="grey"),name=colnames(x$x)[i], textposition='top',
                            legendgroup=paste("Ax",i,sep=""),meta='axis',xaxis="x",yaxis="y",customdata=i,
                            hoverinfo='name',visible=visible) |>
      add_annotations(x=shift$ends[[i]][,1],y=shift$ends[[i]][,2], text=as.character(shift$ends[[i]][,3]),
                      showarrow=FALSE,textangle=-atan(x$m[i])*180/pi,visible=visible,yshift=-10*cos(atan(x$m[i])),
                      xshift=10*sin(atan(x$m[i])),meta='axis',xaxis="x",yaxis="y",customdata=i )|>

      add_trace(x=shift$ends[[i]][index,1],y=shift$ends[[i]][index,2],text=AxName,type="scatter",mode="text",textposition=pos,
                legendgroup=paste("Ax",i,sep=""),showlegend=FALSE,textfont=list(size=14),
                meta='axis',xaxis="x",yaxis="y",visible=visible)



  }
  #insert the densities
  for(i in 1:length(levels(group))){
    Dens<-DensCoors[[i]]
    for(j in 1:p){
      showleg<-FALSE #show legend... only true for first iteration
      if(j==1) showleg<-TRUE
      index_color<-which(levels(group)==unique(group)[i])
      p_ly<-p_ly|> add_trace(x=Dens[,2*j-1],y=Dens[,2*j],mode="lines",type="scatter",
                             line=list(dash="dot",color=Col[index_color],width=1.5),legendgroup=unique(group)[i],
                             showlegend=showleg, name=unique(group)[i], meta='density', xaxis="x",
                             yaxis="y",hoverinfo="skip",customdata=paste("Ax",j,sep=""),visible=visible)
    }
  }


  return(list(p_ly,df,Xhat,counter=sum(AnnotCounter),Dshift=shift$ShiftDist,angles))

}



#' Validate plotting symbols
#'
#' @param x vector of plotting symbols
#' @noRd
#' @return Noting if symbols are valid
validate_symbol<-function(x){
  symbol_list<-c( "0" , "0" , "circle" , "100" , "100" , "circle-open" , "200" , "200" , "circle-dot" , "300" ,
                  "300" , "circle-open-dot" , "1" , "1" , "square" , "101" , "101" , "square-open" , "201" ,
                  "201" , "square-dot" , "301" , "301" , "square-open-dot" , "2" , "2" , "diamond" , "102" ,
                  "102" , "diamond-open" , "202" , "202" , "diamond-dot" , "302" , "302" , "diamond-open-dot" ,
                  "3" , "3" , "cross" , "103" , "103" , "cross-open" , "203" , "203" , "cross-dot" , "303" , "303" ,
                  "cross-open-dot" , "4" , "4" , "x" , "104" , "104" , "x-open" , "204" , "204" , "x-dot" , "304" ,
                  "304" , "x-open-dot" , "5" , "5" , "triangle-up" , "105" , "105" , "triangle-up-open" , "205" ,
                  "205" , "triangle-up-dot" , "305" , "305" , "triangle-up-open-dot" , "6" , "6" , "triangle-down" ,
                  "106" , "106" , "triangle-down-open" , "206" , "206" , "triangle-down-dot" , "306" , "306" ,
                  "triangle-down-open-dot" , "7" , "7" , "triangle-left" , "107" , "107" , "triangle-left-open" ,
                  "207" , "207" , "triangle-left-dot" , "307" , "307" , "triangle-left-open-dot" , "8" ,
                  "triangle-right" , "108" , "108" , "triangle-right-open" , "208" , "208" , "triangle-right-dot" ,
                  "308" , "triangle-right-open-dot" , "9" , "9" , "triangle-ne" , "109" , "109" , "triangle-ne-open" ,
                  "209" , "209" , "triangle-ne-dot" , "309" , "309" , "triangle-ne-open-dot" , "10" , "10" , "triangle-se" ,
                  "110" , "110" , "triangle-se-open" , "210" , "210" , "triangle-se-dot" , "310" , "310" ,
                  "triangle-se-open-dot" , "11" , "11" , "triangle-sw" , "111" , "111" , "triangle-sw-open" ,
                  "211" , "211" , "triangle-sw-dot" , "311" , "311" , "triangle-sw-open-dot" , "12" , "12" ,
                  "triangle-nw" , "112" , "112" , "triangle-nw-open" , "212" , "212" , "triangle-nw-dot" ,
                  "312" , "312" , "triangle-nw-open-dot" , "13" , "13" , "pentagon" , "113" , "113" , "pentagon-open" ,
                  "213" , "213" , "pentagon-dot" , "313" , "313" , "pentagon-open-dot" , "14" , "14" , "hexagon" ,
                  "114" , "114" , "hexagon-open" , "214" , "214" , "hexagon-dot" , "314" , "314" , "hexagon-open-dot" ,
                  "15" , "15" , "hexagon2" , "115" , "115" , "hexagon2-open" , "215" , "215" , "hexagon2-dot" , "315" ,
                  "315" , "hexagon2-open-dot" , "16" , "16" , "octagon" , "116" , "116" , "octagon-open" , "216" , "216" ,
                  "octagon-dot" , "316" , "316" , "octagon-open-dot" , "17" , "17" , "star" , "117" , "117" , "star-open" ,
                  "217" , "217" , "star-dot" , "317" , "317" , "star-open-dot" , "18" , "18" , "hexagram" , "118" ,
                  "118" , "hexagram-open" , "218" , "218" , "hexagram-dot" , "318" , "318" , "hexagram-open-dot" ,
                  "19" , "19" , "star-triangle-up" , "119" , "119" , "star-triangle-up-open" , "219" , "219" ,
                  "star-triangle-up-dot" , "319" , "319" , "star-triangle-up-open-dot" , "20" , "20" , "star-triangle-down" ,
                  "120" , "120" , "star-triangle-down-open" , "220" , "220" , "star-triangle-down-dot" , "320" ,
                  "320" , "star-triangle-down-open-dot" , "21" , "21" , "star-square" , "121" , "121" ,
                  "star-square-open" , "221" , "221" , "star-square-dot" , "321" , "321" , "star-square-open-dot" ,
                  "22" , "22" , "star-diamond" , "122" , "122" , "star-diamond-open" , "222" , "222" ,
                  "star-diamond-dot" , "322" , "322" , "star-diamond-open-dot" , "23" , "23" , "diamond-tall" ,
                  "123" , "123" , "diamond-tall-open" , "223" , "223" , "diamond-tall-dot" , "323" , "323" ,
                  "diamond-tall-open-dot" , "24" , "24" , "diamond-wide" , "124" , "124" , "diamond-wide-open" ,
                  "224" , "224" , "diamond-wide-dot" , "324" , "324" , "diamond-wide-open-dot" , "25" , "25" ,
                  "hourglass" , "125" , "125" , "hourglass-open" , "26" , "26" , "bowtie" , "126" , "126" ,
                  "bowtie-open" , "27" , "27" , "circle-cross" , "127" , "127" , "circle-cross-open" , "28" ,
                  "28" , "circle-x" , "128" , "128" , "circle-x-open" , "29" , "29" , "square-cross" , "129" ,
                  "129" , "square-cross-open" , "30" , "30" , "square-x" , "130" , "130" , "square-x-open" ,
                  "31" , "31" , "diamond-cross" , "131" , "131" , "diamond-cross-open" , "32" , "32" , "diamond-x" ,
                  "132" , "132" , "diamond-x-open" , "33" , "33" , "cross-thin" , "133" , "133" , "cross-thin-open" ,
                  "34" , "34" , "x-thin" , "134" , "134" , "x-thin-open" , "35" , "35" , "asterisk" , "135" , "135" ,
                  "asterisk-open" , "36" , "36" , "hash" , "136" , "136" , "hash-open" , "236" , "236" , "hash-dot" ,
                  "336" , "336" , "hash-open-dot" , "37" , "37" , "y-up" , "137" , "137" , "y-up-open" , "38" , "38" ,
                  "y-down" , "138" , "138" , "y-down-open" , "39" , "39" , "y-left" , "139" , "139" , "y-left-open" ,
                  "40" , "40" , "y-right" , "140" , "140" , "y-right-open" , "41" , "41" , "line-ew" , "141" , "141" ,
                  "line-ew-open" , "42" , "42" , "line-ns" , "142" , "142" , "line-ns-open" , "43" , "43" , "line-ne" ,
                  "143" , "143" , "line-ne-open" , "44" , "44" , "line-nw" , "144" , "144" , "line-nw-open" , "45" , "45" ,
                  "arrow-up" , "145" , "145" , "arrow-up-open" , "46" , "46" , "arrow-down" , "146" , "146" ,
                  "arrow-down-open" , "47" , "47" , "arrow-left" , "147" , "147" , "arrow-left-open" , "48" , "48" ,
                  "arrow-right" , "148" , "148" , "arrow-right-open" , "49" , "49" , "arrow-bar-up" , "149" , "149" ,
                  "arrow-bar-up-open" , "50" , "50" , "arrow-bar-down" , "150" , "150" , "arrow-bar-down-open" , "51" ,
                  "51" , "arrow-bar-left" , "151" , "151" , "arrow-bar-left-open" , "52" , "52" , "arrow-bar-right" ,
                  "152" , "152" , "arrow-bar-right-open" , "53" , "53" , "arrow" , "153" , "153" , "arrow-open" , "54" ,
                  "54" , "arrow-wide" , "154" , "154" , "arrow-wide-open" )
  valid<-x %in% symbol_list
  if(sum(valid)==length(x))
    return()
  return(x[!valid])
}


#' Retrieve all valid plotting symbols for plotly library
#'
#' @return A vector of all the valid plotting symbols used in the \code{\link[plotly]{plot_ly}} library.
#' @export
#' @examples
#' Symbol_List()
Symbol_List<-function(){
  symbol_list<-c( "0" , "0" , "circle" , "100" , "100" , "circle-open" , "200" , "200" , "circle-dot" , "300" ,
                  "300" , "circle-open-dot" , "1" , "1" , "square" , "101" , "101" , "square-open" , "201" ,
                  "201" , "square-dot" , "301" , "301" , "square-open-dot" , "2" , "2" , "diamond" , "102" ,
                  "102" , "diamond-open" , "202" , "202" , "diamond-dot" , "302" , "302" , "diamond-open-dot" ,
                  "3" , "3" , "cross" , "103" , "103" , "cross-open" , "203" , "203" , "cross-dot" , "303" , "303" ,
                  "cross-open-dot" , "4" , "4" , "x" , "104" , "104" , "x-open" , "204" , "204" , "x-dot" , "304" ,
                  "304" , "x-open-dot" , "5" , "5" , "triangle-up" , "105" , "105" , "triangle-up-open" , "205" ,
                  "205" , "triangle-up-dot" , "305" , "305" , "triangle-up-open-dot" , "6" , "6" , "triangle-down" ,
                  "106" , "106" , "triangle-down-open" , "206" , "206" , "triangle-down-dot" , "306" , "306" ,
                  "triangle-down-open-dot" , "7" , "7" , "triangle-left" , "107" , "107" , "triangle-left-open" ,
                  "207" , "207" , "triangle-left-dot" , "307" , "307" , "triangle-left-open-dot" , "8" ,
                  "triangle-right" , "108" , "108" , "triangle-right-open" , "208" , "208" , "triangle-right-dot" ,
                  "308" , "triangle-right-open-dot" , "9" , "9" , "triangle-ne" , "109" , "109" , "triangle-ne-open" ,
                  "209" , "209" , "triangle-ne-dot" , "309" , "309" , "triangle-ne-open-dot" , "10" , "10" , "triangle-se" ,
                  "110" , "110" , "triangle-se-open" , "210" , "210" , "triangle-se-dot" , "310" , "310" ,
                  "triangle-se-open-dot" , "11" , "11" , "triangle-sw" , "111" , "111" , "triangle-sw-open" ,
                  "211" , "211" , "triangle-sw-dot" , "311" , "311" , "triangle-sw-open-dot" , "12" , "12" ,
                  "triangle-nw" , "112" , "112" , "triangle-nw-open" , "212" , "212" , "triangle-nw-dot" ,
                  "312" , "312" , "triangle-nw-open-dot" , "13" , "13" , "pentagon" , "113" , "113" , "pentagon-open" ,
                  "213" , "213" , "pentagon-dot" , "313" , "313" , "pentagon-open-dot" , "14" , "14" , "hexagon" ,
                  "114" , "114" , "hexagon-open" , "214" , "214" , "hexagon-dot" , "314" , "314" , "hexagon-open-dot" ,
                  "15" , "15" , "hexagon2" , "115" , "115" , "hexagon2-open" , "215" , "215" , "hexagon2-dot" , "315" ,
                  "315" , "hexagon2-open-dot" , "16" , "16" , "octagon" , "116" , "116" , "octagon-open" , "216" , "216" ,
                  "octagon-dot" , "316" , "316" , "octagon-open-dot" , "17" , "17" , "star" , "117" , "117" , "star-open" ,
                  "217" , "217" , "star-dot" , "317" , "317" , "star-open-dot" , "18" , "18" , "hexagram" , "118" ,
                  "118" , "hexagram-open" , "218" , "218" , "hexagram-dot" , "318" , "318" , "hexagram-open-dot" ,
                  "19" , "19" , "star-triangle-up" , "119" , "119" , "star-triangle-up-open" , "219" , "219" ,
                  "star-triangle-up-dot" , "319" , "319" , "star-triangle-up-open-dot" , "20" , "20" , "star-triangle-down" ,
                  "120" , "120" , "star-triangle-down-open" , "220" , "220" , "star-triangle-down-dot" , "320" ,
                  "320" , "star-triangle-down-open-dot" , "21" , "21" , "star-square" , "121" , "121" ,
                  "star-square-open" , "221" , "221" , "star-square-dot" , "321" , "321" , "star-square-open-dot" ,
                  "22" , "22" , "star-diamond" , "122" , "122" , "star-diamond-open" , "222" , "222" ,
                  "star-diamond-dot" , "322" , "322" , "star-diamond-open-dot" , "23" , "23" , "diamond-tall" ,
                  "123" , "123" , "diamond-tall-open" , "223" , "223" , "diamond-tall-dot" , "323" , "323" ,
                  "diamond-tall-open-dot" , "24" , "24" , "diamond-wide" , "124" , "124" , "diamond-wide-open" ,
                  "224" , "224" , "diamond-wide-dot" , "324" , "324" , "diamond-wide-open-dot" , "25" , "25" ,
                  "hourglass" , "125" , "125" , "hourglass-open" , "26" , "26" , "bowtie" , "126" , "126" ,
                  "bowtie-open" , "27" , "27" , "circle-cross" , "127" , "127" , "circle-cross-open" , "28" ,
                  "28" , "circle-x" , "128" , "128" , "circle-x-open" , "29" , "29" , "square-cross" , "129" ,
                  "129" , "square-cross-open" , "30" , "30" , "square-x" , "130" , "130" , "square-x-open" ,
                  "31" , "31" , "diamond-cross" , "131" , "131" , "diamond-cross-open" , "32" , "32" , "diamond-x" ,
                  "132" , "132" , "diamond-x-open" , "33" , "33" , "cross-thin" , "133" , "133" , "cross-thin-open" ,
                  "34" , "34" , "x-thin" , "134" , "134" , "x-thin-open" , "35" , "35" , "asterisk" , "135" , "135" ,
                  "asterisk-open" , "36" , "36" , "hash" , "136" , "136" , "hash-open" , "236" , "236" , "hash-dot" ,
                  "336" , "336" , "hash-open-dot" , "37" , "37" , "y-up" , "137" , "137" , "y-up-open" , "38" , "38" ,
                  "y-down" , "138" , "138" , "y-down-open" , "39" , "39" , "y-left" , "139" , "139" , "y-left-open" ,
                  "40" , "40" , "y-right" , "140" , "140" , "y-right-open" , "41" , "41" , "line-ew" , "141" , "141" ,
                  "line-ew-open" , "42" , "42" , "line-ns" , "142" , "142" , "line-ns-open" , "43" , "43" , "line-ne" ,
                  "143" , "143" , "line-ne-open" , "44" , "44" , "line-nw" , "144" , "144" , "line-nw-open" , "45" , "45" ,
                  "arrow-up" , "145" , "145" , "arrow-up-open" , "46" , "46" , "arrow-down" , "146" , "146" ,
                  "arrow-down-open" , "47" , "47" , "arrow-left" , "147" , "147" , "arrow-left-open" , "48" , "48" ,
                  "arrow-right" , "148" , "148" , "arrow-right-open" , "49" , "49" , "arrow-bar-up" , "149" , "149" ,
                  "arrow-bar-up-open" , "50" , "50" , "arrow-bar-down" , "150" , "150" , "arrow-bar-down-open" , "51" ,
                  "51" , "arrow-bar-left" , "151" , "151" , "arrow-bar-left-open" , "52" , "52" , "arrow-bar-right" ,
                  "152" , "152" , "arrow-bar-right-open" , "53" , "53" , "arrow" , "153" , "153" , "arrow-open" , "54" ,
                  "54" , "arrow-wide" , "154" , "154" , "arrow-wide-open" )

    return(unique(symbol_list))
}





#' Insert javascript reactivity code
#'
#' @param plotly_plot Plotly graph
#' @param dat data to be passed as argument
#'
#' @return plotly plot updated with javascript code
#' @noRd
insert_reactivity_TDA<-function(plotly_plot,dat){

  plotly_plot |> htmlwidgets::onRender("

     function(el,x,data) {
     var clicked = false;
     var hasbox = false;
     var arr1 = new Array(data.Xhat[0][0].length).fill(0);
     var active = 0;
     var rel_but = [0,0,0];
     var is_visible=0;
     var selected = 0;
     var bip_domain = [0,1];
     var table_visible = 0;
     var table2_visible = 0;
     var table_trace = el.data[el.data.length-1];
     var pred12 = el.data[el.data.length-3];
     var pred13 = el.data[el.data.length-2];
     var pred23 = el.data[el.data.length-1];
     Plotly.deleteTraces('mydiv',[el.data.length-1,el.data.length-2,el.data.length-3])
     var All_annot = el.layout.annotations;
     function myFunction(up,low) {
        for (let i = up; i < low; i++) {
              All_annot[i].visible = true;
          }
     }



//-------------- UPDATEMENU-----------------

        el.on('plotly_buttonclicked',function(d){
              // toggle selectibility

              var rel_but_sel = rel_but[d.menu._index-1];
              if(d.menu._index==1){
              // that is, the axis predictivity is to be inserted
                  var update = {
                    'updatemenus[1].active': [0,1][rel_but_sel],
                    'xaxis.domain': [[0,0.5],[0,1]][is_visible],
                    'yaxis3.zeroline':true
                  }
                  bip_domain[1] = [0.5,1][is_visible];
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'axis_pred') {
                          update_traces.push(index);
                      }
                  });

                  var plot_update ={
                    'visible':[true,false][is_visible],
                    'xaxis':['x3','x'][is_visible],
                    'yaxis':['y3','y'][is_visible]
                  }
                  is_visible=[1,0][is_visible];
                  Plotly.restyle('mydiv',plot_update,update_traces)
                  rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                  Plotly.relayout('mydiv',update)
                  return;
              }
              if(d.menu._index==2){

                  table2_visible = [1,0][table2_visible];
                  var update = {
                    'updatemenus[2].active': [0,1][rel_but_sel],
                    'yaxis.domain' : [[0,1],[0.3,1],[0.3,1]][table_visible+table2_visible],
                    'yaxis2.domain': [[0.15,0.85],[0.3,1],[0.3,1]][table_visible+table2_visible],
                    'yaxis3.domain': [[0.15,0.85],[0.3,1],[0.3,1]][table_visible+table2_visible],
                    'legend.y':[0.82,0.92,0.92][table_visible+table2_visible]
                  }
                  if(rel_but_sel === 0){
                    Plotly.addTraces('mydiv',[pred12,pred13,pred23][selected])
                }
                if(rel_but_sel === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'PredTable') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces('mydiv',update_traces)
                }

                rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                Plotly.relayout('mydiv',update);
                return;
              }
              if(d.menu._index==3){
                table_visible = [1,0][table_visible];
                  var update = {
                    'updatemenus[3].active': [0,1][rel_but_sel],
                    'yaxis.domain' : [[0,1],[0.3,1],[0.3,1]][table_visible+table2_visible],
                    'yaxis2.domain': [[0.15,0.85],[0.3,1],[0.3,1]][table_visible+table2_visible],
                    'yaxis3.domain': [[0.15,0.85],[0.3,1],[0.3,1]][table_visible+table2_visible],
                    'legend.y':[0.82,0.92,0.92][table_visible+table2_visible]
                  }
                if(rel_but_sel === 0){
                    Plotly.addTraces('mydiv',table_trace)
                }
                if(rel_but_sel === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'table') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces('mydiv',update_traces)
                }
                rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                Plotly.relayout('mydiv',update);
                return;
              }


              // CHANGE PC's

              // first remove prediction lines
              if(clicked){
                    var remove = [];
                    el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                    });
                Plotly.deleteTraces('mydiv', remove);
            }
            selected = d.active;
            var Activetraces = Array(data.num).fill().map((element, index) => index + data.num*active);
            var NewActive = Array(data.num).fill().map((element, index) => index + data.num*selected);
            if (selected === active){//basies hoef fokol te doen
              return;
            }

            if (table2_visible === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'PredTable') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces('mydiv',update_traces)
                  Plotly.addTraces('mydiv',[pred12,pred13,pred23][selected])
            }

            var update = {
                visible: false
            };
            var update2={
                visible: true
            }

            Plotly.restyle('mydiv', update, Activetraces);
            Plotly.restyle('mydiv', update2, NewActive);
            active = selected;

            dp_update = {
            'xaxis.title' : data.DP[selected],
            annotations : All_annot.slice(data.counts[active],data.counts[active+1])
            }
            myFunction(data.counts[active],data.counts[active+1])
            Plotly.relayout('mydiv',dp_update)
            return false;
        })


//------------LEGENDCLICK--------------------

       el.on('plotly_legendclick', function(dat){
          var Activetraces = Array(data.num).fill().map((element, index) => index + data.num*active);
          // Delete predictive lines
          // NOTE: this must come first before rest otherwise error
          if(dat.data[dat.curveNumber].meta=== 'predict'){
            var remove = [];
            el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                });
            //remove prediction lines annotations as well
            for(let i = 0; i < data.a[active].length; i++){
                el.layout.annotations.pop();
            }
            Plotly.deleteTraces('mydiv', remove);
            return false;
         }

          if(dat.data[dat.curveNumber].meta[0] === 'data'){
          return;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'density'){
          return;
          }
          if(dat.data[dat.curveNumber].meta === 'box'){
            Plotly.deleteTraces('mydiv',dat.curveNumber)
            bip_domain[0] = 0;
            var update = {
                'xaxis.domain': bip_domain,   // updates the xaxis range
                'yaxis2.side': 'left'
            };
            Plotly.relayout('mydiv',update);
            return false;
          }


          // REMOVE AXES

          var axis = dat.data[dat.curveNumber].legendgroup;
          var num = dat.data[dat.curveNumber].customdata[0];
          var indeces =[];
          el.data.slice(data.num*active,data.num*active+data.num).forEach(function(item,idx,arr){
              if(arr[idx].legendgroup === undefined){
              return;
              }
              if(arr[idx].legendgroup === axis){
                  indeces.push(idx);
              }
              if(arr[idx].customdata === undefined){
              return;
              }
              if(arr[idx].customdata[0] === axis){
                indeces.push(idx);
              }
          });
          var old_annotations = el.layout.annotations;
          if(active===0){
            old_annotations.slice(data.counts[active],data.counts[active+1]).forEach(function(item,idx,arr){
              if(arr[idx].customdata === num){
                old_annotations[idx].visible = !old_annotations[idx].visible;
              }
            });
          }else{
            old_annotations.forEach(function(item,idx,arr){
                if(arr[idx].customdata === num){
                  old_annotations[idx].visible = !old_annotations[idx].visible;
                }
            });
          }
          hidden = arr1[num-1];
          var update = {'visible': ['legendonly',true][hidden]};
          hidden = [1,0][hidden];
          arr1[num-1] = hidden;
          var new_annot = {annotations:old_annotations};
          Plotly.restyle('mydiv',update,indeces.map((element, index) => element + data.num*active));
          Plotly.relayout('mydiv',new_annot);
          return false;
        });

//-------------------POINTS CLICK--------------

       el.on('plotly_click', function(d) {
       if(d.points[0].meta === 'density'){
          return;
       }
    //-------------BOXPLOT--------------------
       if(d.points[0].meta === 'axis'){
            if(hasbox){
            var deleters = [];
            //need to remove current boxplot
                el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'box') {
                          deleters.push(index);
                      }
                })

                Plotly.deleteTraces('mydiv', deleters);
            }
            bip_domain[0] = 0.15;
            var update = {
                'xaxis.domain': bip_domain,   // updates the xaxis range
                'yaxis2.side': 'left'
            };

        var trace1 = {
            y: data.Xhat2[active][d.points[0].customdata-1],
            type: 'box',
            name: 'Boxplot: <br>'+data.colnames[d.points[0].customdata-1],
            meta: 'box',
            marker: {
              color: 'rgb(7,40,89)'
            },
            jitter: 0.3,
            pointpos: -1.8,
            xaxis: 'x2',
            yaxis: 'y2',
            boxpoints: 'all'
        };


        Plotly.relayout('mydiv',update);
        Plotly.addTraces('mydiv', trace1);
        hasbox = true;
        return;
       }
  //-----------------PREDICTION LINES--------------

         if(clicked){
         var remove = [];
            el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                });
            Plotly.deleteTraces('mydiv', remove);
            for(let i = 0; i < data.a[active].length; i++){
                el.layout.annotations.pop();
            }
         }
         var X = [];
         var Y = [];
         for (let i = 0; i < data.a[active].length; i++) {
            var c = d.points[0].y+1/data.a[active][i].m*d.points[0].x;
            var x_new = (data.a[active][i].c-c)/(-1/data.a[active][i].m-data.a[active][i].m);
            var y_new = data.a[active][i].m*x_new+data.a[active][i].c;
            var showleg = false;
            if(i === 0){showleg = true;}
            X.push(x_new);
            Y.push(y_new);
            var newtrace = {
                x: [d.points[0].x, x_new],
                y: [d.points[0].y, y_new],
                mode: 'lines',
                xaxis: 'x',
                yaxis: 'y',
                showlegend: showleg,
                name: 'Predicted Value',
                meta: 'predict',
                line: {
                  dash: 'dot',
                  color: 'gray',
                  width: 1.5
                             }
            };
            var newAnnotation = {
                x: x_new,
                y: y_new,
                text: data.Xhat[active][d.points[0].customdata-1][i].toFixed(2),
                showarrow: false,
                textangle: -Math.atan(data.a[active][i].m)*180/Math.PI,
                xshift: -10*Math.sin(Math.atan(data.a[active][i].m)),
                yshift: 10*Math.cos(Math.atan(data.a[active][i].m)),
                name: 'Predicted Value',
                meta: 'predict',
                visible: true
            }

            el.layout.annotations.push(newAnnotation);
            Plotly.addTraces('mydiv', newtrace);
         }
        clicked=true;
        var markertrace = {
            x: X,
            y: Y,
            mode: 'markers',
            showlegend: false,
            xaxis: 'x',
            yaxis: 'y',
            meta: 'predict',
            marker: {
              color:'gray'
            }
        }
        Plotly.addTraces('mydiv', markertrace);





       });



}

   ",data=dat)

}
