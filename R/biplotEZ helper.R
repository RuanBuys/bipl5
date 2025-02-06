

#' Initialise plotly plot with layout in place
#'
#' @param dpquality Quality of display. Text string
#' @param basis Basis vectors to construct display
#' @param ax_pred Include axis predictivity button
#' @param FM include fit measures button
#' @param vec_dis include vector display button
#' @noRd
plot_scaffolding<-function(dpquality,basis,PC=TRUE,ax_pred=TRUE,FM=TRUE,vec_dis=TRUE){

  Title<-"Overall quality and axis predictivities (cumulative)"
    p_ly<-plot_ly() |>
    layout(legend=list(tracegroupgap=0,xref="container",
                       yref="container",x=1,y=0.82,
                       groupclick="toggleitem"
                       ),
           xaxis=list(title=dpquality,showticklabels = FALSE,
                      zeroline=FALSE,showgrid = FALSE,domain=c(0,1)),
           yaxis=list(showticklabels = FALSE,zeroline=FALSE,
                      scaleanchor={'x'}, scaleratio=1,showgrid = FALSE),
           xaxis2=list(domain=c(0,0.15),zeroline=TRUE),
           yaxis2=list(zeroline=TRUE,side="left",position=0),
           xaxis3=list(domain=c(0.65,1),zeroline=TRUE,showgrid=TRUE,
                       anchor="y3",dtick=1,title="Dimension of Subspace"),
           yaxis3=list(zeroline=TRUE,side="left",position=0.65,
                       showgrid=TRUE,domain=c(0.15,0.85),layer="below traces",
                       title=Title),
           updatemenus = list(
             list(
               y = 0.8,
               type="buttons",
               x =0,
               showactive=F,
               active=c(0,1),
               buttons = list(
                 list(method = "skip",
                      args = list("type", "scatter"),
                      label = paste("PC:",basis[1],"&",basis[2]),
                      visible=PC),
                 list(method="skip",
                      args=list("type", "scatter"),
                      label="Axis Predictivity",
                      name="AxisStats",
                      visible=ax_pred,
                      execute=FALSE
                 ),
                 list(method="skip",
                      args=list("type", "scatter"),
                      label="Fit Measures",
                      name="FitMeasures",
                      visible=FM,
                      execute=FALSE

                 ),
                 list(method="skip",
                      args=list("type", "scatter"),
                      label="Vector Display",
                      name="vecload",
                      visible=vec_dis,
                      execute=FALSE

                 )
               )
             )
           )
    )

  return(p_ly)
}





#' Insert class means
#'
#' @param p_ly plotly graph
#' @param Z Z coors
#' @param symbol Plotting symbol
#' @param color color of point
#'
#' @noRd
insert_class_means<-function(p_ly,Z,symbol,color){
  for(i in 1:nrow(Z)){
    p_ly<-p_ly |> add_trace(x=Z[i,1],
                            y=Z[i,2],name=rownames(Z)[i],
                            type = "scatter", mode = "markers",
                            hovertext="Class Mean",
                            hoverinfo="text+name",
                            customdata=i-1,
                            meta="ClassMean",xaxis="x",yaxis="y",visible=TRUE,
                            showlegend=FALSE,
                            marker=list(symbol=symbol[i],color=color[i]),
                            legendgroup="ClassMean"
                            )
  }
  return(p_ly)
}



#' Create a biplot with vector loadings and calibrated axes.
#'
#' @param pc12 First two principal components
#' @param colorpalete Colors per class group
#' @param symbol plotting symbol per class group
#'
#' @return plotly graph
#' @noRd
make_biplot_EZ<-function(pc12,colorpalete=NULL,symbol="circle"){

  p_ly<-plot_scaffolding(pc12$DisplQuality,pc12$basis)

  addpc12<-add_vector_biplot(p_ly=p_ly,x=pc12,symbol=symbol,
                             color=colorpalete,visible=TRUE)
  p_ly<-addpc12[[1]]


  pc13<-list(NULL)
  pc23<-list(NULL)

  addpc13<-list(NULL,NULL,NULL,NULL,NULL)
  addpc23<-list(NULL,NULL,NULL,NULL,NULL)

  Xhat<-list(addpc12[[3]],addpc13[[3]],addpc23[[3]])
  Xhat2<-list(t(addpc12[[3]]),NULL,NULL)
  df<-list(addpc12[[2]],addpc13[[2]],addpc23[[2]])

  #need to count the annotations as these are tick marks.
  #JS should toggle visibility
  counter<-c(addpc12[[4]],addpc13[[4]],addpc23[[4]])

  #also need the angles of all the tick marks as annotation for
  #new predict lines

  angles<-list(addpc12[[5]],addpc13[[5]],addpc23[[5]])
  numtraces<-length(levels(pc12$group))+3*pc12$p +1
  Dispquality<-c(pc12$DisplQuality,pc13$DisplQuality,pc23$DisplQuality)



  p_ly<-p_ly|>
    add_trace(x=cos(seq(0,2*pi,length.out=200)),
              y=sin(seq(0,2*pi,length.out=200)), type="scatter",
              mode="lines",line = list(color = 'red',width=1.2),
              name="Unit Circle",showlegend=FALSE,
              meta='veccircle',xaxis="x",yaxis="y",
              hoverinfo='name',visible=FALSE)

  p_ly<-InsertAxisDeets(p_ly,pc12)
  FitMeasures<-InsertFitMeasures(p_ly,pc12)
  p_ly<-FitMeasures[[1]]

  p_ly<-insert_vector_annots(p_ly,pc12,pc13,pc23)
  counter<-c(counter,rep(pc12$p,3))

  p_ly<- p_ly|> htmlwidgets::onRender("

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
     var table2_visible = 1;
     var vect_visible = 0;

     // trace for fit measure table
     var pred12 = el.data[el.data.length-3];
     var pred13 = el.data[el.data.length-2];
     var pred23 = el.data[el.data.length-1];
     Plotly.deleteTraces(el.id,
                        [el.data.length-1,el.data.length-2,el.data.length-3])
     var All_annot = el.layout.annotations;
     function myFunction(up,low) {
        for (let i = up; i < low; i++) {
              All_annot[i].visible = true;
          }
     }

     console.log(el);



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
                  Plotly.restyle(el.id,plot_update,update_traces)
                  rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                  Plotly.relayout(el.id,update)
                  return;
              }

              if(d.menu._index==2){
                  // that is the fit measures table needs to be inserted
                  var idx = table_visible+table2_visible;
                  table2_visible = [1,0][table2_visible];
                  var update = {
                    'updatemenus[2].active': [0,1][rel_but_sel],
                    'yaxis.domain' : [[0,1],[0.3,1],[0.3,1]][idx],
                    'yaxis2.domain': [[0.15,0.85],[0.3,1],[0.3,1]][idx],
                    'yaxis3.domain': [[0.15,0.85],[0.3,1],[0.3,1]][idx],
                    'legend.y':[0.82,0.92,0.92][idx]
                  }
                  if(rel_but_sel === 0){
                    Plotly.addTraces(el.id,[pred12,pred13,pred23][selected])
                }
                if(rel_but_sel === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'PredTable') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces(el.id,update_traces)
                }

                rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                Plotly.relayout(el.id,update);
                return;
              }
              if(d.menu._index==3){
              // that is need to insert/delete red circle and vectors
                if(rel_but_sel === 0){

                    //need to insert vects
                    // first remove prediction lines
                    if(clicked){
                        var remove = [];
                        el.data.forEach(function (item, index, arr) {

                          if (arr[index].meta === 'predict') {
                             remove.push(index);
                          }
                        });
                    Plotly.deleteTraces(el.id, remove);
                    clicked=false;
                    }

                    // next we need to insert red circle and vects pappa
                    var update = {
                      visible: true
                    };
                    var n = 5;

                    myFunction(data.counts[n-4+active],data.counts[n-3+active])
                    All_annot.slice(data.counts[n-3+active],
                                    data.counts[n-2+active])

                    Plotly.restyle(el.id, update, [1*data.num]);
                    console.log(active)
                    console.log(data.counts)
                    var dp_update = {
                      annotations : All_annot.slice(data.counts[n-4+active],
                                    data.counts[n-3+active]),
                      'updatemenus[3].active': [0,1][rel_but_sel],
                    }
                    vect_visible = 1;

                    //alright pappa now need to take away axes

                    var tr_index = []
                    for(let i = data.num*active; i<data.num*(active+1); i++){
                        if(el.data[i].meta === 'axis'){
                        tr_index.push(i)
                        }
                        if(el.data[i].meta[0] === 'axis'){
                          tr_index.push(i)
                        }
                    }
                    var trace_update = {
                        visible: false
                    }
                    Plotly.update(el.id,trace_update,dp_update,tr_index)
                }
                if(rel_but_sel === 1){
                  //need to remove vects and insert axes once more
                    var update = {
                      visible: true
                    };

                    var tr_index = []
                    for(let i = data.num*active; i<data.num*(active+1); i++){
                        if(el.data[i].meta === 'axis'){
                        tr_index.push(i)
                        }
                        if(el.data[i].meta[0] === 'axis'){
                          tr_index.push(i)
                        }
                    }

                    vect_visible=0;
                    dp_update = {
                      'updatemenus[3].active': [0,1][rel_but_sel],
                      annotations : All_annot.slice(data.counts[active],
                                                    data.counts[active+1])
                    }
                    el.data[1*data.num].visible = false;
                    Plotly.update(el.id,update,dp_update,tr_index)
                }

                rel_but[d.menu._index-1] = [1,0][rel_but_sel];
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
                Plotly.deleteTraces(el.id, remove);
              }
            clicked=false;
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
                  Plotly.deleteTraces(el.id,update_traces)
                  Plotly.addTraces(el.id,[pred12,pred13,pred23][selected])
            }

            var update = {
                visible: false
            };
            var update2={
                visible: true
            }


            Plotly.restyle(el.id, update, Activetraces);
            Plotly.restyle(el.id, update2, NewActive);
            active = selected;

          //ensure the vector display button is unselected and red circle gone
            el.data[1*data.num].visible = false;
            rel_but[2] = 0;
            dp_update = {
            'updatemenus[3].active': 1,
            'xaxis.title' : data.DP[selected],
            annotations : All_annot.slice(data.counts[active],
                                          data.counts[active+1])
            }
            myFunction(data.counts[active],data.counts[active+1])
            Plotly.relayout(el.id,dp_update)
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
            Plotly.deleteTraces(el.id, remove);
            return false;
         }

          if(dat.data[dat.curveNumber].meta[0] === 'data'){
          return;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'density'){
          return;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'axis_pred'){
            return;
          }
          if(dat.data[dat.curveNumber].meta === 'box'){
            Plotly.deleteTraces(el.id,dat.curveNumber)
            bip_domain[0] = 0;
            var update = {
                'xaxis.domain': bip_domain,   // updates the xaxis range
                'yaxis2.side': 'left'
            };
            Plotly.relayout(el.id,update);
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
          Plotly.restyle(el.id,update,indeces.map((element, index)=>element +
                                          data.num*active));
          Plotly.relayout(el.id,new_annot);
          return false;
        });





//-------------------POINTS CLICK--------------

       el.on('plotly_click', function(d) {

       if(d.points[0].meta === 'density'){
          return;
       }
       if(vect_visible ===1){
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

                Plotly.deleteTraces(el.id, deleters);
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


        Plotly.relayout(el.id,update);
        Plotly.addTraces(el.id, trace1);
        hasbox = true;
        return;
       }
       console.log('boxplot klaar process')
  //-----------------PREDICTION LINES--------------

         if(clicked){
         console.log('haal ou predict uit begin')
         var remove = [];
            el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                });
            Plotly.deleteTraces(el.id, remove);
            for(let i = 0; i < data.a[active].length; i++){
                el.layout.annotations.pop();
            }
          console.log('ou predict eindig')
         }
         var X = [];
         var Y = [];
         console.log('begin nuwes insit')
         var traces_to_be_added = [];
         for (let i = 0; i < data.a[active].length; i++) {
            var c = d.points[0].y+1/data.a[active][i].m*d.points[0].x;
            var x_new = (data.a[active][i].c-c)/(-1/data.a[active][i].m -
                                                data.a[active][i].m);
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
                  width: 1
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
                visible: true,
                font: {
                  size:10
                }
            }
            traces_to_be_added.push(newtrace)
            el.layout.annotations.push(newAnnotation);
            //Plotly.addTraces(el.id, newtrace);
         }
         Plotly.addTraces(el.id, traces_to_be_added);
         console.log('eindig nuwes insit')
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
              color:'gray',
              size: 4
            }
        }
        Plotly.addTraces(el.id, markertrace);





       });



}

   ",data=list(a=df,Xhat=Xhat,Xhat2=Xhat2,colnames=colnames(pc12$x),
               num=numtraces,DP=Dispquality,counts=c(0,cumsum(counter)),
               Angles=angles))

  return(list(p_ly,FitMeasures[[2]],FitMeasures[[3]]))
}




#' Insert JS code for spline axes
#'
#' @param p_ly plotly graph
#'
#' @noRd
insert_spline_js<-function(p_ly,p){
  p_ly<- p_ly|> htmlwidgets::onRender("

     function(el,x,data) {
        console.log(el);
       var arr1 = new Array(data.p).fill(0);
       el.on('plotly_legendclick', function(dat){


          if(dat.data[dat.curveNumber].meta[0] === 'data'){
          return;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'density'){
          return;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'axis_pred'){
            return;
          }


          // REMOVE AXES

          var axis = dat.data[dat.curveNumber].legendgroup;
          var num = Number(axis.replace('Ax',''));


          var indeces =[];
          el.data.forEach(function(item,idx,arr){
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
            old_annotations.forEach(function(item,idx,arr){
              if(arr[idx].customdata === num){
                old_annotations[idx].visible = !old_annotations[idx].visible;
              }
            });
          var new_annot = {annotations:old_annotations};

          hidden = arr1[num-1];
          var update = {'visible': ['legendonly',true][hidden]};
          hidden = [1,0][hidden];
          arr1[num-1] = hidden;
          var new_annot = {annotations:old_annotations};

          Plotly.restyle(el.id,update,indeces);

          Plotly.relayout(el.id,new_annot);
          return false;
        });

//---------------Click on the graph---------------------
        el.on('plotly_click', function(d) {

        // Click on the axes

        console.log(d);
        //el.layout.annotations.push(newAnnotation);
        var NewAnot1 = {
          x: d.points[0].x,
          y: d.points[0].y,
          text: '&#124;',
          showarrow: false,
          meta:'axis',
          xaxis:'x',
          yaxis:'y',
          visible: true,
          textangle: -Math.atan(d.points[0].customdata)*180/Math.PI,
          font: {
            size: 8
          },
          customdata: Number(d.points[0].data.legendgroup.replace('Ax',''))
        };
        var NewAnot2 = {
          x: d.points[0].x,
          y: d.points[0].y,
          text: (d.points[0].hovertext).toString(),
          showarrow: false,
          meta:'axis',
          xaxis:'x',
          yaxis:'y',
          visible: true,
          textangle: -Math.atan(d.points[0].customdata)*180/Math.PI,
          yshift: -12*Math.cos(Math.atan(d.points[0].customdata)),
          xshift: 12*Math.sin(Math.atan(d.points[0].customdata)),
          font: {
            size: 10
          },
          customdata: Number(d.points[0].data.legendgroup.replace('Ax',''))
        };



        el.layout.annotations.push(NewAnot1,NewAnot2);
        console.log(el.layout.annotations)
        Plotly.relayout(el.id,{annotations:el.layout.annotations});
        });

}

   ",data=list(p=p))

  return(p_ly)
}

#' Insert JS to biplots with linear axes
#'
#' @param p_ly plotly graph
#' @param Xhat predicted values
#' @param p number axes
#' @param m gradient of axes
#'
#' @noRd
insert_linear_js<-function(p_ly,Xhat,p,m,cols){
  p_ly<- p_ly|> htmlwidgets::onRender("

     function(el,x,data) {

     el.bipl5 = {clicked: false,
                 unit_circle: 0,
                 arr1: new Array(data.p).fill(0),
                 active: 0,
                 rel_but: [0,0,0,0],
                 is_visible: 0,
                 selected : 0,
                 bip_domain : [0,1],
                 table_visible : 0,
                 table2_visible : 1,
                 vect_visible : 0,
                 but_names : ['PC','AxisStats','FitMeasures','vecload']
                 };

//     var clicked = false;
//     var unit_circle =0;
//     var arr1 = new Array(data.p).fill(0);
//     var rel_but = [0,0,0,0];
//     var is_visible=0;
//     var selected = 0;
//     var bip_domain = [0,1];
//     var table_visible = 0;
//     var table2_visible = 1;
//     var vect_visible = 0;
//     const but_names = ['PC','AxisStats','FitMeasures','vecload']

     var All_annot = el.layout.annotations;
     function myFunction() {
        for (let i = 0; i < All_annot.length; i++) {
              All_annot[i].visible = !All_annot[i].visible;

        }
     }

     function removeAnnot(){
      for(let i = 0; i < data.p; i++){
                el.layout.annotations.pop();
            }
     }






//-------------- UPDATEMENU-----------------

        el.on('plotly_buttonclicked',function(d){
              // toggle selectibility
              console.log(d);

              var rel_but_sel = el.bipl5.rel_but[el.bipl5.but_names.indexOf(d.button.name)];
              console.log(rel_but_sel);
              if(d.button.name === 'AxisStats'){
              // that is, the axis predictivity is to be inserted
                  var update = {
                    'xaxis.domain': [[0,0.5],[0,1]][el.bipl5.is_visible],
                    'yaxis3.zeroline':true
                  }
                  el.bipl5.bip_domain[1] = [0.5,1][el.bipl5.is_visible];
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'axis_pred') {
                          update_traces.push(index);
                      }
                  });

                  var plot_update ={
                    'visible':[true,false][el.bipl5.is_visible],
                    'xaxis':['x3','x'][el.bipl5.is_visible],
                    'yaxis':['y3','y'][el.bipl5.is_visible]
                  }
                  el.bipl5.is_visible=[1,0][el.bipl5.is_visible];
                  Plotly.restyle(el.id,plot_update,update_traces)
                  el.bipl5.rel_but[el.bipl5.but_names.indexOf(d.button.name)] = [1,0][rel_but_sel];
                  Plotly.relayout(el.id,update)
                  return;
              }

              if(d.button.name === 'regmaak'){
                  // that is the fit measures table needs to be inserted
                  var idx = el.bipl5.table_visible+el.bipl5.table2_visible;
                  el.bipl5.table2_visible = [1,0][el.bipl5.table2_visible];
                  var update = {
                    'updatemenus[2].active': [0,1][rel_but_sel],
                    'yaxis.domain' : [[0,1],[0.3,1],[0.3,1]][idx],
                    'yaxis2.domain': [[0.15,0.85],[0.3,1],[0.3,1]][idx],
                    'yaxis3.domain': [[0.15,0.85],[0.3,1],[0.3,1]][idx],
                    'legend.y':[0.82,0.92,0.92][idx]
                  }
                  if(rel_but_sel === 0){
                    Plotly.addTraces(el.id,[pred12,pred13,pred23][el.bipl5.selected])
                }
                if(rel_but_sel === 1){
                  var update_traces = [];
                  el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta[0] === 'PredTable') {
                          update_traces.push(index);
                      }
                  });
                  Plotly.deleteTraces(el.id,update_traces)
                }

                el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                Plotly.relayout(el.id,update);
                return;
              }
              if(d.button.name === 'vecload'){
              // that is need to insert/delete red circle and vectors
                if(rel_but_sel === 0){
                    //need to insert vects
                    // first remove prediction lines
                    if(el.bipl5.clicked){
                        var remove = [];
                        el.data.forEach(function (item, index, arr) {

                          if (arr[index].meta === 'predict') {
                             remove.push(index);
                          }
                        });
                    removeAnnot();
                    Plotly.deleteTraces(el.id, remove);
                    el.bipl5.clicked=false;
                    }

                    // next we need to insert red circle and vects pappa
                    var update = {
                      visible: true
                    };

                    for(i=0; i<el.data.length; i++){
                        if(el.data[i].meta[0]==='veccircle'){
                            el.data[i].visible=true;
                        }
                    }


                    for(i=0; i<el.layout.annotations.length; i++){
                        if(el.layout.annotations[i].meta !== 'vecload'){
                          el.layout.annotations[i].visible = false;
                        } else {
                          el.layout.annotations[i].visible = true;
                        }
                    }


                    //Plotly.restyle(el.id, update);


                    el.bipl5.vect_visible = 1;

                    //alright pappa now need to take away axes

                    var tr_index = []
                    for(let i = 0; i<el.data.length; i++){
                        if(el.data[i].meta === 'axis'){
                        tr_index.push(i)
                        }
                        if(el.data[i].meta[0] === 'axis'){
                          tr_index.push(i)
                        }
                    }
                    var trace_update = {
                        visible: false
                    }
                    Plotly.restyle(el.id,trace_update,tr_index)
                    el.bipl5.rel_but[d.menu._index-1] = [1,0][rel_but_sel];
                }
                if(rel_but_sel === 1){
                  //need to remove vects and insert axes once more
                    var update = {
                      visible: true
                    };

                    var tr_index = []
                    for(let i = 0; i<el.data.length; i++){
                        if(el.data[i].meta === 'axis'){
                        tr_index.push(i)
                        }
                        if(el.data[i].meta[0] === 'axis'){
                          tr_index.push(i)
                        }
                        if(el.data[i].meta[0]==='veccircle'){
                            el.data[i].visible=false;
                        }
                    }

                    el.bipl5.vect_visible=0;
                    myFunction();
                    Plotly.restyle(el.id,update,tr_index)
                    el.bipl5.arr1.fill(0);
                }

                el.bipl5.rel_but[el.bipl5.but_names.indexOf(d.button.name)] = [1,0][rel_but_sel];
                el.bipl5.clicked=false;
                return;
              }
        });

//------------HOVER EVENT--------------------

      el.on('plotly_hover',function(dat){

        if(dat.points[0].data.meta !== 'ClassMean'){
          return;
        }

        var n = el.data.length;
        var idx = [];
        for(var i =0; i<n; i++){
          if(el.data[i].legendgroup==='data'){
            idx.push(i);
          }
        }
        var idx2 = [];
        for(var i=0; i<idx.length; i++){

            if(i !==dat.points[0].customdata){
              idx2.push(idx[i]);
            }
        }
        var update = {
          opacity : 0.2
        };
        Plotly.restyle(el.id, update, idx2);
      });



      el.on('plotly_unhover',function(dat){
      //return false;


        if(dat.points[0].data.meta !== 'ClassMean'){
          return;
        }

        const n = el.data.length;
        var idx = [];
        for(var i =0; i<n; i++){
          if(el.data[i].legendgroup==='data'){
            idx.push(i);
          }
        }
        idx.splice(dat.points[0].customdata,1)
        var update = {
          opacity : 1
        };
        Plotly.restyle(el.id, update, idx);
      });


//------------LEGENDCLICK--------------------

       el.on('plotly_legendclick', function(dat){

          if(dat.event.detail===2){
            return false;
          }


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
            for(let i = 0; i < data.p; i++){
                el.layout.annotations.pop();
            }
            Plotly.deleteTraces(el.id, remove);
            el.bipl5.clicked = false;
            return false;
         }

          if(dat.data[dat.curveNumber].meta[0] === 'data'){
            var a = ['legendonly', true].indexOf(dat.data[dat.curveNumber].visible)
            var update = {
              visible: [true, 'legendonly'][a]
            }
            Plotly.restyle(el.id,update,dat.curveNumber);
          return false;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'density'){
          return;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'polygon'){
            var a = ['legendonly', true].indexOf(dat.data[dat.curveNumber].visible)
            var update = {
              visible: [true, 'legendonly'][a]
            }
            Plotly.restyle(el.id,update,dat.curveNumber);
          return false;
          }

          if(dat.data[dat.curveNumber].meta[0] === 'axis_pred'){
            return;
          }




          // REMOVE AXES

          var axis = dat.data[dat.curveNumber].legendgroup;
          var num = Number(axis.replace('Ax',''));
          var indeces =[];

          el.data.forEach(function(item,idx,arr){
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
            old_annotations.forEach(function(item,idx,arr){
                if(arr[idx].customdata === num){
                  old_annotations[idx].visible = !old_annotations[idx].visible;
                }
            });

          hidden = el.bipl5.arr1[num-1];
          var update = {'visible': ['legendonly',true][hidden]};
          hidden = [1,0][hidden];
          el.bipl5.arr1[num-1] = hidden;
          var new_annot = {annotations:old_annotations};
          Plotly.update(el.id,update,new_annot,indeces);

          return false;
        });


//-------------------Legend doubleclick pappa------------



//-------------------POINTS CLICK--------------

       el.on('plotly_click', function(d) {
       console.log(d);

       if(d.points[0].meta === 'density'){
          return;
       }
       if(d.points[0].data.meta === 'ClassMean'){
          return;
       }
       if(el.bipl5.vect_visible ===1){
          return;
       }


  //-----------------PREDICTION LINES--------------

         if(el.bipl5.clicked){

         var remove = [];
            el.data.forEach(function (item, index, arr) {

                      if (arr[index].meta === 'predict') {
                          remove.push(index);
                      }
                });
            Plotly.deleteTraces(el.id, remove);
            for(let i = 0; i < data.p; i++){
                el.layout.annotations.pop();
            }

         }
         var X = [];
         var Y = [];

         var traces_to_be_added = [];
         for (let i = 0; i < data.p; i++) {
            var c = d.points[0].y+1/data.m[i] * d.points[0].x;
            var x_new = c/(data.m[i]+1/data.m[i]);
            var y_new = data.m[i]*x_new;
            var showleg = false;
            if(i === (data.p-1)){showleg = true;}
            X.push(x_new);
            Y.push(y_new);
            var newtrace = {
                x: [d.points[0].x, x_new],
                y: [d.points[0].y, y_new],
                mode: 'lines+markers',
                xaxis: 'x',
                yaxis: 'y',
                showlegend: showleg,
                visible: [true,'legendonly'][el.bipl5.arr1[i]],
                name: 'Predicted Value',
                legendgroup:'Ax'+(i+1),
                meta: 'predict',
                line: {
                  dash: 'dot',
                  color: 'gray',
                  width: 1
                      },
                marker: {
                  color:'gray',
                  size: [1,6]
                }
            };
            var newAnnotation = {
                x: x_new,
                y: y_new,
                text: data.Xhat[d.points[0].customdata-1][i].toFixed(2),
                showarrow: false,
                textangle: -Math.atan(data.m[i])*180/Math.PI,
                xshift: -10*Math.sin(Math.atan(data.m[i])),
                yshift: 10*Math.cos(Math.atan(data.m[i])),
                name: 'Predicted Value',
                meta: 'predict',
                visible: [true,false][el.bipl5.arr1[i]],
                customdata: i+1,
                font: {
                  size:10,
                  color:data.cols[i]
                }
            }
            traces_to_be_added.push(newtrace)
            el.layout.annotations.push(newAnnotation);

         }
         Plotly.addTraces(el.id, traces_to_be_added);
        el.bipl5.clicked=true;

       });



}

   ",data=list(m=m,Xhat=Xhat,p=p,cols=cols))
}



#' Get gradients at each point
#'
#' @param z z axes coordinates
#'
#' @noRd
get_gradients<-function(z){
  p<-nrow(z)
  m<-(z[2:(p-1)+1,2]-z[2:(p-1)-1,2])/(z[2:(p-1)+1,1]-z[2:(p-1)-1,1])
  m<-c(NA,m,NA)
  return(m)
}


#' Insert Linear Axes to biplot display
#'
#' @param z.axes Axes coordinates and tick marks
#' @param x biplotEZ object
#' @param p_ly plotly graph
#'
#' @return list containing plotly object, and vector of gradients
#' @noRd
insert_linear_axes<-function(z.axes,x,p_ly){
  p<-x$p
  radius<-max(abs(x$Z))*1.2
  theta<-seq(0,2*pi,length.out=200)
  elipcoords<-cbind(radius*cos(theta),radius*sin(theta))
  z.axes<-check_inside_circle(z.axes,radius,NULL)
  grads<-numeric()
  for(i in 1:p){
    AxName<-paste("<b>",colnames(x$X)[i],"</b>")
    endp<-z.axes[[i]][which.max(z.axes[[i]][,3]),1:2]
    pos<-"right"
    m<-(z.axes[[i]][2,2]-z.axes[[i]][1,2])/(z.axes[[i]][2,1]-z.axes[[i]][1,1])
    grads[i]<-m
    angle<-atan(m)
    if(endp[1]<0){
      pos<-"left"
      angle<-angle-pi
    }

    titles<-c("<b>Axes</b>",rep("",p-1))
    p_ly<-p_ly |>
      add_trace(x=c(radius*cos(atan(m)),radius*cos(atan(m)-pi)),
                y=c(radius*sin(atan(m)),radius*sin(atan(m)-pi)),
                type="scatter",
                mode="lines",line = list(color = x$axes$col[i],
                                         width=1,simplify=FALSE),
                name=colnames(x$X)[i],legendgroup=paste("Ax",i,sep=""),
                meta='axis',xaxis="x",yaxis="y",customdata=m,
                visible=TRUE,hoverinfo="name",
                legendgrouptitle=list(text=titles[i]))|>

      add_annotations(x=z.axes[[i]][,1],y=z.axes[[i]][,2],
                      text=as.character(z.axes[[i]][,3]),
                      showarrow=FALSE,textangle=-atan(m)*180/pi,
                      visible=TRUE,yshift=-12*cos(atan(m)),
                      xshift=12*sin(atan(m)),meta='axis',
                      xaxis="x",yaxis="y",customdata=i,
                      font=list(size=10,color=x$axes$tick.label.col[i]))|>
      add_annotations(x=z.axes[[i]][,1],y=z.axes[[i]][,2],
                      text="&#124;",
                      showarrow=FALSE,textangle=-atan(m)*180/pi,
                      visible=TRUE,meta='axis',
                      xaxis="x",yaxis="y",customdata=i,
                      font=list(size=8,color=x$axes$tick.col[i]))|>
      add_trace(x=radius*cos(angle),y=radius*sin(angle),
                text=AxName,type="scatter",mode="text",textposition=pos,
                legendgroup=paste("Ax",i,sep=""),showlegend=FALSE,
                textfont=list(size=12,color="gray"),
                meta='axis',xaxis="x",yaxis="y",visible=TRUE)
  }


  p_ly<-p_ly|> add_trace(x=elipcoords[,1],y=elipcoords[,2], type="scatter",
                         mode="lines",line = list(color = 'green',width=0.6),
                         name="circle",showlegend=F,
                         meta='circle',xaxis="x",yaxis="y",
                         visible=TRUE,hoverinfo="none")

  return(list(p_ly,grads))
}





