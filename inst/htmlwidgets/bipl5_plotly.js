(function () {
  window.bipl5Attach = function (el, x, data) {
    el.bipl5 = {
      clicked: false,
      unit_circle: 0,
      arr1: new Array(data.p).fill(0),
      active: 0,
      rel_but: [0, 0, 0, 0],
      is_visible: 0,
      selected: 0,
      bip_domain: [0, 1],
      table_visible: 0,
      table2_visible: 1,
      vect_visible: 0,
      but_names: ["PC", "AxisStats", "TransAxes", "vecload"],
    };

    console.log(el.data);
    function metaTag(tr) {
      if (Array.isArray(tr.meta)) return tr.meta[0];
      if (typeof tr.meta === "string") return tr.meta;
      return null;
    }
    function toggleButton(buttonName) {
      const i = el.bipl5.but_names.indexOf(buttonName);
      if (i === -1) return null;
      el.bipl5.rel_but[i] = 1 - el.bipl5.rel_but[i];
    }
    function RemovePredictions() {
      if (!el.bipl5.clicked) return false;
      var remove = [];
      el.data.forEach(function (tr, i, arr) {
        if (arr[i].meta === "predict") remove.push(i);
      });
      if (remove.length){
        removeAnnotation('predict');
        Plotly.deleteTraces(el.id, remove);
      }
      el.bipl5.clicked = false;
      return true;
    }

    var All_annot = el.layout.annotations;
    function myFunction() {
      for (let i = 0; i < All_annot.length; i++) {
        All_annot[i].visible = !All_annot[i].visible;
      }
    }

    function searchAnnot(item, vis){
      for (let i = 0; i < el.layout.annotations.length; i++) {
        let tag = metaTag(el.layout.annotations[i]);
        if(tag === item){
          el.layout.annotations[i].visible = vis;
        }
      }
    }

    function removeAnnot() {
      for (let i = 0; i < data.p; i++) {
        el.layout.annotations.pop();
      }
    }

    function removeAnnotation(item) {
      for (let i = (el.layout.annotations.length-1); i>= 0; i--) {
        let tag = metaTag(el.layout.annotations[i]);
        if(tag === item){
          el.layout.annotations.splice(i, 1);
        }
      }
    }


//-------------- UPDATEMENU-----------------

    el.on("plotly_buttonclicked", function (d) {
      // toggle selectibility

      var rel_but_sel =
        el.bipl5.rel_but[el.bipl5.but_names.indexOf(d.button.name)];
      if (d.button.name === "AxisStats") {
        // that is, the axis predictivity is to be inserted
        var update = {
          "xaxis.domain": [
            [0, 0.5],
            [0, 1],
          ][el.bipl5.is_visible],
          "yaxis3.zeroline": true,
        };
        el.bipl5.bip_domain[1] = [0.5, 1][el.bipl5.is_visible];
        var update_traces = [];
        el.data.forEach(function (item, index, arr) {
          if (arr[index].meta[0] === "axis_pred") {
            update_traces.push(index);
          }
        });

        var plot_update = {
          visible: [true, false][el.bipl5.is_visible],
          xaxis: ["x3", "x"][el.bipl5.is_visible],
          yaxis: ["y3", "y"][el.bipl5.is_visible],
        };
        el.bipl5.is_visible = [1, 0][el.bipl5.is_visible];
        Plotly.restyle(el.id, plot_update, update_traces);
        toggleButton(d.button.name);
        Plotly.relayout(el.id, update);
        return;
      }

      if (d.button.name === "regmaak") {
        // that is the fit measures table needs to be inserted
        var idx = el.bipl5.table_visible + el.bipl5.table2_visible;
        el.bipl5.table2_visible = [1, 0][el.bipl5.table2_visible];
        var update = {
          "updatemenus[2].active": [0, 1][rel_but_sel],
          "yaxis.domain": [
            [0, 1],
            [0.3, 1],
            [0.3, 1],
          ][idx],
          "yaxis2.domain": [
            [0.15, 0.85],
            [0.3, 1],
            [0.3, 1],
          ][idx],
          "yaxis3.domain": [
            [0.15, 0.85],
            [0.3, 1],
            [0.3, 1],
          ][idx],
          "legend.y": [0.82, 0.92, 0.92][idx],
        };
        if (rel_but_sel === 0) {
          Plotly.addTraces(el.id, [pred12, pred13, pred23][el.bipl5.selected]);
        }
        if (rel_but_sel === 1) {
          var update_traces = [];
          el.data.forEach(function (item, index, arr) {
            if (arr[index].meta[0] === "PredTable") {
              update_traces.push(index);
            }
          });
          Plotly.deleteTraces(el.id, update_traces);
        }

        el.bipl5.rel_but[d.menu._index - 1] = [1, 0][rel_but_sel];
        Plotly.relayout(el.id, update);
        return;
      }

      if (d.button.name === "TransAxes") {
        // that is need to swop between normal axes and translated ones
        //
        if (rel_but_sel === 0) {
          // First we remove any prediction lines
          RemovePredictions();
          //next we need to remove the circle and current axes
          //simultaneously sommer visible the ExpAxes
          var ax_hide = [];
          var exp_ax_hide = [];
          for (let i = 0; i < el.data.length; i++) {
            let tag = metaTag(el.data[i]);
            if (tag === "axis" || tag === "OuterCircle") ax_hide.push(i);
            if (tag === "ExpAx" || tag === 'density') exp_ax_hide.push(i);
          }

          var ax_update = {
            visible: false,
          };
          var exp_ax_update = {
            visible: true,
          };

          //haal uit al die annotation
          for (i = 0; i < el.layout.annotations.length; i++) {
            if (el.layout.annotations[i].meta !== "ExpAx") {
              el.layout.annotations[i].visible = false;
            } else {
              el.layout.annotations[i].visible = true;
            }
          }
          //searchAnnot("vecload",false);
          el.bipl5.ax_hide = ax_hide;
          el.bipl5.exp_ax_hide = exp_ax_hide;

          // Sit Exploding asse in

          Plotly.restyle(el.id, ax_update, ax_hide);
          Plotly.restyle(el.id, exp_ax_update, exp_ax_hide);

          toggleButton(d.button.name);

          return;
        }

        if (rel_but_sel === 1) {
          // First we remove any prediction lines or red circle
          if (el.bipl5.clicked) {
            var remove = [];
            el.data.forEach(function (item, index, arr) {
              if (arr[index].meta === "predict") {
                remove.push(index);
              }
              if (arr[index].meta === "veccircle") {
                remove.push(index);
              }
            });
            removeAnnot();
            Plotly.deleteTraces(el.id, remove);
            el.bipl5.clicked = false;
          }
          for (i = 0; i < el.layout.annotations.length; i++) {
            if (el.layout.annotations[i].meta === "ExpAx") {
              el.layout.annotations[i].visible = false;
            } else {
              el.layout.annotations[i].visible = true;
            }
          }
          searchAnnot('vecload', false);

          var exp_ax_update = {
            visible: false,
          };
          var ax_update = {
            visible: true,
          };

          Plotly.restyle(el.id, exp_ax_update, el.bipl5.exp_ax_hide);
          Plotly.restyle(el.id, ax_update, el.bipl5.ax_hide);
          toggleButton(d.button.name);
          return;
        }
      }

      if (d.button.name === "vecload") {
        // that is need to insert/delete red circle and vectors
        if (rel_but_sel === 0) {
          //need to insert vects
          // first remove prediction lines
          RemovePredictions();

          // next we need to insert red circle and vects pappa
          var update = {
            visible: true
          };

          for (i = 0; i < el.data.length; i++) {
            if (el.data[i].meta[0] === "veccircle") {
              el.data[i].visible = true;
            }
          }
          //take out all axis tickmarks - ExpAx stays
          searchAnnot("axis",false);
          //make arrows vect_visible
          searchAnnot("vecload",true);


          //Plotly.restyle(el.id, update);

          el.bipl5.vect_visible = 1;

          //alright pappa now need to take away axes

          var tr_index = [];
          for (let i = 0; i < el.data.length; i++) {
            let tag = metaTag(el.data[i]);
            if (tag === "axis") {
              tr_index.push(i);
            }
          }
          var trace_update = {
            visible: false
          };
          Plotly.restyle(el.id, trace_update, tr_index);
          toggleButton(d.button.name);
        }
        if (rel_but_sel === 1) {
          //need to remove vects and insert axes once more
          var tr_index = [];
          var update = {
            visible: false
          };
          if(el.bipl5.rel_but[el.bipl5.but_names.indexOf("TransAxes")] === 0){
            for (let i = 0; i < el.data.length; i++) {
              let tag = metaTag(el.data[i]);
              if (tag === "axis") {
                el.data[i].visible = true;
              }
              if (tag === "OuterCircle") {
                el.data[i].visible = true;
              }
            }
            //dan net vecload annotations
            searchAnnot('axis',true);

          }
          for (let i = 0; i < el.data.length; i++) {
            let tag = metaTag(el.data[i]);
            if (tag === "veccircle") {
              tr_index.push(i);
            }
          }
          searchAnnot('vecload',false);

          el.bipl5.vect_visible = 0;

          Plotly.restyle(el.id, update, tr_index);
          el.bipl5.arr1.fill(0);
          toggleButton(d.button.name);



        el.bipl5.clicked = false;
        return;
      }
      }
    });

//------------HOVER EVENT--------------------
    if(data.class_mean_hover){
      el.on("plotly_hover", function (dat) {
        if (dat.points[0].data.meta !== "ClassMean") {
        return;
      }

        var n = el.data.length;
        var idx = [];
        for (var i = 0; i < n; i++) {
          if (el.data[i].legendgroup === "data") {
            idx.push(i);
          }
        }
        var idx2 = [];
        for (var i = 0; i < idx.length; i++) {
          if (i !== dat.points[0].customdata) {
            idx2.push(idx[i]);
          }
        }
        var update = {
          opacity: 0.2,
        };
        Plotly.restyle(el.id, update, idx2);
      });

      el.on("plotly_unhover", function (dat) {
        if (dat.points[0].data.meta !== "ClassMean") {
          return;
        }

        const n = el.data.length;
        var idx = [];
        for (var i = 0; i < n; i++) {
          if (el.data[i].legendgroup === "data") {
            idx.push(i);
          }
        }
        idx.splice(dat.points[0].customdata, 1);
        var update = {
          opacity: 1,
        };
        Plotly.restyle(el.id, update, idx);
      });
    }

//------------LEGENDCLICK--------------------

    el.on("plotly_legendclick", function (dat) {
      if (dat.event.detail === 2) {
        return false;
      }

      // Delete predictive lines
      // NOTE: this must come first before rest otherwise error
      if (dat.data[dat.curveNumber].meta === "predict") {
        var remove = [];
        el.data.forEach(function (item, index, arr) {
          if (arr[index].meta === "predict") {
            remove.push(index);
          }
        });
        //remove prediction lines annotations as well
        removeAnnotation('predict');
        Plotly.deleteTraces(el.id, remove);
        el.bipl5.clicked = false;
        return false;
      }

      if (dat.data[dat.curveNumber].meta[0] === "data") {
        var a = ["legendonly", true].indexOf(dat.data[dat.curveNumber].visible);
        var update = {
          visible: [true, "legendonly"][a],
        };
        Plotly.restyle(el.id, update, dat.curveNumber);
        return false;
      }
      if (dat.data[dat.curveNumber].meta[0] === "density") {
        return;
      }
      if (dat.data[dat.curveNumber].meta[0] === "polygon") {
        var a = ["legendonly", true].indexOf(dat.data[dat.curveNumber].visible);
        var update = {
          visible: [true, "legendonly"][a],
        };
        Plotly.restyle(el.id, update, dat.curveNumber);
        return false;
      }

      if (dat.data[dat.curveNumber].meta[0] === "axis_pred") {
        return;
      }

      // REMOVE AXES

      var axis = dat.data[dat.curveNumber].legendgroup;
      var num = Number(axis.replace("Ax", ""));
      var indeces = [];

      el.data.forEach(function (item, idx, arr) {
        if (arr[idx].legendgroup === undefined) {
          return;
        }
        if (arr[idx].legendgroup === axis) {
          indeces.push(idx);
        }
        if (arr[idx].customdata === undefined) {
          return;
        }
        if (arr[idx].customdata[0] === axis) {
          indeces.push(idx);
        }
      });

      var old_annotations = el.layout.annotations;
      old_annotations.forEach(function (item, idx, arr) {
        if (arr[idx].customdata === num) {
          old_annotations[idx].visible = !old_annotations[idx].visible;
        }
      });

      hidden = el.bipl5.arr1[num - 1];
      var update = { visible: ["legendonly", true][hidden] };
      hidden = [1, 0][hidden];
      el.bipl5.arr1[num - 1] = hidden;
      var new_annot = { annotations: old_annotations };
      Plotly.update(el.id, update, new_annot, indeces);

      return false;
    });

//-------------------Legend doubleclick pappa------------

//-------------------POINTS CLICK--------------



    el.on("plotly_click", function (d) {
      if (d.points[0].meta === "density") {
        return false;
      }
      if (d.points[0].data.meta === "ClassMean") {
        return false;
      }
      if (el.bipl5.vect_visible === 1) {
        return false;
      }

      //-----------------PREDICTION LINES--------------

      if (el.bipl5.clicked) {
        var remove = [];
        el.data.forEach(function (item, index, arr) {
          if (arr[index].meta === "predict") {
            remove.push(index);
          }
        });
        Plotly.deleteTraces(el.id, remove);
        removeAnnotation('predict');
      }
      var X = [];
      var Y = [];

      var traces_to_be_added = [];
      for (let i = 0; i < data.p; i++) {
        var c = d.points[0].y + (1 / data.m[i]) * d.points[0].x;
        var x_new = c / (data.m[i] + 1 / data.m[i]);
        var y_new = data.m[i] * x_new;
        var showleg = false;
        if (i === data.p - 1) {
          showleg = true;
        }
        X.push(x_new);
        Y.push(y_new);
        var newtrace = {
          x: [d.points[0].x, x_new],
          y: [d.points[0].y, y_new],
          mode: "lines+markers",
          xaxis: "x",
          yaxis: "y",
          showlegend: showleg,
          visible: [true, "legendonly"][el.bipl5.arr1[i]],
          name: "Predicted Value",
          legendgroup: "Ax" + (i + 1),
          meta: "predict",
          line: {
            dash: "dot",
            color: "gray",
            width: 1,
          },
          marker: {
            color: "gray",
            size: [1, 6],
          },
        };
        var newAnnotation = {
          x: x_new,
          y: y_new,
          text: data.Xhat[d.points[0].customdata - 1][i].toFixed(2),
          showarrow: false,
          textangle: (-Math.atan(data.m[i]) * 180) / Math.PI,
          xshift: -10 * Math.sin(Math.atan(data.m[i])),
          yshift: 10 * Math.cos(Math.atan(data.m[i])),
          name: "Predicted Value",
          meta: "predict",
          visible: [true, false][el.bipl5.arr1[i]],
          customdata: i + 1,
          font: {
            size: 10,
            color: data.cols[i],
          },
        };
        traces_to_be_added.push(newtrace);
        el.layout.annotations.push(newAnnotation);
      }
      Plotly.addTraces(el.id, traces_to_be_added);
      el.bipl5.clicked = true;
    });
  };
})();
