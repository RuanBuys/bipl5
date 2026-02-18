(function () {
  window.bipl5Attach = function (el, x, data) {
    el.bipl5 = {
      clicked: false, //helps keep trac if an observation is clicked
      rel_but: [0, 0, 0, 0], //flags to keep track which buttons have been clicked
      is_visible: true,
      vect_visible: 0,
      but_names: ["PC", "AxisStats", "TransAxes", "vecload"]
    };


    Object.keys(data.payloads).forEach(k => {
      data.payloads[k].bipl5 = deepClone(el.bipl5)

    });

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

    function deepClone(obj) {
      return JSON.parse(JSON.stringify(obj));
    }

    el.bipl5 = el.bipl5 || {};
    el.bipl5.currentPCKey = el.bipl5.currentPCKey || "PC 1 & 2";

    function togglePC(d){
      // new selection
        const newKey = d.button && (d.button.name || d.button.label);
        if (!newKey) return;

        // ignore if user re-clicks same dropdown option
        const oldKey = el.bipl5.currentPCKey || "PC 1 & 2";
        if (newKey === oldKey) return;
        data.payloads = data.payloads || {};

        // ---- A) capture CURRENT RHS (fit panel) state BEFORE we switch ----
        const fitPanelActive =
    !!(el.layout.updatemenus && el.layout.updatemenus[2] && el.layout.updatemenus[2].visible);

        const fmKey = el.bipl5.currentFMKey || "Cum. Predictivity";
        const showingSummary = fitPanelActive && (fmKey === "Summary Table");

        // keep current RHS traces unless we are in the Summary Table corner case
        const currentFitPanelTraces = deepClone(el.data.filter(isFitPanelTrace));


        // ---- B) save CURRENT LHS (biplot) into payloads[oldKey]
        const prev = data.payloads[oldKey] || {};
        const curBiplotTraces = deepClone(el.data.filter(tr => !isFitPanelTrace(tr)));


        // Save CURRENT state into payloads[oldKey]
        // (this is where "PC 1 & 2" gets created the first time)


        data.payloads[oldKey] = Object.assign({}, prev, {
          trace_data: curBiplotTraces,
          layout: deepClone(el.layout),
          bipl5: deepClone(el.bipl5)
        });


        // ----C) Load the NEW payload for LHS display
        const nextPayload = data.payloads[newKey];
        if (!nextPayload) return; // nothing to switch to

        const next_bipl5=deepClone(nextPayload.bipl5);
        next_bipl5.is_visible=deepClone(el.bipl5.is_visible);
        next_bipl5.currentFMKey=deepClone(el.bipl5.currentFMKey || "Cum. Predictivity");
        el.bipl5=next_bipl5;



        const nextBiplotTraces = deepClone(nextPayload.trace_data || []);
        // Build RHS traces to carry over
        let nextFitPanelTraces = currentFitPanelTraces;

        // Corner case: Summary Table must update when PC changes
        if (showingSummary) {
          const tableTraces = nextPayload.fit_table;
          nextFitPanelTraces = deepClone(tableTraces);
        }
         // ---- D) merge layout: need to change title and button names
        var newLayout = Object.assign({}, el.layout || {});
        newLayout.annotations = deepClone((nextPayload.layout && nextPayload.layout.annotations) || []);
        newLayout.xaxis.title = deepClone((nextPayload.layout && nextPayload.layout.xaxis.title) || []);
        newLayout.xaxis.autorange=true;
        newLayout.yaxis.autorange=true;
//        if(nextPayload.layout.updatemenus[0].buttons){
//          console.log(nextPayload.layout.updatemenus[0].buttons)
//          newLayout.updatemenus[0].buttons=deepClone(nextPayload.layout.updatemenus[0].buttons);
//        } else {
//          newLayout.updatemenus[0].buttons[1].label = "Translated Axes"
//        }

        // ---- E) one redraw ----
        const newData = nextBiplotTraces.concat(nextFitPanelTraces);

        // 3) Switch the plot
        Plotly.react(el, newData, newLayout);

        // 4) Update current key
        el.bipl5.currentPCKey = newKey;

    }



    function isFitPanelTrace(tr) {
      // meta can be ["FitPanel", "..."] or "FitPanel"
      return hasMeta(tr, "FitPanel");
    }

    function fitPanelIndices() {
      const idx = [];
      for (let i = 0; i < el.data.length; i++) {
        if (isFitPanelTrace(el.data[i])) idx.push(i);
      }
      return idx;
    }

    function removeFitPanelTraces() {
      const idx = fitPanelIndices();
      if (!idx.length) return Promise.resolve();
      // delete from highest -> lowest is safest
      idx.sort((a, b) => b - a);
      return Plotly.deleteTraces(el, idx);
    }

    function getFitTracesByKey(key) {
      const pcKey = el.bipl5.currentPCKey || "PC 1 & 2";
      const payload = data.payloads?.[pcKey];

      // pick the right source
      if (key === "Cum. Predictivity") return data.fm_payload.CumPred;
      if (key === "Cum. Adequacy")     return data.fm_payload.CumAd;
      if (key === "Scree Plot")        return data.fm_payload.Scree;
      if (key === "Variance Explained")return data.fm_payload.VarExp;
      if (key === "Summary Table")     return payload?.fit_table;

      return null;
    }



    function toggleFit(d){

      const newKey = d.button && (d.button.name || d.button.label);
        if (!newKey) return false;

      // ignore if user re-clicks same dropdown option
      var oldKey = el.bipl5.currentFMKey || "Cum. Predictivity";
      if (newKey === oldKey) return false;


      // 1) Remove existing FitPanel traces from CURRENT plot state
      const baseData = el.data.filter(tr => !isFitPanelTrace(tr));

      // 2) Prepare new traces
      const tracesToAdd = deepClone(getFitTracesByKey(newKey));
      if (!tracesToAdd || !tracesToAdd.length) return false;

      // 3) Build new data + layout for react (single redraw)
      const newData = baseData.concat(tracesToAdd);

      const newLayout = deepClone(el.layout);


      // yaxis3 scaling rule: fixed [0,1] except Scree Plot (autorange)
      if (newKey === "Scree Plot") {
        newLayout.yaxis3 = Object.assign({}, newLayout.yaxis3, { autorange: true });
      } else {
        newLayout.yaxis3 = Object.assign({}, newLayout.yaxis3, { autorange: false, range: [0, 1] });
      }

      return Plotly.react(el, newData, newLayout).then(() => {
              el.bipl5.currentFMKey = newKey;
              });
    }

function getButtonIndex(d) {
  if (d && d.button && Number.isFinite(d.button._index)) return d.button._index;
  if (d && d.menu && Number.isFinite(d.menu.active)) return d.menu.active;

  const key = d.button && (d.button.name || d.button.label);
  const btns = d.menu && d.menu.buttons;
  if (!key || !Array.isArray(btns)) return -1;

  for (let i = 0; i < btns.length; i++) {
    const b = btns[i];
    const k = b && (b.name || b.label);
    if (k === key) return i;
  }
  return -1;
}

function ensureSliderInfo(payload, p, defaultActive) {
  payload.slider_info = payload.slider_info || {};

  const si = payload.slider_info;

  if (!Array.isArray(si.slider_pos)) {
    si.slider_pos = new Array(p).fill(defaultActive);
  } else if (si.slider_pos.length !== p) {
    const tmp = new Array(p).fill(defaultActive);
    for (let i = 0; i < Math.min(p, si.slider_pos.length); i++) tmp[i] = si.slider_pos[i];
    si.slider_pos = tmp;
  }

  if (!Number.isFinite(si.slider_axis_idx)) {
    si.slider_axis_idx = 0;
  }
}

function toggleSlider(d) {
  const pcKey = el.bipl5.currentPCKey || "PC 1 & 2";
  data.payloads = data.payloads || {};
  const payload = data.payloads[pcKey] || (data.payloads[pcKey] = {});

  const p = data.p;

  const sliderActiveNow =
    (el.layout.sliders && el.layout.sliders[0] && Number.isFinite(el.layout.sliders[0].active))
      ? el.layout.sliders[0].active
      : 0;

  ensureSliderInfo(payload, p, sliderActiveNow);

  const si = payload.slider_info;

  // Selected axis name (button)
  const axisName = d.button && (d.button.name || d.button.label);
  if (!axisName) return false;

  // Axis index in dropdown buttons (0-based)
  const newAxisIdx = getButtonIndex(d);
  if (newAxisIdx < 0 || newAxisIdx >= p) return false;

  // 1) Save current slider step for previously selected axis
  const oldAxisIdx = si.slider_axis_idx;
  si.slider_pos[oldAxisIdx] = sliderActiveNow;

  // 2) Switch selected axis
  si.slider_axis_idx = newAxisIdx;

  // 3) Load saved slider step for new axis
  const nextActive = si.slider_pos[newAxisIdx];

  // 4) Update slider UI
  const relayoutPatch = {
    "sliders[0].active": nextActive,
    "sliders[0].currentvalue.prefix": `Axis: ${axisName}  `
  };

  return Plotly.relayout(el, relayoutPatch);
}


    function hasMeta(tr, tag) {
      if (!tr) return false;
      const m = tr.meta;
      if (Array.isArray(m)) return m.includes(tag);
      return m === tag;
    }

    function switch_fit_panel(){
      const show = el.bipl5.is_visible;

      if (show) {
        const add = deepClone(getFitTracesByKey("Cum. Predictivity"));
        if (!add || !add.length) return false;
        const newData = el.data.concat(add);
        const newLayout = Object.assign({}, el.layout, {
          xaxis: Object.assign({}, el.layout.xaxis, { domain: [0, 0.5] })
        });

        newLayout.updatemenus[2].visible=true;
        newLayout.sliders[0].len=0.5;
        newLayout.yaxis3.zeroline=true;

        Plotly.react(el, newData, newLayout).then(() => {
        el.bipl5.is_visible = false;
        });

        return false;
        } else {

          // hide: remove fitpanel traces from el.data yourself, then react
          const keep = el.data.filter(tr => !hasMeta(tr, "FitPanel"));
          const newLayout = Object.assign({}, el.layout, {
            xaxis: Object.assign({}, el.layout.xaxis, { domain: [0, 1] })
          });
          newLayout.updatemenus[2].visible=false;
          newLayout.yaxis3.zeroline=true;
          newLayout.updatemenus[2].active=0;
          newLayout.sliders[0].len=1;

          Plotly.react(el, keep, newLayout).then(() => {
          el.bipl5.is_visible = true;
          el.bipl5.currentFMKey = "Cum. Predictivity";
        });

        return false;
        }
    }
//-------------- UPDATEMENU-----------------

    el.on("plotly_buttonclicked", function (d) {
      // toggle selectibility
      if(d.menu.type==="dropdown"){
        if(d.menu.name==="PC_toggle"){
          togglePC(d);
          return;
        }
        if(d.menu.name==="Fit_toggle"){
          toggleFit(d);
        }

        if(d.menu.name==="Slider_toggle"){
          toggleSlider(d);
        }

      }

      var rel_but_sel =
        el.bipl5.rel_but[el.bipl5.but_names.indexOf(d.button.name)];
      if (d.button.name === "AxisStats") {
        switch_fit_panel();
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
          var old_axes_visible = []
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
          el.layout.sliders[0].visible=true;
          el.layout.updatemenus[3].visible=true;
          //searchAnnot("vecload",false);
          el.bipl5.ax_hide = ax_hide;
          el.bipl5.exp_ax_hide = exp_ax_hide;

          // Sit Exploding asse in
          //voor einde maak label van die knop na teenoorgestelde
          const index = d.button._index;
          el.layout.updatemenus[0].buttons[index].label = "Centered Axes";
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

          el.layout.sliders[0].visible=false;
          el.layout.updatemenus[3].visible=false;

          searchAnnot('vecload', false);

          var exp_ax_update = {
            visible: false,
          };
          var ax_update = {
            visible: true,
          };
          //voor einde maak label van die knop na teenoorgestelde
          const index = d.button._index;
          el.layout.updatemenus[0].buttons[index].label = "Translated Axes";

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
          searchAnnot("Ax",false);
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
            searchAnnot('Ax',true);

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


    function hasLegendgroup(tr) {
      return tr && typeof tr.legendgroup === "string" && tr.legendgroup.length > 0;
    }

    function axisNameFromLegendgroup(lg) {
      // expects "Ax<number>"
      if (typeof lg !== "string") return null;
      const m = lg.match(/^(ExpAx|Ax)(\d+)$/);
      return m ? { axis: lg, num: Number(m[2]), type: m[1]} : null;
    }

    function customAxisRef(tr) {
      // you use: tr.customdata[0] === axis
      // so guard for array-like customdata
      if (!tr || !Array.isArray(tr.customdata)) return null;
      return tr.customdata[0] ?? null;
    }

    function toggleAxisAnnot(num,type) {
      const anns = el?.layout?.annotations;
      if (!Array.isArray(anns)) return 0;

      let changed = 0;

      for (let i = 0; i < anns.length; i++) {
        const ann = anns[i];

        if (ann && ann.customdata === num && metaTag(ann)===type) {
          ann.visible = !ann.visible;
          changed++;
        }

        if(ann && metaTag(ann) === 'predict' && ann.customdata === num){
          ann.visible =!ann.visible;
        }
      }
      return changed;
    }

    function toggleLegendOnly(tr) {
      const isShown = (tr.visible === true || tr.visible === undefined); // undefined behaves like visible
    return { visible: isShown ? "legendonly" : true };
    }

    el.on("plotly_legendclick", function (dat) {
      if (dat.event.detail === 2) {
        //is hierdie 'n double click?
        return false;
      }

      const tr = dat?.data?.[dat.curveNumber];
      if (!tr) return false;
      const tag = metaTag(tr);
      // Delete predictive lines
      if (tag === "predict") {
        RemovePredictions()
        removeAnnotation('predict');
        el.bipl5.clicked = false;
        return false;
      }
      //purely toggle a trace

      if (tag === "data" || hasMeta(tr,"FitPanel") || tag === "polygon") {
        var a = ["legendonly", true].indexOf(tr.visible);
        var update = {
          visible: [true, "legendonly"][a],
        };
        Plotly.restyle(el.id, update, dat.curveNumber);
        return false;
      }
      if (metaTag(tr) === "density") {
        const legend_group = tr.legendgroup;
        console.log(legend_group)
        console.log(el.data)
        const indices = [];
        const ax_counter = [];
        const ax_visible =[];
        let j = 1;
        for (let i = 0; i < el.data.length; i++) {
          const t = el.data[i];
          // Same legendgroup
          if(metaTag(t) === "ExpAx"){
            ax_counter.push("ExpAx"+j);
            ax_visible.push(t.visible === true);
            j++;
          }

          if (t && t.legendgroup === legend_group) {
            //if this trace's lg == lg of density chosen (its data class is the same)
            //first check if the axis is on plot, if not = need not worry about it
            //also when unclick this helps
            if(customAxisRef(t)==='legendentry'){
              indices.push(i);
              continue;
            }

            if(ax_visible[ax_counter.indexOf(customAxisRef(t))]){
              indices.push(i);
            }

          }
          if (customAxisRef(t) === legend_group) indices.push(i);
        }
        var update = toggleLegendOnly(tr);
        Plotly.restyle(el.id, update, indices);
        return false;
      }

      // all that remains now are the axes!
      // remove
      if (!hasLegendgroup(tr)) return false;
      const axisInfo = axisNameFromLegendgroup(tr.legendgroup);
      if (!axisInfo) return false;
      const { axis, num ,type} = axisInfo;

       // Collect trace indices to update/hide/show
      const indices = [];
      const group_counter = [];
      const group_visible =[];
      // we reverse the order sothat hit density traces before axes traces
      for (let i = 0; i < el.data.length; i++) {
        const t = el.data[i];
        // Same legendgroup
        if(customAxisRef(t) === 'legendentry'){
          group_counter.push(t.legendgroup);
          group_visible.push(t.visible);
          continue;
        }
        //now we check the customdata of densities
        if (customAxisRef(t) === axis){
          if(group_visible[group_counter.indexOf(t.legendgroup)]===true){
            indices.push(i)

          }
          continue;
        }
        if (t && t.legendgroup === axis) indices.push(i);

        // Or customdata[0] points to the axis group

        if (customAxisRef(t) === axis) indices.push(i)

      }



      toggleAxisAnnot(num,type);

      var update = toggleLegendOnly(tr);

      Plotly.restyle(el.id, update, indices);

      return false;
    });

//-------------------Slider Change------------


function unitNormalFromXY(x, y) {
  const n = Math.min(x.length, y.length);
  if (n < 2) return { nx: 0, ny: 0 };

  const dx = x[n - 1] - x[0];
  const dy = y[n - 1] - y[0];

  const L = Math.hypot(dx, dy) || 1;
  return { nx: -dy / L, ny: dx / L }; // rotate direction by +90°
}

    el.on("plotly_sliderchange", function(e) {
      // Only do this when TransAxes is ON
      const transOn = el.bipl5.rel_but[el.bipl5.but_names.indexOf("TransAxes")] === 1;
      if (!transOn) return;

      // current PC payload
      const pcKey = el.bipl5.currentPCKey || "PC 1 & 2";
      data.payloads = data.payloads || {};
      const payload = data.payloads[pcKey];
      if (!payload || !payload.slider_info) return;

      const si = payload.slider_info;

      // which axis is selected in your axis dropdown?
      const axisIdx0 = si.slider_axis_idx; // 0-based
      if (!Number.isFinite(axisIdx0)) return;

      const axisNum = axisIdx0 + 1;         // your annotations use customdata = 1..p
      const axisKey = "ExpAx" + axisNum;    // your traces use legendgroup "ExpAx<i>"

      const activeIdx = e && e.slider && Number.isFinite(e.slider.active) ? e.slider.active : null;
      if (activeIdx == null) return;

      // step size (distance per step)
      const step = Number(si.step_size);
      if (!Number.isFinite(step)) return;

      // signed distance (negative left, positive right)
      const dist = (activeIdx - e.previousActive) * step;

    });

//-------------------POINTS CLICK--------------

    function searchAxes(item){
        var idx = []
        el.data.forEach((arr, index) => {
          if(metaTag(arr) === item && arr.mode === 'lines'){
            idx.push(index);
          }
        });
        return(idx)
    }

    function obtain_projection(d,idx){
      var x = el.data[idx].x;
      var y = el.data[idx].y;
      var z_hats = el.data[idx].customdata;

      //right now we need to obtain the equation of this axis
      var slope = (y[0]-y[y.length-1])/(x[0]-x[x.length-1]);
      var c = y[0]-slope*x[0];
      //next equation of orthogonal axis going through p
      var slope_perp = -1/slope;
      var c_perp = d.points[0].y-slope_perp*d.points[0].x;
      //solve simultaneously
      var x_cross = (c_perp - c)/(slope-slope_perp);
      var y_cross = slope*x_cross+c;
      //next we linearly interpolate to obtain zhat
      var zhat = (x_cross-x[0])/(x[x.length-1]-x[0])*(z_hats[z_hats.length-1]-z_hats[0])+z_hats[0];

      return([x_cross,y_cross,zhat,slope])

    }

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

      RemovePredictions();
      if(el.bipl5.clicked) removeAnnotation('predict');

      //obtain the indeces of the relevant axes onto which pred. lines
      // must be drawn
      if(el.bipl5.rel_but[el.bipl5.but_names.indexOf("TransAxes")] === 0){
        var indeces = searchAxes('axis')
      } else {
        var indeces = searchAxes('ExpAx')
      }
      const PRED_GROUP = "Pred";
      var predLegendTrace = {
        x: [0],
        y: [0],
        mode: "lines",
        xaxis: "x",
        yaxis: "y",
        name: "Predicted Value",
        showlegend: true,
        visible: true,     // only a legend entry
        legendgroup: PRED_GROUP,
        meta: "predict",
        hoverinfo: "skip",
        line: { dash: "dot", color: "gray", width: 1 }
      };

      var traces_to_be_added = [predLegendTrace];
      for (let i = 0; i < indeces.length; i++) {
        var idx = indeces[i];
        var coordinates = obtain_projection(d,idx);
        var newtrace = {
          x: [d.points[0].x, coordinates[0]],
          y: [d.points[0].y, coordinates[1]],
          mode: "lines+markers",
          xaxis: "x",
          yaxis: "y",
          showlegend: false,
          visible: el.data[idx].visible,
          name: "Predicted Value",
          legendgroup: el.data[idx].legendgroup,
          meta: "predict",
          line: {
            dash: "dot",
            color: "gray",
            width: 1,
          },
          marker: {
            color: "gray",
            size: [1, 6]
          },
          hoverinfo: 'text',
          hovertext: d.points[0].hovertext
        };
        var newAnnotation = {
          x: coordinates[0],
          y: coordinates[1],
          text: coordinates[2].toFixed(2),
          showarrow: false,
          textangle: (-Math.atan(coordinates[3]) * 180) / Math.PI,
          xshift: -10 * Math.sin(Math.atan(coordinates[3])),
          yshift: 10 * Math.cos(Math.atan(coordinates[3])),
          name: "Predicted Value",
          meta: "predict",
          visible: el.data[idx].visible===true,
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
