(function () {
  window.bipl5Attach = function (el, x, data) {
    el.bipl5 = {
      clicked: false, //helps keep trac if an observation is clicked
      rel_but: [0, 0, 0, 0, 0], // includes EditAxes toggle state
      is_visible: true,
      vect_visible: 0,
      but_names: ["PC", "AxisStats", "TransAxes", "vecload", "EditAxes"], // top-row button ids
      currentPCKey: "PC 1 & 2",
      currentFMKey: "Cum. Predictivity"
    };


    Object.keys(data.payloads).forEach(k => {
      data.payloads[k].bipl5 = deepClone(el.bipl5)

    });

    /**
     * Extracts the primary meta tag from a trace/annotation-like object.
     * Supports both string `meta` and array `meta` formats.
     *
     * @param {Object} tr - Plotly trace or annotation object.
     * @returns {string|null} The first/only meta tag, or null when unavailable.
     */
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

    function normalizeBipl5State(state) {
      // Keep backward compatibility with payloads saved before EditAxes existed.
      const out = (state && typeof state === "object") ? state : {};
      const fallbackNames = ["PC", "AxisStats", "TransAxes", "vecload", "EditAxes"];
      const names = Array.isArray(out.but_names) ? out.but_names.slice() : fallbackNames.slice();

      if (!names.includes("EditAxes")) names.push("EditAxes");

      const rel = Array.isArray(out.rel_but) ? out.rel_but.slice() : [];
      while (rel.length < names.length) rel.push(0);

      out.but_names = names;
      out.rel_but = rel.slice(0, names.length);
      return out;
    }

    function payloadHasExpAxes(payload) {
      // Used to decide whether EditAxes can be shown for a payload.
      const traces = payload && Array.isArray(payload.trace_data) ? payload.trace_data : [];
      for (let i = 0; i < traces.length; i++) {
        if (metaTag(traces[i]) === "ExpAx") return true;
      }
      return false;
    }

    function isSelectAxisButton(btn) {
      // Placeholder entry shown before a real axis is selected.
      const key = btn && (btn.name || btn.label);
      return key === "Select Axis";
    }

    function selectAxisPromptButton() {
      // Synthetic first option so dropdown caption reads "Select Axis".
      return {
        method: "skip",
        args: ["type", "scatter"],
        label: "Select Axis",
        name: "Select Axis",
        execute: false
      };
    }

    function axisButtonsNoPromptFromLayout(layoutObj) {
      // Strip placeholder; remaining entries are real axes only.
      const buttons = deepClone(layoutObj?.updatemenus?.[3]?.buttons || []);
      const stripped = buttons.filter(b => !isSelectAxisButton(b));
      return stripped;
    }

    function axisButtonsWithPromptFromLayout(layoutObj) {
      // Add placeholder back as first option for fresh edit sessions.
      const stripped = axisButtonsNoPromptFromLayout(layoutObj);
      return [selectAxisPromptButton()].concat(stripped);
    }

    function sliderMenuHasPrompt(layoutObj) {
      // Detect whether dropdown currently includes "Select Axis" at index 0.
      const buttons = layoutObj?.updatemenus?.[3]?.buttons;
      if (!Array.isArray(buttons) || !buttons.length) return false;
      return isSelectAxisButton(buttons[0]);
    }

    function sliderPrefixFromButtons(buttons, axisIdx) {
      // Slider caption follows axis name from prompt-free axis list.
      let axisName = `Axis ${axisIdx + 1}`;
      if (Array.isArray(buttons) && buttons[axisIdx]) {
        axisName = buttons[axisIdx].name || buttons[axisIdx].label || axisName;
      }
      return `Axis: ${axisName}  `;
    }

    function sliderPrefixFromLayout(layoutObj, axisIdx) {
      // Keep slider label synced with the selected axis dropdown entry.
      const buttons = axisButtonsNoPromptFromLayout(layoutObj);
      return sliderPrefixFromButtons(buttons, axisIdx);
    }

    function syncPayloadSliderFromLayout(payload) {
      // Persist current axis selection + current slider step to this payload.
      const p = data.p;
      const sliderActiveNow =
        (el.layout.sliders && el.layout.sliders[0] && Number.isFinite(el.layout.sliders[0].active))
          ? el.layout.sliders[0].active
          : 0;

      ensureSliderInfo(payload, p, sliderActiveNow);
      const si = payload.slider_info;
      si.axis_chosen = false;

      // Only persist active axis choice when slider is visible (user selected an axis).
      const sliderVisible =
        !!(el.layout.sliders && el.layout.sliders[0] && el.layout.sliders[0].visible === true);
      if (!sliderVisible) return;

      let axisActiveNow =
        (el.layout.updatemenus && el.layout.updatemenus[3] && Number.isFinite(el.layout.updatemenus[3].active))
          ? el.layout.updatemenus[3].active
          : si.slider_axis_idx;

      if (sliderMenuHasPrompt(el.layout)) {
        if (axisActiveNow <= 0) return;
        axisActiveNow -= 1;
      }

      if (Number.isFinite(axisActiveNow) && axisActiveNow >= 0 && axisActiveNow < p) {
        si.slider_axis_idx = axisActiveNow;
        si.axis_chosen = true;
      }

      const idx = si.slider_axis_idx;
      if (Number.isFinite(idx) && idx >= 0 && idx < p) {
        si.slider_pos[idx] = sliderActiveNow;
      }
    }



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
        // Save per-payload slider selection before leaving this PC view.
        syncPayloadSliderFromLayout(prev);
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

        const next_bipl5=normalizeBipl5State(deepClone(nextPayload.bipl5));
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

        // Restore per-payload slider axis + step selection in UI state.
        const seedActive =
          (nextPayload.slider_info && Array.isArray(nextPayload.slider_info.slider_pos) &&
           Number.isFinite(nextPayload.slider_info.slider_pos[0]))
            ? nextPayload.slider_info.slider_pos[0]
            : 0;
        ensureSliderInfo(nextPayload, data.p, seedActive);
        const nextSI = nextPayload.slider_info;
        let nextAxisIdx = Number(nextSI.slider_axis_idx);
        if (!Number.isFinite(nextAxisIdx) || nextAxisIdx < 0 || nextAxisIdx >= data.p) nextAxisIdx = 0;
        nextSI.slider_axis_idx = nextAxisIdx;
        let nextSliderActive = Number(nextSI.slider_pos[nextAxisIdx]);
        if (!Number.isFinite(nextSliderActive)) nextSliderActive = 0;
        nextSI.slider_pos[nextAxisIdx] = nextSliderActive;
        // Per-payload flag: false means keep prompt visible and slider hidden.
        const nextAxisChosen = !!nextSI.axis_chosen;

        const sliderButtons = nextAxisChosen
          ? axisButtonsNoPromptFromLayout(newLayout)
          : axisButtonsWithPromptFromLayout(newLayout);

        if (newLayout.updatemenus && newLayout.updatemenus[3]) {
          newLayout.updatemenus[3].buttons = sliderButtons;
          newLayout.updatemenus[3].active = nextAxisChosen ? nextAxisIdx : 0;
        }
        if (newLayout.sliders && newLayout.sliders[0]) {
          newLayout.sliders[0].active = nextSliderActive;
          if (nextAxisChosen) {
            newLayout.sliders[0].currentvalue = Object.assign(
              {},
              newLayout.sliders[0].currentvalue || {},
              { prefix: sliderPrefixFromButtons(sliderButtons, nextAxisIdx) }
            );
          }
        }
//        if(nextPayload.layout.updatemenus[0].buttons){
//          newLayout.updatemenus[0].buttons=deepClone(nextPayload.layout.updatemenus[0].buttons);
//        } else {
//          newLayout.updatemenus[0].buttons[1].label = "Translated Axes"
//        }

        // ---- E) one redraw ----
        const newData = nextBiplotTraces.concat(nextFitPanelTraces);

        // 3) Switch the plot and then restore EditAxes visibility for the new payload state.
        Plotly.react(el, newData, newLayout).then(() => {
          // 4) Update current key
          el.bipl5.currentPCKey = newKey;

          const transIdx = el.bipl5.but_names.indexOf("TransAxes");
          const editIdx = el.bipl5.but_names.indexOf("EditAxes");
          const hasExpAxes = payloadHasExpAxes(nextPayload);
          const transOn = transIdx >= 0 && el.bipl5.rel_but[transIdx] === 1;

          // If translated axes are unavailable or not active in this payload,
          // EditAxes must reset and remain hidden.
          if (!hasExpAxes || !transOn) {
            if (editIdx >= 0) el.bipl5.rel_but[editIdx] = 0;
          }

          const editOn = editIdx >= 0 && el.bipl5.rel_but[editIdx] === 1;
          const editButtonVisible = hasExpAxes && transOn;

          if (!editButtonVisible) {
            setEditAxesUI({ editButtonVisible: false, dropdownVisible: false, sliderVisible: false, usePrompt: true });
            return;
          }

          if (!editOn) {
            setEditAxesUI({ editButtonVisible: true, dropdownVisible: false, sliderVisible: false, usePrompt: true });
            return;
          }

          if (nextAxisChosen) {
            setEditAxesUI({
              editButtonVisible: true,
              dropdownVisible: true,
              sliderVisible: true,
              usePrompt: false,
              axisIdx: nextAxisIdx,
              sliderActive: nextSliderActive
            });
            return;
          }

          setEditAxesUI({ editButtonVisible: true, dropdownVisible: true, sliderVisible: false, usePrompt: true });
        });

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

/**
 * Ensures payload-local slider state exists and has consistent dimensions.
 * Initializes per-axis slider positions, selected axis index, and axis-chosen flag.
 *
 * @param {Object} payload - Payload object for the current PC view.
 * @param {number} p - Number of available axes.
 * @param {number} defaultActive - Default slider step index used for initialization.
 * @returns {void}
 */
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

  if (typeof si.axis_chosen !== "boolean") {
    // Tracks whether user chose a real axis (vs Select Axis prompt).
    si.axis_chosen = false;
  }
}

function topMenuButtonIndexByName(name) {
  // Resolve dynamic index since button ordering can change with layout updates.
  const btns = el?.layout?.updatemenus?.[0]?.buttons;
  if (!Array.isArray(btns)) return -1;
  for (let i = 0; i < btns.length; i++) {
    const b = btns[i];
    const k = b && (b.name || b.label);
    if (k === name) return i;
  }
  return -1;
}

function isTraceVisibleForLegendState(tr) {
  // In Plotly, `undefined` behaves like visible=true.
  return !!tr && (tr.visible === true || tr.visible === undefined);
}

function findExpAxisTraceIndex(axisKey) {
  for (let i = 0; i < el.data.length; i++) {
    const tr = el.data[i];
    if (metaTag(tr) === "ExpAx" && tr.legendgroup === axisKey) return i;
  }
  return -1;
}

function triggerLegendClickForAxisIfHidden(axisKey) {
  // Reuse existing legend-click handler so axis trace + linked traces/annotations
  // are restored exactly the same way as a user legend interaction.
  const idx = findExpAxisTraceIndex(axisKey);
  if (idx < 0) return false;
  if (isTraceVisibleForLegendState(el.data[idx])) return false;

  el.emit("plotly_legendclick", {
    curveNumber: idx,
    data: el.data,
    event: { detail: 1 }
  });
  return true;
}

function setEditAxesUI(opts) {
  // Single relayout patch for EditAxes button + axis dropdown + slider visibility.
  const cfg = Object.assign({
    editButtonVisible: false,
    dropdownVisible: false,
    sliderVisible: false,
    usePrompt: true,
    axisIdx: 0,
    sliderActive: null
  }, opts || {});

  // Dropdown content switches between prompt+axes and axes-only modes.
  const axisButtons = cfg.usePrompt
    ? axisButtonsWithPromptFromLayout(el.layout)
    : axisButtonsNoPromptFromLayout(el.layout);

  const patch = {
    "sliders[0].visible": cfg.sliderVisible,
    "updatemenus[3].visible": cfg.dropdownVisible,
    "updatemenus[3].buttons": axisButtons,
    "updatemenus[3].active": cfg.usePrompt ? 0 : cfg.axisIdx
  };

  if (cfg.sliderVisible && Number.isFinite(cfg.sliderActive)) {
    patch["sliders[0].active"] = cfg.sliderActive;
  }
  if (!cfg.usePrompt) {
    patch["sliders[0].currentvalue.prefix"] = sliderPrefixFromButtons(axisButtons, cfg.axisIdx);
  }

  const editIdx = topMenuButtonIndexByName("EditAxes");
  if (editIdx >= 0) {
    patch[`updatemenus[0].buttons[${editIdx}].visible`] = cfg.editButtonVisible;
    patch["updatemenus[0].active"] = (cfg.dropdownVisible || cfg.sliderVisible) ? editIdx : -1;
  }
  return Plotly.relayout(el, patch);
}

function toggleSlider(d) {
  const pcKey = el.bipl5.currentPCKey || "PC 1 & 2";
  data.payloads = data.payloads || {};
  const payload = data.payloads[pcKey] || (data.payloads[pcKey] = {});

  const p = data.p;

  const sliderActiveRaw =
    (el.layout.sliders && el.layout.sliders[0] && Number.isFinite(el.layout.sliders[0].active))
      ? el.layout.sliders[0].active
      : null;
  const sliderActiveNow = Number.isFinite(sliderActiveRaw) ? sliderActiveRaw : 0;

  // Keep slider selection state isolated per payload (per PC view).
  ensureSliderInfo(payload, p, sliderActiveNow);

  const si = payload.slider_info;

  // Selected axis name (button)
  const axisName = d.button && (d.button.name || d.button.label);
  if (!axisName) return false;

  // Axis index in dropdown buttons (0-based, possibly offset by Select Axis prompt).
  const rawIdx = getButtonIndex(d);
  if (rawIdx < 0) return false;

  const hasPrompt = sliderMenuHasPrompt(el.layout);
  if (hasPrompt && rawIdx === 0) {
    // "Select Axis" prompt clicked: keep slider hidden until a real axis is chosen.
    si.axis_chosen = false;
    return Plotly.relayout(el, { "sliders[0].visible": false, "updatemenus[3].active": 0 });
  }

  const newAxisIdx = hasPrompt ? rawIdx - 1 : rawIdx;
  if (newAxisIdx < 0 || newAxisIdx >= p) return false;

  const axisKey = "ExpAx" + (newAxisIdx + 1);
  triggerLegendClickForAxisIfHidden(axisKey);

  // 1) Save current slider step for previously selected axis.
  // Guard this so we do not overwrite axis 1 with 0 when slider.active is unset
  // and no real axis has been selected yet.
  const oldAxisIdx = si.slider_axis_idx;
  const sliderVisibleNow =
    !!(el.layout.sliders && el.layout.sliders[0] && el.layout.sliders[0].visible === true);
  if (si.axis_chosen && sliderVisibleNow &&
      Number.isFinite(sliderActiveRaw) &&
      Number.isFinite(oldAxisIdx) && oldAxisIdx >= 0 && oldAxisIdx < p) {
    si.slider_pos[oldAxisIdx] = sliderActiveRaw;
  }

  // 2) Switch selected axis
  si.slider_axis_idx = newAxisIdx;
  // Once an axis is chosen, remove prompt and show slider.
  si.axis_chosen = true;

  // 3) Load saved slider step for new axis
  const nextActive = si.slider_pos[newAxisIdx];

  const axisButtons = axisButtonsNoPromptFromLayout(el.layout);

  // 4) Update slider UI
  const relayoutPatch = {
    "updatemenus[3].buttons": axisButtons,
    "updatemenus[3].active": newAxisIdx,
    "sliders[0].visible": true,
    "sliders[0].active": nextActive,
    "sliders[0].currentvalue.prefix": sliderPrefixFromButtons(axisButtons, newAxisIdx)
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

      if (d.button.name === "EditAxes") {
        // Edit mode is only available while translated axes are active.
        const transOn = el.bipl5.rel_but[el.bipl5.but_names.indexOf("TransAxes")] === 1;
        const pcKey = el.bipl5.currentPCKey || "PC 1 & 2";
        data.payloads = data.payloads || {};
        const payload = data.payloads[pcKey] || (data.payloads[pcKey] = {});
        const sliderActiveNow =
          (el.layout.sliders && el.layout.sliders[0] && Number.isFinite(el.layout.sliders[0].active))
            ? el.layout.sliders[0].active
            : 0;
        ensureSliderInfo(payload, data.p, sliderActiveNow);
        const si = payload.slider_info;

        if (!transOn) {
          si.axis_chosen = false;
          setEditAxesUI({ editButtonVisible: false, dropdownVisible: false, sliderVisible: false, usePrompt: true });
          return;
        }

        if (rel_but_sel === 0) {
          toggleButton(d.button.name);
          // Enter edit mode: show dropdown prompt first; slider appears after axis selection.
          si.axis_chosen = false;
          setEditAxesUI({ editButtonVisible: true, dropdownVisible: true, sliderVisible: false, usePrompt: true });
          return;
        }

        if (rel_but_sel === 1) {
          toggleButton(d.button.name);
          // Exit edit mode: hide controls and re-arm Select Axis prompt for next entry.
          si.axis_chosen = false;
          setEditAxesUI({ editButtonVisible: true, dropdownVisible: false, sliderVisible: false, usePrompt: true });
          return;
        }
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
          const editBtnStateIdx = el.bipl5.but_names.indexOf("EditAxes");
          if (editBtnStateIdx >= 0) el.bipl5.rel_but[editBtnStateIdx] = 0;
          const pcKey = el.bipl5.currentPCKey || "PC 1 & 2";
          data.payloads = data.payloads || {};
          const payload = data.payloads[pcKey] || (data.payloads[pcKey] = {});
          ensureSliderInfo(payload, data.p, 0);
          payload.slider_info.axis_chosen = false;
          // When TransAxes turns on: show EditAxes button, but keep controls hidden.
          setEditAxesUI({ editButtonVisible: true, dropdownVisible: false, sliderVisible: false, usePrompt: true });
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

          const editBtnStateIdx = el.bipl5.but_names.indexOf("EditAxes");
          if (editBtnStateIdx >= 0) el.bipl5.rel_but[editBtnStateIdx] = 0;
          const pcKey = el.bipl5.currentPCKey || "PC 1 & 2";
          data.payloads = data.payloads || {};
          const payload = data.payloads[pcKey] || (data.payloads[pcKey] = {});
          ensureSliderInfo(payload, data.p, 0);
          payload.slider_info.axis_chosen = false;
          // When TransAxes turns off: hide EditAxes button and controls.
          setEditAxesUI({ editButtonVisible: false, dropdownVisible: false, sliderVisible: false, usePrompt: true });

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

    /**
     * Reads the axis reference key from trace `customdata`.
     * Accepts both array form (e.g. ["ExpAx3"]) and string form ("ExpAx3").
     *
     * @param {Object} tr - Plotly trace object.
     * @returns {string|null} Axis reference key (e.g. "ExpAx3"), or null when absent.
     */
    function customAxisRef(tr) {
      // Density traces can reference an axis in two shapes depending on builder:
      // - customdata: ["ExpAx3"]  (current payload style)
      // - customdata: "ExpAx3"    (legacy style)
      // This helper normalizes both to the same string key.
      if (!tr) return null;
      if (Array.isArray(tr.customdata)) return tr.customdata[0] ?? null;
      if (typeof tr.customdata === "string") return tr.customdata;
      return null;
    }

    /**
     * Returns whether a trace is currently visible on the plot area.
     * Treats both `false` and `"legendonly"` as hidden.
     *
     * @param {Object} tr - Plotly trace object.
     * @returns {boolean} True when trace is rendered in the plot area.
     */
    function isTraceVisible(tr) {
      if (!tr) return false;
      return tr.visible !== false && tr.visible !== "legendonly";
    }

    /**
     * Collects indices of density traces for one class/legend group.
     * Optionally restricts results to densities linked to visible translated axes.
     *
     * @param {string} groupName - Density legend group (class name).
     * @param {boolean} visibleAxesOnly - If true, include only densities on visible ExpAx traces.
     * @returns {number[]} Trace indices to update together.
     */
    function densityIndicesForGroup(groupName, visibleAxesOnly) {
      if (typeof groupName !== "string" || !groupName.length) return [];

      const indices = [];
      const visibleAxes = new Set();

      if (visibleAxesOnly) {
        for (let i = 0; i < el.data.length; i++) {
          const t = el.data[i];
          if (metaTag(t) === "ExpAx" && typeof t.legendgroup === "string" && isTraceVisible(t)) {
            visibleAxes.add(t.legendgroup);
          }
        }
      }

      for (let i = 0; i < el.data.length; i++) {
        const t = el.data[i];
        if (metaTag(t) !== "density") continue;
        if (t.legendgroup !== groupName) continue;

        const axisRef = customAxisRef(t);
        if (axisRef === "legendentry") {
          indices.push(i);
          continue;
        }

        if (!visibleAxesOnly || visibleAxes.has(axisRef)) {
          indices.push(i);
        }
      }

      return indices;
    }

    /**
     * Returns whether translated-axes mode is currently enabled.
     * Uses the top-button state as the source of truth.
     *
     * @returns {boolean} True when "TransAxes" mode is on.
     */
    function translatedAxesModeOn() {
      const names = el?.bipl5?.but_names;
      const rel = el?.bipl5?.rel_but;
      if (!Array.isArray(names) || !Array.isArray(rel)) return false;
      const idx = names.indexOf("TransAxes");
      return idx >= 0 && rel[idx] === 1;
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

      if (tag === "data") {
        // Toggle clicked data class and mirror that state to its linked densities.
        const update = toggleLegendOnly(tr);
        Plotly.restyle(el.id, update, dat.curveNumber);

        // Density placeholder/segments should only follow data toggles
        // when translated axes are active.
        if (!translatedAxesModeOn()) return false;

        // Bring back densities only on ExpAx traces that are currently visible.
        const densityIndices = densityIndicesForGroup(tr.name, true);
        if (densityIndices.length) {
          Plotly.restyle(el.id, update, densityIndices);
        }

        return false;
      }

      if (hasMeta(tr,"FitPanel") || tag === "polygon") {
        const update = toggleLegendOnly(tr);
        Plotly.restyle(el.id, update, dat.curveNumber);
        return false;
      }
      if (metaTag(tr) === "density") {
        // Keep density legend clicks inert while translated axes are off.
        // This prevents placeholder state drift before TransAxes is enabled.
        if (!translatedAxesModeOn()) return false;
        // Toggle selected class densities, constrained to visible translated axes.
        const indices = densityIndicesForGroup(tr.legendgroup, true);
        const update = toggleLegendOnly(tr);
        if (indices.length) {
          Plotly.restyle(el.id, update, indices);
        }
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


/**
 * Computes a unit normal vector from line coordinates using first and last points.
 * The returned normal is orientation-normalized so `nx >= 0`.
 *
 * @param {number[]} x - X coordinates for the line.
 * @param {number[]} y - Y coordinates for the line.
 * @returns {{nx: number, ny: number}} Unit normal vector components.
 */
function unitNormalFromXY(x, y) {
  const n = Math.min(x.length, y.length);
  if (n < 2) return { nx: 0, ny: 0 };

  const dx = x[n - 1] - x[0];
  const dy = y[n - 1] - y[0];

  const L = Math.hypot(dx, dy) || 1;
  let nx = -dy / L;
  let ny = dx / L;
  if (nx < 0) {
    nx = -nx;
    ny = -ny;
  }
  return { nx, ny }; // rotate direction by +90°; enforce nx >= 0
}

/**
 * Shifts numeric entries in an array by a constant delta.
 * Non-numeric entries are preserved unchanged.
 *
 * @param {Array} arr - Array of coordinates or mixed values.
 * @param {number} delta - Translation offset to add to numeric elements.
 * @returns {Array} New array with shifted numeric values.
 */
function shiftNumericArray(arr, delta) {
  // Utility for bulk coordinate translation.
  // Keeps non-numeric entries untouched so mixed arrays do not break updates.
  if (!Array.isArray(arr)) return arr;
  return arr.map(v => (typeof v === "number" && Number.isFinite(v)) ? (v + delta) : v);
}

    el.on("plotly_sliderchange", function(e) {
      // Only do this when TransAxes is ON
      const transOn = el.bipl5.rel_but[el.bipl5.but_names.indexOf("TransAxes")] === 1;
      if (!transOn) return;

      // Each PC pair has its own slider state. Resolve/create that payload bucket first.
      const pcKey = el.bipl5.currentPCKey || "PC 1 & 2";
      data.payloads = data.payloads || {};
      const payload = data.payloads[pcKey] || (data.payloads[pcKey] = {});

      // Plotly slider UI state at the moment of this event.
      // We use this as a fallback if event payload is partial.
      const sliderActiveNow =
        (el.layout.sliders && el.layout.sliders[0] && Number.isFinite(el.layout.sliders[0].active))
          ? el.layout.sliders[0].active
          : 0;

      // Ensure payload.slider_info has expected shape:
      // slider_pos[axisIndex], slider_axis_idx, and step_size.
      ensureSliderInfo(payload, data.p, sliderActiveNow);
      const si = payload.slider_info;

      // Axis currently selected in the "Slider_toggle" dropdown (0-based).
      const axisIdx0 = si.slider_axis_idx; // 0-based
      if (!Number.isFinite(axisIdx0)) return;

      // Tags used throughout traces/annotations for this selected axis.
      const axisNum = axisIdx0 + 1;         // your annotations use customdata = 1..p
      const axisKey = "ExpAx" + axisNum;    // your traces use legendgroup "ExpAx<i>"

      // Slider step index from event payload.
      // Fallback to layout value to be robust across Plotly event differences.
      const activeIdx =
        (e && e.slider && Number.isFinite(e.slider.active))
          ? e.slider.active
          : sliderActiveNow;

      // Geometry scalar from R payload: movement distance in plot units per slider step.
      const step = Number(si.step_size);
      if (!Number.isFinite(step)) return;

      // Hybrid previous-state lookup:
      // 1) prefer per-axis saved position (most reliable for axis switching),
      // 2) fallback to Plotly event previousActive when missing,
      // 3) fallback to current active index.
      const prevActive = Number.isFinite(si.slider_pos[axisIdx0])
        ? si.slider_pos[axisIdx0]
        : ((e && Number.isFinite(e.previousActive)) ? e.previousActive : activeIdx);
      
      
      // Signed translation distance in axis-normal direction.
      const dist = (activeIdx - prevActive) * step;

      // Persist new position for this axis immediately.
      si.slider_pos[axisIdx0] = activeIdx;
      if (!Number.isFinite(dist) || dist === 0) return;

      // Find the selected ExpAx line and compute its normal.
      // Translating along this normal keeps the axis parallel.
      let axisNormal = null;
      for (let i = 0; i < el.data.length; i++) {
        const t = el.data[i];
        if (metaTag(t) === "ExpAx" && t.legendgroup === axisKey &&
            Array.isArray(t.x) && Array.isArray(t.y) &&
            t.x.length > 1 && t.y.length > 1) {
          axisNormal = unitNormalFromXY(t.x, t.y);
          break;
        }
      }
      if (!axisNormal) return;

      // Cartesian translation vector.
      const dx = dist * axisNormal.nx;
      const dy = dist * axisNormal.ny;

      // Collect a batched Plotly.update patch.
      // We move only traces associated with the selected ExpAx:
      // 1) the ExpAx line itself (legendgroup === axisKey)
      // 2) density traces linked via customdata -> axisKey
      // 3) prediction line for this axis (meta="predict", same legendgroup)
      //
      // Prediction traces are handled specially: only the projected endpoint
      // (last point) should move; the clicked observation anchor stays fixed.
      const traceIndices = [];
      const xUpdates = [];
      const yUpdates = [];

      for (let i = 0; i < el.data.length; i++) {
        const t = el.data[i];
        const tag = metaTag(t);

        if (tag === "ExpAx" && t.legendgroup === axisKey && Array.isArray(t.x) && Array.isArray(t.y)) {
          traceIndices.push(i);
          xUpdates.push(shiftNumericArray(t.x, dx));
          yUpdates.push(shiftNumericArray(t.y, dy));
          continue;
        }

        if (tag === "density" && customAxisRef(t) === axisKey && Array.isArray(t.x) && Array.isArray(t.y)) {
          traceIndices.push(i);
          xUpdates.push(shiftNumericArray(t.x, dx));
          yUpdates.push(shiftNumericArray(t.y, dy));
          continue;
        }

        if (tag === "predict" && t.legendgroup === axisKey && Array.isArray(t.x) && Array.isArray(t.y)) {
          const newX = t.x.slice();
          const newY = t.y.slice();
          const j = newX.length - 1;
          if (j >= 0 && Number.isFinite(newX[j]) && Number.isFinite(newY[j])) {
            newX[j] += dx;
            newY[j] += dy;
            traceIndices.push(i);
            xUpdates.push(newX);
            yUpdates.push(newY);
          }
        }
      }

      // Move annotations tied to this axis:
      // - ExpAx tick labels/marks/name/glyph (meta="ExpAx", customdata=axisNum)
      // - prediction value label for this axis (meta="predict", customdata=axisNum)
      //
      // We clone touched annotations so relayout gets a clean, immutable patch.
      let changedAnn = false;
      const oldAnns = Array.isArray(el.layout.annotations) ? el.layout.annotations : [];
      const newAnns = oldAnns.map(function (ann) {
        if (!ann || typeof ann !== "object") return ann;
        const annTag = metaTag(ann);
        const annAxis = Number(ann.customdata);
        if ((annTag === "ExpAx" || annTag === "predict") && annAxis === axisNum) {
          const next = Object.assign({}, ann);
          if (Number.isFinite(next.x)) next.x += dx;
          if (Number.isFinite(next.y)) next.y += dy;
          changedAnn = true;
          return next;
        }
        return ann;
      });

      const traceChanged = traceIndices.length > 0;
      if (!traceChanged && !changedAnn) return;

      // Single batched call when traces changed for better sync/perf.
      // If only annotations changed, relayout is sufficient.
      if (traceChanged) {
        const tracePatch = { x: xUpdates, y: yUpdates };
        const layoutPatch = changedAnn ? { annotations: newAnns } : {};
        Plotly.update(el, tracePatch, layoutPatch, traceIndices);
      } else {
        Plotly.relayout(el, { annotations: newAnns });
      }

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
