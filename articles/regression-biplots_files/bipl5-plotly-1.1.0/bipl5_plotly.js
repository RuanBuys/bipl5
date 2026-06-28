(function () {
  window.bipl5Attach = function (el, x, data) {
    var _initPCKey = data.initialPCKey || "PC 1 & 2";
    var _fitDisplayMode = (data.fitDisplay && data.fitDisplay.mode) || "panel";
    el.bipl5 = {
      clicked: false, //helps keep trac if an observation is clicked
      rel_but: [0, 0, 0, 0, 0], // includes EditAxes toggle state
      fitDisplayMode: _fitDisplayMode,
      fitOpen: false,
      vect_visible: 0,
      but_names: ["PC", "AxisStats", "TransAxes", "vecload", "EditAxes"], // top-row button ids
      currentPCKey: _initPCKey,
      currentFMKey: "Cum. Predictivity"
    };


    Object.keys(data.mdsDisplays).forEach(k => {
      data.mdsDisplays[k].bipl5 = deepClone(el.bipl5)

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
    /**
     * Toggles a top-row button flag in `el.bipl5.rel_but`.
     *
     * @param {string} buttonName - Button identifier from `el.bipl5.but_names`.
     * @returns {number|null} New toggle value, or null when button is unknown.
     */
    function toggleButton(buttonName) {
      const i = el.bipl5.but_names.indexOf(buttonName);
      if (i === -1) return null;
      el.bipl5.rel_but[i] = 1 - el.bipl5.rel_but[i];
    }

    /**
     * Removes all prediction traces/annotations from the current plot.
     * Uses `el.bipl5.clicked` as a fast guard to skip work when inactive.
     *
     * @returns {boolean} True when a prediction state was cleared, else false.
     */
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

    /**
     * Sets visibility for all annotations matching a meta tag.
     *
     * @param {string} item - Annotation meta tag to match.
     * @param {boolean} vis - Target visibility.
     * @returns {void}
     */
    function searchAnnot(item, vis){
      for (let i = 0; i < el.layout.annotations.length; i++) {
        let tag = metaTag(el.layout.annotations[i]);
        if(tag === item){
          el.layout.annotations[i].visible = vis;
        }
      }
    }

    /**
     * Applies centered/translated axis visibility across traces and annotations.
     * Centered mode includes the green outer circle; translated mode includes
     * ExpAx traces plus their linked densities.
     *
     * @param {Object} opts - Visibility controls.
     * @param {boolean|null} [opts.centered=null] - Visibility for centered axes/circle/Ax annotations.
     * @param {boolean|null} [opts.translated=null] - Visibility for translated axes/densities/ExpAx annotations.
     * @returns {void}
     */
    function setAxisLayerVisibility(opts) {
      const cfg = Object.assign({ centered: null, translated: null }, opts || {});
      const traceIndices = [];
      const traceVisible = [];

      for (let i = 0; i < el.data.length; i++) {
        const tag = metaTag(el.data[i]);
        if ((tag === "axis" || tag === "OuterCircle") && cfg.centered !== null) {
          traceIndices.push(i);
          traceVisible.push(cfg.centered);
          continue;
        }
        if ((tag === "ExpAx" || tag === "density") && cfg.translated !== null) {
          traceIndices.push(i);
          traceVisible.push(cfg.translated);
        }
      }

      let relayoutNeeded = false;
      const anns = el?.layout?.annotations;
      if (Array.isArray(anns)) {
        for (let i = 0; i < anns.length; i++) {
          const tag = metaTag(anns[i]);
          if (tag === "Ax" && cfg.centered !== null) {
            anns[i].visible = cfg.centered;
            relayoutNeeded = true;
            continue;
          }
          if (tag === "ExpAx" && cfg.translated !== null) {
            anns[i].visible = cfg.translated;
            relayoutNeeded = true;
          }
        }
      }

      if (traceIndices.length) {
        Plotly.restyle(el.id, { visible: traceVisible }, traceIndices);
      }
      if (relayoutNeeded) {
        Plotly.relayout(el, { annotations: el.layout.annotations });
      }
    }

    /**
     * Removes annotations whose meta tag matches `item`.
     * Iterates backwards so splicing does not skip elements.
     *
     * @param {string} item - Annotation meta tag to remove.
     * @returns {void}
     */
    function removeAnnotation(item) {
      for (let i = (el.layout.annotations.length-1); i>= 0; i--) {
        let tag = metaTag(el.layout.annotations[i]);
        if(tag === item){
          el.layout.annotations.splice(i, 1);
        }
      }
    }

    /**
     * Deep clones plain JSON-compatible objects.
     *
     * @param {*} obj - Value to clone.
     * @returns {*} Deep-cloned value.
     */
    function deepClone(obj) {
      return JSON.parse(JSON.stringify(obj));
    }

    /**
     * Normalizes persisted `bipl5` state to include current button schema.
     * Preserves old mdsDisplays that were saved before newer buttons existed.
     *
     * @param {Object} state - Potentially old bipl5 state object.
     * @returns {Object} Normalized state with aligned `but_names` and `rel_but`.
     */
    function normalizeBipl5State(state) {
      // Keep backward compatibility with mdsDisplays saved before EditAxes existed.
      const out = (state && typeof state === "object") ? state : {};
      const fallbackNames = ["PC", "AxisStats", "TransAxes", "vecload", "EditAxes"];
      const names = Array.isArray(out.but_names) ? out.but_names.slice() : fallbackNames.slice();

      if (!names.includes("EditAxes")) names.push("EditAxes");

      const rel = Array.isArray(out.rel_but) ? out.rel_but.slice() : [];
      while (rel.length < names.length) rel.push(0);

      out.but_names = names;
      out.rel_but = rel.slice(0, names.length);
      if (typeof out.fitDisplayMode !== "string") {
        out.fitDisplayMode = _fitDisplayMode;
      }
      if (typeof out.fitOpen !== "boolean") {
        if (typeof out.is_visible === "boolean") {
          out.fitOpen = !out.is_visible;
        } else {
          out.fitOpen = false;
        }
      }
      return out;
    }

    function fitDisplayMode() {
      return el?.bipl5?.fitDisplayMode || _fitDisplayMode || "panel";
    }

    function fitDisplayConfig(mode) {
      const all = data.fitDisplay || {};
      const key = (mode === "overlay") ? "overlay" : "panel";
      return deepClone(all[key] || {});
    }

    function fitOverlayOpen() {
      return fitDisplayMode() === "overlay" && el?.bipl5?.fitOpen === true;
    }

    function menuButtonIndexByName(layoutObj, menuIdx, name) {
      const btns = layoutObj?.updatemenus?.[menuIdx]?.buttons;
      if (!Array.isArray(btns)) return -1;
      for (let i = 0; i < btns.length; i++) {
        const btn = btns[i];
        const key = btn && (btn.name || btn.label);
        if (key === name) return i;
      }
      return -1;
    }

    function setMenuButtonVisible(layoutObj, menuIdx, name, visible) {
      const idx = menuButtonIndexByName(layoutObj, menuIdx, name);
      if (idx >= 0) {
        layoutObj.updatemenus[menuIdx].buttons[idx].visible = visible;
      }
    }

    function fitYAxisSide(mode, cfg) {
      if (typeof cfg?.yaxis3_side === "string") return cfg.yaxis3_side;
      return "left";
    }

    function fitYAxisPosition(mode, cfg, xaxis3Domain) {
      const rawFallback = Number(cfg?.yaxis3_position);
      if (Number.isFinite(rawFallback)) return rawFallback;
      return Number((xaxis3Domain || [0, 1])[0]);
    }

    function fitMenuActiveIndex(layoutObj, key) {
      const idx = menuButtonIndexByName(layoutObj, 2, key);
      return idx >= 0 ? idx : 0;
    }

    function fitMenuPadRight(mode) {
      const cfg = fitDisplayConfig(mode);
      const raw = Number(cfg?.menu_pad_right);
      return Number.isFinite(raw) ? raw : 0;
    }

    function applyFitMenuPadding(layoutObj, mode, fitOpen) {
      if (!layoutObj || !Array.isArray(layoutObj.updatemenus)) return;

      const padRight = (mode === "overlay" && fitOpen === true)
        ? fitMenuPadRight(mode)
        : 0;

      [0, 1].forEach((menuIdx) => {
        if (!layoutObj.updatemenus[menuIdx]) return;
        layoutObj.updatemenus[menuIdx].pad = Object.assign(
          {},
          layoutObj.updatemenus[menuIdx].pad || {},
          { r: padRight }
        );
      });
    }

    function stripFitPanelLayout(layoutObj) {
      const out = deepClone(layoutObj || {});
      out.annotations = stripFitCaptionAnnotations(out.annotations);

      out.xaxis = Object.assign({}, out.xaxis || {}, { domain: [0, 1] });
      if (out.updatemenus && out.updatemenus[2]) {
        out.updatemenus[2].visible = false;
        out.updatemenus[2].active = 0;
      }
      if (out.sliders && out.sliders[0]) {
        out.sliders[0].len = 1;
      }
      applyFitMenuPadding(out, fitDisplayMode(), false);

      return out;
    }

    function patchFitTracesForMode(traces, mode) {
      const cloned = deepClone(Array.isArray(traces) ? traces : []);
      const cfg = fitDisplayConfig(mode);
      for (let i = 0; i < cloned.length; i++) {
        const tr = cloned[i];
        if (!tr || tr.type !== "table") continue;
        tr.domain = Object.assign({}, tr.domain || {}, {
          x: deepClone(cfg.table_domain_x || [0.5, 1]),
          y: deepClone(cfg.table_domain_y || [0.15, 0.85])
        });
      }
      return cloned;
    }

    function buildFitLayout(layoutObj, mode, key, fitTraces, pcKey) {
      const out = deepClone(layoutObj || {});
      const cfg = fitDisplayConfig(mode);
      const xaxis3Domain = deepClone(cfg.xaxis3_domain || [0.65, 1]);
      const yaxisSide = fitYAxisSide(mode, cfg);
      const yaxisPosition = fitYAxisPosition(mode, cfg, xaxis3Domain);

      out.xaxis = Object.assign({}, out.xaxis || {}, {
        domain: deepClone(cfg.xaxis_domain || [0, 1])
      });
      out.xaxis3 = Object.assign({}, out.xaxis3 || {}, {
        domain: deepClone(xaxis3Domain)
      });
      out.yaxis3 = Object.assign({}, out.yaxis3 || {}, {
        domain: deepClone(cfg.yaxis3_domain || [0.15, 0.85]),
        anchor: "free",
        side: yaxisSide,
        position: yaxisPosition
      });

      if (out.updatemenus && out.updatemenus[2]) {
        out.updatemenus[2].visible = true;
        out.updatemenus[2].active = fitMenuActiveIndex(out, key);
      }
      if (out.sliders && out.sliders[0]) {
        out.sliders[0].len = Number(cfg.slider_len || 1);
      }
      applyFitMenuPadding(out, mode, true);

      if (mode === "overlay") {
        out.annotations = [];
        out.xaxis.title = "";
        setMenuButtonVisible(out, 0, "TransAxes", false);
        setMenuButtonVisible(out, 0, "vecload", false);
        setMenuButtonVisible(out, 0, "EditAxes", false);
        if (out.updatemenus && out.updatemenus[3]) {
          out.updatemenus[3].visible = false;
        }
        if (out.sliders && out.sliders[0]) {
          out.sliders[0].visible = false;
        }
      }

      if (key === "Scree Plot") {
        out.yaxis3 = Object.assign({}, out.yaxis3, { autorange: true });
      } else if (key === "Summary Table") {
        out.yaxis3 = Object.assign({}, out.yaxis3, { autorange: false });
      } else {
        out.yaxis3 = Object.assign({}, out.yaxis3, { autorange: false, range: [0, 1] });
      }

      applyFitPanelTitlesAndCaption(out, key, fitTraces, pcKey);
      return out;
    }

    function saveCurrentBiplotSnapshot(pcKey) {
      const prev = data.mdsDisplays[pcKey] || {};
      syncmdsDisplaySliderFromLayout(prev);
      const snapshotLayout = (fitDisplayMode() === "panel" && el.bipl5.fitOpen)
        ? stripFitPanelLayout(el.layout)
        : deepClone(el.layout || {});
      applyFitMenuPadding(snapshotLayout, fitDisplayMode(), false);
      const snapshotState = normalizeBipl5State(deepClone(el.bipl5));
      snapshotState.fitOpen = false;

      data.mdsDisplays[pcKey] = Object.assign({}, prev, {
        trace_data: deepClone(el.data.filter(tr => !isFitPanelTrace(tr))),
        layout: snapshotLayout,
        bipl5: snapshotState
      });

      return data.mdsDisplays[pcKey];
    }

    function restoreBiplotSnapshot(pcKey) {
      const snap = data.mdsDisplays?.[pcKey];
      if (!snap || !Array.isArray(snap.trace_data)) return false;

      const layout = deepClone(snap.layout || {});
      applyFitMenuPadding(layout, fitDisplayMode(), false);
      const state = normalizeBipl5State(deepClone(snap.bipl5));
      state.fitDisplayMode = fitDisplayMode();
      state.fitOpen = false;
      state.currentFMKey = "Cum. Predictivity";
      state.currentPCKey = pcKey;

      return Plotly.react(el, deepClone(snap.trace_data), layout).then(() => {
        el.bipl5 = state;
      });
    }

    /**
     * Checks whether a mdsDisplay contains translated-axis traces.
     *
     * @param {Object} mdsDisplay - PC mdsDisplay object.
     * @returns {boolean} True when at least one `meta === "ExpAx"` trace exists.
     */
    function mdsDisplayHasExpAxes(mdsDisplay) {
      // Used to decide whether EditAxes can be shown for a mdsDisplay.
      const traces = mdsDisplay && Array.isArray(mdsDisplay.trace_data) ? mdsDisplay.trace_data : [];
      for (let i = 0; i < traces.length; i++) {
        if (metaTag(traces[i]) === "ExpAx") return true;
      }
      return false;
    }

    /**
     * Identifies the synthetic "Select Axis" dropdown placeholder button.
     *
     * @param {Object} btn - Updatemenu button object.
     * @returns {boolean} True when this is the placeholder entry.
     */
    function isSelectAxisButton(btn) {
      // Placeholder entry shown before a real axis is selected.
      const key = btn && (btn.name || btn.label);
      return key === "Select Axis";
    }

    /**
     * Builds the synthetic prompt button shown before an axis is selected.
     *
     * @returns {Object} Plotly updatemenu button config.
     */
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

    /**
     * Returns slider dropdown axis buttons without the prompt entry.
     *
     * @param {Object} layoutObj - Plotly layout object.
     * @returns {Object[]} Axis-only button list.
     */
    function axisButtonsNoPromptFromLayout(layoutObj) {
      // Strip placeholder; remaining entries are real axes only.
      const buttons = deepClone(layoutObj?.updatemenus?.[3]?.buttons || []);
      const stripped = buttons.filter(b => !isSelectAxisButton(b));
      return stripped;
    }

    /**
     * Returns slider dropdown buttons with prompt prepended.
     *
     * @param {Object} layoutObj - Plotly layout object.
     * @returns {Object[]} Prompt + axis button list.
     */
    function axisButtonsWithPromptFromLayout(layoutObj) {
      // Add placeholder back as first option for fresh edit sessions.
      const stripped = axisButtonsNoPromptFromLayout(layoutObj);
      return [selectAxisPromptButton()].concat(stripped);
    }

    /**
     * Checks whether the slider dropdown currently includes the prompt entry.
     *
     * @param {Object} layoutObj - Plotly layout object.
     * @returns {boolean} True when button index 0 is "Select Axis".
     */
    function sliderMenuHasPrompt(layoutObj) {
      // Detect whether dropdown currently includes "Select Axis" at index 0.
      const buttons = layoutObj?.updatemenus?.[3]?.buttons;
      if (!Array.isArray(buttons) || !buttons.length) return false;
      return isSelectAxisButton(buttons[0]);
    }

    /**
     * Builds slider current-value prefix text from axis dropdown buttons.
     *
     * @param {Object[]} buttons - Axis dropdown buttons (prompt removed).
     * @param {number} axisIdx - Zero-based axis index.
     * @returns {string} Label prefix for slider current-value display.
     */
    function sliderPrefixFromButtons(buttons, axisIdx) {
      // Slider caption follows axis name from prompt-free axis list.
      let axisName = `Axis ${axisIdx + 1}`;
      if (Array.isArray(buttons) && buttons[axisIdx]) {
        axisName = buttons[axisIdx].name || buttons[axisIdx].label || axisName;
      }
      return `Axis: ${axisName}  `;
    }

    /**
     * Persists current slider selection/position into a mdsDisplay bucket.
     *
     * @param {Object} mdsDisplay - mdsDisplay object for current PC view.
     * @returns {void}
     */
    function syncmdsDisplaySliderFromLayout(mdsDisplay) {
      // Persist current axis selection + current slider step to this mdsDisplay.
      const p = data.p;
      const sliderActiveNow =
        (el.layout.sliders && el.layout.sliders[0] && Number.isFinite(el.layout.sliders[0].active))
          ? el.layout.sliders[0].active
          : 0;

      ensureSliderInfo(mdsDisplay, p, sliderActiveNow);
      const si = mdsDisplay.config.slider_info;
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

    /**
     * Sets the TransAxes button caption to match the current axis mode.
     * Shows "Translated Axes" when centered axes are active, and
     * "Centered Axes" when translated axes are active.
     *
     * @param {Object} layoutObj - Layout object containing top-row updatemenus buttons.
     * @param {boolean} transOn - Whether translated-axis mode is currently enabled.
     * @returns {void}
     */
    function setTransAxesButtonLabel(layoutObj, transOn) {
      // Keep top-button label aligned with the currently active axis mode.
      const buttons = layoutObj?.updatemenus?.[0]?.buttons;
      if (!Array.isArray(buttons)) return;

      for (let i = 0; i < buttons.length; i++) {
        const b = buttons[i];
        const key = b && (b.name || b.label);
        // Match by stable name, or by either display label if already relabeled.
        if (key === "TransAxes" || key === "Translated Axes" || key === "Centered Axes") {
          b.label = transOn ? "Centered Axes" : "Translated Axes";
          return;
        }
      }
    }
    /**
     * Switches the active PC mdsDisplay while preserving interaction state.
     * Keeps fit-panel traces where possible and restores per-PC slider/edit state.
     *
     * @param {Object} d - Plotly button-click mdsDisplay for `PC_toggle`.
     * @returns {void}
     */
    function togglePC(d){
      // new selection
        const newKey = d.button && (d.button.name || d.button.label);
        if (!newKey) return;

        // ignore if user re-clicks same dropdown option
        const oldKey = el.bipl5.currentPCKey || _initPCKey;
        if (newKey === oldKey) return;
        data.mdsDisplays = data.mdsDisplays || {};

        const mode = fitDisplayMode();
        const fmKey = el.bipl5.currentFMKey || "Cum. Predictivity";

        if (fitOverlayOpen()) {
          el.bipl5.currentPCKey = newKey;
          if (fmKey !== "Summary Table") return;

          const overlayFitTraces = getFitTracesByKey(fmKey, mode, newKey);
          if (!overlayFitTraces || !overlayFitTraces.length) return;

          const overlayLayout = buildFitLayout(el.layout, mode, fmKey, overlayFitTraces, newKey);
          Plotly.react(el, overlayFitTraces, overlayLayout).then(() => {
            el.bipl5.currentPCKey = newKey;
          });
          return;
        }

        // ---- A) capture CURRENT RHS (fit panel) state BEFORE we switch ----
        const fitPanelActive = mode === "panel" && el.bipl5.fitOpen === true;
        const showingSummary = fitPanelActive && (fmKey === "Summary Table");

        // keep current RHS traces unless we are in the Summary Table corner case
        const currentFitPanelTraces = fitPanelActive
          ? deepClone(el.data.filter(isFitPanelTrace))
          : [];


        // ---- B) save CURRENT LHS (biplot) into mdsDisplays[oldKey]
        saveCurrentBiplotSnapshot(oldKey);


        // ----C) Load the NEW mdsDisplay for LHS display
        const nextmdsDisplay = data.mdsDisplays[newKey];
        if (!nextmdsDisplay) return; // nothing to switch to

        const next_bipl5=normalizeBipl5State(deepClone(nextmdsDisplay.bipl5));
        next_bipl5.fitDisplayMode=mode;
        next_bipl5.fitOpen=fitPanelActive;
        next_bipl5.currentFMKey=deepClone(fmKey);
        el.bipl5=next_bipl5;



        const nextBiplotTraces = deepClone(nextmdsDisplay.trace_data || []);
        // Build RHS traces to carry over
        let nextFitPanelTraces = currentFitPanelTraces;

        // Corner case: Summary Table must update when PC changes
        if (showingSummary) {
          nextFitPanelTraces = getFitTracesByKey("Summary Table", mode, newKey);
        }
         // ---- D) merge layout: need to change title and button names
        var newLayout = Object.assign({}, el.layout || {});
        newLayout.annotations = stripFitCaptionAnnotations(
          deepClone((nextmdsDisplay.layout && nextmdsDisplay.layout.annotations) || [])
        );
        newLayout.xaxis = Object.assign({}, newLayout.xaxis || {}, {
          title: deepClone((nextmdsDisplay.layout && nextmdsDisplay.layout.xaxis.title) || []),
          autorange: true
        });
        newLayout.yaxis = Object.assign({}, newLayout.yaxis || {}, { autorange: true });
        if (fitPanelActive) {
          newLayout = buildFitLayout(newLayout, mode, fmKey, nextFitPanelTraces, newKey);
        }

        // Ensure TransAxes button caption reflects restored state after PC switch.
        const transIdxForLabel = el.bipl5.but_names.indexOf("TransAxes");
        const transOnForLabel = transIdxForLabel >= 0 && el.bipl5.rel_but[transIdxForLabel] === 1;
        setTransAxesButtonLabel(newLayout, transOnForLabel);

        // Restore per-mdsDisplay slider axis + step selection in UI state.
        const nextCfgSI = nextmdsDisplay.config && nextmdsDisplay.config.slider_info;
        const seedActive =
          (nextCfgSI && Array.isArray(nextCfgSI.slider_pos) &&
           Number.isFinite(nextCfgSI.slider_pos[0]))
            ? nextCfgSI.slider_pos[0]
            : 0;
        ensureSliderInfo(nextmdsDisplay, data.p, seedActive);
        const nextSI = nextmdsDisplay.config.slider_info;
        let nextAxisIdx = Number(nextSI.slider_axis_idx);
        if (!Number.isFinite(nextAxisIdx) || nextAxisIdx < 0 || nextAxisIdx >= data.p) nextAxisIdx = 0;
        nextSI.slider_axis_idx = nextAxisIdx;
        let nextSliderActive = Number(nextSI.slider_pos[nextAxisIdx]);
        if (!Number.isFinite(nextSliderActive)) nextSliderActive = 0;
        nextSI.slider_pos[nextAxisIdx] = nextSliderActive;
        // Per-mdsDisplay flag: false means keep prompt visible and slider hidden.
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
//        if(nextmdsDisplay.layout.updatemenus[0].buttons){
//          newLayout.updatemenus[0].buttons=deepClone(nextmdsDisplay.layout.updatemenus[0].buttons);
//        } else {
//          newLayout.updatemenus[0].buttons[1].label = "Translated Axes"
//        }

        // ---- E) one redraw ----
        const newData = nextBiplotTraces.concat(nextFitPanelTraces);

        // 3) Switch the plot and then restore EditAxes visibility for the new mdsDisplay state.
        Plotly.react(el, newData, newLayout).then(() => {
          // 4) Update current key
          el.bipl5.currentPCKey = newKey;

          const transIdx = el.bipl5.but_names.indexOf("TransAxes");
          const editIdx = el.bipl5.but_names.indexOf("EditAxes");
          const hasExpAxes = mdsDisplayHasExpAxes(nextmdsDisplay);
          const transOn = transIdx >= 0 && el.bipl5.rel_but[transIdx] === 1;
          const vectorsOn = vectorModeOn();

          // If translated axes are unavailable or not active in this mdsDisplay,
          // EditAxes must reset and remain hidden.
          if (!hasExpAxes || !transOn || vectorsOn) {
            if (editIdx >= 0) el.bipl5.rel_but[editIdx] = 0;
          }

          const editOn = editIdx >= 0 && el.bipl5.rel_but[editIdx] === 1;
          const editButtonVisible = hasExpAxes && transOn && !vectorsOn;

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



    /**
     * Returns whether a trace belongs to the fit panel.
     *
     * @param {Object} tr - Plotly trace object.
     * @returns {boolean} True when trace is tagged as FitPanel.
     */
    function isFitPanelTrace(tr) {
      // meta can be ["FitPanel", "..."] or "FitPanel"
      return hasMeta(tr, "FitPanel");
    }

    /**
     * Collects indices of fit-panel traces in the current `el.data`.
     *
     * @returns {number[]} Trace indices for FitPanel traces.
     */
    function fitPanelIndices() {
      const idx = [];
      for (let i = 0; i < el.data.length; i++) {
        if (isFitPanelTrace(el.data[i])) idx.push(i);
      }
      return idx;
    }

    /**
     * Resolves precomputed fit-panel traces by UI key.
     *
     * @param {string} key - Fit-menu key.
     * @returns {Object[]|null} Trace list for that key, or null if unknown.
     */
    function getFitTracesByKey(key, mode, pcKey) {
      const resolvedMode = mode || fitDisplayMode();
      const resolvedPCKey = pcKey || el.bipl5.currentPCKey || _initPCKey;
      const mdsDisplay = data.mdsDisplays?.[resolvedPCKey];
      let source = null;

      // pick the right source
      if (key === "Cum. Predictivity") source = data.fm_mdsDisplay.CumPred;
      if (key === "Cum. Adequacy")     source = data.fm_mdsDisplay.CumAd;
      if (key === "Scree Plot")        source = data.fm_mdsDisplay.Scree;
      if (key === "Variance Explained")source = data.fm_mdsDisplay.VarExp;
      if (key === "Summary Table")     source = mdsDisplay?.fit_table;

      if (!source) return null;
      return patchFitTracesForMode(source, resolvedMode);
    }

    /**
     * Returns axis-title and caption text for each fit-panel mode.
     *
     * @param {string} key - Active fit-panel key.
     * @param {string} pcKey - Active PC-pair key.
     * @returns {{xTitle: string, yTitle: string, caption: string, isTable: boolean}}
     */
    function fitPanelTextByKey(key, pcKey) {
      // Build table numbering dynamically from available mdsDisplay keys
      const pcKeys = Object.keys(data.mdsDisplays);
      const tableNumMap = {};
      pcKeys.forEach(function(k, i) { tableNumMap[k] = i + 1; });
      const tableNum = tableNumMap[pcKey] || 1;

      const map = {
        "Cum. Predictivity": {
          xTitle: "Dimension of subspace",
          yTitle: "Overall quality and axis predictivities (cumulative)",
          caption: "Figure 1: Cumulative quality and axis predictivities<br>across the subspace.",
          isTable: false
        },
        "Cum. Adequacy": {
          xTitle: "Dimension of subspace",
          yTitle: "Cumulative adequacy",
          caption: "Figure 2: Cumulative adequacy across dimensions<br>of the subspace.",
          isTable: false
        },
        "Scree Plot": {
          xTitle: "Dimension of subspace",
          yTitle: "Scree profile (eigenvalues)",
          caption: "Figure 3: Scree profile of eigenvalues across<br>subspace dimensions.",
          isTable: false
        },
        "Variance Explained": {
          xTitle: "Dimension of subspace",
          yTitle: "Proportion of total variation (cumulative)",
          caption: "Figure 4: Cumulative proportion of total variation<br>explained across subspace dimensions.",
          isTable: false
        },
        "Summary Table": {
          xTitle: "",
          yTitle: "",
          caption: `Table ${tableNum}: Marginal predictivity and adequacy of the axes<br>for the ${pcKey} pair biplot.`,
          isTable: true
        }
      };
      return map[key] || map["Cum. Predictivity"];
    }

    /**
     * Removes fit-panel caption annotations by meta tag.
     *
     * @param {Object[]|undefined} annotations - Layout annotations.
     * @returns {Object[]} Annotation list without fit-panel captions.
     */
    function stripFitCaptionAnnotations(annotations) {
      const anns = Array.isArray(annotations) ? deepClone(annotations) : [];
      return anns.filter((ann) => {
        const m = ann && ann.meta;
        if (m === "FitCaption") return false;
        if (Array.isArray(m) && m.includes("FitCaption")) return false;
        return true;
      });
    }

    /**
     * Computes the center x-position of the RHS panel from `xaxis3.domain`.
     *
     * @param {Object} layoutObj - Plotly layout object.
     * @returns {number} Paper-coordinate center x.
     */
    function rhsGraphCenterX(layoutObj) {
      const dom = layoutObj?.xaxis3?.domain;
      if (Array.isArray(dom) && dom.length === 2) {
        const a = Number(dom[0]);
        const b = Number(dom[1]);
        if (Number.isFinite(a) && Number.isFinite(b)) return (a + b) / 2;
      }
      return 0.825;
    }

    /**
     * Computes the center x-position for a fit table from table trace domain.
     *
     * @param {Object[]|null} fitTraces - Active fit traces.
     * @param {number} fallback - Fallback center x.
     * @returns {number} Paper-coordinate center x.
     */
    function rhsTableCenterX(fitTraces, fallback) {
      if (!Array.isArray(fitTraces) || !fitTraces.length) return fallback;
      const tr = fitTraces[0];
      const dom = tr?.domain?.x;
      if (Array.isArray(dom) && dom.length === 2) {
        const a = Number(dom[0]);
        const b = Number(dom[1]);
        if (Number.isFinite(a) && Number.isFinite(b)) return (a + b) / 2;
      }
      return fallback;
    }

    /**
     * Applies fit-panel axis titles and centered caption annotation.
     *
     * @param {Object} layoutObj - Layout to patch.
     * @param {string} key - Active fit-panel key.
     * @param {Object[]|null} fitTraces - Active fit traces.
     * @param {string} pcKey - Active PC-pair key.
     * @returns {void}
     */
    function applyFitPanelTitlesAndCaption(layoutObj, key, fitTraces, pcKey) {
      const txt = fitPanelTextByKey(key, pcKey);

      layoutObj.xaxis3 = Object.assign({}, layoutObj.xaxis3 || {});
      layoutObj.yaxis3 = Object.assign({}, layoutObj.yaxis3 || {});

      if (txt.isTable) {
        layoutObj.xaxis3.title = "";
        layoutObj.yaxis3.title = "";
        layoutObj.xaxis3.showticklabels = false;
        layoutObj.yaxis3.showticklabels = false;
        layoutObj.xaxis3.showgrid = false;
        layoutObj.yaxis3.showgrid = false;
        layoutObj.xaxis3.zeroline = false;
        layoutObj.yaxis3.zeroline = false;
        layoutObj.xaxis3.ticks = "";
        layoutObj.yaxis3.ticks = "";
      } else {
        layoutObj.xaxis3.title = txt.xTitle;
        layoutObj.yaxis3.title = txt.yTitle;
        layoutObj.xaxis3.showticklabels = true;
        layoutObj.yaxis3.showticklabels = true;
        layoutObj.xaxis3.showgrid = true;
        layoutObj.yaxis3.showgrid = true;
        layoutObj.xaxis3.zeroline = true;
        layoutObj.yaxis3.zeroline = true;
      }

      const cleaned = stripFitCaptionAnnotations(layoutObj.annotations);
      const graphCenter = rhsGraphCenterX(layoutObj);
      const xCenter = txt.isTable ? rhsTableCenterX(fitTraces, graphCenter) : graphCenter;

      const graphTop = (Array.isArray(layoutObj?.yaxis3?.domain) && Number.isFinite(layoutObj.yaxis3.domain[1]))
        ? layoutObj.yaxis3.domain[1]
        : 0.85;
      // Keep caption in a dedicated strip just above the panel so menus stay aligned.
      const captionY = graphTop + 0.012;

      cleaned.push({
        xref: "paper",
        yref: "paper",
        x: xCenter,
        y: captionY,
        xanchor: "center",
        yanchor: "bottom",
        align: "center",
        text: `<b>${txt.caption}</b>`,
        showarrow: false,
        meta: ["FitPanel", "FitCaption"],
        font: { size: 13 }
      });
      layoutObj.annotations = cleaned;
    }



    /**
     * Handles fit-menu selection and swaps the active fit-panel traces.
     *
     * @param {Object} d - Plotly button-click mdsDisplay for `Fit_toggle`.
     * @returns {Promise<boolean>|boolean} Plotly promise on update, else false.
     */
    function toggleFit(d){

      const newKey = d.button && (d.button.name || d.button.label);
        if (!newKey) return false;

      // ignore if user re-clicks same dropdown option
      var oldKey = el.bipl5.currentFMKey || "Cum. Predictivity";
      if (newKey === oldKey) return false;

      const mode = fitDisplayMode();
      const pcKey = el.bipl5.currentPCKey || _initPCKey;
      const tracesToAdd = getFitTracesByKey(newKey, mode, pcKey);
      if (!tracesToAdd || !tracesToAdd.length) return false;

      if (mode === "overlay") {
        const newLayout = buildFitLayout(el.layout, mode, newKey, tracesToAdd, pcKey);
        return Plotly.react(el, tracesToAdd, newLayout).then(() => {
          el.bipl5.currentFMKey = newKey;
        });
      }

      // 1) Remove existing FitPanel traces from CURRENT plot state
      const baseData = el.data.filter(tr => !isFitPanelTrace(tr));

      // 3) Build new data + layout for react (single redraw)
      const newData = baseData.concat(tracesToAdd);
      const newLayout = buildFitLayout(el.layout, mode, newKey, tracesToAdd, pcKey);

      return Plotly.react(el, newData, newLayout).then(() => {
              el.bipl5.currentFMKey = newKey;
              });
    }

/**
 * Resolves clicked button index from Plotly button-click mdsDisplay.
 * Falls back from `_index` to menu active index to name/label matching.
 *
 * @param {Object} d - Plotly button-click mdsDisplay.
 * @returns {number} Zero-based index, or -1 when unresolved.
 */
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
 * Ensures mdsDisplay-local slider state exists and has consistent dimensions.
 * Initializes per-axis slider positions, selected axis index, and axis-chosen flag.
 *
 * @param {Object} mdsDisplay - mdsDisplay object for the current PC view.
 * @param {number} p - Number of available axes.
 * @param {number} defaultActive - Default slider step index used for initialization.
 * @returns {void}
 */
function ensureSliderInfo(mdsDisplay, p, defaultActive) {
  mdsDisplay.config = mdsDisplay.config || {};
  mdsDisplay.config.slider_info = mdsDisplay.config.slider_info || {};

  const si = mdsDisplay.config.slider_info;

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

/**
 * Finds a top-row button index by stable name/label text.
 *
 * @param {string} name - Button name to find.
 * @returns {number} Zero-based index, or -1 if missing.
 */
function topMenuButtonIndexByName(name) {
  // Resolve dynamic index since button ordering can change with layout updates.
  return menuButtonIndexByName(el.layout, 0, name);
}

/**
 * Returns whether a trace is visible under Plotly legend semantics.
 *
 * @param {Object} tr - Plotly trace object.
 * @returns {boolean} True for visible/undefined traces, false for legendonly/false.
 */
function isTraceVisibleForLegendState(tr) {
  // In Plotly, `undefined` behaves like visible=true.
  return !!tr && (tr.visible === true || tr.visible === undefined);
}

/**
 * Finds the displayed translated-axis trace index by axis key.
 *
 * @param {string} axisKey - Axis legendgroup key, e.g. "ExpAx3".
 * @returns {number} Trace index, or -1 when not found.
 */
function findExpAxisTraceIndex(axisKey) {
  for (let i = 0; i < el.data.length; i++) {
    const tr = el.data[i];
    if (metaTag(tr) === "ExpAx" && tr.legendgroup === axisKey) return i;
  }
  return -1;
}

/**
 * Emits a synthetic legend click to reveal a hidden translated axis.
 * Reuses legend-click logic so linked traces/annotations stay consistent.
 *
 * @param {string} axisKey - Axis legendgroup key, e.g. "ExpAx3".
 * @returns {boolean} True when a synthetic click was emitted.
 */
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

/**
 * Normalizes Plotly axis-title specs to plain text.
 * Accepts either string titles or object titles with a `text` field.
 *
 * @param {string|Object|null|undefined} titleSpec - Plotly axis title spec.
 * @returns {string} Title text, or empty string when unavailable.
 */
function titleTextFromSpec(titleSpec) {
  if (typeof titleSpec === "string") return titleSpec;
  if (titleSpec && typeof titleSpec === "object" && typeof titleSpec.text === "string") {
    return titleSpec.text;
  }
  return "";
}

function activemdsDisplayForCurrentPC() {
  const pcKey = el.bipl5.currentPCKey || _initPCKey;
  data.mdsDisplays = data.mdsDisplays || {};
  return data.mdsDisplays[pcKey] || (data.mdsDisplays[pcKey] = {});
}

function cacheCurrentQualityTitle() {
  const mdsDisplay = activemdsDisplayForCurrentPC();
  ensureSliderInfo(mdsDisplay, data.p, 0);
  const si = mdsDisplay.config.slider_info;

  if (typeof si.quality_title === "string" && si.quality_title.length > 0) {
    return si.quality_title;
  }

  const frommdsDisplay = titleTextFromSpec(mdsDisplay?.layout?.xaxis?.title);
  if (frommdsDisplay) {
    si.quality_title = frommdsDisplay;
    return frommdsDisplay;
  }

  const fromLayout = titleTextFromSpec(el?.layout?.xaxis?.title);
  if (fromLayout) {
    si.quality_title = fromLayout;
    return fromLayout;
  }

  return "";
}

/**
 * Returns the quality-of-display title for the current PC mdsDisplay.
 * Prefers mdsDisplay layout title and falls back to current live layout title.
 *
 * @returns {string} Current quality title text (possibly empty).
 */
function currentQualityTitleText() {
  const mdsDisplay = activemdsDisplayForCurrentPC();
  ensureSliderInfo(mdsDisplay, data.p, 0);
  const cached = mdsDisplay.config.slider_info.quality_title;
  if (typeof cached === "string" && cached.length > 0) return cached;
  return cacheCurrentQualityTitle();
}

/**
 * Applies EditAxes controls visibility/state through a single relayout patch.
 *
 * @param {Object} opts - UI options.
 * @param {boolean} [opts.editButtonVisible=false] - Show/hide EditAxes button.
 * @param {boolean} [opts.dropdownVisible=false] - Show/hide axis dropdown.
 * @param {boolean} [opts.sliderVisible=false] - Show/hide slider.
 * @param {boolean} [opts.usePrompt=true] - Use prompt+axes vs axes-only dropdown.
 * @param {number} [opts.axisIdx=0] - Active axis index when prompt removed.
 * @param {number|null} [opts.sliderActive=null] - Slider step to set when visible.
 * @returns {Promise} Plotly relayout promise.
 */
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

  if (cfg.sliderVisible) {
    // Cache title before clearing it so restore does not depend on live layout.
    cacheCurrentQualityTitle();
  }

  const patch = {
    "sliders[0].visible": cfg.sliderVisible,
    "updatemenus[3].visible": cfg.dropdownVisible,
    "updatemenus[3].buttons": axisButtons,
    "updatemenus[3].active": cfg.usePrompt ? 0 : cfg.axisIdx,
    // Hide quality title only while slider is visible to prevent overlap.
    "xaxis.title.text": cfg.sliderVisible ? "" : currentQualityTitleText()
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

/**
 * Handles axis dropdown selection for translated-axis slider editing.
 * Persists per-PC slider state and updates dropdown/slider UI.
 *
 * @param {Object} d - Plotly button-click mdsDisplay for `Slider_toggle`.
 * @returns {Promise|boolean} Plotly relayout promise on change, else false.
 */
function toggleSlider(d) {
  const pcKey = el.bipl5.currentPCKey || _initPCKey;
  data.mdsDisplays = data.mdsDisplays || {};
  const mdsDisplay = data.mdsDisplays[pcKey] || (data.mdsDisplays[pcKey] = {});

  const p = data.p;

  const sliderActiveRaw =
    (el.layout.sliders && el.layout.sliders[0] && Number.isFinite(el.layout.sliders[0].active))
      ? el.layout.sliders[0].active
      : null;
  const sliderActiveNow = Number.isFinite(sliderActiveRaw) ? sliderActiveRaw : 0;

  // Keep slider selection state isolated per mdsDisplay (per PC view).
  ensureSliderInfo(mdsDisplay, p, sliderActiveNow);

  const si = mdsDisplay.config.slider_info;

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
    return Plotly.relayout(el, {
      "sliders[0].visible": false,
      "updatemenus[3].active": 0,
      // Restore title once slider is hidden.
      "xaxis.title.text": currentQualityTitleText()
    });
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
  // Cache current title before hiding it while slider is shown.
  cacheCurrentQualityTitle();
  const relayoutPatch = {
    "updatemenus[3].buttons": axisButtons,
    "updatemenus[3].active": newAxisIdx,
    "sliders[0].visible": true,
    "sliders[0].active": nextActive,
    "sliders[0].currentvalue.prefix": sliderPrefixFromButtons(axisButtons, newAxisIdx),
    // Hide title while slider is on-screen to avoid overlap.
    "xaxis.title.text": ""
  };

  return Plotly.relayout(el, relayoutPatch);
}

    /**
     * Checks whether a trace/annotation meta field contains a given tag.
     * Supports both scalar and array meta formats.
     *
     * @param {Object} tr - Plotly trace or annotation object.
     * @param {string} tag - Meta tag to test.
     * @returns {boolean} True when tag is present.
     */
    function hasMeta(tr, tag) {
      if (!tr) return false;
      const m = tr.meta;
      if (Array.isArray(m)) return m.includes(tag);
      return m === tag;
    }

    /**
     * Shows or hides the fit panel by adjusting data and layout domains.
     *
     * @returns {boolean} False to suppress default button behavior.
     */
    function switch_fit_panel(){
      const mode = fitDisplayMode();
      const fitOpen = el.bipl5.fitOpen === true;
      const pcKey = el.bipl5.currentPCKey || _initPCKey;

      if (!fitOpen) {
        if (mode === "overlay") {
          saveCurrentBiplotSnapshot(pcKey);
        }

        const add = getFitTracesByKey("Cum. Predictivity", mode, pcKey);
        if (!add || !add.length) return false;

        const newData = (mode === "overlay")
          ? add
          : el.data.concat(add);
        const newLayout = buildFitLayout(el.layout, mode, "Cum. Predictivity", add, pcKey);

        Plotly.react(el, newData, newLayout).then(() => {
          el.bipl5.fitOpen = true;
          el.bipl5.currentFMKey = "Cum. Predictivity";
        });

        return false;
      }

      if (mode === "overlay") {
        const restored = restoreBiplotSnapshot(pcKey);
        if (restored === false) return false;
        return restored.then(() => {
          el.bipl5.fitOpen = false;
          el.bipl5.currentFMKey = "Cum. Predictivity";
        });
      }

      const keep = el.data.filter(tr => !hasMeta(tr, "FitPanel"));
      const newLayout = stripFitPanelLayout(el.layout);

      Plotly.react(el, keep, newLayout).then(() => {
        el.bipl5.fitOpen = false;
        el.bipl5.currentFMKey = "Cum. Predictivity";
      });

      return false;
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
          return;
        }

        if(d.menu.name==="Slider_toggle"){
          if (fitOverlayOpen()) return false;
          toggleSlider(d);
          return;
        }

      }

      var rel_but_sel =
        el.bipl5.rel_but[el.bipl5.but_names.indexOf(d.button.name)];
      if (d.button.name === "AxisStats") {
        switch_fit_panel();
        return;
      }

      if (fitOverlayOpen()) {
        return false;
      }

      if (d.button.name === "EditAxes") {
        // Edit mode is only available while translated axes are active.
        const transOn = el.bipl5.rel_but[el.bipl5.but_names.indexOf("TransAxes")] === 1;
        const vectorsOn = vectorModeOn();
        const pcKey = el.bipl5.currentPCKey || _initPCKey;
        data.mdsDisplays = data.mdsDisplays || {};
        const mdsDisplay = data.mdsDisplays[pcKey] || (data.mdsDisplays[pcKey] = {});
        const sliderActiveNow =
          (el.layout.sliders && el.layout.sliders[0] && Number.isFinite(el.layout.sliders[0].active))
            ? el.layout.sliders[0].active
            : 0;
        ensureSliderInfo(mdsDisplay, data.p, sliderActiveNow);
        const si = mdsDisplay.config.slider_info;

        if (!transOn || vectorsOn) {
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
        if (rel_but_sel === 0) {
          RemovePredictions();
          const vectorsOn = vectorModeOn();
          const editBtnStateIdx = el.bipl5.but_names.indexOf("EditAxes");
          if (editBtnStateIdx >= 0) el.bipl5.rel_but[editBtnStateIdx] = 0;
          const pcKey = el.bipl5.currentPCKey || _initPCKey;
          data.mdsDisplays = data.mdsDisplays || {};
          const mdsDisplay = data.mdsDisplays[pcKey] || (data.mdsDisplays[pcKey] = {});
          ensureSliderInfo(mdsDisplay, data.p, 0);
          mdsDisplay.config.slider_info.axis_chosen = false;

          if (vectorsOn) {
            // Keep all axes hidden while vector display is active.
            setAxisLayerVisibility({ centered: false, translated: false });
            setEditAxesUI({ editButtonVisible: false, dropdownVisible: false, sliderVisible: false, usePrompt: true });
          } else {
            setAxisLayerVisibility({ centered: false, translated: true });
            setEditAxesUI({ editButtonVisible: true, dropdownVisible: false, sliderVisible: false, usePrompt: true });
          }

          const index = d.button._index;
          el.layout.updatemenus[0].buttons[index].label = "Centered Axes";
          toggleButton(d.button.name);
          return;
        }

        if (rel_but_sel === 1) {
          RemovePredictions();
          const vectorsOn = vectorModeOn();
          const editBtnStateIdx = el.bipl5.but_names.indexOf("EditAxes");
          if (editBtnStateIdx >= 0) el.bipl5.rel_but[editBtnStateIdx] = 0;
          const pcKey = el.bipl5.currentPCKey || _initPCKey;
          data.mdsDisplays = data.mdsDisplays || {};
          const mdsDisplay = data.mdsDisplays[pcKey] || (data.mdsDisplays[pcKey] = {});
          ensureSliderInfo(mdsDisplay, data.p, 0);
          mdsDisplay.config.slider_info.axis_chosen = false;

          setAxisLayerVisibility(
            vectorsOn
              ? { centered: false, translated: false }
              : { centered: true, translated: false }
          );
          setEditAxesUI({ editButtonVisible: false, dropdownVisible: false, sliderVisible: false, usePrompt: true });
          if (!vectorsOn) {
            searchAnnot("vecload", false);
          }

          const index = d.button._index;
          el.layout.updatemenus[0].buttons[index].label = "Translated Axes";
          toggleButton(d.button.name);
          return;
        }
      }

      if (d.button.name === "vecload") {
        // Toggle vector annotations.
        if (rel_but_sel === 0) {
          RemovePredictions();
          setAxisLayerVisibility({ centered: false, translated: false });
          searchAnnot("vecload", true);
          const editBtnStateIdx = el.bipl5.but_names.indexOf("EditAxes");
          if (editBtnStateIdx >= 0) el.bipl5.rel_but[editBtnStateIdx] = 0;
          setEditAxesUI({ editButtonVisible: false, dropdownVisible: false, sliderVisible: false, usePrompt: true });

          el.bipl5.vect_visible = 1;
          toggleButton(d.button.name);
          return;
        }

        if (rel_but_sel === 1) {
          searchAnnot("vecload", false);
          const transOn = translatedAxesModeOn();
          setAxisLayerVisibility({
            centered: !transOn,
            translated: transOn
          });
          const hasExpAxes = searchAxes("ExpAx").length > 0;
          setEditAxesUI({
            editButtonVisible: transOn && hasExpAxes,
            dropdownVisible: false,
            sliderVisible: false,
            usePrompt: true
          });

          el.bipl5.vect_visible = 0;
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


    /**
     * Returns whether a trace has a usable legendgroup string.
     *
     * @param {Object} tr - Plotly trace object.
     * @returns {boolean} True when legendgroup is a non-empty string.
     */
    function hasLegendgroup(tr) {
      return tr && typeof tr.legendgroup === "string" && tr.legendgroup.length > 0;
    }

    /**
     * Parses axis legendgroup values of form "AxN" or "ExpAxN".
     *
     * @param {string} lg - Legendgroup key.
     * @returns {{axis:string,num:number,type:string}|null} Parsed axis metadata.
     */
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
      // - customdata: ["ExpAx3"]  (current mdsDisplay style)
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

    /**
     * Returns whether vector-display mode is currently enabled.
     *
     * @returns {boolean} True when vector display is active.
     */
    function vectorModeOn() {
      return el?.bipl5?.vect_visible === 1;
    }

    /**
     * Toggles axis annotation visibility for one axis number and type.
     * Also mirrors visibility for prediction annotations on the same axis.
     *
     * @param {number} num - Axis index stored in annotation customdata.
     * @param {string} type - Axis meta tag ("Ax" or "ExpAx").
     * @returns {number} Number of axis annotations toggled.
     */
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

    /**
     * Builds a Plotly visibility patch that flips trace legend state.
     *
     * @param {Object} tr - Plotly trace object.
     * @returns {{visible:(true|string)}} Restyle patch.
     */
    function toggleLegendOnly(tr) {
      const isShown = (tr.visible === true || tr.visible === undefined); // undefined behaves like visible
    return { visible: isShown ? "legendonly" : true };
    }

    /**
     * Parses metadata for dummy sample-legend traces.
     *
     * @param {Object} tr - Plotly trace object.
     * @returns {{kind:string,key:string}|null} Legend bucket info.
     */
    function sampleLegendInfo(tr) {
      if (!hasMeta(tr, "sample-legend")) return null;

      const meta = Array.isArray(tr.meta) ? tr.meta : [tr.meta];
      let kind = null;
      let key = null;

      for (let i = 0; i < meta.length; i++) {
        const item = meta[i];
        if (item === "color" || item === "symbol") {
          kind = item;
          continue;
        }
        if (typeof item !== "string") continue;
        if (item.indexOf("color:") === 0) {
          kind = "color";
          key = item.slice(6);
          continue;
        }
        if (item.indexOf("symbol:") === 0) {
          kind = "symbol";
          key = item.slice(7);
        }
      }

      if (!kind || key === null) return null;
      return { kind: kind, key: key };
    }

    /**
     * Parses metadata for sample combination traces used in dual stratification.
     *
     * @param {Object} tr - Plotly trace object.
     * @returns {{color:string|null,symbol:string|null}|null} Combination metadata.
     */
    function sampleComboInfo(tr) {
      if (!hasMeta(tr, "sample-combo")) return null;

      const meta = Array.isArray(tr.meta) ? tr.meta : [tr.meta];
      let colorKey = null;
      let symbolKey = null;

      for (let i = 0; i < meta.length; i++) {
        const item = meta[i];
        if (typeof item !== "string") continue;
        if (item.indexOf("color:") === 0) {
          colorKey = item.slice(6);
          continue;
        }
        if (item.indexOf("symbol:") === 0) {
          symbolKey = item.slice(7);
        }
      }

      if (colorKey === null && symbolKey === null) return null;
      return { color: colorKey, symbol: symbolKey };
    }

    /**
     * Returns active sample-legend keys for one stratification kind.
     *
     * @param {string} kind - Either "color" or "symbol".
     * @returns {Set<string>|null} Active keys, or null when that legend section is absent.
     */
    function activeSampleLegendKeys(kind) {
      const active = new Set();
      let found = false;

      for (let i = 0; i < el.data.length; i++) {
        const info = sampleLegendInfo(el.data[i]);
        if (!info || info.kind !== kind) continue;
        found = true;
        if (isTraceVisible(el.data[i])) active.add(info.key);
      }

      return found ? active : null;
    }

    /**
     * Recomputes visibility of sample combination traces from legend filters.
     *
     * @returns {boolean} True when combination traces were updated.
     */
    function applySampleLegendFilters() {
      const activeColors = activeSampleLegendKeys("color");
      const activeSymbols = activeSampleLegendKeys("symbol");
      const indices = [];
      const visible = [];

      for (let i = 0; i < el.data.length; i++) {
        const info = sampleComboInfo(el.data[i]);
        if (!info) continue;

        let keep = true;
        if (activeColors && info.color !== null) {
          keep = keep && activeColors.has(info.color);
        }
        if (activeSymbols && info.symbol !== null) {
          keep = keep && activeSymbols.has(info.symbol);
        }

        const nextVisible = keep ? true : "legendonly";
        el.data[i].visible = nextVisible;
        indices.push(i);
        visible.push(nextVisible);
      }

      if (!indices.length) return false;
      Plotly.restyle(el.id, { visible: visible }, indices);
      return true;
    }

    el.on("plotly_legendclick", function (dat) {
      if (dat.event.detail === 2) {
        //is hierdie 'n double click?
        return false;
      }

      const tr = dat?.data?.[dat.curveNumber];
      if (!tr) return false;
      const tag = metaTag(tr);
      if (fitOverlayOpen() && !hasMeta(tr, "FitPanel")) {
        return false;
      }
      // Delete predictive lines
      if (tag === "predict") {
        RemovePredictions()
        removeAnnotation('predict');
        el.bipl5.clicked = false;
        return false;
      }
      //purely toggle a trace

      const sampleLegend = sampleLegendInfo(tr);
      if (sampleLegend) {
        const update = toggleLegendOnly(tr);
        tr.visible = update.visible;
        Plotly.restyle(el.id, update, dat.curveNumber);
        applySampleLegendFilters();
        return false;
      }

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
      if (fitOverlayOpen()) return;
      // Only do this when TransAxes is ON
      const transOn = el.bipl5.rel_but[el.bipl5.but_names.indexOf("TransAxes")] === 1;
      if (!transOn) return;

      // Each PC pair has its own slider state. Resolve/create that mdsDisplay bucket first.
      const pcKey = el.bipl5.currentPCKey || _initPCKey;
      data.mdsDisplays = data.mdsDisplays || {};
      const mdsDisplay = data.mdsDisplays[pcKey] || (data.mdsDisplays[pcKey] = {});

      // Plotly slider UI state at the moment of this event.
      // We use this as a fallback if event mdsDisplay is partial.
      const sliderActiveNow =
        (el.layout.sliders && el.layout.sliders[0] && Number.isFinite(el.layout.sliders[0].active))
          ? el.layout.sliders[0].active
          : 0;

      // Ensure mdsDisplay.config.slider_info has expected shape:
      // slider_pos[axisIndex], slider_axis_idx, and step_size.
      ensureSliderInfo(mdsDisplay, data.p, sliderActiveNow);
      const si = mdsDisplay.config.slider_info;

      // Axis currently selected in the "Slider_toggle" dropdown (0-based).
      const axisIdx0 = si.slider_axis_idx; // 0-based
      if (!Number.isFinite(axisIdx0)) return;

      // Tags used throughout traces/annotations for this selected axis.
      const axisNum = axisIdx0 + 1;         // your annotations use customdata = 1..p
      const axisKey = "ExpAx" + axisNum;    // your traces use legendgroup "ExpAx<i>"

      // Slider step index from event mdsDisplay.
      // Fallback to layout value to be robust across Plotly event differences.
      const activeIdx =
        (e && e.slider && Number.isFinite(e.slider.active))
          ? e.slider.active
          : sliderActiveNow;

      // Geometry scalar from R mdsDisplay: movement distance in plot units per slider step.
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

    /**
     * Returns line-trace indices whose primary meta tag matches `item`.
     *
     * @param {string} item - Meta tag to match ("axis" or "ExpAx").
     * @returns {number[]} Matching axis trace indices.
     */
    function searchAxes(item){
        var idx = []
        el.data.forEach((arr, index) => {
          if(metaTag(arr) === item && arr.mode === 'lines'){
            idx.push(index);
          }
        });
        return(idx)
    }

    /**
     * Computes orthogonal projection of a clicked point onto an axis trace.
     * Also interpolates the projected z-hat value along that axis.
     *
     * @param {Object} d - Plotly click event mdsDisplay.
     * @param {number} idx - Axis trace index in `el.data`.
     * @returns {[number, number, number, number]} [xCross, yCross, zhat, slope].
     */
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
      if (fitOverlayOpen()) return false;
      const clickedPoint = d && Array.isArray(d.points) ? d.points[0] : null;
      const clickedTrace = clickedPoint && clickedPoint.data ? clickedPoint.data : null;
      if (!clickedPoint || !clickedTrace) return false;
      // Prediction lines are only valid for observation clicks.
      if (!hasMeta(clickedTrace, "data")) return false;
      if (el.bipl5.vect_visible === 1) {
        return false;
      }

      //-----------------PREDICTION LINES--------------

      // Distinguish first insertion from "replace existing prediction" flow.
      const wasClicked = el.bipl5.clicked === true;

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

      // Candidate trace set for this click:
      // [0] legend entry + [1..] one prediction trace per visible axis.
      var traces_to_be_added = [predLegendTrace];
      // Prediction value labels corresponding to traces_to_be_added[1..].
      var new_predict_annotations = [];
      for (let i = 0; i < indeces.length; i++) {
        var idx = indeces[i];
        var coordinates = obtain_projection(d,idx);
        var newtrace = {
          x: [clickedPoint.x, coordinates[0]],
          y: [clickedPoint.y, coordinates[1]],
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
          hovertext: clickedPoint.hovertext
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
        new_predict_annotations.push(newAnnotation);
      }

      // If a point was already selected, reuse prediction traces/annotations and
      // refresh them in one Plotly.update call instead of delete+add cycles.
      if (wasClicked) {
        // Existing prediction trace slots currently on the graph.
        var pred_trace_idx = [];
        for (let i = 0; i < el.data.length; i++) {
          if (metaTag(el.data[i]) === "predict") pred_trace_idx.push(i);
        }

        // Map new per-axis traces by legendgroup so we can align them to old slots.
        var trace_by_axis = {};
        for (let i = 1; i < traces_to_be_added.length; i++) {
          trace_by_axis[traces_to_be_added[i].legendgroup] = traces_to_be_added[i];
        }

        // Keep trace indices stable: preserve old ordering and swap in new geometry.
        var aligned_traces = pred_trace_idx.map(function(idx){
          const old_trace = el.data[idx] || {};
          if (old_trace.legendgroup === PRED_GROUP) return traces_to_be_added[0];
          return trace_by_axis[old_trace.legendgroup] || old_trace;
        });

        // Replace only prediction annotations; keep all non-prediction annotations intact.
        const old_annotations = Array.isArray(el.layout.annotations) ? el.layout.annotations : [];
        const kept_annotations = old_annotations.filter(function(ann){
          return metaTag(ann) !== "predict";
        });
        const updated_annotations = kept_annotations.concat(new_predict_annotations);

        // Batched trace + layout patch in a single Plotly API call.
        var trace_update = {
          x: aligned_traces.map(function(tr){ return tr.x; }),
          y: aligned_traces.map(function(tr){ return tr.y; }),
          visible: aligned_traces.map(function(tr){ return tr.visible; }),
          legendgroup: aligned_traces.map(function(tr){ return tr.legendgroup; }),
          hovertext: aligned_traces.map(function(tr){ return tr.hovertext; })
        };

        Plotly.update(el, trace_update, { annotations: updated_annotations }, pred_trace_idx);
        el.bipl5.clicked = true;
        return;
      }

      RemovePredictions();
      if (!Array.isArray(el.layout.annotations)) el.layout.annotations = [];
      el.layout.annotations = el.layout.annotations.concat(new_predict_annotations);
      Plotly.addTraces(el.id, traces_to_be_added);
      el.bipl5.clicked = true;
    });
  };
})();
