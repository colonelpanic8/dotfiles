var hint = slate.op("hint", {
  "characters" : "ASDFGHJKLQWERTYUIOPCVBN"
});
var grid = slate.op("grid", {
  grids: {
    "1920x1080": {"width": 8, "height": 6}
  }
});

slate.bindAll({
  "esc:cmd": hint,
  "space:alt": grid,
  "tab:cmd": slate.op("switch")
});

slate.configAll({
  windowHintsIgnoreHiddenWindows: false,
  windowHintsShowIcons: true,
  windowHintsSpread: true,
  switchShowTitles: true
});