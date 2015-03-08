slate.configAll({
    windowHintsIgnoreHiddenWindows: false,
    windowHintsShowIcons: true,
    windowHintsSpread: true,
    switchShowTitles: true,
    windowHintsSpreadSearchHeight: 100,
    windowHintsSpreadSearchWidth: 100,
    windowHintsSpreadPadding: 40
});

var hint = slate.op("hint", {
  "characters" : "ASDFGHJKLQWERTYUIOPCVBN"
});

var grid = slate.op("grid", {
  grids: {
    "1920x1080": {"width": 8, "height": 6},
    "1440x900": {"width": 8, "height": 6}
  }
});
function quarterCorner(corner) {
  return slate.op("corner", {
    direction: corner,
    width: "screenSizeX/2",
    height: "screenSizeY/2"
  });
}

var hyper = ":ctrl;shift;alt;cmd";
function hyperBindAll(mapping) {
  console.log(_.object(_.map(
    _.pairs(mapping),
    function(key, op) { return [key + hyper, op]; }
  )));
  return slate.bindAll(
    _.object(_.map(
      _.pairs(mapping),
      function(pair) { return [pair[0] + hyper, pair[1]]; }
    ))
  );
}

function focusApp(app) {
  return slate.op("focus", {"app" : app});
}

hyperBindAll({
  h: hint,
  // Simple Layout
  f: slate.op("corner", {
    direction: "top-left",
    width: "screenSizeX",
    height: "screenSizeY"
  }),
  q: slate.op("corner", {
    direction: "top-left",
    width: "screenSizeX/2",
    height: "screenSizeY"
  }),
  w: slate.op("corner", {
    direction: "top-right",
    width: "screenSizeX/2",
    height: "screenSizeY"
  }),
  a: quarterCorner("top-left"),
  s: quarterCorner("top-right"),
  z: quarterCorner("bottom-left"),
  n: slate.op("throw", {
    "width": "windowSizeX",
    "height": "windowSizeY",
    "screen": "next"
  }),
  x: quarterCorner("bottom-right"),
  // App focus
  e: focusApp("Emacs"),
  c: focusApp("Google Chrome"),
  t: focusApp("iTerm"),
  m: focusApp("Spotify"),
  // Slate operations
  r: slate.op("relaunch"),
  u: slate.op("undo")
});

// Snapshot Stuff.
// hyperBindAll(_.object(
//   _.range(1, 5).map(function(index) {
//     var asString = index.toString();
//     return [asString, slate.op("snapshot", {"name": asString, "save": true, "stack": false})];
//   })
// ));

// hyperBindAll(_.object(
//   _.range(5, 9).map(function(index) {
//     var name = (index-4).toString();
//     var asString = index.toString();
//     return [asString, slate.op("activate-snapshot", {"name": name})];
//   })
// ));
slate.bindAll({"esc:cmd": hint, "space:alt": grid});
