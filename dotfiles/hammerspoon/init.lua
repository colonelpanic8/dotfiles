local hyper = { "cmd", "ctrl", "alt" }
local hyperShift = { "cmd", "ctrl", "alt", "shift" }
local wf = hs.window.filter.defaultCurrentSpace

hs.window.animationDuration = 0
pcall(function()
  hs.ipc.cliInstall()
end)

local config = {
  gap = 8,
  autoColumns = false,
}

local retileTimer = nil
local arranging = false
local rightCommandDown = false
local rightCommandUsed = false
local rightCommandKeyCode = 54

local function notify(message)
  hs.alert.show(message, 0.7)
end

local function screenFrame(screen)
  return screen:frame()
end

local function sameScreen(a, b)
  return a and b and a:id() == b:id()
end

local function centerX(win)
  local f = win:frame()
  return f.x + (f.w / 2)
end

local function centerY(win)
  local f = win:frame()
  return f.y + (f.h / 2)
end

local function distance(a, b)
  local dx = a.x - b.x
  local dy = a.y - b.y
  return math.sqrt((dx * dx) + (dy * dy))
end

local function directionScore(focused, candidate, direction)
  local focusedCenter = {
    x = centerX(focused),
    y = centerY(focused),
  }
  local candidateCenter = {
    x = centerX(candidate),
    y = centerY(candidate),
  }

  if direction == "left" and candidateCenter.x >= focusedCenter.x then
    return nil
  elseif direction == "right" and candidateCenter.x <= focusedCenter.x then
    return nil
  elseif direction == "up" and candidateCenter.y >= focusedCenter.y then
    return nil
  elseif direction == "down" and candidateCenter.y <= focusedCenter.y then
    return nil
  end

  return distance(focusedCenter, candidateCenter)
end

local function columnWindows(screen)
  local windows = {}

  for _, win in ipairs(wf:getWindows()) do
    if win:isStandard()
      and not win:isMinimized()
      and sameScreen(win:screen(), screen)
    then
      table.insert(windows, win)
    end
  end

  table.sort(windows, function(a, b)
    local af = a:frame()
    local bf = b:frame()

    if math.abs(centerX(a) - centerX(b)) > 24 then
      return centerX(a) < centerX(b)
    end

    return af.y < bf.y
  end)

  return windows
end

local function neighborWindow(direction)
  local focused = hs.window.focusedWindow()
  if not focused then
    return nil
  end

  local focusedId = focused:id()
  local best = nil
  local bestScore = nil

  for _, win in ipairs(wf:getWindows()) do
    if win:isStandard()
      and not win:isMinimized()
      and sameScreen(win:screen(), focused:screen())
      and win:id() ~= focusedId
    then
      local score = directionScore(focused, win, direction)
      if score and (not bestScore or score < bestScore) then
        best = win
        bestScore = score
      end
    end
  end

  return best
end

local function setFrame(win, frame)
  win:setFrame(frame, 0)
end

local function tileWindows(windows)
  if #windows == 0 then
    return
  end

  arranging = true

  local screen = windows[1]:screen()
  local frame = screenFrame(screen)
  local gap = config.gap
  local count = #windows
  local width = (frame.w - (gap * (count - 1))) / count

  for index, win in ipairs(windows) do
    setFrame(win, {
      x = frame.x + ((index - 1) * (width + gap)),
      y = frame.y,
      w = width,
      h = frame.h,
    })
  end

  arranging = false
end

local function tileFocusedScreen()
  local focused = hs.window.focusedWindow()
  if not focused then
    notify("No focused window")
    return
  end

  local windows = columnWindows(focused:screen())
  tileWindows(windows)
end

local function focusWindow(direction)
  local target = neighborWindow(direction)
  if target then
    target:focus()
  end
end

local function swapWindow(direction)
  local focused = hs.window.focusedWindow()
  local target = neighborWindow(direction)
  if not focused or not target then
    return
  end

  arranging = true

  local focusedFrame = focused:frame()
  local targetFrame = target:frame()

  setFrame(focused, targetFrame)
  setFrame(target, focusedFrame)
  focused:focus()

  arranging = false
end

local function placeFocused(startColumn, spanColumns, totalColumns)
  local focused = hs.window.focusedWindow()
  if not focused then
    return
  end

  local frame = screenFrame(focused:screen())
  local gap = config.gap
  local unit = (frame.w - (gap * (totalColumns - 1))) / totalColumns
  local x = frame.x + ((startColumn - 1) * (unit + gap))
  local width = (unit * spanColumns) + (gap * (spanColumns - 1))

  setFrame(focused, {
    x = x,
    y = frame.y,
    w = width,
    h = frame.h,
  })
end

local function moveFocusedToScreen(direction)
  local focused = hs.window.focusedWindow()
  if not focused then
    return
  end

  local target = direction < 0 and focused:screen():previous() or focused:screen():next()
  focused:moveToScreen(target, false, true)
  tileWindows(columnWindows(target))
end

local function scheduleRetile()
  if arranging or not config.autoColumns then
    return
  end

  if retileTimer then
    retileTimer:stop()
  end

  retileTimer = hs.timer.doAfter(0.25, tileFocusedScreen)
end

local function toggleAutoColumns()
  config.autoColumns = not config.autoColumns
  notify(config.autoColumns and "Auto columns on" or "Auto columns off")

  if config.autoColumns then
    tileFocusedScreen()
  end
end

local function toggleMonitorInput()
  hs.task.new("/bin/zsh", function(exitCode)
    if exitCode ~= 0 then
      notify("Monitor input toggle failed")
    end
  end, {
    "-lc",
    "export PATH=\"$HOME/.nix-profile/bin:/run/current-system/sw/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH\"; \"$HOME/dotfiles/dotfiles/lib/functions/mpg341cx_input\" toggle",
  }):start()
end

wf:subscribe({
  hs.window.filter.windowCreated,
  hs.window.filter.windowDestroyed,
  hs.window.filter.windowMoved,
  hs.window.filter.windowInCurrentSpace,
  hs.window.filter.windowNotInCurrentSpace,
  hs.window.filter.windowUnminimized,
  hs.window.filter.windowNotVisible,
}, scheduleRetile)

hs.hotkey.bind(hyper, "c", tileFocusedScreen)
hs.hotkey.bind(hyper, "v", toggleAutoColumns)
hs.hotkey.bind(hyper, "\\", toggleMonitorInput)

hs.hotkey.bind(hyper, "a", function()
  focusWindow("left")
end)

hs.hotkey.bind(hyper, "d", function()
  focusWindow("right")
end)

hs.hotkey.bind(hyper, "w", function()
  focusWindow("up")
end)

hs.hotkey.bind(hyper, "s", function()
  focusWindow("down")
end)

hs.hotkey.bind(hyperShift, "a", function()
  swapWindow("left")
end)

hs.hotkey.bind(hyperShift, "d", function()
  swapWindow("right")
end)

hs.hotkey.bind(hyperShift, "w", function()
  swapWindow("up")
end)

hs.hotkey.bind(hyperShift, "s", function()
  swapWindow("down")
end)

hs.hotkey.bind(hyper, "m", function()
  placeFocused(1, 1, 1)
end)

hs.hotkey.bind(hyper, "f", function()
  placeFocused(1, 1, 1)
end)

hs.hotkey.bind(hyper, "1", function()
  placeFocused(1, 1, 3)
end)

hs.hotkey.bind(hyper, "2", function()
  placeFocused(2, 1, 3)
end)

hs.hotkey.bind(hyper, "3", function()
  placeFocused(3, 1, 3)
end)

hs.hotkey.bind(hyper, "4", function()
  placeFocused(1, 2, 3)
end)

hs.hotkey.bind(hyper, "5", function()
  placeFocused(2, 2, 3)
end)

hs.hotkey.bind(hyper, "q", function()
  moveFocusedToScreen(-1)
end)

hs.hotkey.bind(hyper, "e", function()
  moveFocusedToScreen(1)
end)

hs.hotkey.bind(hyper, "r", hs.reload)

local rguiBindings = {}

local function bindRgui(key, handler, shiftedHandler)
  rguiBindings[hs.keycodes.map[key]] = {
    normal = handler,
    shifted = shiftedHandler,
  }
end

bindRgui("a", function()
  focusWindow("left")
end, function()
  swapWindow("left")
end)

bindRgui("d", function()
  focusWindow("right")
end, function()
  swapWindow("right")
end)

bindRgui("w", function()
  focusWindow("up")
end, function()
  swapWindow("up")
end)

bindRgui("s", function()
  focusWindow("down")
end, function()
  swapWindow("down")
end)

bindRgui("c", tileFocusedScreen)
bindRgui("v", toggleAutoColumns)
bindRgui("\\", toggleMonitorInput)

bindRgui("m", function()
  placeFocused(1, 1, 1)
end)

bindRgui("f", function()
  placeFocused(1, 1, 1)
end)

bindRgui("1", function()
  placeFocused(1, 1, 3)
end)

bindRgui("2", function()
  placeFocused(2, 1, 3)
end)

bindRgui("3", function()
  placeFocused(3, 1, 3)
end)

bindRgui("4", function()
  placeFocused(1, 2, 3)
end)

bindRgui("5", function()
  placeFocused(2, 2, 3)
end)

bindRgui("q", function()
  moveFocusedToScreen(-1)
end)

bindRgui("e", function()
  moveFocusedToScreen(1)
end)

bindRgui("r", hs.reload)

local rguiTap = hs.eventtap.new({
  hs.eventtap.event.types.flagsChanged,
  hs.eventtap.event.types.keyDown,
}, function(event)
  local keyCode = event:getKeyCode()
  local eventType = event:getType()

  if eventType == hs.eventtap.event.types.flagsChanged and keyCode == rightCommandKeyCode then
    rightCommandDown = event:getFlags().cmd
    if rightCommandDown then
      rightCommandUsed = false
    elseif not rightCommandUsed then
      hs.eventtap.keyStroke({}, "escape", 0)
    end
    return false
  end

  if eventType ~= hs.eventtap.event.types.keyDown or not rightCommandDown then
    return false
  end

  local binding = rguiBindings[keyCode]
  if not binding then
    return false
  end

  rightCommandUsed = true
  local flags = event:getFlags()
  local handler = flags.shift and binding.shifted or binding.normal
  if handler then
    handler()
  end

  return true
end)

rguiTap:start()

notify("Hammerspoon loaded")
