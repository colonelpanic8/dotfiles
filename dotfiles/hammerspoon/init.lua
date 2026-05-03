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
  widgets = {
    disk = {
      enabled = true,
      interval = 60,
      volume = "/",
    },
    memory = {
      enabled = true,
      interval = 10,
    },
  },
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

local function userSpacesForScreen(screen)
  local spaces, err = hs.spaces.spacesForScreen(screen)
  if not spaces then
    return nil, err
  end

  local userSpaces = {}
  for _, space in ipairs(spaces) do
    if hs.spaces.spaceType(space) == "user" then
      table.insert(userSpaces, space)
    end
  end

  return userSpaces
end

local function currentSpaceForScreen(screen)
  local activeSpaces = hs.spaces.activeSpaces()
  if activeSpaces and screen.getUUID then
    local uuid = screen:getUUID()
    if uuid and activeSpaces[uuid] then
      return activeSpaces[uuid]
    end
  end

  return hs.spaces.focusedSpace()
end

local function nextUserSpaceForScreen(screen, currentSpace)
  local spaces, err = userSpacesForScreen(screen)
  if not spaces then
    return nil, err
  end

  if #spaces < 2 then
    return nil, "no other Desktop on this screen"
  end

  for index, space in ipairs(spaces) do
    if space == currentSpace then
      return spaces[(index % #spaces) + 1]
    end
  end

  return spaces[1]
end

local function containsValue(values, target)
  if not values then
    return false
  end

  for _, value in ipairs(values) do
    if value == target then
      return true
    end
  end

  return false
end

local function moveFocusedToNextDesktop()
  local focused = hs.window.focusedWindow()
  if not focused then
    notify("No focused window")
    return
  end

  local screen = focused:screen()
  local targetSpace, err = nextUserSpaceForScreen(screen, currentSpaceForScreen(screen))
  if not targetSpace then
    notify("Desktop move failed: " .. tostring(err))
    return
  end

  local ok, moveErr = hs.spaces.moveWindowToSpace(focused, targetSpace, true)
  if not ok then
    notify("Desktop move failed: " .. tostring(moveErr))
    return
  end

  hs.timer.doAfter(0.2, function()
    if containsValue(hs.spaces.windowSpaces(focused), targetSpace) then
      return
    end

    notify("Desktop move blocked by macOS")
  end)
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
hs.hotkey.bind(hyper, "h", moveFocusedToNextDesktop)

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
bindRgui("h", moveFocusedToNextDesktop)

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
    return true
  end

  if eventType ~= hs.eventtap.event.types.keyDown or not rightCommandDown then
    return false
  end

  local binding = rguiBindings[keyCode]
  if not binding then
    rightCommandUsed = true
    return true
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

local menuWidgets = {}
local widgetTimers = {}

local function round(number)
  return math.floor(number + 0.5)
end

local function formatBytes(bytes)
  local units = { "B", "K", "M", "G", "T" }
  local value = bytes
  local unitIndex = 1

  while value >= 1024 and unitIndex < #units do
    value = value / 1024
    unitIndex = unitIndex + 1
  end

  if unitIndex <= 2 then
    return string.format("%d%s", round(value), units[unitIndex])
  end

  return string.format("%.1f%s", value, units[unitIndex])
end

local function formatGb(bytes)
  return string.format("%.1fGB", bytes / 1024 / 1024 / 1024)
end

local function formatCompactGb(bytes, decimals)
  return string.format("%." .. decimals .. "f", bytes / 1024 / 1024 / 1024)
end

local function updateDiskWidget()
  local widget = menuWidgets.disk
  local widgetConfig = config.widgets.disk
  if not widget then
    return
  end

  local output, success = hs.execute(string.format(
    "/bin/df -k %q | /usr/bin/awk 'NR==2 {print $2, $3, $4, $5}'",
    widgetConfig.volume
  ))
  local totalKb, usedKb, availableKb, capacity = output:match("(%d+)%s+(%d+)%s+(%d+)%s+(%d+%%)")

  if not success or not totalKb then
    widget:setTitle("Disk ?")
    widget:setTooltip("Disk usage unavailable")
    return
  end

  widget:setTitle(string.format(
    "D %s/%sGB",
    formatCompactGb(tonumber(availableKb) * 1024, 0),
    formatCompactGb(tonumber(totalKb) * 1024, 0)
  ))
  widget:setTooltip(string.format(
    "%s used, %s available on %s (%s full)",
    formatBytes(tonumber(usedKb) * 1024),
    formatBytes(tonumber(availableKb) * 1024),
    widgetConfig.volume,
    capacity
  ))
end

local function updateMemoryWidget()
  local widget = menuWidgets.memory
  if not widget then
    return
  end

  local stats = hs.host.vmStat()
  local pageSize = stats.pageSize
  local usedBytes = (
    stats.anonymousPages
    + stats.pagesWiredDown
    + stats.pagesUsedByVMCompressor
  ) * pageSize
  local totalBytes = stats.memSize
  local availableBytes = totalBytes - usedBytes
  local cacheBytes = stats.fileBackedPages * pageSize
  local freeBytes = (stats.pagesFree + stats.pagesSpeculative) * pageSize

  widget:setTitle(string.format(
    "R %s/%sGB",
    formatCompactGb(availableBytes, 1),
    formatCompactGb(totalBytes, 1)
  ))
  widget:setTooltip(string.format(
    "%s used, %s available of %s\n%s cached, %s free",
    formatBytes(usedBytes),
    formatBytes(availableBytes),
    formatBytes(totalBytes),
    formatBytes(cacheBytes),
    formatBytes(freeBytes)
  ))
end

local function createMenuWidget(name, update, interval)
  menuWidgets[name] = hs.menubar.new()
  menuWidgets[name]:setClickCallback(update)
  update()
  widgetTimers[name] = hs.timer.doEvery(interval, update)
end

local function startMenuWidgets()
  if config.widgets.disk.enabled then
    createMenuWidget("disk", updateDiskWidget, config.widgets.disk.interval)
  end

  if config.widgets.memory.enabled then
    createMenuWidget("memory", updateMemoryWidget, config.widgets.memory.interval)
  end
end

startMenuWidgets()

notify("Hammerspoon loaded")
