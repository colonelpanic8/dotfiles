local command = "/run/current-system/sw/bin/mac-camera"
local camera = {}

if hs.fs.attributes(command) then
  local function control(action)
    hs.task.new(command, function(code, _, err)
      hs.alert.show(code == 0 and ("Camera/microphone: " .. action) or err)
    end, { action }):start()
  end

  camera.task = hs.task.new(command, function(code)
    if code ~= 0 then return end
    camera.menu = hs.menubar.new():setTitle("Camera/Mic")
    camera.menu:setMenu(function()
      local paused = hs.fs.attributes(os.getenv("HOME") .. "/Library/Application Support/TailnetCamera/paused")
      return {
        { title = paused and "Paused" or "Enabled at login", disabled = true },
        { title = "Pause camera and microphone", fn = function() control("pause") end },
        { title = "Resume camera and microphone", fn = function() control("resume") end },
        { title = "Show OBS controls", fn = function() hs.application.open("OBS") end },
      }
    end)
  end, { "status" }):start()
end

return camera
