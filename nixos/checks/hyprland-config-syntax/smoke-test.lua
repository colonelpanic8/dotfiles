local config_path = arg[1] or "./hyprland.lua"
local callbacks = {}

local function noop() end

local dispatcher = setmetatable({}, {
  __call = function()
    error("dispatcher objects cannot be called directly; use hl.dispatch(dispatcher)", 2)
  end,
})

local function dispatcher_namespace()
  return setmetatable({}, {
    __index = function()
      return function()
        return dispatcher
      end
    end,
  })
end

local notification = {
  is_alive = function()
    return true
  end,
  set_text = noop,
  set_timeout = noop,
  pause = noop,
  resume = noop,
  set_paused = noop,
  dismiss = noop,
}

local monitor = {
  id = 1,
  name = "stub-monitor",
  focused = true,
}

local workspace = {
  id = 1,
  name = "1",
  windows = 0,
  special = false,
  monitor = monitor,
}

monitor.active_workspace = workspace

hl = {
  animation = noop,
  bind = noop,
  config = noop,
  curve = noop,
  dispatch = noop,
  env = noop,
  exec_cmd = noop,
  define_submap = function(_, reset_or_callback, callback)
    local cb = type(reset_or_callback) == "function" and reset_or_callback or callback
    if cb then
      cb()
    end
  end,
  monitor = noop,
  workspace_rule = noop,
  window_rule = noop,
  dsp = setmetatable({
    group = dispatcher_namespace(),
    window = dispatcher_namespace(),
    workspace = dispatcher_namespace(),
  }, {
    __index = function()
      return function()
        return dispatcher
      end
    end,
  }),
  notification = {
    create = function()
      return notification
    end,
  },
  plugin = {
    load = noop,
  },
  get_active_workspace = function()
    return workspace
  end,
  get_active_monitor = function()
    return monitor
  end,
  get_active_window = function()
    return nil
  end,
  get_monitor = function()
    return monitor
  end,
  get_workspace = function(id)
    if tostring(id) == "1" then
      return workspace
    end
    return nil
  end,
  get_windows = function()
    return {}
  end,
  get_workspace_windows = function()
    return {}
  end,
  on = function(_, callback)
    callbacks[#callbacks + 1] = callback
  end,
  timer = function(callback)
    callback()
    return {
      set_enabled = noop,
    }
  end,
}

dofile(config_path)

for _, callback in ipairs(callbacks) do
  callback()
end
