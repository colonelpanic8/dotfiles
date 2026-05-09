local function config_dir()
  local source = debug.getinfo(1, "S").source
  if source:sub(1, 1) == "@" then
    source = source:sub(2)
  end

  local dir = source:match("^(.*)/[^/]*$")
  if dir and dir ~= "" then
    return dir
  end

  return "."
end

local base_dir = config_dir()
package.path = table.concat({
  base_dir .. "/?.lua",
  base_dir .. "/?/init.lua",
  package.path,
}, ";")

local modules = {
  "hyprland.state",
  "hyprland.core",
  "hyprland.layouts",
  "hyprland.windows",
  "hyprland.settings",
  "hyprland.binds",
  "hyprland.events",
}

for _, module in ipairs(modules) do
  package.loaded[module] = nil
end

local ctx = require(modules[1])
setmetatable(ctx, { __index = _G })

for i = 2, #modules do
  require(modules[i]).setup(ctx)
end
