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

local function shell_quote(value)
  return "'" .. tostring(value):gsub("'", "'\\''") .. "'"
end

local function module_load_phase(name)
  if name == "state" then
    return 0
  elseif name == "binds" then
    return 20
  elseif name == "events" then
    return 30
  end

  return 10
end

local function discover_modules()
  local modules_dir = base_dir .. "/hyprland"
  local handle = assert(io.popen("find " .. shell_quote(modules_dir) .. " -maxdepth 1 -type f -name '*.lua' -print"))
  local discovered = {}

  for path in handle:lines() do
    local name = path:match("/([^/]+)%.lua$")
    if name then
      discovered[#discovered + 1] = {
        name = name,
        module = "hyprland." .. name,
        phase = module_load_phase(name),
      }
    end
  end

  handle:close()

  table.sort(discovered, function(left, right)
    if left.phase ~= right.phase then
      return left.phase < right.phase
    end

    return left.name < right.name
  end)

  local modules = {}
  for _, item in ipairs(discovered) do
    modules[#modules + 1] = item.module
  end

  if modules[1] ~= "hyprland.state" then
    error("hyprland/state.lua is required")
  end

  return modules
end

local modules = discover_modules()

for _, module in ipairs(modules) do
  package.loaded[module] = nil
end

local ctx = require(modules[1])
setmetatable(ctx, { __index = _G })

for i = 2, #modules do
  require(modules[i]).setup(ctx)
end
