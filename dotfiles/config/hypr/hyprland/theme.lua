local M = {}

-- Material 3 roles DMS generated for its stock dark scheme.
local fallback_colors = {
  primary = "d0bcff",
  on_primary = "381e72",
  primary_container = "4f378b",
  on_primary_container = "e6e0e9",
  secondary = "ccc2dc",
  surface = "141218",
  surface_container = "211f24",
  surface_container_high = "2b292f",
  on_surface = "e6e0e9",
  on_surface_variant = "cac4cf",
  outline = "948f99",
  outline_variant = "49454e",
  error = "f2b8b5",
  on_error = "601410",
}

local fallback_font_family = "Sans"

local function read_file(path)
  local handle = io.open(path, "r")
  if not handle then
    return nil
  end

  local contents = handle:read("*a")
  handle:close()
  return contents
end

local function dms_cache_path(name)
  local cache_home = os.getenv("XDG_CACHE_HOME")
  if not cache_home or cache_home == "" then
    cache_home = (os.getenv("HOME") or "") .. "/.cache"
  end

  return cache_home .. "/dms/" .. name
end

local function load_dms_colors()
  local colors = {}
  for role, value in pairs(fallback_colors) do
    colors[role] = value
  end

  local contents = read_file(dms_cache_path("dms-colors.json"))
  if not contents then
    return colors
  end

  local mode = contents:match('"mode"%s*:%s*"(%a+)"') or "dark"
  local _, colors_start = contents:find('"colors"%s*:%s*{')
  local scheme = colors_start and contents:match('"' .. mode .. '"%s*:%s*(%b{})', colors_start)
  if scheme then
    for role, value in scheme:gmatch('"([%w_]+)"%s*:%s*"#(%x%x%x%x%x%x)"') do
      colors[role] = value:lower()
    end
  end

  return colors
end

local function load_dms_font_family()
  local contents = read_file(dms_cache_path("dms-typography.json"))
  return contents and contents:match('"fontFamily"%s*:%s*"([^"]+)"') or fallback_font_family
end

function M.setup(ctx)
  local _ENV = ctx

  -- DMS rewrites this alongside dms-colors.json on every theme change.
  -- Requiring it makes Hyprland watch it and reload, which re-reads the palette.
  pcall(require, "dms.colors")

  local colors = load_dms_colors()

  theme = {
    colors = colors,
    font_family = load_dms_font_family(),
  }

  function theme.rgba(role, alpha)
    return string.format("rgba(%s%02x)", colors[role] or fallback_colors[role], math.floor((alpha or 1) * 255 + 0.5))
  end
end

return M
