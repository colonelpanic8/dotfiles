local callback
local started = false
hs = {
  eventtap = {
    event = {
      rawFlagMasks = {
        command = 0x100000,
        control = 0x40000,
        alternate = 0x80000,
        deviceLeftCommand = 0x8,
        deviceLeftControl = 0x1,
        deviceLeftAlternate = 0x20,
        deviceRightCommand = 0x10,
      },
    },
    new = function(types, handler)
      assert(types[1] == "all")
      callback = handler
      return {
        start = function(self)
          started = true
          return self
        end,
      }
    end,
  },
}

dofile("dotfiles/hammerspoon/hyperkey.lua")
assert(started)

local cases = {
  { "Right Command", 0x100010, 0x1C0029 },
  { "Right Command with Shift", 0x120012, 0x1E002B },
  { "Left Command", 0x100008, 0x100008 },
  { "Control", 0x40001, 0x40001 },
  { "Both Command keys", 0x100018, 0x1C0029 },
  { "Right Command with Fn", 0x900010, 0x9C0029 },
  { "Released modifiers", 0, 0 },
}

for _, case in ipairs(cases) do
  local flags = case[2]
  local event = {
    rawFlags = function(self, value)
      if value ~= nil then
        flags = value
        return self
      end
      return flags
    end,
  }
  assert(callback(event) == false, case[1] .. " must propagate")
  assert(flags == case[3], case[1] .. " emitted incorrect modifiers")
end
print("Hyper key modifier tests passed")
