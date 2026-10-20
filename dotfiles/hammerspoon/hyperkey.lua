local masks = hs.eventtap.event.rawFlagMasks
local hyper = masks.command | masks.control | masks.alternate
  | masks.deviceLeftCommand | masks.deviceLeftControl | masks.deviceLeftAlternate

return hs.eventtap.new({ "all" }, function(event)
  local flags = event:rawFlags()
  if flags & masks.deviceRightCommand ~= 0 then
    event:rawFlags((flags & ~masks.deviceRightCommand) | hyper)
  end
  return false
end):start()
