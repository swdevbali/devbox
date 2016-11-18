-- ==========================================
-- ESC
-- ==========================================
send_escape = false
last_mods = {}

control_key_handler = function()
  send_escape = false
end

control_key_timer = hs.timer.delayed.new(0.15, control_key_handler)

control_handler = function(evt)
  local new_mods = evt:getFlags()
  if last_mods["ctrl"] == new_mods["ctrl"] then
    return false
  end
  if not last_mods["ctrl"] then
    last_mods = new_mods
    send_escape = true
    control_key_timer:start()
  else
    if send_escape then
      hs.eventtap.keyStroke({"ctrl"}, "[")
    end
    last_mods = new_mods
    control_key_timer:stop()
  end
  return false
end

control_tap = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, control_handler)
control_tap:start()

other_handler = function(evt)
  send_escape = false
  return false
end

other_tap = hs.eventtap.new({hs.eventtap.event.types.keyDown}, other_handler)
other_tap:start()

-- ==========================================
-- Left Shift
-- ==========================================

send_left_paren = false
left_paren_last_mods = {}

left_shift_key_handler = function()
  send_left_paren = false
end

left_shift_key_timer = hs.timer.delayed.new(0.15, left_shift_key_handler)

left_shift_handler = function(evt)
  if evt:getKeyCode() ~= 56 then
    return
  end
  local new_mods = evt:getFlags()
  if left_paren_last_mods["shift"] == new_mods["shift"] then
    return false
  end
  if not left_paren_last_mods["shift"] then
    left_paren_last_mods = new_mods
    send_left_paren = true
    left_shift_key_timer:start()
  else
    if send_left_paren then
      hs.eventtap.keyStroke({"shift"}, "9")
    end
    left_paren_last_mods = new_mods
    left_shift_key_timer:stop()
  end
  return false
end

left_shift_tap = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, left_shift_handler)
left_shift_tap:start()

left_shift_other_handler = function(evt)
  send_left_paren = false
  return false
end

left_shift_other_tap = hs.eventtap.new({hs.eventtap.event.types.keyDown}, left_shift_other_handler)
left_shift_other_tap:start()

-- ==========================================
-- Right Shift
-- ==========================================

send_right_paren = false
right_paren_last_mods = {}

right_shift_key_handler = function()
  send_right_paren = false
end

right_shift_key_timer = hs.timer.delayed.new(0.15, right_shift_key_handler)

right_shift_handler = function(evt)
  if evt:getKeyCode() ~= 60 then
    return
  end
  local new_mods = evt:getFlags()
  if right_paren_last_mods["shift"] == new_mods["shift"] then
    return false
  end
  if not right_paren_last_mods["shift"] then
    right_paren_last_mods = new_mods
    send_right_paren = true
    right_shift_key_timer:start()
  else
    if send_right_paren then
      hs.eventtap.keyStroke({"shift"}, "0")
    end
    right_paren_last_mods = new_mods
    right_shift_key_timer:stop()
  end
  return false
end

right_shift_tap = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, right_shift_handler)
right_shift_tap:start()

right_shift_other_handler = function(evt)
  send_right_paren = false
  return false
end

right_shift_other_tap = hs.eventtap.new({hs.eventtap.event.types.keyDown}, right_shift_other_handler)
right_shift_other_tap:start()