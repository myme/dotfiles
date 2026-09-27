-- Hyprland 0.56 Lua configuration. Home Manager installs this as hyprland.lua.
local terminal = "ghostty"
local fileManager = "nautilus"
local mainMod = "SUPER"

hl.config({
  ecosystem = { no_update_news = true },
  general = {
    gaps_in = 5,
    gaps_out = 5,
    border_size = 2,
    col = {
      active_border = { colors = { "rgba(ff79c6ee)", "rgba(50fa7bee)" }, angle = 45 },
      inactive_border = "rgba(595959aa)",
    },
    resize_on_border = false,
    allow_tearing = false,
    layout = "dwindle",
  },
  group = {
    col = {
      border_active = { colors = { "rgba(ff79c6ee)", "rgba(50fa7bee)" }, angle = 45 },
      border_inactive = "rgba(595959aa)",
    },
    groupbar = {
      indicator_height = 10,
      rounding = 5,
      col = {
        active = { colors = { "rgba(ff79c6ee)", "rgba(50fa7bee)" }, angle = 45 },
        inactive = "rgba(595959aa)",
      },
      render_titles = false,
    },
  },
  decoration = {
    rounding = 10,
    active_opacity = 1.0,
    inactive_opacity = 1.0,
    shadow = { enabled = true, range = 4, render_power = 3, color = "rgba(1a1a1aee)" },
    blur = { enabled = true, size = 3, passes = 1, vibrancy = 0.1696 },
  },
  animations = { enabled = true },
  dwindle = { preserve_split = true },
  master = { new_status = "master" },
  misc = { force_default_wallpaper = 0, disable_hyprland_logo = true },
  input = {
    kb_layout = "us",
    kb_variant = "alt-intl-unicode",
    follow_mouse = 1,
    sensitivity = 0.5,
    touchpad = { natural_scroll = true },
  },
})

hl.curve("myBezier", { type = "bezier", points = { { 0.05, 0.9 }, { 0.1, 1.05 } } })
hl.animation({ leaf = "windows", enabled = true, speed = 3, bezier = "myBezier" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 3, bezier = "default", style = "popin 80%" })
hl.animation({ leaf = "border", enabled = true, speed = 5, bezier = "default" })
hl.animation({ leaf = "borderangle", enabled = true, speed = 4, bezier = "default" })
hl.animation({ leaf = "fade", enabled = true, speed = 3, bezier = "default" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 3, bezier = "default" })

hl.gesture({ fingers = 3, direction = "horizontal", action = "workspace" })
hl.device({ name = "epic-mouse-v1", sensitivity = -0.5 })

hl.on("hyprland.start", function()
  hl.exec_cmd("hyprctl setcursor capitaine-cursors 50")
end)

-- Keep the existing nwg-displays monitor presets usable. Only this small
-- generated monitor file is read; the main config and all bindings use Lua.
local configHome = os.getenv("XDG_CONFIG_HOME") or (os.getenv("HOME") .. "/.config")
local monitorFile = io.open(configHome .. "/hypr/monitors.conf", "r")
if monitorFile then
  for line in monitorFile:lines() do
    local output, mode, position, scale = line:match("^%s*monitor%s*=%s*([^,]*),([^,]+),([^,]+),([^,%s]+)")
    if output then
      hl.monitor({ output = output, mode = mode, position = position, scale = tonumber(scale) or scale })
    end
  end
  monitorFile:close()
else
  hl.monitor({ output = "", mode = "preferred", position = "auto", scale = 1 })
end

-- Launchers and window controls.
hl.bind(mainMod .. " + Return", hl.dsp.exec_cmd(terminal .. " -e tmux"))
hl.bind(mainMod .. " + SHIFT + Return", hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + SHIFT + W", hl.dsp.window.close())
hl.bind(mainMod .. " + SHIFT + Q", hl.dsp.exec_cmd("hyprquit"))
hl.bind(mainMod .. " + E", hl.dsp.exec_cmd(fileManager))
hl.bind(mainMod .. " + D", hl.dsp.exec_cmd("rofi -show drun -show-icons"))
hl.bind(mainMod .. " + SHIFT + D", hl.dsp.exec_cmd('rofi -show combi -combi-modi "run,drun"'))
hl.bind(mainMod .. " + Tab", hl.dsp.exec_cmd("rofi -show window -show-icons"))
hl.bind(mainMod .. " + SHIFT + E", hl.dsp.exec_cmd("rofimoji"))
hl.bind(mainMod .. " + S", hl.dsp.exec_cmd("rofi -show ssh"))
hl.bind("CTRL + ALT + L", hl.dsp.exec_cmd("hyprlock"))

hl.bind(mainMod .. " + V", function()
  hl.dispatch(hl.dsp.window.float({ action = "set" }))
  hl.dispatch(hl.dsp.window.center())
  hl.dispatch(hl.dsp.window.alter_zorder({ mode = "top" }))
end)
hl.bind(mainMod .. " + SHIFT + V", hl.dsp.window.float({ action = "unset" }))
hl.bind(mainMod .. " + T", hl.dsp.group.toggle())
hl.bind(mainMod .. " + X", hl.dsp.exec_cmd("nixon -b rofi run"))
hl.bind(mainMod .. " + SHIFT + X", hl.dsp.exec_cmd("nixon -b rofi project"))
hl.bind("Print", hl.dsp.exec_cmd("hyprgrab output"))
hl.bind("SHIFT + Print", hl.dsp.exec_cmd("hyprgrab region"))
hl.bind(mainMod .. " + SHIFT + backslash", hl.dsp.window.pseudo())
hl.bind(mainMod .. " + backslash", hl.dsp.layout("togglesplit"))
hl.bind(mainMod .. " + F", hl.dsp.window.fullscreen())
hl.bind(mainMod .. " + SHIFT + F", hl.dsp.window.fullscreen_state({ internal = 0, client = 2 }))

-- Focus and move windows. The letter bindings mirror the arrow bindings.
local directions = { left = "l", right = "r", up = "u", down = "d", h = "l", l = "r" }
for key, direction in pairs(directions) do
  hl.bind(mainMod .. " + " .. key, hl.dsp.focus({ direction = direction }))
  hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ direction = direction }))
end
hl.bind(mainMod .. " + K", hl.dsp.window.cycle_next())
hl.bind(mainMod .. " + J", hl.dsp.window.cycle_next({ next = false }))
hl.bind(mainMod .. " + SHIFT + K", hl.dsp.window.move({ direction = "u" }))
hl.bind(mainMod .. " + SHIFT + J", hl.dsp.window.move({ direction = "d" }))

hl.bind(mainMod .. " + CTRL + H", hl.dsp.window.resize({ x = -40, y = 0, relative = true }))
hl.bind(mainMod .. " + CTRL + L", hl.dsp.window.resize({ x = 40, y = 0, relative = true }))
hl.bind(mainMod .. " + CTRL + K", hl.dsp.window.resize({ x = 0, y = -40, relative = true }))
hl.bind(mainMod .. " + CTRL + J", hl.dsp.window.resize({ x = 0, y = 40, relative = true }))

hl.bind(mainMod .. " + N", hl.dsp.focus({ workspace = "m+1" }))
hl.bind(mainMod .. " + P", hl.dsp.focus({ workspace = "m-1" }))
hl.bind(mainMod .. " + SHIFT + N", hl.dsp.window.move({ workspace = "+1" }))
hl.bind(mainMod .. " + SHIFT + P", hl.dsp.window.move({ workspace = "-1" }))
hl.bind(mainMod .. " + C", hl.dsp.focus({ workspace = "empty" }))
hl.bind(mainMod .. " + SHIFT + C", hl.dsp.window.move({ workspace = "empty" }))

for workspace = 1, 10 do
  local key = tostring(workspace % 10)
  hl.bind(mainMod .. " + " .. key, hl.dsp.focus({ workspace = workspace }))
  hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = workspace }))
  hl.bind(mainMod .. " + CTRL + " .. key, function()
    hl.dispatch(hl.dsp.workspace.move({ workspace = workspace, monitor = "current" }))
    hl.dispatch(hl.dsp.focus({ workspace = workspace }))
  end)
end
hl.bind(mainMod .. " + slash", hl.dsp.workspace.toggle_special("magic"))
hl.bind(mainMod .. " + SHIFT + slash", hl.dsp.window.move({ workspace = "special:magic" }))
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up", hl.dsp.focus({ workspace = "e-1" }))
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })
hl.bind(mainMod .. " + CTRL + SHIFT + H", hl.dsp.workspace.move({ monitor = "l" }))
hl.bind(mainMod .. " + CTRL + SHIFT + L", hl.dsp.workspace.move({ monitor = "r" }))

hl.bind("XF86AudioMute", hl.dsp.exec_cmd("amixer set Master toggle"), { locked = true })
hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd("amixer set Capture toggle"), { locked = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("amixer set Master 1%-"), { locked = true })
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("amixer set Master 1%+"), { locked = true })

hl.window_rule({ match = { class = ".*" }, suppress_event = "maximize" })
