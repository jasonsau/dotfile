import subprocess
from os import listdir
from os import path
import json
import os
from Xlib import display as xdisplay
from libqtile.config import Key, Screen, Group, Drag, Click, Match
from libqtile.lazy import lazy
from libqtile import layout, bar, widget, hook
from typing import List  
from libqtile import qtile

my_Term = "alacritty"
mod = "mod4"


keys = [
        # Switch between windows in current stack pane
        Key([mod], "j", lazy.layout.down()),
        Key([mod], "k", lazy.layout.up()),

        # Moviendo el focus en stack

        # Movimientos en Monadtall
        Key([mod, "control"], "h", lazy.layout.left()),
        Key([mod, "control"], "l", lazy.layout.right()),
        Key([mod, "shift"], "h", lazy.layout.swap_left()),
        Key([mod, "shift"], "l", lazy.layout.swap_right()),
        Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
        Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
        Key([mod], "m", lazy.layout.maximize()),
        Key([mod], "n", lazy.layout.normalize()),
        Key([mod], "h", lazy.layout.grow()),
        Key([mod], "l", lazy.layout.shrink()),

        # Switch window focus to other pane(s) of stack
        Key([mod], "space", lazy.layout.next()),

        # Swap panes of split stack
        Key([mod, "shift"], "space", lazy.layout.rotate()),
        Key([mod, "shift"], "Return", lazy.layout.toggle_split()),
        Key([mod], "Return", lazy.spawn(my_Term)),

        # Toggle between different layouts as defined below
        Key([mod], "Tab", lazy.next_layout()),
        Key([mod], "w", lazy.window.kill()),

        Key([mod, "control"], "r", lazy.restart()),
        Key([mod, "control"], "q", lazy.shutdown()),

        # Comandos personalizados
        Key([mod, "shift"], "m", lazy.spawn("rofi -show drun -show-icons")),
        Key([mod, "shift"], "n", lazy.spawn("rofi -show window -show-icons")),
        Key([mod, "shift"], "r", lazy.spawn("dmenu_run -p 'Run: '")),
        Key([mod, "shift"], "Return", lazy.spawn("kitty")),

        # Subir y bajar brillo
        Key([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +10%")),
        Key([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 10%-")),

        # Subir y bajar volumen
        Key([], "XF86AudioRaiseVolume", lazy.spawn(
            "pactl set-sink-volume @DEFAULT_SINK@ +5%")),
        Key([], "XF86AudioLowerVolume", lazy.spawn(
            "pactl set-sink-volume @DEFAULT_SINK@ -5%")),
        Key([mod], "v", lazy.spawn("pavucontrol")),

        # widgets_screen2 = init_widgets_list()
        Key([], "XF86Search", lazy.spawn("firefox")),
        Key([mod, "shift"], "p", lazy.spawn("pcmanfm")),

        # Cambio de pantalla
        Key([mod, "shift"], "w", lazy.to_screen(0)),
        Key([mod, "shift"], "e", lazy.to_screen(1)),
        Key([mod], "period", lazy.next_screen()),
        Key([mod], "comma", lazy.prev_screen()),

        Key([mod, "shift"], "f", lazy.window.toggle_floating()),

        Key([mod, "shift"], "g", lazy.window.toggle_fullscreen()),

        # Stack Controls
        Key([mod, "shift"], "space", lazy.layout.rotate(), lazy.layout.flip()),
        Key([mod, "control"], "Return", lazy.layout.toggle_split()),
    ]

group_names = [(" ", {'layout': 'monadtall'}),
               (" ", {'layout': 'monadtall'}),
               (" ", {'layout': 'monadtall'}),
               ("謹 ", {'layout': 'monadtall'}),
               (" ", {'layout': 'monadtall'}),
               (" ", {'layout': 'monadtall'}),
               (" ", {'layout': 'floating'}),
               ("ﱘ ", {'layout': 'monadtall'}),
               ]

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    # Switch to another group
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name)))

layout_theme = {"border_width": 2,
                "margin": 8,
                "border_focus": "#e1acff",
                "border_normal": "#1D2330"
                }

layouts = [
    # layout.Stack(stacks=2, **layout_theme),
    # layout.RatioTile(**layout_theme),
    layout.MonadTall(**layout_theme),
    layout.Max(**layout_theme),
    layout.MonadWide(**layout_theme),
    layout.Columns(**layout_theme, split = False, num_columns = 3),
    layout.Matrix(**layout_theme),
    layout.Zoomy(**layout_theme),
    # layout.VerticalTile(**layout_theme),
    # layout.Bsp(**layout_theme),
    # layout.Tile(shift_windows=True, **layout_theme),
    # layout.Stack(num_stacks=2),

    layout.Floating(**layout_theme)
]

colors = [["#282a36", "#282a36"],  # panel background
          ["#434758", "#434758"],  # background for current screen tab
          ["#ffffff", "#ffffff"],  # font color for group names
          ["#ff5555", "#ff5555"],  # border line color for current tab
          ["#8d62a9", "#8d62a9"],  # border line color for other tab and odd widgets
          ["#668bd7", "#668bd7"],  # color for the even widgets
          ["#e1acff", "#e1acff"], ]  # window name


##### PROMPT #####
# prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

### MOUSE CALLBACKS ###


def open_pavucontrol(qtile):
    qtile.cmd_spawn("pavucontrol")

def open_psensor(qtile):
    qtile.cmd_spawn("psensor")


# Widgets Default
widget_defaults = dict(
    font="UbuntuMono NerdFont",
    fontsize=12,
    padding=3,
    background=colors[2]
)
extension_defaults = widget_defaults.copy()

def init_widgets_list():
    widgets_list = [
        widget.CurrentLayoutIcon(
            foreground=colors[2],
            background=colors[4],
            padding=5,
        ),
        widget.CurrentLayout(
            font="mononoki NerdFont",
            fontsize=10,
            foreground=colors[2],
            background=colors[0],
        ),
        widget.GroupBox(
            font="mononoki nerd font",
            fontsize=12,
            margin_y=3,
            margin_x=0,
            padding_y=5,
            padding_x=5,
            borderwidth=3,
            active=colors[2],
            inactive=colors[2],
            rounded = True,
            highlight_color=colors[1],
            highlight_method="line",
            this_current_screen_border=colors[6],
            this_screen_border=colors[4],
            other_current_screen_border=colors[0],
            other_screen_border=colors[0],
            foreground=colors[2],
            background=colors[0],
        ),
        widget.WindowCount(
            background = colors[0],
            foreground = colors[6],
            font = "mononoki NerdFont",
            show_zero = True,
            text_format = '  {num}',
        ),
        widget.WindowName(
            foreground=colors[6],
            background=colors[0],
            font="mononoki NerdFont",
            fontsize=12,
        ),
        widget.TextBox(
            text="",
            foreground=colors[5],
            background=colors[0],
            padding=-3.3,
            fontsize=45
        ),
        widget.TextBox(
            foreground=colors[2],
            background=colors[5],
            text="",
            padding=4
        ),
        widget.Net(
            interface="enp37s0",
            format='{down}↓↑{up}',
            foreground=colors[2],
            background=colors[5],
        ),
        widget.TextBox(
            text="",
            background=colors[5],
            foreground=colors[2],
            padding=10,
        ),
        widget.KeyboardLayout(
            foreground=colors[2],
            background=colors[5],
            padding=0,
            configured_keyboards=['us dvorak', 'us', 'latam'],
        ),
        widget.TextBox(
            text="墳",
            foreground=colors[2],
            background=colors[5],
            padding=8,
            mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("pavucontrol")},
        ),
        widget.Volume(
            foreground=colors[2],
            background=colors[5],
        ),
        widget.TextBox(
            text="",
            foreground="#bd93f9",
            background=colors[5],
            padding=-3.3,
            fontsize=45
        ),
        widget.TextBox(
            text=" ",
            foreground=colors[2],
            background="#bd93f9",
            padding=5,
            mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("alacritty -e htop")},
        ),
        widget.CPU(
            foreground=colors[2],
            background="#bd93f9",
            format = '{freq_current}GHz {load_percent}%'
        ),
        widget.TextBox(
            text="",
            foreground=colors[2],
            background="#bd93f9",
            padding=5,
            mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("alacritty -e htop")},
        ),
        widget.Memory(
            foreground=colors[2],
            background="#bd93f9",
        ),
        widget.TextBox(
            text=" ",
            foreground=colors[2],
            background="#bd93f9",
            padding=8,
        ),
        widget.ThermalSensor(
            foreground=colors[2],
            background="#bd93f9",
            padding=0,
            mouse_callbacks={'Button1': lambda: qtile.cmd_spawn("psensor")},
        ),
        widget.TextBox(
            text="",
            foreground="#6272a4",
            background="#bd93f9",
            padding=-3.3,
            fontsize=45
        ),
        widget.TextBox(
            foreground=colors[2],
            background="#6272a4",
            text="  ",
        ),
        widget.Clock(
            foreground=colors[2],
            background="#6272a4",
            format='%I:%M %p',
        ),
        widget.TextBox(
            foreground=colors[2],
            background="#6272a4",
            text=" ",
        ),
        widget.Clock(
            foreground=colors[2],
            background="#6272a4",
            format='%a %d-%m',
        ),
        widget.CheckUpdates(
            background = "#6272a4",
            foreground = colors[2],
            colour_no_updates = colors[2],
            custom_command = 'checkupdates | wc -l',
            display_format =  ' ﮮ Updates: {updates}',
            distro = 'Arch',
            mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(my_Term+" -e sudo pacman -Syu")},
        ),
        widget.TextBox(
            text=" ⏻ ",
            foreground=colors[2],
            background="#6272a4",
            padding=5,
            mouse_callbacks={'Button1': lambda qtile: qtile.cmd_spawn("arcolinux-logout") },
            fontsize = 13,
        ),
        widget.TextBox(
            text="",
            foreground=colors[0],
            background="#6272a4",
            padding=-3.3,
            fontsize=45
        ),
        widget.Systray(
            background=colors[0],
            padding=5,
        ),

    ]
    return widgets_list

def init_widgets_screen1():
    widgets_screen1 = init_widgets_list()
    return widgets_screen1

def init_screens():
    screens = [
        Screen(top=bar.Bar(widgets = init_widgets_screen1(), opacity = 0.90, size=22)),
        Screen(top=bar.Bar(widgets = init_widgets_screen1()[0:5], size = 22)),
    ]
    return screens


if __name__ in ["config", "__main__"]:
    screens = init_screens()
    widgets_list = init_widgets_list()

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False


floating_layout = layout.Floating(float_rules=[
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),
    Match(wm_class='makebranch'),
    Match(wm_class='maketag'),
    Match(wm_class='ssh-askpass'),
    Match(wm_class='branchdialog'),
    Match(wm_class='pinentry'),
    Match(wm_class='Arandr'),
    Match(wm_class='spotify'),
    Match(wm_class='redshift-gtk')
])

#autostart
@hook.subscribe.startup_complete
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([home])

auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
