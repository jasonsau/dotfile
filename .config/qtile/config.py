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
        Key([mod], "Up", "XF86AudioRaiseVolume", lazy.spawn(
            "pactl set-sink-volume @DEFAULT_SINK@ +5%")),
        Key([mod], "Down", "XF86AudioLowerVolume", lazy.spawn(
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
               (" ", {'layout': 'monadtall'}),
               (" ", {'layout': 'monadtall'}),
               (" ", {'layout': 'monadtall'}),
               (" ", {'layout': 'floating'}),
               ("ﱘ ", {'layout': 'monadtall'}),
               ]
colors_dracula = {
        "Background": "#282a36",
        "Foreground": "#f8f8f2",
        "Comment": "#6272a4",
        "Current": "#44475a",
        "Cyan": "#8be9fd",
        "Green": "#50fa7b",
        "Orange": "#ffb86c",
        "Pink": "#ad00a8",
        "Purple": "#bd93f9",
        "Red": "#ff5555",
        "Yellow": "#f1fa8c"
        }

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    # Switch to another group
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name)))

layout_theme = {"border_width": 2,
                "margin": 10,
                "border_focus": colors_dracula["Purple"],
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




# Widgets Default
widget_defaults = dict(
    font="UbuntuMono NerdFont",
    fontsize=12,
    padding=3,
    background=colors_dracula["Background"]
)

extension_defaults = widget_defaults.copy()

def init_widgets_list():
    widgets_list = [
        widget.CurrentLayoutIcon(
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Purple"],
            padding=5,
        ),
        widget.CurrentLayout(
            font="mononoki NerdFont",
            fontsize=11,
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Background"],
        ),
        widget.GroupBox(
            font="mononoki nerd font",
            fontsize=12,
            margin_y=3,
            margin_x=0,
            padding_y=5,
            padding_x=5,
            borderwidth=3,
            active=colors_dracula["Pink"],
            inactive=colors_dracula["Foreground"],
            block_highlight_text_color=colors_dracula["Foreground"],
            rounded = True,
            highlight_method="block",
            this_current_screen_border=colors_dracula["Comment"],
            this_screen_border=colors_dracula["Current"],
            other_current_screen_border=colors_dracula["Comment"],
            other_screen_border=colors_dracula["Current"],
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Background"],
        ),
        widget.WindowCount(
            background = colors_dracula["Background"],
            foreground = colors_dracula["Foreground"],
            font = "mononoki NerdFont",
            show_zero = True,
            text_format = '  {num}',
        ),
        widget.WindowName(
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Background"],
            font="mononoki NerdFont",
            fontsize=12,
        ),
        widget.TextBox(
            # text="",
            text="",
            foreground=colors_dracula["Current"],
            background=colors_dracula["Background"],
            # padding=-3.3,
             padding=-0.5,
            # fontsize=45
            fontsize=25,
        ),
        widget.TextBox(
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Current"],
            text="",
            padding=4
        ),
        widget.Net(
            interface="enp37s0",
            format='{down}↓↑{up}',
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Current"],
        ),
        widget.TextBox(
            text=" ",
            foreground=colors_dracula["Current"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            # text="",
            text="",
            foreground=colors_dracula["Comment"],
            background=colors_dracula["Background"],
            # padding=-3.3,
             padding=-0.5,
            # fontsize=45
            fontsize=25,
        ),
        widget.TextBox(
            text="",
            background=colors_dracula["Comment"],
            foreground=colors_dracula["Foreground"],
            padding=10,
        ),
        widget.KeyboardLayout(
            background=colors_dracula["Comment"],
            foreground=colors_dracula["Foreground"],
            padding=0,
            configured_keyboards=['us dvorak', 'us', 'latam'],
        ),
        widget.TextBox(
            text=" ",
            foreground=colors_dracula["Comment"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            # text="",
            text="",
            foreground=colors_dracula["Purple"],
            background=colors_dracula["Background"],
            # padding=-3.3,
             padding=-0.5,
            # fontsize=45
            fontsize=25,
        ),
        widget.TextBox(
            text="墳",
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Purple"],
            padding=8,
            mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("pavucontrol")},
        ),
        widget.Volume(
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Purple"],
        ),
        widget.TextBox(
            text=" ",
            foreground=colors_dracula["Purple"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            text="",
            foreground=colors_dracula["Pink"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            text=" ",
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Pink"],
            padding=5,
            mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("alacritty -e htop")},
        ),
        widget.CPU(
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Pink"],
            format = '{freq_current}GHz {load_percent}%',
            font = "mononoki Nerd Font"
        ),
        widget.TextBox(
            text=" ",
            foreground=colors_dracula["Pink"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            text="",
            foreground=colors_dracula["Current"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            text="",
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Current"],
            padding=5,
            mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("alacritty -e htop")},
        ),
        widget.Memory(
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Current"],
        ),
        widget.TextBox(
            text=" ",
            foreground=colors_dracula["Current"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            text="",
            foreground=colors_dracula["Comment"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            text=" ",
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Comment"],
            padding=8,
        ),
        widget.ThermalSensor(
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Comment"],
            padding=2,
            mouse_callbacks={'Button1': lambda: qtile.cmd_spawn("psensor")},
        ),
        widget.TextBox(
            text=" ",
            foreground=colors_dracula["Comment"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            text="",
            foreground=colors_dracula["Purple"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Purple"],
            text="  ",
        ),
        widget.Clock(
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Purple"],
            format='%I:%M %p',
        ),
        widget.TextBox(
            text=" ",
            foreground=colors_dracula["Purple"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            text="",
            foreground=colors_dracula["Pink"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Pink"],
            text=" ",
        ),
        widget.Clock(
            foreground=colors_dracula["Foreground"],
            background=colors_dracula["Pink"],
            format='%a %d-%m',
        ),
        widget.TextBox(
            text=" ",
            foreground=colors_dracula["Pink"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.TextBox(
            text="",
            foreground=colors_dracula["Current"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.CheckUpdates(
            update_interval = 1800,
            background = colors_dracula["Current"],
            foreground = colors_dracula["Foreground"],
            colour_no_updates = colors_dracula["Red"],
            # custom_command = "checkupdates",
            display_format =  "{updates} ﮮ Updates",
            distro = 'Arch_checkupdates',
            mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(my_Term+" -e sudo pacman -Syu")},
        ),
        widget.TextBox(
            text=" ",
            foreground=colors_dracula["Current"],
            background=colors_dracula["Background"],
            padding=-0.5,
            fontsize=25,
        ),
        widget.Systray(
            background="#000000",
            padding=5,
        ),

    ]
    return widgets_list

def init_widgets_screen1():
    widgets_screen1 = init_widgets_list()
    return widgets_screen1

def init_screens():
    screens = [
        Screen(top=bar.Bar(widgets = init_widgets_screen1(), opacity = 1, size=22)),
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
auto_minimize = True


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
    Match(wm_class='redshift-gtk'),
    Match(wm_class='nitrogen'),
])

#autostart
@hook.subscribe.startup_complete
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([home])

auto_fullscreen = True
focus_on_window_activation = "smart"

wmname = "LG3D"
