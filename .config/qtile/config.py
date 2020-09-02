# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
import os
import re
import socket
import subprocess
import psutil

from os import listdir
from os import path
import json

from Xlib import display as xdisplay
from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.lazy import lazy
from libqtile import layout, bar, widget, hook
from typing import List  # noqa: F401



my_Term = "alacritty"
mod = "mod4"

theme = "/home/jason/.config/qtile/themes/dark-grey"


keys = [
    # Switch between windows in current stack pane
    Key([mod], "k", lazy.layout.down()),
    Key([mod], "j", lazy.layout.up()),

    #Moviendo el focus en stack
    Key([mod, "shift"], "k", lazy.shuffle_down()),
    Key([mod, "shift"], "j", lazy.shuffle_up()),

    # Switch window focus to other pane(s) of stack
    Key([mod], "space", lazy.layout.next()),

    # Swap panes of split stack
    Key([mod, "shift"], "space", lazy.layout.rotate()),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return", lazy.layout.toggle_split()),
    Key([mod], "Return", lazy.spawn(my_Term)),

    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod], "w", lazy.window.kill()),

    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),

    #Comandos personalizados
    Key([mod, "shift"], "m", lazy.spawn("rofi -show run")),
    Key([mod, "shift"], "n", lazy.spawn("rofi -show window")),
    Key([mod, "shift"], "r",  lazy.spawn("dmenu_run -p 'Run: '")),
    Key([mod, "shift"], "f", lazy.spawn("/usr/lib/firefoxD/firefox")),

    #Subir y bajar brillo
    Key([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +10%")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 10%-")),

    #Subir y bajar volumen
    Key([], "XF86AudioRaiseVolume", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%")),
    
    #Cambio de pantalla
    Key([mod, "shift"], "w", lazy.to_screen(0)),
    Key([mod, "shift"], "e", lazy.to_screen(1)),
    Key([mod], "period", lazy.next_screen()),
    Key([mod], "comma", lazy.prev_screen()), 


    Key([mod], "h", lazy.layout.grow()),
    Key([mod], "l", lazy.layout.shrink()),
    Key([mod, "shift"], "f", lazy.window.toggle_floating()),

    Key([mod], "m", lazy.layout.maximize()),
    Key([mod], "n", lazy.layout.normalize()),
    
    Key([mod, "shift"], "g", lazy.window.toggle_fullscreen()),


    ##Stack Controls
    Key([mod, "shift"], "space",lazy.layout.rotate(), lazy.layout.flip()),
    Key([mod, "control"], "Return", lazy.layout.toggle_split()),
]

group_names = [("WWW", {'layout': 'monadtall'}),
               ("DEV", {'layout': 'monadtall'}),
               ("SYS", {'layout': 'monadtall'}),
               ("DOC", {'layout': 'monadtall'}),
               ("VBOX", {'layout': 'floating'}),
               ("MED", {'layout': 'monadtall'}),
]


groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))        # Switch to another group
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name)))


layout_theme = {"border_width": 2,
                "margin": 6,
                "border_focus": "e1acff",
                "border_normal": "1D2330"
}

layouts = [
        #layout.MonadWide(**layout_theme),
        #layout.Bsp(**layout_theme),
        #layout.Stack(stacks=2, **layout_theme),
        #layout.Columns(**layout_theme),
        #layout.RatioTile(**layout_theme),
        #layout.VerticalTile(**layout_theme),
        #layout.Matrix(**layout_theme),
        #layout.Zoomy(**layout_theme),
        layout.MonadTall(**layout_theme),
        layout.MonadWide(**layout_theme),
        layout.Max(**layout_theme),
        layout.Tile(shift_windows=True, **layout_theme),
        layout.Stack(num_stacks=2),
        layout.Floating(**layout_theme)
]

colors = [["#282a36", "#282a36"], # panel background
          ["#434758", "#434758"], # background for current screen tab
          ["#ffffff", "#ffffff"], # font color for group names
          ["#ff5555", "#ff5555"], # border line color for current tab
          ["#8d62a9", "#8d62a9"], # border line color for other tab and odd widgets
          ["#668bd7", "#668bd7"], # color for the even widgets
          ["#e1acff", "#e1acff"]] # window name

##### PROMPT #####
#prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

### MOUSE CALLBACKS ###

def open_pavucontrol(qtile):
    qtile.cmd_spawn("pavucontrol")

def shutdown(qtile):
    qtile.cmd_spawn("alacritty -e shutdown now")


def open_psensor(qtile):
    qtile.cmd_spawn("psensor")



widget_defaults = dict(
    font="Ubuntu Mono",
    fontsize=12,
    padding=3,
    background=colors[2]
)
extension_defaults = widget_defaults.copy()

def init_widgets_list():
    widgets_list=[
            widget.CurrentLayoutIcon(
                foreground = colors[2],
                background = colors[4],
                padding = 5
                ),
            widget.CurrentLayout(
                font = "Ubuntu Bold",
                fontsize = 10,
                foreground = colors[2],
                background = colors[0]
                ),
            widget.GroupBox(
                font = "Ubuntu Bold",
                fontsize = 9,
                margin_y = 3,
                margin_x = 0,
                padding_y = 5,
                padding_x = 5,
                borderwidth = 3,
                active = colors[2],
                inactive = colors[2],
                rounded = False,
                highlight_color = colors[1],
                highlight_method = "line",
                this_current_screen_border = colors[3],
                this_screen_border = colors [4],
                other_current_screen_border = colors[0],
                other_screen_border = colors[0],
                foreground = colors[2],
                background = colors[0]
                ),
           widget.WindowName(
               foreground = colors[6],
               background = colors[0],
               padding =0,
               markup=True
               ),
            widget.TextBox(
                    foreground = colors[2],
                    background = colors[5],
                    text ='',
                    fontsize =19
                    ),
            widget.Net(
                    interface = "wlan0",
                    format = '{down} ↓↑ {up}',
                    foreground = colors[2],
                    background = colors[5]
                    ),
            widget.TextBox(
                    text = "墳",
                    foreground = colors[2],
                    background = colors[5],
                    padding = 5,
                    fontsize = 20,
                    mouse_callbacks = {'Button1': open_pavucontrol}
                ),
            widget.Volume(
                background = colors[5],
                foreground = colors[2],
                ),
            widget.TextBox(
                    text = "",
                    background = colors[5],
                    foreground = colors[2],
                    padding = 5,
                    fontsize = 20
                ),
            widget.KeyboardLayout(
                   foreground = colors[2],
                   background = colors[5],
                   markup = True
                ),
            widget.TextBox(
                   text = "",
                   foreground = colors[2],
                   background = colors[5],
                   padding = 5,
                   fontsize = 20
               ),
            widget.Memory(
                    foreground = colors[2],
                    background = colors[5]
                ),
            widget.TextBox(
                    text = "",
                    foreground = colors[2],
                    background = colors[5],
                    padding = 5,
                    fontsize = 20
                ),
            widget.ThermalSensor(
                    foreground = colors[2],
                    background = colors[5],
                    padding = 5,
                    mouse_callbacks = {'Button1': open_psensor}
                ),
            widget.TextBox(
                    foreground = colors[2],
                    background = colors[5],
                    text ='',
                    fontsize =19
                    ),
            widget.Clock(
                foreground = colors[2],
                background = colors[5],
                format='%Y-%m-%d %a %I:%M %p',
                ),
            widget.Sep(
                linewidth = 0,
                padding = 5,
                foreground = colors[0],
                background= colors[5]
                ),
           widget.TextBox(
                    text = "",
                    foreground = colors[2],
                    background = colors[5],
                    padding = 5,
                    fontsize = 20,
                    mouse_callbacks = {'Button1': shutdown}
                ),
           widget.Systray(
                foreground = colors[2],
                background = colors[5],
                padding = 5
                ),
            ]

    return widgets_list


def init_widgets_screen1():
    widgets_screen1  = init_widgets_list()
    return widgets_screen1

def init_widgets_screen2():
    widgets_screen2 = init_widgets_list()
    return widgets_screen2

prueba = init_widgets_screen1()
prueba2 = init_widgets_screen2()


def init_screens():
    screens = [
            Screen(top=bar.Bar(prueba,24),wallpaper = "/home/jason/.config/qtile/walls/nasa.jpg", wallpaper_mode = "fill"),
            Screen(top=bar.Bar(prueba2,24), wallpaper = "/home/jason/.config/qtile/walls/0006.jpg", wallpaper_mode = "fill"),
            ]
    return screens

if __name__ in ["config", "__main__"]:
    screens = init_screens()
    widgets_screen1 = init_widgets_list()
    widgets_screen2 = init_widgets_list()


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
    # Run the utility of `xprop` to see the wm class and name of an X client.
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
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
