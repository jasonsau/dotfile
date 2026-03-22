#!/bin/sh

/home/js/.screenlayout/double-screen.sh &
#nm-applet &
picom --config $HOME/.config/picom/picom.conf &
#cbatticon &
#volumeicon &
udiskie -t &
#redshift-gtk &
setxkbmap -layout us -variant dvorak &
nitrogen --restore &
/home/js/.local/bin/volumebar &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
