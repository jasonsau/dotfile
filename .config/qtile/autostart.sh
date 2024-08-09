#!/bin/sh
.local/bin/pulse-volume-xob.py | xob -s volume &
nm-applet &
picom --config $HOME/.config/picom/picom.conf &
cbatticon &
volumeicon &
udiskie -t &
redshift-gtk &
setxkbmap -layout us -variant dvorak &
double-screen.sh &
nitrogen --restore &
