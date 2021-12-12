#!/usr/bin/env bash
.local/bin/pulse-volume-xob.py | xob -s volume &
nm-applet &
picom --experimental-backends &
cbatticon &
volumeicon &
udiskie -t &
redshift-gtk &
setxkbmap -layout us -variant dvorak &
double-screen.sh &
nitrogen --restore &


