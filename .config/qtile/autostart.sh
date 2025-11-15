#!/bin/sh

/home/js/.screenlayout/triple-screen.sh &
nm-applet &
picom --config $HOME/.config/picom/picom.conf &
cbatticon &
volumeicon &
udiskie -t &
redshift-gtk &
setxkbmap -layout us -variant dvorak &
nitrogen --restore &
/home/js/.local/bin/volumebar &
