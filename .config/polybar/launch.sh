#!/bin/bash

killall -q polybar
sleep 2s
echo "--" tee -a /tmp/polybar1.log
polybar mybar -c ~/.config/polybar/config | tee -a /tmp/polybar1.log & disown

echo "Bars launched..."
