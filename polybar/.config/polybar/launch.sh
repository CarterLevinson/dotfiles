#!/usr/bin/env bash

#Terminate all running bar instances
killall -q polybar

#launch main bar and alt bar
echo "---" | tee -a /tmp/poolybar_main.log /tmp/polybar_alt.log
polybar main >> /tmp/polybar_main.log 2>&1 &
polybar alt >> /tmp/polybar_alt.log 2>&1 &

echo "Bars launched..."
