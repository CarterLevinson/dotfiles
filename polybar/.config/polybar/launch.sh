#!/usr/bin/bash

#Terminate all running bar instances using ipc
polybar-msg cmd quit

# launch both the status bars
echo "---" | tee -a /tmp/poolybar_main.log /tmp/polybar_alt.log
polybar main >> /tmp/polybar_main.log 2>&1 &
polybar alt >> /tmp/polybar_alt.log 2>&1 &

echo "Polybars launched..."
