#!/bin/sh
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

LANG="en_GB.UTF-8"

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# For when you have scaling issues at 4k!
# if [[ `xdpyinfo | awk '/dimensions/{print $2}'` -eq '3840x2160']]; then
#   xrdb -merge <<< 'xterm*faceName: xft:inconsolata:size=20'
#   xrandr --output HDMI-0 --scale 1x1
# fi

if [[ `uname -n` -eq "mikepjb-desktop" ]]; then
  xcalib -d :0 $HOME/src/arch-configuration/samsung_C27F591.icm 
fi


xmodmap ~/.Xmodmap

xset r rate 200 25
exec bspwm
