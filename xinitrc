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

if [[ `uname -n` -eq 'mikepjb-laptop' ]]; then
  xrdb -merge <<< 'xterm*faceName: xft:inconsolata:size=20'
fi

xmodmap ~/.Xmodmap

xset r rate 200 25
exec bspwm
