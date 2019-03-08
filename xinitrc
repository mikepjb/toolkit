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

if [ `xdpyinfo | awk '/dimensions/{print $2}'` = '3840x2160' ]; then
  xrdb -merge <<< 'Xft.dpi: 100'
fi

if [ `uname -n` = "mikepjb-desktop" ]; then
  xcalib -d :0 $HOME/src/arch-configuration/dell_u2515h.icc
  xrdb -merge <<< 'Xft.dpi: 70'
fi

if [ `uname -n` = "mikepjb-london" ]; then
  xinput set-button-map 9 1 2 3 5 4 6 7 # reverse mouse scroll direction
  xinput set-prop 9 "libinput Accel Speed" -0.7
fi


xmodmap ~/.Xmodmap

xset r rate 200 25
exec bspwm
