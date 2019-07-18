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

if [ `uname -n` = "mikepjb-mercury" ]; then
  # xrdb -merge <<< 'Xft.dpi: 180'
  xrandr --output eDP-1 --mode 1920x1080
  # TODO ideally we scale on the external monitor but --scale fails silently
  # when using displaylink
  # scales 4k down to 1440p (mirror is good but main screen is blurred slightly)
  # use this when connected
  # xrandr --output eDP-1 --mode 3840x2160 --scale 0.6666x0.6666

fi
# if [ `xdpyinfo | awk '/dimensions/{print $2}'` = '3840x2160' ]; then
#   xrdb -merge <<< 'xterm*faceName: xft:inconsolata:size=18'
# fi
# 
# if [ `uname -n` = "mikepjb-desktop" ]; then
#   # xcalib -d :0 $HOME/src/arch-configuration/dell_u2515h.icc
#   # TODO xcalib is in AUR, what alternatives do we have?
# fi
# 
# if [ `uname -n` = "mikepjb-london" ]; then
#   xinput set-button-map 9 1 2 3 5 4 6 7 # reverse mouse scroll direction
#   xinput set-prop 9 "libinput Accel Speed" -0.7
# fi

xmodmap ~/.Xmodmap

xset r rate 200 25

# emacsclient --daemon &
exec openbox-session
