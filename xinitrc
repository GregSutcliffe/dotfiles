#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

export BROWSER=chromium
export GTK2_RC_FILES="$HOME/.gtkrc.mine"

# Fix dual-head

#/home/gsutcliffe/bin/xrandr.sh

# LoadX resource
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# xrandr / tray space
~/bin/xrandr.sh
# trayer --edge top --align right --SetDockType true --SetPartialStrut true --width 10 --tint 0x000000 --transparent true --height 17  &
# sh ~/.fehbg &


# WM config
setxkbmap gb
xsetroot -cursor_name left_ptr

# Apps
xscreensaver &
wicd-gtk -t &
mpd

if `which keychain` ; then
  eval `keychain --eval --agents ssh,gpg id_greg id_github CA0AEB93`
fi
. ./.keychain/emerald-sh
. ./.keychain/emerald-sh-gpg

# compositing
xcompmgr -c -t-5 -l-5 -r4.2 -o.55 &
conky -d; sleep 1 && transset-df .5

# exec gnome-session
# exec startkde
# exec ck-launch-session startxfce4
# ...or the Window Manager of your choice

DEFAULT_SESSION=xmonad
case $1 in
  xmonad) exec $1 ;;
  *) exec $DEFAULT_SESSION ;;
esac
