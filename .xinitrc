#!/bin/bash

# Enable X-restart hotkey ctrl_alt_bksp. This line
# should remain at the top, since it seems to also
# reset the keymaps, undoing any custom xmodmap
# commands that happen previously
#setxkbmap -option terminate:ctrl_alt_bksp
#xmodmap ~/.xmodmaprc
#
if [ "$GDMSESSION" == "awesome" ]; then
  xmodmap ~/.xmodmap_awesome
fi

~/.xinputrc
xrandr --output 'VGA1' --rotate left
xrandr --output 'HDMI1' --rotate left
xrandr --output 'HDMI1' --pos 0x0
xrandr --output 'VGA1' --pos 1080x0
