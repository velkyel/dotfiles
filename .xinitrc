xmodmap .Xmodmap &
xrdb .Xresources &
xsetroot -solid black
xset m 1/5 
xset b off
urxvtd --quiet --opendisplay --fork
exec ck-launch-session xmonad
