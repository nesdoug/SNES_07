@echo off

set name="Grad"

superfamiconv -B 4 -i %name%.png -p %name%.pal -t %name%.chr -m %name%.map

pause
