#!/bin/sh

# node solve.js a_example.txt
# node solve.js c_memorable_moments.txt
node solve.js d_pet_pictures.txt
# node solve.js e_shiny_selfies.txt
# node solve.js b_lovely_landscapes.txt

zip -9 solution.zip solve.js
cp -fv solution.zip *.output /home/remi/.sandboxes/chromium/Downloads
