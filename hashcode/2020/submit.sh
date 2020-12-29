#!/bin/sh

# node solve.js ./a_example.txt
node solve.js ./d_tough_choices.txt
# node solve.js ./e_so_many_books.txt

# node solve.js ./f_libraries_of_the_world.txt


# node solve.js ./c_incunabula.txt
# node solve.js ./b_read_on.txt

zip -9 solution.zip solve.js
cp -fv solution.zip ./*.output /home/remi/.sandboxes/chromium/Downloads
