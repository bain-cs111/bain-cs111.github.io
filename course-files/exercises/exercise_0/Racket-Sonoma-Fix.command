#!/bin/bash
cd
cd Downloads
curl -OL https://users.cs.utah.edu/~mflatt/tmp/libcairo.2.dylib
find /Applications/ -maxdepth 1 -type d -name 'Racket v*' -exec cp libcairo.2.dylib {}/lib/ \;
