#!/bin/bash

cd $HOME/rtags
git pull --rebase
git submodule update
make -j 5
make install

emacs -u $(whoami) --eval '(auto-package-update-now)' --batch

