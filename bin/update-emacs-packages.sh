#!/bin/bash -ex

cd $HOME/cquery
git pull --rebase
git submodule update
cmake -DCMAKE_BUILD_TYPE=release .
make -j4
#./waf configure
#./waf build

#cd $HOME/rtags
#git pull --rebase
#git submodule update
#make
#make install

cd $HOME
emacs -u $(whoami) --eval '(auto-package-update-now)' --batch
