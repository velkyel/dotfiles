#!/bin/bash -ex

cd $HOME/ccls
git pull --rebase
git submodule update
cmake -H. -BRelease
cmake --build Release
# make -j4

#cd $HOME/cquery
#git pull --rebase
#git submodule update
#cmake -DCMAKE_BUILD_TYPE=release .
#make -j4

#cd $HOME/rtags
#git pull --rebase
#git submodule update
#make
#make install

cd $HOME
emacs -u $(whoami) --eval '(progn (auto-package-update-now) (quelpa-upgrade))' --batch
