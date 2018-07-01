#!/bin/bash -ex

cd $HOME/emacs
git pull --rebase

./configure --prefix=$HOME/.local --without-makeinfo --with-x-toolkit=gtk3 --without-pop --with-mailutils --with-imagemagick

make -j 5
make install

