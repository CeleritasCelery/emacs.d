#!/usr/bin/bash

# Look in the variable system-configuration-options and system-configuration-features

sudo apt install -y autoconf automake autotools-dev bsd-mailx build-essential \
    diffstat gnutls-dev imagemagick libasound2-dev libc6-dev libdatrie-dev \
    libdbus-1-dev libgconf2-dev libgif-dev libgnutls28-dev libgpm-dev libgtk2.0-dev \
    libgtk-3-dev libice-dev libjpeg-dev liblockfile-dev liblqr-1-0 libm17n-dev \
    libmagickwand-dev libncurses5-dev libncurses-dev libotf-dev libpng-dev \
    librsvg2-dev libsm-dev libthai-dev libtiff5-dev libtiff-dev libtinfo-dev libtool \
    libx11-dev libxext-dev libxi-dev libxml2-dev libxmu-dev libxmuu-dev libxpm-dev \
    libxrandr-dev libxt-dev libxtst-dev libxv-dev quilt sharutils texinfo xaw3dg \
    xaw3dg-dev xorg-dev xutils-dev zlib1g-dev libjansson4 libjansson-dev libwebkit2gtk-4.0-dev \
    libgccjit0 libgccjit-9-dev

git clone git://git.sv.gnu.org/emacs.git ~/emacs-git
cd ~/emacs-git

./autogen.sh
./configure --with-modules --with-mailutils --with-xwidgets --with-imagemagick --with-native-compilation
make
sudo make install

# brew 28
# brew install emacs-plus@28 --with-native-comp --with-imagemagick --with-xwidgets --with-elrumo2-icon

# brew 29
# brew install emacs-plus@29 --with-native-comp --with-imagemagick --with-xwidgets --with-elrumo2-icon

# install with custom macos version
# note that this takes a really long time to clone. almost an hour.
# brew edit emacs-plus@29
#  url "https://github.com/tyler-dodge/emacs.git", :branch => "tyler-main-2"
# brew install --verbose emacs-plus@29 --with-native-comp --with-elrumo2-icon --with-imagemagick --with-xwidgets
