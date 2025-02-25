#!/bin/bash

# Look in the variable system-configuration-options and system-configuration-features

# brew 30
# brew tap d12frosted/emacs-plus
# brew install emacs-plus@30 --with-native-comp --with-imagemagick --with-xwidgets --with-elrumo2-icon

# git clone https://git.savannah.gnu.org/cgit/emacs/elpa.git to make sure it is working

# install with custom macos version
# note that this takes a really long time to clone. almost an hour.
# brew edit emacs-plus@29
#  url "https://github.com/tyler-dodge/emacs.git", :branch => "tyler-main-2"
# brew install --verbose emacs-plus@29 --with-native-comp --with-elrumo2-icon --with-imagemagick --with-xwidgets

#######################
# installing manually #
#######################

# brew install \
#     pkg-config \
#     texinfo \
#     libgccjit \
#     autoconf \
#     automake \
#     make \
#     jansson \
#     gnutls \
#     mailutils \
#     imagemagick \
#     tree-sitter \
#     mailutils

export LDFLAGS="-L/opt/homebrew/Cellar/libgccjit/13.2.0/lib/gcc/13"
export LIBRARY_PATH="-L/opt/homebrew/Cellar/libgccjit/13.2.0/lib/gcc/13"
export CPATH="-I/opt/homebrew/Cellar/libgccjit/13.2.0/include"
export CFLAGS="-DFD_SETSIZE=10000 -DDARWIN_UNLIMITED_SELECT"

# export CC=/opt/homebrew/bin/gcc-13
# export CXX=/opt/homebrew/bin/g++-13
# export CPP=/opt/homebrew/bin/cpp-13

./autogen.sh
./configure --with-modules --with-mailutils --with-xwidgets --with-imagemagick --with-ns --disable-ns-self-contained --disable-gc-mark-trace
gmake -j4
