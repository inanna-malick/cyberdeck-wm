#!/bin/sh -eu

################################################################################
# Edit this file and then rename it to ~/.xmonad/build

################################################################################
# The directory holding your source code and stack.yaml file:
SRC_DIR=~/dev/oss/xmonad-pk

################################################################################
# The name of the executable produced by stack.  This comes from the
# executable section of your Cabal file.
EXE_NAME=xmonad-pk-exe

################################################################################
# This script will be given a single argument, the path to the
# executable it should produce.
output_file=$1; shift

################################################################################
cd $SRC_DIR

DIR=`mktemp -d`
touch A
stack install --local-bin-path $DIR
touch B
mv -u $DIR/xmonad-pk-exe $output_file
touch C
rm -r $DIR
touch D
