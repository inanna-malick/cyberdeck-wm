{ pkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:
pkgs.haskell.packages.${compiler}.callPackage ./xmonad-pk.nix { }

