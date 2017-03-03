{ mkDerivation, base, stdenv, xmonad, xmonad-contrib }:
mkDerivation {
  pname = "xmonad-pk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base xmonad xmonad-contrib ];
  homepage = "https://github.com/githubuser/xmonad-pk#readme";
  license = stdenv.lib.licenses.bsd3;
}
