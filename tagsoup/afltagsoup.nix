{ mkDerivation, base, HandsomeSoup, hxt, stdenv }:
mkDerivation {
  pname = "afltagsoup";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base HandsomeSoup hxt ];
  license = stdenv.lib.licenses.bsd3;
}
