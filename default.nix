{ mkDerivation, Agda, base, containers, directory, filepath, hspec
, mtl, stdenv, text
}:
mkDerivation {
  pname = "agda-unused";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    Agda base containers directory filepath mtl text
  ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.mit;
}
