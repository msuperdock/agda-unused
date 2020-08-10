{ mkDerivation, Agda, base, containers, directory, filepath, hspec
, megaparsec, mtl, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "agda-unused";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    Agda base containers directory filepath megaparsec mtl text
  ];
  executableHaskellDepends = [
    base directory filepath mtl optparse-applicative text
  ];
  testHaskellDepends = [ base containers filepath hspec ];
  license = stdenv.lib.licenses.mit;
}
