{ mkDerivation, aeson, Agda, base, containers, directory, filepath
, hspec, megaparsec, mtl, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "agda-unused";
  version = "0.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    Agda base containers directory filepath megaparsec mtl text
  ];
  executableHaskellDepends = [
    aeson base directory filepath mtl optparse-applicative text
  ];
  testHaskellDepends = [ base containers filepath hspec text ];
  description = "Check for unused code in an Agda project";
  license = stdenv.lib.licenses.mit;
}
