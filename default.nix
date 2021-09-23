{ mkDerivation, aeson, Agda, base, containers, directory, filepath
, hspec, lib, mtl, optparse-applicative, text
}:
mkDerivation {
  pname = "agda-unused";
  version = "0.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    Agda base containers directory filepath mtl text
  ];
  executableHaskellDepends = [
    aeson base directory mtl optparse-applicative text
  ];
  testHaskellDepends = [ base containers filepath hspec text ];
  description = "Check for unused code in an Agda project";
  license = lib.licenses.mit;
}
