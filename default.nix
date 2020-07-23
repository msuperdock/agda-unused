{ mkDerivation, Agda, base, containers, directory, filepath, hspec
, megaparsec, mtl, stdenv, text
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
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [ base containers hspec ];
  license = stdenv.lib.licenses.mit;
}
