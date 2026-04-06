{ mkDerivation, base, bytestring, containers, criterion, directory
, filepath, hspec, lib, libssh2, mtl, optparse-applicative, process
, retry, tasty, tasty-hspec, tasty-quickcheck, text
}:
mkDerivation {
  pname = "hpci";
  version = "0.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers filepath libssh2 mtl
    optparse-applicative retry text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring directory filepath hspec process retry tasty
    tasty-hspec tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [ base criterion ];
  license = lib.licenses.gpl3Plus;
  mainProgram = "hpci-exe";
}
