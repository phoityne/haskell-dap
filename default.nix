{ mkDerivation, base, containers, lib }:
mkDerivation {
  pname = "haskell-dap";
  version = "0.0.16.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  homepage = "https://github.com/phoityne/haskell-dap";
  description = "Haskell implementation of the DAP interface data";
  license = lib.licenses.bsd3;
}
