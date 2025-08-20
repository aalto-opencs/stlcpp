{ pkgs, craneLib, ... }:
let
  stlcppFilter = path: _type: builtins.match ".*\\.stlc$" path != null;
  stlcppOrCargo = path: type: (stlcppFilter path type) || (craneLib.filterCargoSources path type);
in
craneLib.buildPackage {
  src = pkgs.lib.cleanSourceWith {
    src = ./.;
    filter = stlcppOrCargo;
    name = "source";
  };
}
