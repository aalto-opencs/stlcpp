{ pkgs, craneLib, cargoExtraArgs ? "", doCheck ? true, ... }@args:
let
  stlcppFilter = path: _type: builtins.match ".*\\.stlc$" path != null;
  stlcppOrCargo = path: type: (stlcppFilter path type) || (craneLib.filterCargoSources path type);
  
  extraArgs = builtins.removeAttrs args [ "pkgs" "craneLib" "cargoExtraArgs" "doCheck" ];
in
craneLib.buildPackage (extraArgs // {
  src = pkgs.lib.cleanSourceWith {
    src = ./.;
    filter = stlcppOrCargo;
    name = "source";
  };
  inherit cargoExtraArgs doCheck;
})
