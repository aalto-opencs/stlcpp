{ pkgs, stlcpp-wasm, ... }:

pkgs.stdenv.mkDerivation {
  pname = "stlcpp-playground";
  version = "0.1.0";

  src = ./playground;

  nativeBuildInputs = [ pkgs.wasm-bindgen-cli ];

  buildPhase = ''
    mkdir pkg
    # craneLib.buildPackage puts the wasm file in lib/ if it's a cdylib
    wasm-bindgen ${stlcpp-wasm}/lib/stlcpp.wasm \
      --out-dir pkg \
      --target web \
      --no-typescript
  '';

  installPhase = ''
    mkdir -p $out
    cp -r . $out/
  '';
}
