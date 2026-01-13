{
  sources ? import ./npins,
  system ? builtins.currentSystem,
  pkgs ? import sources.nixpkgs {
    inherit system;
    config = { };
    overlays = [ ];
  },
}:
let
  fenix = import sources.fenix { inherit pkgs; };
  toolchain = fenix.combine [
    fenix.stable.toolchain
    fenix.targets.wasm32-unknown-unknown.stable.rust-std
  ];
  craneLib = (import sources.crane { inherit pkgs; }).overrideToolchain toolchain;

  # Use older nixpkgs for tree-sitter 0.22.6 (generates ABI v14 compatible with web-tree-sitter 0.24.7)
  pkgs-tree-sitter = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/ab7b6889ae9d484eed2876868209e33eb262511d.tar.gz";
    sha256 = "0wl2rq7jxr7b0g0inxbh9jgiifamn9i45p7fgra8rhhnrmcdlqjz";
  }) { inherit system; };

  # Fetch tree-sitter-stlcpp grammar from GitHub
  tree-sitter-stlcpp-src = pkgs.fetchFromGitHub {
    owner = "aalto-opencs";
    repo = "tree-sitter-stlcpp";
    rev = "033c0a58f41b0abf63dd8f373de04f442468f693";
    sha256 = "sha256-BWw6+B1MBjCUO/CwG7e7nu+C3Np2aGmIPD6fHekZM8I=";
  };

  # Build tree-sitter STLC++ grammar WASM
  tree-sitter-stlcpp-wasm = pkgs.stdenv.mkDerivation {
    pname = "tree-sitter-stlcpp-wasm";
    version = "0.0.1";
    src = tree-sitter-stlcpp-src;

    nativeBuildInputs = [
      pkgs-tree-sitter.tree-sitter
      pkgs.nodejs  # Required by tree-sitter generate
      pkgs.emscripten  # Required for WASM build
    ];

    buildPhase = ''
      # Generate parser with tree-sitter 0.22.6 (ABI v14)
      tree-sitter generate
      # Build WASM
      tree-sitter build --wasm
    '';

    installPhase = ''
      mkdir -p $out
      cp tree-sitter-stlcpp.wasm $out/
      cp queries/highlights.scm $out/
    '';
  };
in
rec {
  packages.stlcpp = pkgs.callPackage ./stlcpp.nix { inherit craneLib; };
  packages.stlcpp-wasm = pkgs.callPackage ./stlcpp.nix {
    inherit craneLib;
    cargoExtraArgs = "--target wasm32-unknown-unknown --lib";
    doCheck = false;
    nativeBuildInputs = [ pkgs.llvmPackages.bintools ];
    CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "lld";
  };
  packages.tree-sitter-stlcpp-wasm = tree-sitter-stlcpp-wasm;
  packages.playground = pkgs.callPackage ./playground.nix {
    inherit (packages) stlcpp-wasm tree-sitter-stlcpp-wasm;
  };

  packages.run-playground = pkgs.writeShellScriptBin "run-playground" ''
    echo "Serving STLC++ playground..."
    echo "Open http://localhost:8000 in your browser"
    ${pkgs.python3}/bin/python3 -m http.server 8000 -d ${packages.playground.overrideAttrs { VITE_BASE_PATH = "/"; }}
  '';

  package = packages.stlcpp;
  shell = craneLib.devShell {
    packages = [
      (import sources.npins { inherit pkgs; })
      pkgs.cargo-watch
      pkgs.wasm-pack
      pkgs.llvmPackages.bintools
      pkgs.wasm-bindgen-cli
      pkgs.nodejs

      pkgs.tree-sitter
    ];
    CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "lld";
  };

  tests.default = import ./test.nix {
    inherit pkgs;
    inherit (packages) stlcpp;
  };

  oci = pkgs.dockerTools.buildLayeredImage {
    name = "stlcpp";
    tag = "latest";

    # Layer contents
    contents = [
      packages.stlcpp
      pkgs.busybox
      pkgs.python3
    ];

    config = {
      Entrypoint = [ "${packages.stlcpp}/bin/stlcpp" ];
      WorkingDir = "/";
      Env = [
        "PATH=${
          pkgs.lib.makeBinPath [
            packages.stlcpp
            pkgs.coreutils
            pkgs.busybox
            pkgs.llvmPackages.bintools
            pkgs.python3
          ]
        }"
      ];
    };

    # Optional: extra metadata
    created = "now";
  };
}
