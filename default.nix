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
in
rec {
  packages.stlcpp = pkgs.callPackage ./stlcpp.nix { inherit craneLib; };
  package = packages.stlcpp;
  shell = craneLib.devShell {
    packages = [
      (import sources.npins { inherit pkgs; })
      pkgs.cargo-watch
      pkgs.wasm-pack
      pkgs.llvmPackages.bintools
      pkgs.wasm-bindgen-cli
      pkgs.nodejs
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

    # Optional: environment, entrypoint, or command
    config = {
      Cmd = [ "${packages.stlcpp}/bin/stlcpp" ];
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
