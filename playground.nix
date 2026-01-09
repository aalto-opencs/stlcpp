{
  pkgs,
  stlcpp-wasm,
  tree-sitter-stlcpp-wasm,
  ...
}:

pkgs.buildNpmPackage {
  pname = "stlcpp-playground";
  version = "0.1.0";

  src = ./playground;

  npmDepsHash = "sha256-6nntf8nFU5i23anG1WPOvqTcUu7LHXV0lPOYxqEhkv8=";

  nativeBuildInputs = [
    pkgs.wasm-bindgen-cli
    pkgs.nodejs
  ];

  # Build the WASM bindings and copy tree-sitter files before npm build
  preBuild = ''
    # Build WASM bindings for STLC++ compiler
    rm -rf pkg public
    mkdir -p pkg public
    wasm-bindgen ${stlcpp-wasm}/lib/stlcpp.wasm \
      --out-dir pkg \
      --target web \
      --no-typescript

    # Copy tree-sitter STLC++ grammar WASM and highlights
    cp ${tree-sitter-stlcpp-wasm}/tree-sitter-stlcpp.wasm public/
    cp ${tree-sitter-stlcpp-wasm}/highlights.scm public/

    # Copy tree-sitter runtime WASM from web-tree-sitter npm package
    cp node_modules/web-tree-sitter/tree-sitter.wasm public/
  '';

  # Use vite to build the production bundle
  buildPhase = ''
    runHook preBuild
    npm run build
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r dist/* $out/
    runHook postInstall
  '';

  meta = {
    description = "Web playground for STLC++ with Monaco Editor and Tree-Sitter";
    homepage = "https://github.com/aalto-opencs/stlcpp";
  };
}
