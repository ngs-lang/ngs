{
  description = "NGS - Next Generation Shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Local source filter - exclude build artifacts
        localSrc = builtins.path {
          path = ./.;
          name = "ngs-source";
          filter = path: type:
            let
              baseName = baseNameOf path;
            in
            !(
              baseName == "build" ||
              baseName == "result" ||
              baseName == ".git" ||
              baseName == ".direnv"
            );
        };

        # For local development, override the package to use local source
        ngs = (pkgs.callPackage ./package.nix { }).overrideAttrs (oldAttrs: {
          src = localSrc;
          version = oldAttrs.version + "-dev";
        });
      in
      {
        packages = {
          default = ngs;
          ngs = ngs;
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ ngs ];

          packages = with pkgs; [
            # Development tools
            gdb
            clang-tools # clangd, clang-format
          ] ++ lib.optionals stdenv.isLinux [
            valgrind
            strace
          ];

          shellHook = ''
            echo "╔══════════════════════════════════════════════════════════╗"
            echo "║           NGS Development Environment                    ║"
            echo "╠══════════════════════════════════════════════════════════╣"
            echo "║ Build:    cmake -B build && cmake --build build          ║"
            echo "║ Test:     cd build && ctest --output-on-failure          ║"
            echo "║ Run:      NGS_PATH=lib ./build/ngs                       ║"
            echo "║ Package:  nix build                                      ║"
            echo "╚══════════════════════════════════════════════════════════╝"
          '';
        };

        # For nix fmt
        formatter = pkgs.nixpkgs-fmt;
      }
    );
}
