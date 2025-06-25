{
  description = "Symbolic evaluation as a library";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (self: super: {
              # kissat = super.kissat.overrideAttrs (prev: {
              #   version = "rel-4.0.2";
              #   src = super.fetchFromGitHub {
              #     owner = "arminbiere";
              #     repo = "kissat";
              #     rev = "rel-4.0.2";
              #     hash = "sha256-XVaWO1zHMXM83Qih3HnmIsOvM1zpefF6u9lBP420/mQ=";
              #   };
              #   installPhase = prev.installPhase + ''
              #     ls build
              #   '';
              # });
              # bitwuzla = super.bitwuzla.overrideAttrs (prev: {
              #   buildInputs = prev.buildInputs ++ [ self.kissat.lib self.kissat.dev ];
              #   preConfigure = ''
              #     export C_INCLUDE_PATH="${self.kissat.dev}/include"
              #     export LIBRARY_PATH="${self.kissat.lib}/lib"
              #     ls ${self.kissat.lib}/lib
              #     mesonFlagsArray+=(
              #       "-Dc_link_args=\"-L${self.kissat.lib}/lib -lkissat\""
              #       "-Dcpp_link_args=\"-L${self.kissat.lib}/lib -lkissat\""
              #     )
              #   '';
              #   mesonFlags = prev.mesonFlags ++ [
              #     # Tell Meson to add this library path and link against libkissat
              #     "-Dkissat=true"
              #   ];
              #   patches = [ ./kissat.patch ];
              # });
            })
          ];
        };
        cross = pkgs.pkgsCross.riscv64;
        isCross = system != flake-utils.lib.system.riscv64-linux;
        riscv_pkgs = (if isCross then cross else pkgs).extend (
          self: super: {
            libhwy = super.libhwy.overrideAttrs (prev: {
              version = "1.2.0";
              src = super.fetchFromGitHub {
                owner = "google";
                repo = "highway";
                rev = "1.2.0";
                hash = "sha256-yJQH5ZkpEdJ6lsTAt6yJSN3TQnVoxNpkbChENaxhcHo=";
              };
              patches = [ ];
              doCheck = false;
            });
          }
        );
        riscv_llvm = if isCross then cross.buildPackages.llvmPackages_19 else pkgs.llvmPackages_19;

        stableHPkgs = pkgs.haskell.packages."ghc982";
        hPkgs = pkgs.haskell.packages."ghc982";

        riscvDevTools = [
          (riscv_llvm.llvm.overrideAttrs (_: {
            doCheck = false;
          }))
          (riscv_llvm.clang.overrideAttrs (_: {
            doCheck = false;
          }))
          riscv_pkgs.bintools
          (riscv_pkgs.libhwy.override { stdenv = riscv_llvm.stdenv; })
          riscv_pkgs.gtest
          riscv_pkgs.boost
          # riscv_pkgs.glibc
        ];

        myDevTools = [
          # pkgs.glibc
          # pkgs.clang_15
          # (riscv_llvm.clang.overrideAttrs (_: { doCheck = false; }))
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          hPkgs.ghcid # Continuous terminal Haskell compile checker
          # hPkgs.ormolu # Haskell formatter
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.haskell-language-server # LSP server for editor
          stableHPkgs.cabal-install
          stack-wrapped
          pkgs.zlib # External C library needed by some Haskell packages
          pkgs.boolector
          pkgs.yices
          pkgs.cvc5
          pkgs.z3
          pkgs.nixpkgs-fmt
          pkgs.libxml2
          pkgs.bitwuzla
          pkgs.cmake-format
          pkgs.cmake
          pkgs.ninja
          pkgs.cairo
          pkgs.expat
          pkgs.xorg.libXdmcp
          pkgs.pkg-config
          pkgs.ghostscript
          pkgs.hyperfine
          pkgs.shfmt
          pkgs.nil
          pkgs.shellcheck
          (pkgs.python3.withPackages (pypkgs: [
            pypkgs.numpy
            pypkgs.scipy
            pypkgs.matplotlib
            pypkgs.black
            pypkgs.redis
            pypkgs.pandas
            pypkgs.adjusttext
          ]))
          pkgs.redis
        ];
        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
        synthesizerDevShell = pkgs.mkShell {
          buildInputs = [ pkgs.llvm_15 ] ++ myDevTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };
        slicerDevShell = pkgs.mkShell {
          buildInputs = [
            pkgs.llvm_19
            pkgs.clang_19
            pkgs.lit
          ] ++ myDevTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };

        treefmt = treefmt-nix.lib.mkWrapper pkgs {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
          programs.ormolu.enable = true;
          programs.ormolu.package = hPkgs.ormolu;
          programs.hlint.enable = true;
          programs.yamlfmt.enable = true;
          programs.shfmt.enable = true;
          programs.cmake-format.enable = true;
          settings.formatter.cmake-format.includes = [ "**/CMakeLists.txt" ];
          programs.clang-format.enable = true;
          settings.formatter.nixfmt.excludes = [ ".direnv" ];
          settings.formatter.ormolu.includes = [
            "*.hs"
            "*.hs-boot"
            "*.lhs"
          ];
          settings.excludes = [
            "LICENSE"
            "*.lock"
            "cabal.project"
            ".gitignore"
            "*.sir"
            "*.cfg"
            "*.cfg.in"
            "results/**"
            "*.ll"
            "*.clang-tidy"
          ];
        };
      in
      {
        formatter = treefmt;

        devShells.synthesizer = synthesizerDevShell;
        devShells.default = synthesizerDevShell;
        devShells.slicer = slicerDevShell;
        devShells.riscv = pkgs.mkShell {
          buildInputs = riscvDevTools ++ myDevTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };
      }
    );
}
