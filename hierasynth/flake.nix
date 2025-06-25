{
  description = "Symbolic evaluation as a library";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.grisette-nix-build-env.url = "github:lsrcz/grisette-nix-build-env/main";
  inputs.grisette.url = "github:lsrcz/grisette/main";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      grisette-nix-build-env,
      grisette,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      grisette-nix-build-env.lib.${system}.output {
        inherit nixpkgs system;
        srcRoot = ./.;
        extraHaskellPackages = pkgs: ghcVersion: hfinal: helpers: setCIOptions: {
          grisette = pkgs.haskell.lib.dontCheck grisette.packages.${system}.grisette.${ghcVersion};
          grisette-synth-lib = setCIOptions {
            extraTestToolDepends = [ pkgs.z3 ];
            package = hfinal.callCabal2nix "grisette-synth-lib" ./. { };
            mixDirs = [
              ""
              "spec/spec-tmp"
            ];
          };
        };
        devShellExtraBuildInputs = pkgs: _: [
          pkgs.cairo
          pkgs.expat
          pkgs.pkg-config
          pkgs.xorg.libXdmcp
        ];
        pname = "grisette-synth-lib";
        extraOutputs = pkgs: haskellPackages: devShellWithVersion: {
          devShells.default = devShellWithVersion {
            ghcVersion = "9101";
            config = {
              isDevelopmentEnvironment = true;
            };
          };
        };
      }
    );
}
