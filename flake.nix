{
  inputs = {
    opam-repository.url = "github:ocaml/opam-repository";
    opam-repository.flake = false;

    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";
    opam-nix.inputs.opam-repository.follows = "opam-repository";

    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, utils, opam-nix, nixpkgs, opam-repository }:
    utils.lib.eachDefaultSystem (system:
      let
        package = "illuaminate";
        pkgs = import nixpkgs { inherit system; };
        illuaminate-full = (opam-nix.lib.${system}.buildDuneProject {
          inherit pkgs;
          repos = [opam-repository];
          resolveArgs = { dev = false; "with-test" = true; };
        } package ./. {

        }).${package}.overrideAttrs(oa: {
          nativeBuildInputs = oa.nativeBuildInputs ++ [pkgs.upx pkgs.git];

          # We can't run tests here, as flakes don't have access to the full Git
          # history (see https://github.com/NixOS/nix/issues/6900). This also
          # breaks %VERSION%.
          doCheck = false;
          removeOcamlReferences = true;
          propagateInputs = false;
        });

        mkSingleExe = { name, exe ? name }: pkgs.stdenv.mkDerivation {
          inherit name;
          inherit (illuaminate-full) version;

          unpackPhase = ":";
          installPhase = ''
            mkdir -p $out/bin
            cp "${illuaminate-full}/bin/${exe}" "$out/bin/${exe}"
          '';
        };

      in {
        packages.illuaminate-full = illuaminate-full;
        packages.default = mkSingleExe { name = "illuaminate"; };
      });
}
