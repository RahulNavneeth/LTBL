{
	description = "LTBL: Let There Be Light";

	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils } :
		flake-utils.lib.eachDefaultSystem (system:
			let
			  pkgs = nixpkgs.legacyPackages.${system};
			in  {
				devShells.default = pkgs.mkShell {
					buildInputs = with pkgs; [
						ghc
						cabal-install
						clang
					];
				};
			});
}
