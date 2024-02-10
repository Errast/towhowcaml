{ 
 inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    /* ... */
  };

  outputs = { self, flake-utils, nixpkgs /*, ... */ }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs) { inherit system; };
	ocamlPkgs = pkgs.ocamlPackages;
	dynamicInputs = with pkgs; [ radare2 ];
	libPath = pkgs.lib.makeLibraryPath dynamicInputs;
      in
      {
        devShells.default = pkgs.mkShell {
  	  buildInputs = with ocamlPkgs; [
            ocaml
            findlib
            dune_3
            ocaml-lsp
            ocamlformat
	    ounit
            utop
	    ctypes
	    yojson
	    ppx_yojson_conv
          ] ++ [
	    pkgs.radare2
	  ];
	  LD_LIBRARY_PATH = libPath;
	};
      }
    );
}
