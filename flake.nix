{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    nixpkgs.url = github:NixOS/nixpkgs;
  };

  outputs = { flake-utils, nixpkgs, ... }: flake-utils.lib.eachDefaultSystem (system:
  let
    np = nixpkgs.legacyPackages.${system};
  in {
    devShells.default = np.mkShell {
      nativeBuildInputs = with np; [
        rustc cargo
        pkg-config

        # extra for dev
        rust-analyzer clippy rustfmt
        xdot
      ];

      buildInputs = with np; [
        rustPlatform.bindgenHook
        graphviz
      ];

      # Unfortunately `graphviz-rs` does not use `pkg-config` to find out what
      # libraries `graphviz` needs:
      # https://github.com/furiosa-ai/dot-graph/blob/3e04030c3ac532a08a22038ffcd702172228b633/build.rs#L19-L20
      shellHook = ''
        export NIX_LDFLAGS+=" -lcdt"
      ''; # TODO: change to invoking `${stdenv.targetPrefix}pkg-config --libs graphviz` or w/e
    };

    # TODO: make nixpkg
    # TODO: add to nixpkgs
  });
}
