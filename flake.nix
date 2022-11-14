{
  description = "A very basic flake";

  inputs = {
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils }:
  {
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override ( old: {
        overrides = final.lib.composeExtensions ( old.overrides or (_: _: {})) (f: p: {
          feed-proxy = f.callPackage ./. {};
        });
      });
      feed-proxy = with final.haskell.lib; justStaticExecutables (dontHaddock final.haskellPackages.feed-proxy);
    };
  }
    //
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] ( system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        hp = pkgs.haskellPackages;
      in
      rec {

        packages.feed-proxy = pkgs.feed-proxy;

        defaultPackage = packages.feed-proxy;
        devShell = hp.shellFor {
          packages = h: [h.feed-proxy];
          withHoogle = true;
          buildInputs = with pkgs; [
            entr
            cabal-install
            hp.hlint
            stylish-haskell
            ghcid
            hp.haskell-language-server

            hp.graphmod

            nix-du
          ];
        };
      }
    );
}
