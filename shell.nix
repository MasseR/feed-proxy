with (import <nixpkgs> {});

let
  hp = haskellPackages.extend(self: super: {
    feed-proxy = self.callPackage ./. {};
  });

in

hp.shellFor {
  packages = h: [ h.feed-proxy ];
  buildInputs = [
    ghcid
    stylish-haskell
    cabal-install
    cabal2nix
  ];
}
