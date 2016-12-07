with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "lab6";
     ghc = hsPkgs.ghcWithPackages (p: with p; [
       gtk2hs-buildtools
     ]);
     buildInputs =
       [ freeglut glew zlib cairo gnome2.gtk ];
  }
