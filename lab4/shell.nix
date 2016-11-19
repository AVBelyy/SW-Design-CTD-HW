with import <nixpkgs> { };
haskell.lib.buildStackProject {
   ghc = haskell.packages.ghc801.ghc;
   name = "yesod-mvc-example";
   buildInputs = [ zlib ];
}

