let
  reflex-platform = import ./reflex-platform { enableLibraryProfiling = true; };
  pkgs = reflex-platform.nixpkgs;
  app = (import ./. { inherit reflex-platform; useWarp = true; }).ghc.edit-log-editor;
in
  pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.enableExecutableProfiling app)
