{ system ? builtins.currentSystem, reflex-platform ? import ./reflex-platform { inherit system; }, useWarp ? true }:
reflex-platform.project ({ pkgs, ... }: {
  inherit useWarp;

  packages = {
    edit-log-editor = ./editor;
    edit-log-core = ./core;
    edit-log-renderdiff = ./renderdiff;
  };

  shells = {
    ghc = ["edit-log-editor" "edit-log-core" "edit-log-renderdiff"];
    ghcjs = ["edit-log-editor"];
  };

  overrides = self: super: {
    hedgehog = self.callHackage "hedgehog" "1.0.2" {};
    tasty-hedgehog = self.callHackage "tasty-hedgehog" "1.0.0.2" {};
    hspec-hedgehog = self.callHackage "hspec-hedgehog" "0.0.1.2" {};
  };
})
