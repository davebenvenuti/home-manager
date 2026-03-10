{ lib, features, inputs, ... }:

let
  containersModulePath = ./containers/default.nix;
  containersModuleExists = builtins.pathExists containersModulePath;
in
lib.optionalAttrs (features.containers or false && containersModuleExists) {
  imports = [
    inputs.sops-nix.homeManagerModules.sops
    containersModulePath
  ];
}