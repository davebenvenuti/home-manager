{ lib, features, inputs, ... }:

lib.optionalAttrs (features.containers or false) {
  imports = [
    inputs.sops-nix.homeManagerModules.sops
    inputs.containers.homeManagerModules.default
  ];
}