{ lib, features, config, pkgs, ... }:

let
  agentsEnabled = features.agents.opencode || features.agents.pi;
in {
  # Import agent-specific configurations (they have internal feature guards)
  imports = [
    ./opencode.nix
    ./pi.nix
  ];
}
