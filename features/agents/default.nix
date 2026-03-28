{ lib, features, config, pkgs, ... }:

let
  agentsEnabled = features.agents.opencode || features.agents.pi;
in {
  # Import agent-specific configurations (they have internal feature guards)
  imports = [
    ./opencode.nix
    ./pi.nix
  ];

  # Shared .agents directory (convenience)
  home.file = lib.mkIf agentsEnabled {
    ".agents/AGENTS.md".source = ./AGENTS.global.md;
    ".agents/skills".source = ./skills;
  };
}