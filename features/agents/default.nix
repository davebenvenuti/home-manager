{ lib, features, config, pkgs, ... }:

let
  agentsEnabled = features.agents.opencode || features.agents.pi;
in {
  # Import agent-specific configurations (they have internal feature guards)
  imports = [
    ./opencode.nix
    ./pi.nix
  ];

  # Shared .agents directory and agent-specific symlinks
  home.file = lib.mkIf agentsEnabled (lib.mkMerge [
    # Shared directory (convenience)
    {
      ".agents/AGENTS.md".source = ./AGENTS.global.md;
      ".agents/skills".source = ./skills;
    }
    # opencode-specific files (conditionally added)
    (lib.mkIf features.agents.opencode {
      ".config/opencode/AGENTS.md".source = ./AGENTS.global.md;
      ".config/opencode/skills".source = ./skills;
    })
    # pi-specific files (conditionally added)
    (lib.mkIf features.agents.pi {
      ".pi/agent/AGENTS.md".source = ./AGENTS.global.md;
    })
  ]);
}