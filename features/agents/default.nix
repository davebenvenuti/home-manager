{ lib, features, config, pkgs, ... }:

let
  agentsEnabled = features.agents.opencode || features.agents.pi;
in {
  # Import agent-specific configurations (they have internal feature guards)
  imports = [
    ./opencode.nix
    ./pi.nix
  ];

  # Create shared agents directory and symlinks when any agent is enabled
  home.activation = lib.mkIf agentsEnabled {
    createAgentsDir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      mkdir -p "$HOME/.agents"
      mkdir -p "$HOME/.agents/skills"
      
      # Copy AGENTS.global.md to shared directory
      cp -f "${./AGENTS.global.md}" "$HOME/.agents/AGENTS.md"
      
      # Copy all skills to shared directory
      for skill in "${./skills}/"*; do
        if [ -f "$skill" ]; then
          cp -f "$skill" "$HOME/.agents/skills/"
        fi
      done
    '';
  };
}
