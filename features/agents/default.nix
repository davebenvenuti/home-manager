{ lib, features, config, pkgs, ... }:

let
  agentsEnabled = features.agents.opencode || features.agents.pi;
  
  # Get all skill files
  skillFiles = builtins.attrNames (builtins.readDir ./skills);
  
  # Create home.file declarations for each skill
  skillFileDeclarations = lib.flatten (builtins.map (skillFile: [
    {
      ".agents/skills/${skillFile}".source = ./skills/${skillFile};
    }
  ]) skillFiles);
in {
  # Import agent-specific configurations (they have internal feature guards)
  imports = [
    ./opencode.nix
    ./pi.nix
  ];

  # Create shared agents directory and files when any agent is enabled
  home.file = lib.mkIf agentsEnabled (lib.mkMerge ([
    {
      ".agents/AGENTS.md".source = ./AGENTS.global.md;
    }
  ] ++ skillFileDeclarations));
}
