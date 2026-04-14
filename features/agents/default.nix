{ lib, features, config, pkgs, ... }:

let
  agentsEnabled = features.agents.opencode || features.agents.pi;
  
  # Get all skill directories
  skillEntries = builtins.attrNames (builtins.readDir ./skills);
  
  # Filter to only directories (skills are now in directories)
  skillDirs = builtins.filter (name: 
    let path = (toString ./skills) + "/" + name;
    type = builtins.readFileType path;
    in type == "directory"
  ) skillEntries;
  
  # Create home.file declarations for each skill directory
  skillDirDeclarations = lib.flatten (builtins.map (skillDir: [
    {
      ".agents/skills/${skillDir}".source = ./skills/${skillDir};
    }
  ]) skillDirs);
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
  ] ++ skillDirDeclarations));
}
