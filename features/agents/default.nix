{ lib, features, config, pkgs, ... }:

let
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
  # Agent-specific configurations — gated at import level
  imports =
    [ ]
    ++ lib.optionals features.agents.opencode [ ./opencode.nix ]
    ++ lib.optionals features.agents.pi [ ./pi.nix ];

  # Create shared agents directory and files
  home.file = lib.mkMerge ([
    {
      ".agents/AGENTS.md".source = ./AGENTS.global.md;
    }
  ] ++ skillDirDeclarations);
}
