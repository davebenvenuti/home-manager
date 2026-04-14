{ lib, features, pkgs, ... }:
{
  programs.opencode = lib.mkIf features.agents.opencode {
    enable = true;
    package = pkgs.opencode;

    settings = {
      model = "deepseek/deepseek-chat";
      autoupdate = false;
      permission = {
        "bash" = {
          "*" = "ask";
          "ls *" = "allow";
          "which *" = "allow";
          "mkdir *" = "allow";
          "echo *" = "allow";
          "git status *" = "allow";
          "head *" = "allow";
          "tail *" = "allow";
          "find *" = "allow";
          "git remote -v" = "allow";
          "git log *" = "allow";
          "wc *" = "allow";
          "grep *" = "allow";
          "sort *" = "allow";
          "pwd *" = "allow";
        };
        "webfetch" = "allow";
        "websearch" = "allow";
      };
    };
  };

  home.activation = lib.mkIf features.agents.opencode {
    setupOpencodeSymlinks = lib.hm.dag.entryAfter [ "createAgentsDir" ] ''
      # Create symlinks from opencode directories to shared agents directory
      mkdir -p "$HOME/.config/opencode"
      ln -sf "$HOME/.agents/AGENTS.md" "$HOME/.config/opencode/AGENTS.md"
      ln -sf "$HOME/.agents/skills" "$HOME/.config/opencode/skills"
    '';
  };
}
