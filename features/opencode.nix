{ lib, features, pkgs, ... }:
{
  programs.opencode = lib.mkIf features.opencode {
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
        };
        "webfetch" = "allow";
        "websearch" = "allow";
      };
    };
  };

  home.file.".config/opencode/AGENTS.md" = {
    source = ./opencode/AGENTS.md;
  };
}
