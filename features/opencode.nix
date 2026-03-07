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
