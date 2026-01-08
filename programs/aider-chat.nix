{ config, pkgs, ... }:

{
  programs.aider-chat = {
    enable = true;
    package = pkgs.aider-chat;

    settings = {
      model = "deepseek/deepseek-coder";
      editor = "/usr/bin/nano -w";

      read = [ "~/.aider/AGENTS.md" "AGENTS.md" ];

      verify-ssl = false;
      architect = false;
      cache-prompts = true;
      dark-mode = true;
      dirty-commits = false;
      lint = false;
      watch-files = true;
    };
  };
}
