{ config, pkgs, features, ... }:

{
  programs.aider-chat = {
    enable = features.aider;
    package = pkgs.aider-chat-with-playwright;

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
