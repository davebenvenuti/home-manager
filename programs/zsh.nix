{ config, pkgs, ... }:

{
  programs.zsh = {
    enable = true;

    # Enable useful features
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    # History configuration
    history = {
      size = 10000;
      save = 10000;
      path = "${config.home.homeDirectory}/.zsh_history";
      ignoreDups = true;
      share = true;
    };

    # Read the extra configuration from a separate file
    initContent = builtins.readFile ./zsh.extra;
  };

  home.file.".local/bin/sync-zshrc-private.sh" = {
    source = ./zsh/sync-zshrc-private.sh;
    executable = true;
  };

  home.file.".local/share/zsh/zshrc-private-sync.zsh" = {
    source = ./zsh/zshrc-private-sync.zsh;
  };
}
