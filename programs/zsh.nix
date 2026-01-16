{ config, pkgs, lib, ... }:

{
  programs.zsh = {
    enable = true;

    # Enable useful features
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    # History configuration
    history = {
      size = 100000;
      save = 100000;
      path = "${config.home.homeDirectory}/.zsh_history";
      ignoreDups = true;
      ignoreSpace = true;
      share = true;
    };

    # Read the extra configuration from a separate file
    initContent = lib.mkDefault (builtins.readFile ./zsh.extra);
  };

  # Further modified in features/zshrc-private-sync.nix
}
