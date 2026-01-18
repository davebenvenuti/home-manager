{ config, pkgs, lib, ... }:

let
  initContentExtra = builtins.readFile ./zsh.extra;
in
{
  programs.zsh = with lib; {
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
    initContent = mkAfter initContentExtra;

    shellAliases = {
      grep = "grep --color=auto";
    };
  };

  # Further modified in features/zshrc-private-sync.nix
}
