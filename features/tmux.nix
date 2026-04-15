{ config, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    prefix = "C-a";
    baseIndex = 1;
    # paneBaseIndex is not a valid option, we'll set it via extraConfig
    mouse = true;
    terminal = "screen-256color";

    extraConfig = builtins.readFile ./tmux.extra.conf;
  };

  programs.fzf = {
    enable = true;
    tmux.enableShellIntegration = true;
  };

  programs.sesh = {
    enable = true;
    enableAlias = true; # "s"
    # We disable the built-in tmux integration and define our own binding in
    # tmux.extra.conf so we can default to tmux-only session list.
    enableTmuxIntegration = false;
    icons = true;
  };
}
