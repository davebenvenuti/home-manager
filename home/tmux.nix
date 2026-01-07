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
}
