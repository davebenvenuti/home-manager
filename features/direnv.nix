{ lib, features, pkgs, ... }:
{
  programs.direnv = lib.mkIf features.direnv {
    enable = true;
    nix-direnv.enable = true;
    
    config = {
      global = {
        hide_env_diff = true;
      };
    };
  };
}