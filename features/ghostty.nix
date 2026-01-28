{ lib, features, pkgs, ... }:
{
  programs.ghostty = lib.optionalAttrs features.ghostty {
    enable = true;

    # Installing Ghostty isn't supported on Darwin
    package = lib.mkIf pkgs.stdenv.isDarwin null;

    enableZshIntegration = true;

    settings = {
      font-family = "Menlo";
      font-size = 20;
    };
  };
}
