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
      # See: https://github.com/anomalyco/opencode/issues/15907#issuecomment-3994009370
      "copy-on-select" = "clipboard";
      "clipboard-read" = "allow";
      "clipboard-write" = "allow";
    };
  };
}
