{ pkgs, ... }:
{
  home.packages = with pkgs; [ librewolf ];

  programs.librewolf = {
    enable = true;

    # Start with sane defaults
    settings = {
      # Don't check if LibreWolf is the default browser on startup
      "browser.shell.checkDefaultBrowser" = false;

      # Enable DRM content (e.g. Netflix, Spotify)
      "media.eme.enabled" = true;
    };
  };
}
