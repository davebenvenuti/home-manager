{ config, pkgs, lib, features, ... }:

lib.optionalAttrs features.zshrc-private-sync {
  programs.starship.settings.custom.zshrc_private_sync = {
    description = "Indicates whether .zshrc.private needs to be synced to bitwarden";
    when = "zsh -c \"source ${config.home.homeDirectory}/.local/share/zsh/zshrc-private-sync.zsh && _zshrc_private_needs_sync_indicator\"";
    command = "echo \"*\""; # Display a star
    style = "yellow";
  };

  home.file.".local/bin/sync-zshrc-private.sh" = {
    source = ./zshrc-private-sync/sync-zshrc-private.sh;
    executable = true;
  };

  home.file.".local/share/zsh/zshrc-private-sync.zsh" = {
    source = ./zshrc-private-sync/zshrc-private-sync.zsh;
  };
}
