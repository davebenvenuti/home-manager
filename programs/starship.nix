{ config, pkgs, ... }:

{
  programs.starship = {
    enable = true;

    settings = {
      character = {
        success_symbol = "[➜](bold green)";
        error_symbol = "[➜](bold red)";
      };

      add_newline = false;

      directory = {
        truncation_length = 1;
      };

      git_branch = {
        symbol = "git";
        format = "[$symbol:\\([$branch](bold red)\\)](bold blue) ";
      };

      git_status = {
        style = "bold yellow";
      };

      custom = {
        zshrc_private_sync = {
          description = "Indicates whether .zshrc.private needs to be synced to bitwarden";
          when = "zsh -c \"source ${config.home.homeDirectory}/.local/share/zsh/zshrc-private-sync.zsh && _zshrc_private_needs_sync_indicator\"";
          command = "echo \"*\""; # Display a star
          style = "yellow";
        };
      };

      # https://calvinpyong.com/blog/starship-robbyrussell/
      format = " $custom$character$directory$git_branch$git_status";
    };
  };
}
