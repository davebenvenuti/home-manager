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

      os = {
        disabled = false;
      };

      # https://calvinpyong.com/blog/starship-robbyrussell/
      format = "$custom$character$directory$git_branch$git_status";

      hostname = {
        ssh_only = false;
        format = "[$ssh_symbol$hostname]($style)";
      };

      right_format = "$os$hostname";

      # Further modified in features/zshrc-private-sync.nix
    };
  };
}
