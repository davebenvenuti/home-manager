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

      # https://calvinpyong.com/blog/starship-robbyrussell/
      format = " $character$directory$git_branch$git_status";
    };
  };
}
