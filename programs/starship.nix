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

      cmd_duration = {
        min_time = 0;
        format = " duration: [$duration]($style)";
        style = "bold yellow";
      };

      custom.cmd_start_time = {
        description = "Command start time";
        command = "echo $CMD_START_TIME";
        when = "test -n \"$CMD_START_TIME\"";
        format = "start: [$output]($style)";
        style = "bold cyan";
      };

      custom.cmd_end_time = {
        description = "Command end time";
        command = "echo $CMD_END_TIME";
        when = "test -n \"$CMD_END_TIME\"";
        format = " end: [$output]($style)";
        style = "bold green";
      };

      # https://calvinpyong.com/blog/starship-robbyrussell/
      format = "$custom$character$directory$git_branch$git_status\n\${custom.cmd_start_time}\${custom.cmd_end_time}$cmd_duration";

      hostname = {
        ssh_only = false;
        format = "[$ssh_symbol$hostname]($style)";
      };

      right_format = "$os$hostname";

      # Further modified in features/zshrc-private-sync.nix
    };
  };
}
