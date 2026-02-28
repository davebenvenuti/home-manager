{ lib, features, pkgs, ... }:
{
  programs.opencode = lib.mkIf features.opencode {
    enable = true;
    package = pkgs.opencode; # Assuming opencode is available in pkgs

    settings = {
      model = "deepseek/deepseek-chat";
      autoupdate = false;
      permission = {
        "bash" = "ask";
        "webfetch" = "allow";
        "websearch" = "allow";
      };
    };
  };
}
