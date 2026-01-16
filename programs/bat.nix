{ ... }:
{
  programs.bat = {
    enable = true;
    config = {
      theme = "TwoDark";
    };
  };

  programs.zsh = {
    shellAliases = {
      cat="bat --paging=never";
    };
  };
}
