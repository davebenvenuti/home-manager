{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    package = pkgs.git;

    # Include private gitconfig - using relative path to match your gitconfig
    includes = [
      { path = ".gitconfig-private"; }
    ];

    # All configuration using the new settings attribute
    settings = {
      apply = {
        whitespace = "nowarn";
      };
      color = {
        ui = true;
      };
      push = {
        default = "matching";
      };
      pull = {
        rebase = true;
      };
      init = {
        defaultBranch = "main";
      };
      core = {
        excludesFile = "~/.gitignore";
      };
      alias = {
        ci = "commit";
        co = "checkout";
        st = "status";
        stat = "status";
        br = "branch";
        unci = "reset --soft HEAD^";
        unstage = "reset HEAD";
        put = "push origin HEAD";
        rmbr = ''!sh -c "git branch -d \\$0 && git push origin :heads/\\$0 && git remote prune origin"'';
        rmremote = ''!sh -c "git push origin :heads/\\$0"'';
        get = "!git pull && git submodule init && git submodule update";
        "permission-reset" = ''!git diff -p -R --no-color | grep -E "^(diff|(old|new) mode)" --color=never | git apply'';
      };
    };
  };

  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      navigate = true;
      light = false;
      side-by-side = true;
    };
  };
}
