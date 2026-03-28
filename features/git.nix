{ lib, config, pkgs, features, ... }:

with lib; {
  options = {
    git.readmeHookForceConfirm = mkOption {
      type = types.bool;
      default = false;
      description = "Force confirmation before committing when README updates might be needed (requires git-hooks feature)";
    };
  };

  config = mkIf features.git-hooks {
    # Create git template directory
    home.file = {
      ".config/git/templates/hooks/pre-commit" = {
        source = ./git/pre-commit-readme-reminder.sh;
        executable = true;
        onChange = ''
          echo "Git pre-commit hook updated. Run 'git init' in existing repos to apply."
        '';
      };
      
      ".config/git/templates/description" = {
        text = ''
          Git template directory with pre-commit hook for README reminders.
          Managed by Home Manager.
        '';
      };
    };

    # Extend git configuration with template directory
    programs.git.settings.init.templatedir = "~/.config/git/templates";

    # Environment variable to control hook behavior
    home.sessionVariables = {
      GIT_HOOK_README_FORCE_CONFIRM = if config.git.readmeHookForceConfirm then "true" else "false";
      LLM_AGENT_ENABLED = "true"; # Enable LLM prompt generation
    };

    # Simple activation script to provide user feedback
    home.activation.setupGitHooks = hm.dag.entryAfter ["writeBoundary"] ''
      echo "Git pre-commit hook for README reminders has been installed."
      echo ""
      echo "To apply to existing repositories:"
      echo "  cd /path/to/repo && git init"
      echo ""
      echo "New repositories will automatically get the hook when created with 'git init'."
      echo "The hook will remind you to update READMEs when making significant changes."
    '';
  };
}