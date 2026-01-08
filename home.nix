{ config, pkgs, lib, darwin, system, ... }:

let
  isMacOS = builtins.match ".*darwin.*" system != null;
in

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "dave";
  home.homeDirectory = if isMacOS
    then "/Users/dave"
    else "/home/dave";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # Git will be installed via programs.git, but we can list it here for clarity
    pkgs.git
    # Tmux will be installed via programs.tmux, but we can list it here for clarity
    pkgs.tmux
    # Zsh will be installed via programs.zsh, but we can list it here for clarity
    pkgs.zsh
    # Tig - text-mode interface for Git
    pkgs.tig
    # Emacs without X11 GUI
    pkgs.emacs-nox

    pkgs.aider-chat
    # Bitwarden CLI for password management
    pkgs.bitwarden-cli
    # jq for JSON processing in activation scripts
    pkgs.jq

    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file =
    let
      baseFiles = {
        # Files that should get installed in all platforms
        ".local/bin/sync-zshrc-private.sh" = {
          source = ./script/zsh/sync-zshrc-private.sh;
          executable = true;
        };
        ".local/share/zsh/zshrc-private-sync.zsh" = {
          source = ./script/zsh/zshrc-private-sync.zsh;
        };
      };
      macFiles = lib.optionalAttrs isMacOS {
        "Library/Application Support/com.mitchellh.ghostty/config".source = ./dotfiles/ghostty/config;
      };
    in
    baseFiles // macFiles;

  home.sessionPath = [ "$HOME/.local/bin" ];

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/dave/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    EDITOR = "nano -w";
    GIT_EDITOR = "nano -w";
  };

  # Import program configurations from separate modules
  imports = [
    ./programs/git.nix
    ./programs/tmux.nix
    ./programs/zsh.nix
    ./programs/aider-chat.nix
  ];

  # Activation scripts run after configuration is applied
  # TODO: Switch to fetchFromGitHub for reproducible setup of emacs configuration
  home.activation = {
    cloneEmacsConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
      EMACS_DIR="$HOME/.emacs.d"
      if [ ! -d "$EMACS_DIR" ]; then
        echo "Cloning emacs configuration repository..."
        ${pkgs.git}/bin/git clone git@github.com:davebenvenuti/emacs.d.git "$EMACS_DIR"
      else
        echo "Emacs configuration already exists at $EMACS_DIR"
      fi
    '';
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
