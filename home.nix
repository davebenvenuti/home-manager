{ config, pkgs, lib, darwin, system, features, homeDirectory, ... }:

let username = "dave";
in
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = username;
  home.homeDirectory = homeDirectory;

  # This works better in /etc/nix/nix.conf or if you're a trust-user (which isn't recommended)
  # nix = {
  #   package = pkgs.nix;
  #   settings = {
  #    substituters = [
  #      "https://cache.nixos.org"
  #      "https://nix-community.cachix.org"
  #    ];

  #    trusted-public-keys = [
  #      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
  #      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  #    ];

  #    experimental-features = "nix-command flakes";
  #   };
  # };

  # Validate features
  assertions = [
    {
      assertion = !(features.zshrc-private-sync or false) || (features.bitwarden-cli or false);
      message = "If features.zshrc-private-sync is true, then features.bitwarden-cli must also be true";
    }
  ];

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
  # Most of these are managed in programs/*.nix
  home.packages = with pkgs; [
    # jq for JSON processing in activation scripts
    jq

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
  ]
  ++ (lib.optional features.bitwarden-cli pkgs.bitwarden-cli);

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = { };

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
    RCLONE_FAST_LIST = "true";
  };

  # Import program configurations from separate modules
  imports = [
    ./programs/git.nix
    ./programs/tmux.nix
    ./programs/zsh.nix
    ./programs/eza.nix
    ./programs/bat.nix
    ./programs/starship.nix
    ./programs/emacs.nix

    ./features/zshrc-private-sync.nix
    ./features/aider.nix
    ./features/ghostty.nix
    ./features/opencode.nix
  ];

  # Activation scripts run after configuration is applied
  home.activation = {
    # Remove .profile if it exists
    removeProfile = lib.hm.dag.entryAfter ["writeBoundary"] ''
      PROFILE_PATH="$HOME/.profile"
      if [ -e "$PROFILE_PATH" ]; then
        echo "Removing $PROFILE_PATH..."
        rm -f "$PROFILE_PATH"
      else
        echo "$PROFILE_PATH does not exist (nothing to remove)"
      fi
    '';
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
