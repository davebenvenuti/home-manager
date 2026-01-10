# nix-conf

My personal nix configuration.

## Installing nix

### Darwin

Install [Deterministic Nix](https://github.com/DeterminateSystems/nix-installer) `curl -fsSL https://install.determinate.systems/nix | sh -s -- install`

### Linux

Use the [multiuser installer](https://nixos.org/download/#nix-install-linux)

`sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --daemon`

## Setup this home-manager config

```bash
nix run home-manager/master -- --init switch

rm -rf ~/.config/home-manager

git clone git@github.com:davebenvenuti/home-manager.git ~/.config/home-manager
```

## Apply

Assuming the hostnames are set properly (`air` or `shithouse`),

`home-manager switch`

If they're not set, you can switch to individual configurations by doing

`home-manager switch --flake .#dave@air`

If you get an error about clobber files, run

`home-manager switch -b hmbackup`

## Uninstalling Nix

### Darwin

`/nix/nix-installer uninstall`

### Linux

See [Nix Reference Manual | Uninstalling Nix](https://nix.dev/manual/nix/2.21/installation/uninstall#linux)

## Notes

**home-manager** was initialized with `nix run home-manager/master -- init --switch`.  This created `~/.config/home-manager`.

For more information see the [home-manager flakes standalone docs](https://nix-community.github.io/home-manager/index.xhtml#sec-flakes-standalone)

`man home-configuration.nix` explains Home Manager configuration specification

[https://github.com/sadjow/home-manager/](https://github.com/sadjow/home-manager/) was helpful

## Zshrc.private Sync System

This configuration includes a system to securely sync your `~/.zshrc.private` file to Bitwarden. This file is meant for sensitive environment variables, API keys, and other secrets that shouldn't be committed to version control.

### How it works:

1. **Prompt Indicator**: A yellow asterisk (`*`) appears in your zsh prompt when `~/.zshrc.private` has been modified but not yet synced to Bitwarden.

2. **Sync Command**: Run `sync-zshrc-private` to upload the current version to Bitwarden as a secure note. The note is named `.zshrc.private (HOSTNAME)`.

3. **Status Check**: Use `zshrc-private-status` to check if your file is synced without syncing.

4. **Manual Refresh**: If needed, run `refresh-zsh-prompt` to immediately update the prompt indicator.

### Setup:

1. Ensure Bitwarden CLI is installed and configured (`bw` command available)
2. Run `bw login` to log into your Bitwarden account
3. Run `bw unlock` to unlock your vault and get a session key
4. Set the session key as an environment variable:
   ```bash
   export BW_SESSION="$(bw unlock --raw)"
   ```
   (Consider adding this to your shell configuration)

### Files:

- `~/.zshrc.private`: Your private shell configuration (not tracked in git)
- `~/.zshrc.private.lastsync`: Stores the hash of the last synced version
- `~/.local/bin/sync-zshrc-private.sh`: Sync script
- `~/.local/share/zsh/zshrc-private-sync.zsh`: Zsh plugin for prompt integration

### How the prompt indicator updates:

The indicator is part of the PROMPT string itself (`$(_zshrc_private_prompt_indicator)`), so it's evaluated each time the prompt is displayed. This means it updates automatically without needing to reload your shell or run special commands.
