# Home Manager Configuration

This is a home-manager nix configuration for managing user environments across Linux and macOS systems.

## Common Tasks

### Apply home-manager config

`home-manager switch -b hmbackup`

### Create a new Rails project

Create new Rails projects using:

```bash
nix run nixpkgs#rubyPackages_3_4.rails -- new project_name

## Feature Flags

The configuration is customizable via feature flags defined in `flake.nix`. These flags enable or disable specific packages and configurations:

- `bitwarden-cli`: Bitwarden CLI tool for password management
- `zshrc-private-sync`: Private zshrc synchronization to a Bitwarden encrypted note
- `aider`: AI-powered coding assistant
- `ghostty`: Terminal emulator (enabled on macOS by default)
- `opencode`: Interactive CLI tool for software engineering tasks
- `direnv`: Environment variable management tool
- `ruby`: Ruby 3.4 programming language

Feature flags can be configured per system in the `homeConfigurations` section of `flake.nix`.
