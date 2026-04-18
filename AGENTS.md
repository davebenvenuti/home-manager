# Home Manager Configuration

This is a home-manager nix configuration for managing user environments across Linux and macOS systems.

## Common Tasks

### Apply home-manager config

`home-manager switch -b hmbackup`

## Feature Flags

The configuration is customizable via feature flags defined in `flake.nix`. These flags enable or disable specific packages and configurations:

- `bitwarden-cli`: Bitwarden CLI tool for password management
- `zshrc-private-sync`: Private zshrc synchronization to a Bitwarden encrypted note
- `ghostty`: Terminal emulator (enabled on macOS by default)
- `direnv`: Environment variable management tool
- `git-hooks`: Global git hooks for documentation reminders
- `ruby`: Ruby 3.4 programming language
- `monitoring`: System monitoring tools
- `agents`: AI agent configurations (nested):
  - `agents.opencode`: Interactive CLI tool for software engineering tasks
  - `agents.pi`: Terminal coding harness for agentic workflows

Feature flags can be configured per system in the `homeConfigurations` section of `flake.nix`.

## AI Agents Configuration

When `agents.opencode` or `agents.pi` are enabled, a shared directory `~/.agents/` is created containing:

- `AGENTS.md`: General guidelines for using AI agents
- `skills/`: Directory of reusable skill documents

Agent-specific directories are symlinked to these shared resources:
- `~/.config/opencode/AGENTS.md` → `~/.agents/AGENTS.md`
- `~/.config/opencode/skills` → `~/.agents/skills`
- `~/.pi/agent/AGENTS.md` → `~/.agents/AGENTS.md`
- `~/.pi/agent/skills` → `~/.agents/skills`

Skills are managed via Home Manager and can be updated by editing files in `~/.config/home-manager/features/agents/skills/`.

## Troubleshooting Notes

### Nix Evaluation Issues

When testing home-manager configurations, avoid using `nix eval` commands that try to evaluate the entire configuration (like `nix eval .#homeConfigurations."dave@air".config`). These commands often fail due to:
- Deprecated option warnings that interrupt evaluation
- Import-from-derivation (IFD) issues
- Evaluation of optional features that aren't fully defined

Instead, use:
- `home-manager switch -b hmbackup` to apply changes directly
- `nix build .#homeConfigurations."dave@air".activationPackage` to test buildability
- Check specific attributes with limited scope if needed

### Cross-Platform Building

When modifying packages for multiple platforms (Linux/Darwin):
- Test builds on the actual target platform when possible
- Remote builders or QEMU emulation may be needed for cross-platform testing
- Platform-specific dependencies should be conditionally added using `lib.optionals pkgs.stdenv.isDarwin` or `lib.optionals pkgs.stdenv.isLinux`