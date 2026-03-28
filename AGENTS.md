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