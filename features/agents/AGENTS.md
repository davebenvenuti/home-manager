# AI Agents Feature Configuration

This directory manages the AI agents feature in the home-manager configuration.

## Structure

The agents feature provides shared configuration for AI coding assistants (opencode, pi) via Home Manager:

```
features/agents/
├── default.nix          # Main configuration module
├── AGENTS.md            # This file - explains the agents feature
├── AGENTS.global.md     # Global instructions for LLMs on other projects
├── skills/              # Shared skill files
│   └── rails-nix-development.md
├── opencode.nix         # opencode-specific configuration
├── opencode/            # Legacy opencode files
├── pi.nix               # pi-specific configuration
└── pi/                  # pi configuration files
    └── settings.json    # pi settings template
```

## Feature Flags

Agents are controlled via nested feature flags in `flake.nix`:

```nix
defaultFeatures = {
  agents = {
    opencode = true;   # Enable opencode
    pi = false;        # Disable pi by default
  };
};
```

Enable/disable per system in `homeConfigurations`.

## Shared Resources

When enabled, the configuration creates:
- `~/.agents/` - Shared directory with AGENTS.md and skills
- Symlinks from agent directories to shared resources

## Adding New Agents

1. Create agent configuration in `features/agents/[agent-name].nix`
2. Use `lib.mkIf features.agents.[agent-name]` guard
3. Add symlinks in `default.nix` if needed
4. Update feature flags in `flake.nix`

## Adding Skills

Skills go in `features/agents/skills/` and are automatically symlinked to all enabled agents.

To add a skill:
1. Create `features/agents/skills/[skill-name].md`
2. Document purpose, commands, examples
3. Run `home-manager switch` to deploy

## Configuration Updates

All configuration is managed via Nix. To modify:
- Edit files in this directory
- Commit changes to git
- Run `home-manager switch --flake .#dave@shithouse`

## Agent-Specific Notes

### opencode
- Managed by `programs.opencode` in Nix
- Configuration: `~/.config/opencode/config.yaml` (auto-generated)
- Shared resources symlinked from `~/.agents/`

### pi  
- Custom package build from GitHub
- Configuration: `~/.pi/agent/settings.json` (managed template)
- Shared resources symlinked from `~/.agents/`