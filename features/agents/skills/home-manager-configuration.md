# Home Manager Configuration Management

This skill provides guidance for working with this specific home-manager configuration project.

## Project Location
- **Path**: `/home/dave/.config/home-manager/`
- **Repository**: Git-managed home-manager configuration
- **Structure**: Modular Nix configuration with feature flags

## Applying Configuration Changes

When making changes to this home-manager configuration, **always** apply them using:

```bash
home-manager switch -b hmbackup
```

### Why `-b hmbackup`?
- Creates a backup of the current generation before applying changes
- Provides a safety net to roll back if something goes wrong
- Preserves previous configuration state

### Alternative Commands
- **Dry-run (check syntax)**: `home-manager build`
- **Build only**: `nix build .#homeConfigurations.dave@shithouse.activationPackage`
- **Check flake**: `nix flake check`

## Development Workflow

### 1. Make Changes
Edit Nix files in the configuration directory:
- `home.nix` - Main configuration
- `flake.nix` - Feature flags and system configurations
- `features/` - Modular feature configurations
- `programs/` - Program-specific configurations

### 2. Test Changes
Before applying, test the configuration:
```bash
# Check flake syntax
nix flake check

# Build the activation package (dry run)
nix build .#homeConfigurations.dave@shithouse.activationPackage
```

### 3. Apply Changes
```bash
home-manager switch -b hmbackup
```

### 4. Verify
Check that the changes were applied correctly:
```bash
# Check current generation
home-manager generations

# View activation script
cat $(home-manager generations | head -1 | awk '{print $7}')/activate
```

## Feature Flags

This configuration uses feature flags defined in `flake.nix` to enable/disable components:

```nix
defaultFeatures = {
  bitwarden-cli = true;
  zshrc-private-sync = true;
  ghostty = false;
  direnv = true;
  git-hooks = false;
  ruby = false;
  monitoring = true;
  agents = {
    opencode = true;
    pi = false;
  };
};
```

### Enabling/Disabling Features
1. Edit `flake.nix` in the `homeConfigurations` section
2. Use `nixpkgs.lib.recursiveUpdate` to modify features per system
3. Apply with `home-manager switch -b hmbackup`

## Agent Configuration

When `agents.opencode` or `agents.pi` are enabled:
- Shared directory: `~/.agents/` with `AGENTS.md` and `skills/`
- Symlinks from agent directories to shared resources
- Skills are managed in `features/agents/skills/`

### Adding New Skills
1. Create `features/agents/skills/[skill-name].md`
2. Document purpose, commands, examples
3. Apply with `home-manager switch -b hmbackup`
4. Skills will be available in all enabled agents

## Troubleshooting

### Common Issues
1. **Build fails**: Check Nix syntax with `nix flake check`
2. **Activation fails**: Use backup to roll back: `home-manager switch --previous`
3. **Missing packages**: Verify feature flags are enabled in `flake.nix`

### Rollback
If something goes wrong:
```bash
# Switch to previous generation
home-manager switch --previous

# List all generations
home-manager generations

# Switch to specific generation
home-manager switch --generation N
```

## Git Integration

This configuration is git-managed. Follow these practices:

1. **Work on feature branches**: `git checkout -b feature/description`
2. **Commit changes**: Use descriptive commit messages
3. **Test before pushing**: Always run `home-manager switch -b hmbackup` before considering changes complete

## System-Specific Notes

- **Linux system**: `dave@shithouse` - Has pi agent enabled
- **macOS system**: `dave@air` - Has ghostty enabled

Check `flake.nix` for system-specific feature configurations.