# Git Pre-commit Hook for README Reminders

This feature adds a global git pre-commit hook that reminds you (and LLM agents) to update README documentation when making significant changes to a project.

## What It Does

When you run `git commit`, the hook:
1. Analyzes staged files for changes that might need documentation updates
2. Provides a friendly reminder about updating README files
3. Generates LLM-ready prompts for documentation updates
4. Optionally requires confirmation before committing

## File Types That Trigger Reminders

- **Configuration files**: `.json`, `.yaml`, `.yml`, `.toml`, `.ini`, `.cfg`, `.conf`
- **Source code**: `.py`, `.js`, `.ts`, `.java`, `.go`, `.rs`, `.cpp`, `.c`, `.h`, `.rb`, `.php`
- **Scripts/automation**: `.sh`, `.bash`, `.zsh`, `Makefile`, `Dockerfile`
- **Dependencies**: `package.json`, `requirements.txt`, `Pipfile`, `pyproject.toml`, `Cargo.toml`, `go.mod`
- **Project setup**: `.gitignore`, `.editorconfig`, `flake.nix`, `shell.nix`

## Files That Are Skipped

- Test files: `*.test.js`, `*.spec.ts`, `*.test.py`
- Test directories: `test/`, `spec/`
- README files themselves
- Markdown documentation files

## Configuration Options

In your `flake.nix`:

```nix
{
  features = {
    git-hooks = true;  # Enable the feature
  };
}
```

You can also configure force confirmation in the git module:

```nix
{
  git = {
    readmeHookForceConfirm = false;  # Set to true to require confirmation
  };
}
```

## Environment Variables

- `GIT_HOOK_README_FORCE_CONFIRM`: Set to "true" to force confirmation before committing
- `LLM_AGENT_ENABLED`: Set to "true" to generate LLM prompts (enabled by default)

## Applying to Existing Repositories

The hook is automatically applied to new repositories when you run `git init`. For existing repositories:

```bash
cd /path/to/your/repo
git init
```

This will copy the hook template without affecting your existing git history.

## LLM Integration

When working with AI agents (like pi or opencode), the hook:

1. Saves a ready-to-use prompt to `/tmp/readme_update_prompt_*.txt`
2. Attempts to copy the prompt to your clipboard
3. Provides context about what changed and what needs documentation

Example LLM prompt structure:
```
# README Update Assistant

## Changed Files:
- src/main.py
- requirements.txt
- config.yaml

## Task:
Review the README.md file and suggest updates based on the changes above.
Focus on:
1. Usage examples that need updating
2. New features to document
3. Configuration changes
4. Installation/requirements updates
...
```

## Disabling the Hook

Temporarily disable for a single commit:
```bash
git commit --no-verify
```

Disable the feature entirely:
```nix
{
  features.git-hooks = false;
}
```

Or remove from your configuration by deleting the import from `home.nix` and removing the feature flag.

## Customization

To customize the hook behavior:

1. Edit `features/git/pre-commit-readme-reminder.sh`
2. Update the `SKIP_PATTERNS` array to add/remove file patterns
3. Modify the file type detection logic in the case statement
4. Adjust the output messages and colors

After making changes, run `home-manager switch` to apply.

## How It Works with Home Manager

1. The hook script is installed to `~/.config/git/templates/hooks/pre-commit`
2. Git is configured to use `~/.config/git/templates` as the template directory
3. When you run `git init`, the hook is copied to `.git/hooks/pre-commit`
4. The activation script ensures proper setup on each Home Manager switch