# Global AI Agent Instructions

This file provides general guidance and preferences for AI-assisted development across all projects.

## Communication Style

- **Be direct and concise** - Get to the point without excessive pleasantries
- **Explain your reasoning** - When suggesting changes, briefly explain why
- **Admit uncertainty** - If you're not sure about something, say so
- **Prefer solutions over problems** - When identifying issues, suggest fixes

## Development Workflow Preferences

### Code Changes
- **Incremental changes** - Make small, focused changes rather than large rewrites
- **Preserve existing patterns** - Match the style and patterns already in the codebase
- **Document as you go** - Update comments and documentation when changing code

### Source Control
- **Always prompt before committing** - Ask the user whether to commit changes to source control. Some users prefer committing themselves, others want the LLM to handle it.
- **Work from feature branches** - If currently on the `main` or `master` branch, suggest creating and switching to a feature branch before making changes. Example: `git checkout -b feature/description-of-changes`
- **Use LLM environment variable** - When performing any git operation, set the environment variable `LLM=true` to indicate the operation is being performed by an LLM. Example: `LLM=true git commit -m "feat: add feature description"`

### Nix/NixOS Development
- **Use flakes when possible** - Prefer `flake.nix` over `shell.nix` for new projects
- **Keep dependencies explicit** - Declare all inputs in flake.nix
- **Enable direnv integration** - Add `use flake` to `.envrc` for automatic shell activation

### Rails Development  
- **Follow Rails conventions** - Use built-in generators and established patterns
- **Test-driven approach** - Write tests alongside features when appropriate
- **Keep controllers thin** - Move business logic to models or service objects

## Project-Specific Guidelines

### Home Manager Configuration
- **Feature flags** - Use feature flags for optional components (see `flake.nix`)
- **Modular structure** - Keep configurations in separate `.nix` files
- **Activation scripts** - Use home.activation for post-install setup

### Ruby Projects
- **Ruby version** - Prefer Ruby 4.0+ for new projects
- **Gem management** - Use `bundler` with explicit version constraints
- **Testing** - Use RSpec with factory_bot and shoulda-matchers

### JavaScript/TypeScript Projects
- **Package management** - Use npm with package-lock.json
- **Type safety** - Prefer TypeScript over plain JavaScript
- **Build tooling** - Use esbuild or similar for fast builds

## Tool Preferences

### Editors
- **Primary**: VS Code with extensions for language support
- **Terminal**: Zsh with oh-my-zsh, powerlevel10k theme
- **Git**: Use command-line git with helpful aliases

### Development Environment
- **Nix**: Primary package manager for reproducible environments
- **direnv**: Automatic environment switching
- **Home Manager**: User environment configuration

## Common Tasks Reference

### Creating New Projects
```bash
# Rails with Nix
cd /tmp
nix-shell -p ruby_4_0 sqlite libyaml --run "gem install rails -v 8.0.0 && rails new [project_name] --database=sqlite3"
mv [project_name] /path/to/desired/location

# Node.js project
mkdir new-project && cd new-project
npm init -y
```

### Git Workflow
```bash
# Check current branch and create feature branch if needed
current_branch=$(git branch --show-current)
if [[ "$current_branch" == "main" || "$current_branch" == "master" ]]; then
    echo "Currently on $current_branch branch. Creating feature branch..."
    LLM=true git checkout -b feature/description-of-changes
fi

# Create feature branch (if not already on one)
LLM=true git checkout -b feature/description

# Commit with meaningful messages (prompt user first)
# Ask: "Should I commit these changes? If so, what commit message would you like?"
LLM=true git add .
LLM=true git commit -m "feat: add feature description"

# Keep commits focused
LLM=true git add -p  # Review changes interactively
```

## Quality Standards

- **Code readability** over cleverness
- **Explicit over implicit** - Make dependencies and assumptions clear
- **Documentation** - Keep READMEs and comments up-to-date
- **Error handling** - Graceful degradation with clear error messages

## Asking for Help

When you need clarification:
1. State what you're trying to accomplish
2. Show what you've tried
3. Ask specific questions

Remember: The goal is efficient collaboration, not proving who knows more.