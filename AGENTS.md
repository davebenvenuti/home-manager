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

### Nix Language Patterns

#### `lib.mkIf` vs `lib.optionals` / `lib.optionalAttrs`

These serve different purposes and are not interchangeable. Use the right one:

| Situation | Use | Reason |
|---|---|---|
| Conditional **option definition block** inside a module's `config = { ... }` | `lib.mkIf` | Module system property — when condition is false, the definition disappears entirely (doesn't participate in merging or override priority) |
| Conditional elements **inside a list value** (e.g. `home.packages`, `buildInputs`, list of file paths) | `lib.optionals` | Plain Nix function — returns `[]` when false, concats into the list |
| Conditional keys **inside an attribute set value** (pure Nix, not module opts) | `lib.optionalAttrs` | Plain Nix function — returns `{}` when false, merges into the attrset |
| Any conditional where the condition **references other `config` values being defined** | **Must** use `mkIf` | Plain `if` causes infinite recursion because it evaluates immediately; `mkIf` delays the condition into the module system |

**Key points:**

- `mkIf` only has special meaning **inside module option definitions**. Outside that context (e.g. in a `let ... in` expression, building up a plain value), it acts as a no-op wrapper — confusing and useless.
- `optionals`/`optionalAttrs` are just plain Nix functions and work anywhere.
- `mkIf false` removes the definition entirely. `optionalAttrs false` still produces `{}`, which **can accidentally override** a `mkDefault` value from another module. This is the subtle bug to watch for.

**Examples — correct patterns:**

```nix
# mkIf: wrapping a complete option definition block under a feature flag
{ config, lib, features, ... }: {
  config = lib.mkIf features.direnv {
    programs.direnv.enable = true;
  };
}

# mkIf: conditional block inside mkMerge
config = lib.mkMerge [
  { home.packages = with pkgs; [ htop btop ]; }
  (lib.mkIf features.monitoring {
    home.packages = with pkgs; [ btop procs ];
  })
];

# optionals: conditional items inside a list
home.packages = with pkgs; [
  htop
  btop
] ++ lib.optionals pkgs.stdenv.isLinux [
  iotop
  nmon
];

# optionalAttrs: conditional keys in an attrset (outside module config)
someValue = {
  a = 1;
} // lib.optionalAttrs (someCondition) {
  b = 2;
};
```

**Avoid these anti-patterns:**

```nix
# BAD: wrapping a list value with mkIf (optionals or optionalAttrs is clearer)
home.packages = lib.mkIf features.ruby [ ruby_4_0 ];  ← works but misleading

# BAD: using plain if in module config when condition references config
config = if config.services.xxx.enable then { ... } else {};  ← infinite recursion!

# BAD: mkIf outside module config
let result = lib.mkIf true { a = 1; };  ← mkIf is a no-op here, just returns { a = 1 }
```

### Cross-Platform Building

When modifying packages for multiple platforms (Linux/Darwin):
- Test builds on the actual target platform when possible
- Remote builders or QEMU emulation may be needed for cross-platform testing
- Platform-specific dependencies should be conditionally added using `lib.optionals pkgs.stdenv.isDarwin` or `lib.optionals pkgs.stdenv.isLinux`