---
name: rails-nix-development
description: Guides for developing Ruby on Rails applications using Nix for reproducible development environments. Includes project setup, dependency management, and database configuration patterns
---

# Rails Development with Nix

This skill covers Rails development workflows using Nix for environment management.

## Key principles

- **Define the environment first** — create `flake.nix` at the project destination before running any Rails commands. This ensures a consistent Ruby version and all system dependencies from the start.
- **Project-local GEM_HOME** — use a project-specific `.gem/` directory to avoid conflicts with user-global gems compiled against different Ruby versions. The flake sets `GEM_HOME=$PWD/.gem` via `shellHook`.
- **No vendoring** — `.gem/` is added to `.gitignore`. The project-local gem home is for development only.

## Creating a new Rails project

### 1. Create the project directory and flake

```bash
mkdir /path/to/project
cd /path/to/project
```

Create `flake.nix` (see examples below), then enter the Nix development shell:

```bash
nix develop
```

This generates `flake.lock` and sets up the environment with `GEM_HOME` pointing to `.gem/`.

### 2. Install Rails and generate the project

From within the Nix shell:

```bash
gem install rails -v 8.0.0 --no-doc
rails new . --database=sqlite3 --skip-git --force
```

**Flags explained:**
- `--skip-git` — prevents Rails from running `git init`, since we want to add our own `.gitignore` (with `.gem/` ignored) before committing
- `--force` — allows creating the project in the current (non-empty) directory

### 3. Set up Git, .gitignore, and .envrc

After `rails new` completes, create `.gitignore` with `.gem/` and `.direnv/` ignored:

```gitignore
# ...
/.gem/
/.direnv/
```

Create `.envrc` for direnv integration:

```
use flake
```

Then initialize git and commit:

```bash
git init
git add -A
git commit -m "Initialize new Rails project"
```

### Why this approach

- **Consistent environment** — the Nix shell provides Ruby 4.0, SQLite, and libyaml consistently, avoiding version mismatches with user-global gems
- **No /tmp shuffling** — the project is created directly at its final destination
- **Isolated gems** — `GEM_HOME` is scoped to `.gem/` in the project, so `gem install` and `bundle install` use fresh native extensions compiled against the Nix shell's Ruby
- **Gems not vendored** — `.gem/` is in `.gitignore`

## Example flake.nix for Rails projects

### SQLite-based Rails app (Ruby 4.0):

```nix
{
  description = "Developer environment for Rails app";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in {
      devShells = forAllSystems ({ pkgs }: with pkgs; {
        default = mkShell {
          packages = [
            git
            gnumake
            nixpkgs-fmt
            ruby_4_0
            sqlite  # SQLite database
            libyaml  # Required for psych gem
          ];

          shellHook = ''
            export GEM_HOME="$PWD/.gem"
            export PATH="$GEM_HOME/bin:$PATH"
          '';
        };
      });
    };
}
```

### PostgreSQL-based Rails app (Ruby 4.0):

```nix
{
  description = "Developer environment for Rails app";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in {
      devShells = forAllSystems ({ pkgs }: with pkgs; {
        default = mkShell {
          packages = [
            git
            gnumake
            nixpkgs-fmt
            ruby_4_0
            postgresql_18
            libyaml  # Required for psych gem
          ];

          shellHook = ''
            export GEM_HOME="$PWD/.gem"
            export PATH="$GEM_HOME/bin:$PATH"
          '';
        };
      });
    };
}
```

## Common Rails development commands in Nix environment

```bash
# Enter development environment
nix develop

# Or with direnv (if .envrc exists)
direnv allow

# Install gems
bundle install

# Database setup
bin/rails db:create
bin/rails db:migrate

# Start development server
bin/rails server -p 3000
```

## Adding new dependencies

- **Ruby gems**: Add to `Gemfile` and run `bundle install`. Bundler will install into the project's `.gem/` directory.
- **System libraries**: Add to `packages` list in `flake.nix` (e.g., `libpq` for PostgreSQL C library, `sqlite` for SQLite database)
- **Development tools**: Add to `packages` list in `flake.nix`

## Database-specific considerations

- **PostgreSQL**: Include `postgresql` package and ensure `pg` gem is in Gemfile
- **SQLite**: Include `sqlite` package and ensure `sqlite3` gem is in Gemfile
- **MySQL**: Include `mysql` package and ensure `mysql2` gem is in Gemfile
