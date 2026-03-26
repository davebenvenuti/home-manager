# Rails Development with Nix

This skill covers Rails development workflows using Nix for environment management.

## Creating a new Rails project

Use the clean environment approach (recommended):

```bash
cd /tmp
nix-shell -p ruby_4_0 sqlite libyaml --run "gem install rails -v 8.0.0 && rails new [project_name] --database=sqlite3"
mv [project_name] /path/to/desired/location
```

**Note:** This ensures you get Rails 8.0.0 with Ruby 4.0, regardless of what Rails version is packaged in Nixpkgs.

## Nix-managed Rails Development Environment

When working with existing Rails projects that use Nix for development environment management:

1. **Check for Nix configuration**: Look for `flake.nix`, `shell.nix`, or `.envrc` files
2. **Enter development shell**: Use `nix develop` or `nix-shell` to enter the development environment
3. **Use direnv**: If `.envrc` exists with `use flake` or `use nix`, ensure direnv is installed and configured
4. **Install gems**: Run `bundle install` within the Nix shell
5. **External dependencies**: If gems require external libraries (e.g., `libyaml` for psych, `postgresql` for pg), ensure they're included in the Nix configuration

### Example flake.nix for Rails projects

**PostgreSQL-based Rails app (Ruby 4.0):**
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
        };
      });
    };
}
```

**SQLite-based Rails app (Ruby 4.0):**
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

- **Ruby gems**: Add to `Gemfile` and run `bundle install`
- **System libraries**: Add to `packages` list in `flake.nix` (e.g., `libpq` for PostgreSQL C library, `sqlite` for SQLite database)
- **Development tools**: Add to `packages` list in `flake.nix`

## Database-specific considerations

- **PostgreSQL**: Include `postgresql` package and ensure `pg` gem is in Gemfile
- **SQLite**: Include `sqlite` package and ensure `sqlite3` gem is in Gemfile
- **MySQL**: Include `mysql` package and ensure `mysql2` gem is in Gemfile