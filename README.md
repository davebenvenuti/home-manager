# nix-conf

My personal nix configuration.

## Installing nix

### Darwin

Install [Deterministic Nix](https://github.com/DeterminateSystems/nix-installer) `curl -fsSL https://install.determinate.systems/nix | sh -s -- install`

## Apply

`home-manager switch`

If you get an error about clobber files, run

`home-manager switch -b hmbackup`

## Uninstalling Nix

`/nix/nix-installer uninstall`

## Notes

**home-manager** was initialized with `nix run home-manager/master -- init --switch`.  This created `~/.config/home-manager`.

For more information see the [home-manager flakes standalone docs](https://nix-community.github.io/home-manager/index.xhtml#sec-flakes-standalone)

`man home-configuration.nix` explains Home Manager configuration specification

[https://github.com/sadjow/home-manager/](https://github.com/sadjow/home-manager/) was helpful
