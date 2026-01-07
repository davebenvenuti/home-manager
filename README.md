# nix-conf

My personal nix configuration.

## Installing nix

### Darwin

Install [Deterministic Nix](https://github.com/DeterminateSystems/nix-installer) `curl -fsSL https://install.determinate.systems/nix | sh -s -- install`

### Linux

Use the [multiuser installer](https://nixos.org/download/#nix-install-linux)

`sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --daemon`

## Setup this home-manager config

```bash
git clone git@github.com:davebenvenuti/home-manager.git ~/.config/home-manager

nix run home-manager/master -- --switch
```

## Apply

`home-manager switch`

If you get an error about clobber files, run

`home-manager switch -b hmbackup`

## Uninstalling Nix

### Darwin

`/nix/nix-installer uninstall`

### Linux

See [Nix Reference Manual | Uninstalling Nix](https://nix.dev/manual/nix/2.21/installation/uninstall#linux)

## Notes

**home-manager** was initialized with `nix run home-manager/master -- init --switch`.  This created `~/.config/home-manager`.

For more information see the [home-manager flakes standalone docs](https://nix-community.github.io/home-manager/index.xhtml#sec-flakes-standalone)

`man home-configuration.nix` explains Home Manager configuration specification

[https://github.com/sadjow/home-manager/](https://github.com/sadjow/home-manager/) was helpful
