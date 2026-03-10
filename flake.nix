{
  description = "Home Manager configuration of dave";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Needed for macOS support
    darwin = {
      url = "github:nix-darwin/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # For secret management in containers
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    # Containers configuration
    containers.url = "git+file:///home/dave/.config/containers/systemd";
  };

  outputs = { nixpkgs, home-manager, darwin, sops-nix, containers, ... }:
    let
      # Supported systems
      supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Function to create a home configuration for a given system
      mkHomeConfig = system: homeDirectory: features:
        let
          # pkgs = nixpkgs.legacyPackages.${system};
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };

          extraSpecialArgs = {
            inherit darwin;
            inherit system;
            inherit homeDirectory;
            inherit features;
            configDir = builtins.toString ./.;
            inputs = { inherit sops-nix containers; };
          };
          
          # Base modules - containers.nix will handle conditional inclusion
          modules = [ ./home.nix ];
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          inherit extraSpecialArgs;
          modules = modules;
        };

      defaultFeatures = {
        bitwarden-cli = true;
        zshrc-private-sync = true;
        aider = true;
        ghostty = false;
        opencode = true;
        direnv = true;
        ruby = false;
        containers = true;
      };
    in
    {
      # Home configurations
      homeConfigurations = {
        "dave@shithouse" = mkHomeConfig "x86_64-linux" "/home/dave" (defaultFeatures // {
          ruby = true;
        });
        "dave@air" = mkHomeConfig "aarch64-darwin" "/Users/dave" (defaultFeatures // {
          ghostty = true;
          ruby = true;
        });
      };
    };
}
