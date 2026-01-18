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
  };

  outputs = { nixpkgs, home-manager, darwin, ... }:
    let
      # Supported systems
      supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Function to create a home configuration for a given system
      mkHomeConfig = system: homeDirectory: features:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          isDarwin = system == "aarch64-darwin";
          extraSpecialArgs = {
            inherit darwin;
            inherit system;
            inherit homeDirectory;
            inherit features;
            configDir = builtins.toString ./.;
          };
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          inherit extraSpecialArgs;
          modules = [ ./home.nix ];
        };

      defaultFeatures = {
        bitwarden-cli = true;
        zshrc-private-sync = true;
        aider = true;
        ghostty = false;
        nodejs = true;
      };
    in
    {
      # Home configurations
      homeConfigurations = {
        "dave@shithouse" = mkHomeConfig "x86_64-linux" "/home/dave" (defaultFeatures // {
        });
        "dave@air" = mkHomeConfig "aarch64-darwin" "/Users/dave" (defaultFeatures // {
          ghostty = true;
        });
      };
    };
}
