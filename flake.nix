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
      mkHomeConfig = system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          isDarwin = system == "aarch64-darwin";
          extraSpecialArgs = if isDarwin then { inherit darwin; } else { };
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ ./home.nix ];
          extraSpecialArgs = extraSpecialArgs;
        };
    in
    {
      # Home configurations
      homeConfigurations = {
        "dave@linux" = mkHomeConfig "x86_64-linux";
        "dave@darwin" = mkHomeConfig "aarch64-darwin";
        # dave is an attribute set keyed by system
        "dave" = forAllSystems (system: mkHomeConfig system);
      };

      # Default package for `nix run`
      defaultPackage = forAllSystems (system:
        (mkHomeConfig system).activationPackage
      );

      # Packages per system
      packages = forAllSystems (system: {
        "home-manager-configuration" = (mkHomeConfig system).activationPackage;
      });

      # Legacy packages for compatibility with home-manager's lookup
      legacyPackages = forAllSystems (system: {
        homeConfigurations = {
          "dave" = (mkHomeConfig system);
        };
      });
    };
}
