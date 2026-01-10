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
          extraSpecialArgs = { inherit darwin; inherit system; };
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
        "dave@shithouse" = mkHomeConfig "x86_64-linux";
        "dave@air" = mkHomeConfig "aarch64-darwin";
      };
    };
}
