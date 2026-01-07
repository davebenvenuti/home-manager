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
    in
    {
      homeConfigurations = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          isDarwin = system == "aarch64-darwin";
          # Only pass darwin to home.nix if we're on Darwin
          extraSpecialArgs = if isDarwin then { inherit darwin; } else { };
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [
            ./home.nix
          ];

          # Pass extra arguments to home.nix
          extraSpecialArgs = extraSpecialArgs;
        }
      );
    };
}
