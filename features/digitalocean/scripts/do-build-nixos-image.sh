echo "=== Building NixOS image for DigitalOcean ==="
echo ""

# Create a temporary directory for the build
BUILD_DIR="$(mktemp -d)"
echo "Building in temporary directory: $BUILD_DIR"

# Create the flake.nix for building the image
cat > "$BUILD_DIR/flake.nix" << 'EOF'
{
  description = "NixOS image for DigitalOcean";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixos-generators, ... }: {
    packages.x86_64-linux.digitalocean-image = nixos-generators.nixosGenerate {
      system = "x86_64-linux";
      format = "do";
      modules = [
        # Basic NixOS configuration for DigitalOcean
        ({ config, pkgs, ... }: {
          # Enable SSH
          services.openssh.enable = true;
          services.openssh.settings.PasswordAuthentication = false;
          services.openssh.settings.PermitRootLogin = "prohibit-password";
          
          # Set root password to empty (use SSH keys only)
          users.users.root.initialHashedPassword = "";
          
          # Add your SSH public key here
          users.users.root.openssh.authorizedKeys.keys = [
            # Add your SSH public key(s) here, e.g.:
            # "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAI..."
          ];
          
          # Basic system packages
          environment.systemPackages = with pkgs; [
            vim
            htop
            git
          ];
          
          # Enable nix flakes
          nix.settings.experimental-features = [ "nix-command" "flakes" ];
          
          # Set timezone
          time.timeZone = "UTC";
          
          # Networking
          networking.firewall.enable = true;
          networking.firewall.allowedTCPPorts = [ 22 80 443 ];
          
          # DigitalOcean-specific configuration
          systemd.services.digitalocean-metadata = {
            description = "Fetch DigitalOcean metadata";
            wantedBy = [ "multi-user.target" ];
            after = [ "network-online.target" ];
            serviceConfig = {
              Type = "oneshot";
              ExecStart = "${pkgs.curl}/bin/curl -s http://169.254.169.254/metadata/v1.json -o /etc/digitalocean-metadata.json";
              RemainAfterExit = true;
            };
          };
        })
      ];
    };
  };
}
EOF

echo "Building NixOS image..."
cd "$BUILD_DIR"

# Build the image
nix build .#digitalocean-image

# Copy the result to current directory
IMAGE_PATH="$(readlink -f result/nixos.img.tar.gz)"
if [ -f "$IMAGE_PATH" ]; then
  cp "$IMAGE_PATH" ./nixos-digitalocean.img.tar.gz
  echo ""
  echo "✅ Image built successfully!"
  echo "Image saved to: $(pwd)/nixos-digitalocean.img.tar.gz"
  echo "Size: $(du -h nixos-digitalocean.img.tar.gz | cut -f1)"
else
  echo "❌ Error: Image not found at expected location"
  echo "Expected: $IMAGE_PATH"
  ls -la result/
  exit 1
fi

# Clean up
rm -rf "$BUILD_DIR"