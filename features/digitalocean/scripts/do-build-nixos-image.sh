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
          
          # Set system state version to avoid warning
          system.stateVersion = "26.05";
          
          # DigitalOcean configuration
          # The digital-ocean-config.nix module is automatically included
          # when building DigitalOcean images and provides:
          # - digitalocean-metadata service (fetches metadata with retry logic)
          # - digitalocean-ssh-keys service (fetches SSH keys from metadata)
          # - automatic root password setting from metadata (if enabled)
          # - hostname configuration from metadata
          # - entropy seeding from vendor data
          virtualisation.digitalOcean = {
            setRootPassword = false;  # We use SSH keys only
            setSshKeys = true;        # Allow DigitalOcean to inject SSH keys
            seedEntropy = true;       # Seed RNG from vendor data
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
# Check for the actual image file (name may vary)
if [ -f "result/nixos.img.tar.gz" ]; then
  IMAGE_PATH="$(readlink -f result/nixos.img.tar.gz)"
  cp "$IMAGE_PATH" ./nixos-digitalocean.img.tar.gz
  echo ""
  echo "✅ Image built successfully!"
  echo "Image saved to: $(pwd)/nixos-digitalocean.img.tar.gz"
  echo "Size: $(du -h nixos-digitalocean.img.tar.gz | cut -f1)"
elif ls result/*.qcow2.gz 1> /dev/null 2>&1; then
  # Newer versions produce .qcow2.gz files
  IMAGE_PATH="$(readlink -f result/*.qcow2.gz)"
  cp "$IMAGE_PATH" ./nixos-digitalocean.img.tar.gz
  echo ""
  echo "✅ Image built successfully! (QCow2 format)"
  echo "Image saved to: $(pwd)/nixos-digitalocean.img.tar.gz"
  echo "Size: $(du -h nixos-digitalocean.img.tar.gz | cut -f1)"
  echo "Note: File has .qcow2.gz extension but renamed to .img.tar.gz for compatibility"
else
  echo "❌ Error: Image not found at expected location"
  echo "Checked: result/nixos.img.tar.gz and result/*.qcow2.gz"
  ls -la result/
  exit 1
fi

# Clean up
rm -rf "$BUILD_DIR"