{ lib, config, pkgs, ... }:

let
  # Helper function to create scripts with proper shebang and environment check
  mkScript = name: script: pkgs.writeShellScriptBin name ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Check for DIGITALOCEAN_TOKEN
    if [ -z "''${DIGITALOCEAN_TOKEN:-}" ]; then
      echo "❌ Error: DIGITALOCEAN_TOKEN environment variable is not set" >&2
      echo "Please set it before running this script:" >&2
      echo "  export DIGITALOCEAN_TOKEN='your-token-here'" >&2
      exit 1
    fi
    
    ${script}
  '';
in {
  # DigitalOcean feature configuration
  home.packages = with pkgs; [
    # Required tools
    doctl
    s3cmd
    terraform
    
    # Image building tools
    nixos-generators
    qemu
    gzip
    pigz
    parted
    util-linux
    
    # Scripts
    (mkScript "do-build-nixos-image" (builtins.readFile ./digitalocean/scripts/do-build-nixos-image.sh))
    (mkScript "do-upload-nix-image" (builtins.readFile ./digitalocean/scripts/do-upload-nix-image.sh))
    (mkScript "do-enable-nix-image" (builtins.readFile ./digitalocean/scripts/do-enable-nix-image.sh))
  ];
  
  # Terraform configuration for DigitalOcean Spaces bucket
  home.file.".config/digitalocean/terraform/main.tf".source = ./digitalocean/terraform/main.tf;
  home.file.".config/digitalocean/terraform/variables.tf".source = ./digitalocean/terraform/variables.tf;
  home.file.".config/digitalocean/terraform/outputs.tf".source = ./digitalocean/terraform/outputs.tf;
  
  # Documentation
  home.file.".config/digitalocean/README.md".text = ''
    # DigitalOcean NixOS Image Builder
    
    This configuration provides tools for building and deploying NixOS images to DigitalOcean.
    
    ## Available Commands
    
    1. **do-build-nixos-image** - Build a NixOS image suitable for DigitalOcean droplets
    2. **do-upload-nix-image** - Upload the built image to a DigitalOcean Spaces bucket
    3. **do-enable-nix-image** - Make the uploaded image available in DigitalOcean
    
    ## Prerequisites
    
    1. Set your DigitalOcean API token:
       ```bash
       export DIGITALOCEAN_TOKEN="your-api-token-here"
       ```
    
    2. Create a DigitalOcean Spaces bucket (or use the Terraform configuration):
       ```bash
       doctl spaces create my-nixos-images --region nyc3
       ```
    
    ## Workflow
    
    1. Build the image:
       ```bash
       do-build-nixos-image
       ```
    
    2. Upload to Spaces:
       ```bash
       do-upload-nix-image
       ```
       Follow the prompts to enter your bucket name.
    
    3. Enable in DigitalOcean:
       ```bash
       do-enable-nix-image "https://your-bucket.nyc3.digitaloceanspaces.com/nixos-digitalocean.img.tar.gz"
       ```
    
    ## Terraform Configuration
    
    Terraform files for creating a Spaces bucket are available in:
    ~/.config/digitalocean/terraform/
    
    To use:
    ```bash
    cd ~/.config/digitalocean/terraform
    terraform init
    terraform apply
    ```
    
    ## Notes
    
    - The image is built with SSH enabled (key-based authentication only)
    - Root password is disabled for security
    - Basic system packages (vim, htop, git) are included
    - Nix flakes are enabled
    - Timezone is set to UTC
  '';
}