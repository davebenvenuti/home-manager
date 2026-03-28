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