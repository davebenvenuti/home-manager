# DigitalOcean NixOS Image Builder

This configuration provides tools for building and deploying NixOS images to DigitalOcean.

## Available Commands

1. **do-build-nixos-image** - Build a NixOS image suitable for DigitalOcean droplets
2. **do-upload-nix-image** - Upload the built image to a Backblaze B2 bucket
3. **do-enable-nix-image** - Make the uploaded image available in DigitalOcean

## Prerequisites

1. Set your DigitalOcean API token:
   ```bash
   export DIGITALOCEAN_TOKEN="your-api-token-here"
   ```

2. Set your Backblaze B2 credentials:
   ```bash
   export BACKBLAZE_APPLICATION_KEY_ID="your-application-key-id"
   export BACKBLAZE_APPLICATION_KEY="your-application-key"
   export BACKBLAZE_BUCKET_NAME="digital-ocean-images"
   # Optional: Set a different endpoint if not using US West region
   # export BACKBLAZE_ENDPOINT="s3.us-west-002.backblazeb2.com"
   ```

3. Create a Backblaze B2 bucket named "digital-ocean-images" (or adjust the environment variable)

## Workflow

### Option 1: Step-by-step (explicit control)

1. Build the image:
   ```bash
   do-build-nixos-image
   ```

2. Upload to Backblaze B2:
   ```bash
   do-upload-nix-image
   ```

3. Enable in DigitalOcean (using the URL from step 2):
   ```bash
   do-enable-nix-image "https://digital-ocean-images.s3.us-west-002.backblazeb2.com/nixos-digitalocean.img.tar.gz"
   ```

### Option 2: Combined upload and enable (recommended)

1. Build the image:
   ```bash
   do-build-nixos-image
   ```

2. Upload and enable in one step:
   ```bash
   do-enable-nix-image
   ```
   
   This will:
   - Upload the local `nixos-digitalocean.img.tar.gz` to Backblaze B2
   - Create a DigitalOcean image from the uploaded URL
   - Use the same Backblaze B2 credentials as `do-upload-nix-image`

## Troubleshooting

### Build Errors

If you encounter build errors about conflicting `systemd.services.digitalocean-metadata` definitions:
- The script has been updated to avoid defining services that conflict with the built-in `digital-ocean-config.nix` module
- The module is automatically included when building DigitalOcean images and provides proper metadata fetching with retry logic

### Image Format

Recent versions of nixos-generators produce `.qcow2.gz` files instead of `.img.tar.gz`. The script automatically handles this by renaming the file for compatibility with the upload process.

## Notes

- The image is built with SSH enabled (key-based authentication only)
- Root password is disabled for security
- Basic system packages (vim, htop, git) are included
- Nix flakes are enabled
- Timezone is set to UTC
- Images are uploaded to Backblaze B2 using their S3-compatible API