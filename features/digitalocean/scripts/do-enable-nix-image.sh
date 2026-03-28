echo "=== Enabling NixOS image in DigitalOcean ==="
echo ""

if [ $# -lt 1 ]; then
  echo "Usage: do-enable-nix-image <IMAGE_URL> [IMAGE_NAME]"
  echo ""
  echo "Where:"
  echo "  IMAGE_URL - Public URL to the image (from do-upload-nix-image)"
  echo "  IMAGE_NAME - Optional name for the image (default: nixos-$(date +%Y%m%d))"
  echo ""
  echo "Example:"
  echo "  do-enable-nix-image \"https://my-bucket.nyc3.digitaloceanspaces.com/nixos-digitalocean.img.tar.gz\""
  exit 1
fi

IMAGE_URL="$1"
IMAGE_NAME="${2:-nixos-$(date +%Y%m%d)}"
REGION="${REGION:-nyc3}"
IMAGE_DESCRIPTION="NixOS image built on $(date)"

echo "Image URL: $IMAGE_URL"
echo "Image name: $IMAGE_NAME"
echo "Region: $REGION"
echo "Description: $IMAGE_DESCRIPTION"
echo ""

# Check if doctl is authenticated
echo "Checking doctl authentication..."
if ! doctl auth list > /dev/null 2>&1; then
  echo "🔐 Authenticating doctl..."
  doctl auth init --access-token "$DIGITALOCEAN_TOKEN"
fi

if ! doctl auth list > /dev/null 2>&1; then
  echo "❌ Failed to authenticate doctl"
  exit 1
fi

echo "✓ Authenticated as: $(doctl account get --format Email)"
echo ""

# Check if image already exists
echo "Checking for existing images..."
EXISTING_IMAGE=$(doctl compute image list --public false --format "ID,Name" | { grep "^[^ ]* $IMAGE_NAME$" || true; } | head -1 | awk '{print $1}')

if [ -n "$EXISTING_IMAGE" ]; then
  echo "⚠️  Image '$IMAGE_NAME' already exists (ID: $EXISTING_IMAGE)"
  echo ""
  read -p "Do you want to delete the existing image and create a new one? (y/N): " -n 1 -r
  echo ""
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Deleting existing image..."
    doctl compute image delete "$EXISTING_IMAGE" --force
    echo "✓ Image deleted"
  else
    echo "Keeping existing image. Exiting."
    exit 0
  fi
fi

# Create the image
echo "Creating DigitalOcean image from URL..."
echo "This may take several minutes..."
echo ""

doctl compute image create "$IMAGE_NAME" \
  --region "$REGION" \
  --image-description "$IMAGE_DESCRIPTION" \
  --image-url "$IMAGE_URL"

echo ""
echo "✅ Image created successfully!"
echo ""

echo "Image details:"
doctl compute image list --public false --format "ID,Name,Type,Distribution,MinDiskSize,Status,Created" | grep "$IMAGE_NAME"

echo ""
echo "You can now use this image when creating droplets:"
echo "  doctl compute droplet create my-droplet --image '$IMAGE_NAME' --region $REGION --size s-1vcpu-1gb"