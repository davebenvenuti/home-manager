echo "=== Uploading NixOS image to DigitalOcean Spaces ==="
echo ""

# Check for image file
IMAGE_FILE="nixos-digitalocean.img.tar.gz"
if [ ! -f "$IMAGE_FILE" ]; then
  echo "❌ Error: Image file '$IMAGE_FILE' not found in current directory"
  echo "Run 'do-build-nixos-image' first to build the image"
  exit 1
fi

echo "Image file: $IMAGE_FILE"
echo "Size: $(du -h "$IMAGE_FILE" | cut -f1)"
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

# Ask for bucket name
echo "Please enter your DigitalOcean Spaces bucket name:"
read -r BUCKET_NAME

if [ -z "$BUCKET_NAME" ]; then
  echo "❌ Error: Bucket name cannot be empty"
  exit 1
fi

# Check if bucket exists
echo "Checking if bucket '$BUCKET_NAME' exists..."
if ! doctl spaces list --format Name | grep -q "^$BUCKET_NAME$"; then
  echo "Bucket '$BUCKET_NAME' not found."
  echo "Please create it first using the DigitalOcean console or:"
  echo "  doctl spaces create $BUCKET_NAME --region nyc3"
  exit 1
fi

echo "✓ Bucket found"
echo ""

# Upload the image
echo "Uploading image to Spaces bucket '$BUCKET_NAME'..."
doctl spaces object put "$BUCKET_NAME" "$IMAGE_FILE" --acl public-read

# Get the public URL
REGION="nyc3"  # Default region, adjust if needed
IMAGE_URL="https://$BUCKET_NAME.$REGION.digitaloceanspaces.com/$IMAGE_FILE"

echo ""
echo "✅ Image uploaded successfully!"
echo ""
echo "Public URL: $IMAGE_URL"
echo ""
echo "Next, run 'do-enable-nix-image' to make this image available in DigitalOcean:"
echo "  do-enable-nix-image \"$IMAGE_URL\""