echo "=== Enabling NixOS image in DigitalOcean ==="
echo ""

# Default values
IMAGE_NAME="${2:-nixos-$(date +%Y%m%d)}"
REGION="${REGION:-nyc3}"
IMAGE_DESCRIPTION="NixOS image built on $(date)"
LOCAL_IMAGE_FILE="nixos-digitalocean.img.tar.gz"

# Function to upload image to Backblaze B2
upload_to_backblaze() {
  local image_file="$1"
  
  # All progress messages go to stderr, only URL goes to stdout
  echo "Checking Backblaze B2 credentials..." >&2
  
  if [ -z "${BACKBLAZE_APPLICATION_KEY_ID:-}" ]; then
    echo "❌ Error: BACKBLAZE_APPLICATION_KEY_ID environment variable is not set" >&2
    echo "Please set it before running this script:" >&2
    echo "  export BACKBLAZE_APPLICATION_KEY_ID='your-application-key-id'" >&2
    exit 1
  fi

  if [ -z "${BACKBLAZE_APPLICATION_KEY:-}" ]; then
    echo "❌ Error: BACKBLAZE_APPLICATION_KEY environment variable is not set" >&2
    echo "Please set it before running this script:" >&2
    echo "  export BACKBLAZE_APPLICATION_KEY='your-application-key'" >&2
    exit 1
  fi

  if [ -z "${BACKBLAZE_BUCKET_NAME:-}" ]; then
    echo "❌ Error: BACKBLAZE_BUCKET_NAME environment variable is not set" >&2
    echo "Please set it before running this script:" >&2
    echo "  export BACKBLAZE_BUCKET_NAME='your-bucket-name'" >&2
    exit 1
  fi

  echo "✓ Backblaze B2 credentials found" >&2
  echo "" >&2

  # Set Backblaze B2 endpoint (S3-compatible API)
  # Default to US West region, can be overridden with BACKBLAZE_ENDPOINT
  BACKBLAZE_ENDPOINT="${BACKBLAZE_ENDPOINT:-s3.us-west-002.backblazeb2.com}"
  BUCKET_NAME="$BACKBLAZE_BUCKET_NAME"

  echo "Using Backblaze B2 bucket: $BUCKET_NAME" >&2
  echo "Endpoint: $BACKBLAZE_ENDPOINT" >&2
  echo "" >&2

  # Configure s3cmd for Backblaze B2
  echo "Configuring s3cmd for Backblaze B2..." >&2
  
  S3CMD_CONFIG="$HOME/.s3cfg-backblaze"
  cat > "$S3CMD_CONFIG" << EOF
access_key = $BACKBLAZE_APPLICATION_KEY_ID
secret_key = $BACKBLAZE_APPLICATION_KEY
host_base = $BACKBLAZE_ENDPOINT
host_bucket = $BACKBLAZE_ENDPOINT
use_https = True
EOF

  # Upload the image using s3cmd
  echo "Uploading image to Backblaze B2 bucket '$BUCKET_NAME'..." >&2
  
  s3cmd --config="$S3CMD_CONFIG" put "$image_file" "s3://$BUCKET_NAME/" >&2

  # Get the public URL
  IMAGE_URL="https://$BUCKET_NAME.$BACKBLAZE_ENDPOINT/$image_file"
  
  echo "" >&2
  echo "✅ Image uploaded successfully to Backblaze B2!" >&2
  echo "Public URL: $IMAGE_URL" >&2
  echo "" >&2
  
  # Return URL via stdout (for capture)
  echo "$IMAGE_URL"
}

# Determine image URL
if [ $# -ge 1 ]; then
  # URL provided as argument
  IMAGE_URL="$1"
  echo "Using provided image URL: $IMAGE_URL"
else
  # No URL provided, check for local image
  if [ ! -f "$LOCAL_IMAGE_FILE" ]; then
    echo "❌ Error: No image URL provided and local image file '$LOCAL_IMAGE_FILE' not found"
    echo ""
    echo "Usage: do-enable-nix-image [IMAGE_URL] [IMAGE_NAME]"
    echo ""
    echo "Where:"
    echo "  IMAGE_URL - Optional public URL to the image (if not provided, will upload local file)"
    echo "  IMAGE_NAME - Optional name for the image (default: nixos-$(date +%Y%m%d))"
    echo ""
    echo "Examples:"
    echo "  do-enable-nix-image \"https://my-bucket.s3.us-west-002.backblazeb2.com/nixos-digitalocean.img.tar.gz\""
    echo "  do-enable-nix-image  # Will upload local nixos-digitalocean.img.tar.gz to Backblaze B2"
    exit 1
  fi
  
  echo "No URL provided, uploading local image '$LOCAL_IMAGE_FILE' to Backblaze B2..."
  echo "Size: $(du -h "$LOCAL_IMAGE_FILE" | cut -f1)"
  echo ""
  
  # Upload and capture URL (show progress)
  IMAGE_URL=$(upload_to_backblaze "$LOCAL_IMAGE_FILE")
fi

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