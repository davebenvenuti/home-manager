echo "=== Uploading NixOS image to Backblaze B2 ==="
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

# Check for Backblaze B2 credentials
echo "Checking Backblaze B2 credentials..."
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

echo "✓ Backblaze B2 credentials found"
echo ""

# Set Backblaze B2 endpoint (S3-compatible API)
# Default to US West region, can be overridden with BACKBLAZE_ENDPOINT
BACKBLAZE_ENDPOINT="${BACKBLAZE_ENDPOINT:-s3.us-west-002.backblazeb2.com}"
BUCKET_NAME="$BACKBLAZE_BUCKET_NAME"

echo "Using Backblaze B2 bucket: $BUCKET_NAME"
echo "Endpoint: $BACKBLAZE_ENDPOINT"
echo ""

# Configure s3cmd for Backblaze B2
echo "Configuring s3cmd for Backblaze B2..."
S3CMD_CONFIG="$HOME/.s3cfg-backblaze"
cat > "$S3CMD_CONFIG" << EOF
access_key = $BACKBLAZE_APPLICATION_KEY_ID
secret_key = $BACKBLAZE_APPLICATION_KEY
host_base = $BACKBLAZE_ENDPOINT
host_bucket = $BACKBLAZE_ENDPOINT
use_https = True
EOF

# Upload the image using s3cmd
echo "Uploading image to Backblaze B2 bucket '$BUCKET_NAME'..."
s3cmd --config="$S3CMD_CONFIG" put "$IMAGE_FILE" "s3://$BUCKET_NAME/"

# Get the public URL
IMAGE_URL="https://$BUCKET_NAME.$BACKBLAZE_ENDPOINT/$IMAGE_FILE"

echo ""
echo "✅ Image uploaded successfully to Backblaze B2!"
echo ""
echo "Public URL: $IMAGE_URL"
echo ""
echo "Next, run 'do-enable-nix-image' to make this image available in DigitalOcean:"
echo "  do-enable-nix-image \"$IMAGE_URL\""