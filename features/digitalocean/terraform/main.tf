terraform {
  required_providers {
    digitalocean = {
      source = "digitalocean/digitalocean"
      version = "~> 2.0"
    }
  }
}

provider "digitalocean" {
  token = var.do_token
}

# Create a Spaces bucket for storing NixOS images
resource "digitalocean_spaces_bucket" "nixos_images" {
  name   = var.bucket_name
  region = var.region
  
  # Enable versioning to keep history of uploaded images
  versioning {
    enabled = true
  }
  
  # CORS configuration to allow public access
  cors_rule {
    allowed_headers = ["*"]
    allowed_methods = ["GET", "PUT", "POST", "DELETE", "HEAD"]
    allowed_origins = ["*"]
    max_age_seconds = 3000
  }
}

# Create a Spaces bucket policy to allow public read access
resource "digitalocean_spaces_bucket_policy" "public_read" {
  region = digitalocean_spaces_bucket.nixos_images.region
  bucket = digitalocean_spaces_bucket.nixos_images.name
  
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid       = "PublicReadGetObject"
        Effect    = "Allow"
        Principal = "*"
        Action    = "s3:GetObject"
        Resource  = "arn:aws:s3:::${digitalocean_spaces_bucket.nixos_images.name}/*"
      }
    ]
  })
}

# Output the bucket endpoint for easy access
output "bucket_endpoint" {
  value = "https://${digitalocean_spaces_bucket.nixos_images.name}.${digitalocean_spaces_bucket.nixos_images.region}.digitaloceanspaces.com"
}

output "bucket_name" {
  value = digitalocean_spaces_bucket.nixos_images.name
}

output "bucket_region" {
  value = digitalocean_spaces_bucket.nixos_images.region
}