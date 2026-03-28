output "upload_instructions" {
  value = <<EOT

=== DigitalOcean Spaces Bucket Created ===

Bucket Name: ${digitalocean_spaces_bucket.nixos_images.name}
Region: ${digitalocean_spaces_bucket.nixos_images.region}
Endpoint: https://${digitalocean_spaces_bucket.nixos_images.name}.${digitalocean_spaces_bucket.nixos_images.region}.digitaloceanspaces.com

To upload a NixOS image to this bucket:

1. Build the image:
   do-build-nixos-image

2. Upload to Spaces:
   doctl spaces object put ${digitalocean_spaces_bucket.nixos_images.name} nixos-digitalocean.img.tar.gz --acl public-read

3. Enable in DigitalOcean:
   do-enable-nix-image "https://${digitalocean_spaces_bucket.nixos_images.name}.${digitalocean_spaces_bucket.nixos_images.region}.digitaloceanspaces.com/nixos-digitalocean.img.tar.gz"

Or use the provided script:
   do-upload-nix-image

EOT
}

output "terraform_commands" {
  value = <<EOT

To manage this infrastructure:

# Initialize Terraform (first time only)
terraform init

# Plan changes
terraform plan -var="do_token=YOUR_TOKEN"

# Apply changes
terraform apply -var="do_token=YOUR_TOKEN"

# Destroy everything
terraform destroy -var="do_token=YOUR_TOKEN"

EOT
}