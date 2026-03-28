variable "do_token" {
  description = "DigitalOcean API token"
  type        = string
  sensitive   = true
}

variable "bucket_name" {
  description = "Name of the Spaces bucket for NixOS images"
  type        = string
  default     = "nixos-images"
}

variable "region" {
  description = "DigitalOcean region for the Spaces bucket"
  type        = string
  default     = "nyc3"
  
  validation {
    condition     = contains(["nyc3", "sfo3", "ams3", "sgp1", "fra1", "tor1", "blr1"], var.region)
    error_message = "Region must be one of: nyc3, sfo3, ams3, sgp1, fra1, tor1, blr1"
  }
}