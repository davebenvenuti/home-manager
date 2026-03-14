{ lib, features, pkgs, ... }:
{
  home.packages = with pkgs; lib.mkIf features.monitoring [
    htop
    btop
    procs
    duf
    ncdu
    iotop
    bandwhich
    nmon
  ];
}
