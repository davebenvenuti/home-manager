{ lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    htop
    btop
    procs
    duf
    ncdu
    bandwhich
  ] ++ lib.optionals (!stdenv.isDarwin) [
    iotop
    nmon
  ];
}
