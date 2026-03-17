{ lib, features, pkgs, ... }:
{
  home.packages = with pkgs; lib.mkIf features.monitoring (
    [
      htop
      btop
      procs
      duf
      ncdu
      bandwhich
    ] ++ lib.optionals (!stdenv.isDarwin) [
      iotop
      nmon
    ]
  );
}
