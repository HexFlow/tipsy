{pkgs? import <nixpkgs> {} }:

with pkgs; let
  pq = (python27.withPackages (
  python27Packages: [
    python27Packages.scipy
    python27Packages.numpy
    python27Packages.pyqt4
    python27Packages.pymysql
  ]));
in
  stdenv.mkDerivation {
    name = "pythonForTipsy";
    src = ./.;
    buildInputs = [
      pq
    ];
    installPhase = ''
      mkdir -p $out/bin
      echo -e "#!${bash}/bin/bash\n${pq.interpreter}" '$@' > $out/bin/pythonForTipsy
      chmod +x $out/bin/pythonForTipsy
    '';
  }
