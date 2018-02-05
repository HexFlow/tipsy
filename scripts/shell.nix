{ pkgs ? import <nixpkgs> {} }:
  with pkgs;
  python27Packages.buildPythonApplication {
    name = "tipsy-cluster";
    buildInputs =  (with python27Packages; [
      numpy
      (matplotlib.override { enableQt = true;  })
      scipy
      pyqt4
    ]);
  }
