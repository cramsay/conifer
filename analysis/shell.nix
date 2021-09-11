{ nixpkgs ? import ../nix/nixpkgs.nix {} }:

with nixpkgs;
let
  jupyter = import (builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "6024eb9329c5f47102adc26f541712121bba62d4";
  }) {};

    iPython = jupyter.kernels.iPythonWith {
            name = "python";
                packages = p: with p; [ numpy plotly sympy pandas ];
                  };

  jupyterEnvironment =
    jupyter.jupyterlabWith {
      kernels = [ iPython ];
      directory = jupyter.mkDirectoryWith {
        extensions = [ "jupyterlab-plotly" ];
      };
    };
in
  jupyterEnvironment.env
