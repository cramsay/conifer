{ nixpkgs ? import ../nix/nixpkgs.nix {} }:

with nixpkgs;
let
  jupyter = import (builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "";
  }) {};

    iPython = jupyter.kernels.iPythonWith {
            name = "python";
                packages = p: with p; [ numpy plotly sympy pandas ];
                  };

  jupyterEnvironment =
    jupyter.jupyterlabWith {
      kernels = [ iPython ];
    };
in
  jupyterEnvironment.env
