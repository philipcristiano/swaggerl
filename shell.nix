let
  sysPkg = import <nixpkgs> { };
  releasedPkgs = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "20.03-beta";
    sha256 = "04g53i02hrdfa6kcla5h1q3j50mx39fchva7z7l32pk699nla4hi";
  };
  pinnedPkgs = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "0dccdd61899b9e629cddcb348324e0d990937f69";
    sha256 = "0qrn10x9g3qv9791di75rlgjhwxxvwgx4pwxkwa5468j5hp9sqz0";
  };

  released_pkgs = import pinnedPkgs {};
  pinned_pkgs = import pinnedPkgs {};
  stdenv = released_pkgs.stdenv;

in stdenv.mkDerivation {
  name = "env";
  buildInputs = [ released_pkgs.gnumake
                  released_pkgs.erlang
                ];
}
