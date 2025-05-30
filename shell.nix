let
  pkgs = import <nixpkgs> {};
  pkgs-unstable = import <nixpkgs-unstable> {};
in 
pkgs.mkShell.override { stdenv = pkgs-unstable.llvmPackages_20.libcxxStdenv; } {
  nativeBuildInputs = [
    pkgs.xmake

    pkgs-unstable.llvmPackages_20.bintools-unwrapped
    pkgs-unstable.llvmPackages_20.libcxx.dev
    pkgs-unstable.llvmPackages_20.compiler-rt
    (pkgs-unstable.llvmPackages_20.clang-tools.override { enableLibcxx = true; })
    pkgs.mold

    pkgs.pkg-config
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs-unstable.llvmPackages_20.libcxx}/lib:$LD_LIBRARY_PATH
  '';

  hardeningDisable = [ "all" ];
}

