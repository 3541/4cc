{
  description = "A toy C compiler.";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
    utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    a3.url = "github:3541/liba3/v0.4.1";
  };

  outputs = { self, nixpkgs, utils, a3, ... }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages = utils.lib.flattenTree {
          fcc = pkgs.lib.recurseIntoAttrs (let
            fccbuild = ({ buildType, san ? false, compiler, extra ? [ ]
              , extraMesonArgs ? "" }:
              pkgs.stdenv.mkDerivation {
                name = "4cc";
                version = "0.1.0";

                nativeBuildInputs = with pkgs;
                  [ compiler git doxygen meson gtest pkg-config ninja ]
                  ++ extra;
                buildInputs = [ a3.defaultPackage.${system} pkgs.nasm ];
                src = ./.;

                mesonArgs = (if buildType == "release" then
                  "-Db_lto=true"
                else
                  "-Db_coverage=true") + (pkgs.lib.optionalString san
                    "-Db_sanitize=address,undefined") + extraMesonArgs;

                configurePhase = ''
                  CC=${compiler}/bin/cc CXX=${compiler}/bin/c++ meson setup --prefix $out \
                      --buildtype ${buildType} --wrap-mode nodownload build
                '';
                buildPhase = "meson compile -C build";
                checkPhase = "meson test -C build";
                doCheck = true;
                installPhase = "meson install -C build";
              });
            buildTypes = (compiler: extra: mesonArgs: {
              debug = fccbuild {
                buildType = "debug";
                compiler = compiler;
                extra = extra;
                extraMesonArgs = mesonArgs;
              };
              san = fccbuild {
                buildType = "debug";
                compiler = compiler;
                extra = extra;
                extraMesonArgs = mesonArgs;
                san = true;
              };
              release = fccbuild {
                buildType = "release";
                compiler = compiler;
                extra = extra;
                extraMesonArgs = mesonArgs;
              };
            });
          in (buildTypes pkgs.gcc [ ] "") // {
            clang = pkgs.lib.recurseIntoAttrs
              (buildTypes pkgs.llvmPackages_latest.clang
                [ pkgs.llvmPackages_latest.libllvm ]
                "--native-file boilerplate/meson/clang.ini");
          });
        };
        defaultPackage = packages."fcc/release";

        devShell = pkgs.mkShell {
          packages = with pkgs; [
            gdb
            rr
            clang-tools
            (let unwrapped = include-what-you-use;
            in stdenv.mkDerivation {
              pname = "include-what-you-use";
              version = lib.getVersion unwrapped;

              dontUnpack = true;

              clang = llvmPackages_latest.clang;
              inherit unwrapped;

              installPhase = ''
                runHook preInstall

                mkdir -p $out/bin
                substituteAll ${./nix-wrapper} $out/bin/include-what-you-use
                chmod +x $out/bin/include-what-you-use

                cp ${unwrapped}/bin/iwyu_tool.py $out/bin/iwyu_tool.py
                sed -i \
                    "s,executable_name = '.*\$,executable_name = '$out/bin/include-what-you-use'," \
                    $out/bin/iwyu_tool.py

                runHook postInstall
              '';
            })
          ];

          inputsFrom = [ packages."fcc/clang/debug" packages."fcc/debug" ];

          shellHook = ''
            unset AR
          '';
        };
      });
}
