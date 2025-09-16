{
  description = "yaaaaaaaaaaaaaaaaaaaaa";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    zig-overlay = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    zls = {
      url = "github:zigtools/zls/0.14.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.zig-overlay.follows = "zig-overlay";
    };

    zig2nix = {
      url = "github:Cloudef/zig2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system: let
      flakePackage = flake: package: flake.packages."${system}"."${package}";
      flakeDefaultPackage = flake: flakePackage flake "default";

      # - [zig-overlay/sources.json](https://github.com/mitchellh/zig-overlay/blob/main/sources.json)
      zig-env = inputs.zig2nix.zig-env.${system};
      zigv = pkgs.callPackage "${inputs.zig2nix}/zig.nix" rec {
        zigSystem = (zig-env {}).lib.zigDoubleFromString system;
        zigHook = (zig-env {}).zig-hook;
        # - [get info from](https://machengine.org/zig/index.json)
        version = "0.14.0";
        release = {
          "version" = version;
          # "date" = "2024-10-14";
          # "src" = {
          #   "shasum" = "3ef8c05894701190c7bb5990f7e410eeb20afc5bc1f018262727891869c00c79";
          #   "tarball" = "https://github.com/ziglang/zig/archive/3bf89f55c20550ba2acf0c9d904ff040d87979fa.tar.gz";

          #   # "shasum" = "53b40ced023a41631014931c1141d9d4245ce41b674c46c34beceb2ba24ba9f9";
          #   # "size" = "17611756";
          #   # "tarball" = "https://pkg.machengine.org/zig/zig-${version}.tar.xz";
          #   # "zigTarball" = "https://ziglang.org/builds/zig-${version}.tar.xz";
          # };
          # "bootstrap" = {
          #   "shasum" = "213284ce3259ac6ebd085f6f3d4e3d25dc855de2d39975d3c094fbfde2662a21";
          #   "size" = "47879500";
          #   "tarball" = "https://pkg.machengine.org/zig/zig-bootstrap-${version}.tar.xz";
          #   "zigTarball" = "https://ziglang.org/builds/zig-bootstrap-${version}.tar.xz";
          # };
          "x86_64-linux" = {
            "shasum" = "sha256-Rz7CaAYTPPTRkYyvGkEPhAOhPZeXJqkEW0IbaFAxqYI=";
            "size" = "";
            "zigTarball" = "https://pkg.machengine.org/zig/zig-linux-x86_64-${version}.tar.xz";
            "tarball" = "https://ziglang.org/builds/zig-linux-x86_64-${version}.tar.xz";
          };
        };
      };
      rust-toolchain = pkgs.rust-bin.stable.latest.default.override {
        extensions = ["cargo" "rustc" "rust-src"];
        targets = ["x86_64-unknown-linux-musl"];
      };
      naersk-lib = inputs.naersk.lib.${system}.override {
        cargo = rust-toolchain;
        rustc = rust-toolchain;
      };
      overlays = [
        (self: super: rec {
          zig = zigv.bin;
          # zig = zigv.src;
          # zig = inputs.zig2nix.outputs.packages.${system}.zig.master.bin;
          # zig = inputs.zig2nix.outputs.packages.${system}.zig.default.bin;

          zls = (flakePackage inputs.zls "zls").overrideAttrs (old: {
            nativeBuildInputs = [
              zig
            ];
            buildInputs = [
              zig
            ];
          });
        })
      ];

      pkgs = import inputs.nixpkgs {
        config.allowUnfree = true;
        inherit system;
        inherit overlays;
      };

      fhs = pkgs.buildFHSEnv {
        name = "fhs-shell";
        targetPkgs = p: (env-packages p) ++ (custom-commands p);
        runScript = "${pkgs.zsh}/bin/zsh";
        profile = ''
          export FHS=1
          source ./.venv/bin/activate
          # source .env
        '';
      };
      custom-commands = pkgs: [
        (pkgs.writeShellScriptBin "todo" ''
          #!/usr/bin/env bash
          cd $PROJECT_ROOT
        '')
      ];

      env-packages = pkgs:
        (with pkgs; [
          (python312.withPackages (ps:
            with ps; [
              # for dear imgui bindings
              ply

              # python-lsp-server # (lsp)
            ]))

          pkg-config
          # curl
          fswatch
          glslang
          shaderc
          glfw
          portaudio
          wine64

          vulkan-headers
          vulkan-validation-layers
          libxkbcommon
          wayland
          libGL.dev

          zig

          zls

          # - [nixOS usage | Mach: zig game engine & graphics toolkit](https://machengine.org/about/nixos-usage/)
          xorg.libX11
          vulkan-loader

          renderdoc
          gdb
          linuxKernel.packages.linux_zen.perf
          hotspot
          tracy
          libunwind.dev # optional for tracy
        ])
        ++ []
        ++ (custom-commands pkgs);

      stdenv = pkgs.clangStdenv;
      # stdenv = pkgs.gccStdenv;
    in {
      packages = {};
      overlays = {};

      devShells.default =
        pkgs.mkShell.override {
          inherit stdenv;
        } {
          nativeBuildInputs = (env-packages pkgs) ++ [fhs];
          inputsFrom = [];
          shellHook = ''
            export PROJECT_ROOT="$(pwd)"
            export LD_LIBRARY_PATH=${pkgs.xorg.libX11}/lib:${pkgs.vulkan-loader}/lib:$LD_LIBRARY_PATH

            export CLANGD_FLAGS="--compile-commands-dir=$PROJECT_ROOT --query-driver=$(which $CXX)"
          '';
        };
    });
}
