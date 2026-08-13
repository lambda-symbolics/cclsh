{
  description = "A system shell running inside Clozure CL";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    {
      self,
      nixpkgs,
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      lib = pkgs.lib;
      buildCommit = self.shortRev or self.dirtyShortRev or "unknown";

      dependencyLines = lib.splitString "\n" (builtins.readFile ./dependencies.lock);
      dependencyRevision =
        name:
        let
          prefix = "${name}=";
          matches = builtins.filter (line: lib.hasPrefix prefix line) dependencyLines;
        in
        if builtins.length matches != 1 then
          throw "dependencies.lock must contain exactly one ${prefix} entry"
        else
          let
            revision = lib.removePrefix prefix (builtins.head matches);
          in
          if builtins.match "[0-9a-f]{40}" revision == null then
            throw "dependencies.lock ${prefix} entry must be a full Git revision"
          else
            revision;

      cclBaseRev = dependencyRevision "ccl-base";
      cclXstateRev = dependencyRevision "ccl-xstate";
      cclRev = dependencyRevision "ccl";
      clinediRev = dependencyRevision "clinedi";
      structlispRev = dependencyRevision "structlisp";
      clColoristRev = dependencyRevision "cl-colorist";
      colorlispRev = dependencyRevision "colorlisp";
      colordiffRev = dependencyRevision "colordiff";

      clinediSource = pkgs.fetchgit {
        name = "clinedi-${builtins.substring 0 7 clinediRev}";
        url = "https://github.com/lambda-symbolics/clinedi.git";
        rev = clinediRev;
        hash = "sha256-fHXs6jbP8pUAVFd6tczM22GD++3/jXmf/70YDYzWqL0=";
        leaveDotGit = true;
      };

      structlispSource = pkgs.fetchgit {
        name = "structlisp-${builtins.substring 0 7 structlispRev}";
        url = "https://github.com/lambda-symbolics/structlisp.git";
        rev = structlispRev;
        hash = "sha256-hz+vHx7WNA1QLpS9bUzyMufeBxoce6qjjWliKp4WKtQ=";
        leaveDotGit = true;
      };

      clColoristSource = pkgs.fetchgit {
        name = "cl-colorist-${builtins.substring 0 7 clColoristRev}";
        url = "https://github.com/lambda-symbolics/cl-colorist.git";
        rev = clColoristRev;
        hash = "sha256-xo+U65B57TCQdJDsokdGtiG0jW5oJiq/cQW6rTlgXic=";
        leaveDotGit = true;
      };

      colorlispSource = pkgs.fetchgit {
        name = "colorlisp-${builtins.substring 0 7 colorlispRev}";
        url = "https://github.com/lambda-symbolics/colorlisp.git";
        rev = colorlispRev;
        hash = "sha256-MtvbDJlQkpIG947+XWfTcmtP6Sq5PALPxMQaubkyz+E=";
        leaveDotGit = true;
      };

      colordiffSource = pkgs.fetchgit {
        name = "colordiff-${builtins.substring 0 7 colordiffRev}";
        url = "https://github.com/lambda-symbolics/colordiff.git";
        rev = colordiffRev;
        hash = "sha256-B3DsZb8WsRqKPn4Al8SiQ+HhBSJ95nZukmeB2nXU/eI=";
        leaveDotGit = true;
      };

      # The CCL release archive supplies bootstrap binaries that are absent
      # from Git. Keep that archive as the base, but fetch every divergent
      # source change from its immutable commit in the maintained fork.
      cclXstatePatch = pkgs.fetchurl {
        url = "https://github.com/luciusmagn/ccl/commit/${cclXstateRev}.diff";
        hash = "sha256-YSitne9gEbXXQTZc0Pndc9YUefzIcEWdaQ2MooRQ0Vk=";
      };

      cclArgvPatch = pkgs.fetchurl {
        url = "https://github.com/luciusmagn/ccl/commit/${cclRev}.diff";
        hash = "sha256-q41Ywy0qIllOg/oaCO/ts3A1KfKMnW2Rw3KAv5DwVGg=";
      };

      quicklispTar = pkgs.fetchurl {
        url = "https://beta.quicklisp.org/client/2021-02-13/quicklisp.tar";
        hash = "sha256-qKPIyRtR3RhRdautTXw5meu04lIL5abO4hJwNaxsh74=";
      };

      quicklispSetup = pkgs.fetchurl {
        url = "https://beta.quicklisp.org/client/2021-02-11/setup.lisp";
        hash = "sha256-VJ/j5+DyZp2u3phDfJnNYOAsC4U209E1yaqdNG7ZUbY=";
      };

      quicklispAsdf = pkgs.fetchurl {
        url = "https://beta.quicklisp.org/asdf/3.2.1/asdf.lisp";
        hash = "sha256-UZEvP3wsYsIE9RXZc0bVYBFSg5m78KdqEjMYND69i/A=";
      };

      quicklispClientInfo = pkgs.fetchurl {
        url = "https://beta.quicklisp.org/client/2021-02-13/client-info.sexp";
        hash = "sha256-tPUVxe0gTZ+k6oY35gwLEldWCfRQ8mkZ2/uDXuKZM+A=";
      };

      quicklispDistInfo = pkgs.fetchurl {
        url = "https://beta.quicklisp.org/dist/quicklisp/2026-01-01/distinfo.txt";
        hash = "sha256-/ENut1gsi2ny7IfXe9qkCHUCMo3sCfIcV214+si/XhU=";
      };

      quicklispReleases = pkgs.fetchurl {
        url = "https://beta.quicklisp.org/dist/quicklisp/2026-01-01/releases.txt";
        hash = "sha256-nK1ZOY52ADLeAFPK/P3PFutDfK8aINZWaKjjtyZIXaQ=";
      };

      quicklispSystems = pkgs.fetchurl {
        url = "https://beta.quicklisp.org/dist/quicklisp/2026-01-01/systems.txt";
        hash = "sha256-y4MJMhhAiHtUL6hT3E/pX5pOotfKAoxSHTK2fSvfUcc=";
      };

      quicklispProjectArchives = [
        (pkgs.fetchurl {
          url = "https://beta.quicklisp.org/archive/alexandria/2024-10-12/alexandria-20241012-git.tgz";
          hash = "md5-JEO5hs0nYPvZM4YxHw3OIQ==";
        })
        (pkgs.fetchurl {
          url = "https://beta.quicklisp.org/archive/babel/2026-01-01/babel-20260101-git.tgz";
          hash = "md5-YJwiEo3h/ZML21J9j5hSlw==";
        })
        (pkgs.fetchurl {
          url = "https://beta.quicklisp.org/archive/cffi/2026-01-01/cffi-20260101-git.tgz";
          hash = "md5-RubiAmn0u6CgzHk6+Sc6Ww==";
        })
        (pkgs.fetchurl {
          url = "https://beta.quicklisp.org/archive/cl-ppcre/2025-06-22/cl-ppcre-20250622-git.tgz";
          hash = "md5-5poTLfO75dJRc3g0wP267Q==";
        })
        (pkgs.fetchurl {
          url = "https://beta.quicklisp.org/archive/trivial-features/2025-06-22/trivial-features-20250622-git.tgz";
          hash = "md5-m6Er6bTs1leX6t9BRzmJ8A==";
        })
        (pkgs.fetchurl {
          url = "https://beta.quicklisp.org/archive/trivial-gray-streams/2024-10-12/trivial-gray-streams-20241012-git.tgz";
          hash = "md5-rw1kuvtgMmWRUqWvqzi/3w==";
        })
      ];

      quicklispTemplate =
        pkgs.runCommand "cclsh-quicklisp-2026-01-01"
          {
            nativeBuildInputs = [ pkgs.gnutar ];
          }
          ''
            mkdir -p "$out"
            tar -xf ${quicklispTar} -C "$out"
            cp ${quicklispSetup} "$out/setup.lisp"
            cp ${quicklispAsdf} "$out/asdf.lisp"
            cp ${quicklispClientInfo} "$out/client-info.sexp"
            mkdir -p "$out/dists/quicklisp" "$out/local-projects"
            cp ${quicklispDistInfo} "$out/dists/quicklisp/distinfo.txt"
            cp ${quicklispReleases} "$out/dists/quicklisp/releases.txt"
            cp ${quicklispSystems} "$out/dists/quicklisp/systems.txt"
            touch "$out/dists/quicklisp/enabled.txt"
          '';

      patchedCcl = pkgs.ccl.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [
          cclXstatePatch
          cclArgvPatch
        ];
      });

      cclPatchProvenance =
        pkgs.runCommand "cclsh-ccl-patch-provenance"
          {
            nativeBuildInputs = [
              pkgs.gnutar
              pkgs.patch
            ];
          }
          ''
            mkdir local fork
            tar -xf ${pkgs.ccl.src} -C local --strip-components=1
            tar -xf ${pkgs.ccl.src} -C fork --strip-components=1

            patch -d local -p1 < ${./patches/ccl-linux-xstate.patch}
            patch -d local -p1 < ${./patches/ccl-cclsh-argv.patch}
            patch -d fork -p1 < ${cclXstatePatch}
            patch -d fork -p1 < ${cclArgvPatch}

            cmp local/lisp-kernel/x86-exceptions.c \
              fork/lisp-kernel/x86-exceptions.c
            cmp local/lisp-kernel/pmcl-kernel.c \
              fork/lisp-kernel/pmcl-kernel.c
            printf '%s\n' \
              "base ${cclBaseRev}" \
              "xstate ${cclXstateRev}" \
              "runtime ${cclRev}" \
              >$out
          '';

      cclsh = pkgs.stdenv.mkDerivation {
        pname = "cclsh";
        version = "1.4.0";
        src = self;

        nativeBuildInputs = [
          patchedCcl
          pkgs.coreutils
          pkgs.findutils
          pkgs.git
          pkgs.gnugrep
          pkgs.gnumake
          pkgs.util-linux
        ];

        postPatch = ''
          patchShebangs scripts tests
          substituteInPlace scripts/build scripts/verify-argument-boundary \
            --replace-fail /usr/bin/timeout ${pkgs.coreutils}/bin/timeout
          substituteInPlace scripts/verify-argument-boundary \
            --replace-fail /usr/bin/id ${pkgs.coreutils}/bin/id
          substituteInPlace source/pipeline.lisp \
            --replace-fail /usr/bin/cat ${pkgs.coreutils}/bin/cat \
            --replace-fail /bin/cat ${pkgs.coreutils}/bin/cat \
            --replace-fail /usr/bin/sh ${pkgs.runtimeShell} \
            --replace-fail /bin/sh ${pkgs.runtimeShell}
        '';

        buildPhase = ''
          runHook preBuild

          export HOME="$TMPDIR/home"
          mkdir -p "$HOME" "$out/share/cclsh"

          cp -R ${clinediSource} "$out/share/cclsh/clinedi"
          chmod -R u+w "$out/share/cclsh/clinedi"
          git -C "$out/share/cclsh/clinedi" reset --hard ${clinediRev}
          git -C "$out/share/cclsh/clinedi" clean -fdx

          cp -R ${structlispSource} "$out/share/cclsh/structlisp"
          chmod -R u+w "$out/share/cclsh/structlisp"
          git -C "$out/share/cclsh/structlisp" reset --hard ${structlispRev}
          git -C "$out/share/cclsh/structlisp" clean -fdx

          cp -R ${clColoristSource} "$out/share/cclsh/cl-colorist"
          chmod -R u+w "$out/share/cclsh/cl-colorist"
          git -C "$out/share/cclsh/cl-colorist" reset --hard ${clColoristRev}
          git -C "$out/share/cclsh/cl-colorist" clean -fdx

          cp -R ${colorlispSource} "$out/share/cclsh/colorlisp"
          chmod -R u+w "$out/share/cclsh/colorlisp"
          git -C "$out/share/cclsh/colorlisp" reset --hard ${colorlispRev}
          git -C "$out/share/cclsh/colorlisp" clean -fdx

          cp -R ${colordiffSource} "$out/share/cclsh/colordiff"
          chmod -R u+w "$out/share/cclsh/colordiff"
          git -C "$out/share/cclsh/colordiff" reset --hard ${colordiffRev}
          git -C "$out/share/cclsh/colordiff" clean -fdx

          for archive in ${lib.concatStringsSep " " (map toString quicklispProjectArchives)}; do
            tar -xf "$archive" -C "$out/share/cclsh"
          done

          cp -R ${quicklispTemplate} "$TMPDIR/quicklisp"
          chmod -R u+w "$TMPDIR/quicklisp"
          mkdir -p "$TMPDIR/quicklisp/local-init"
          printf '%s\n' \
            '(in-package #:quicklisp-client)' \
            "(pushnew #P\"$out/share/cclsh/\" *local-project-directories* :test #'equal)" \
            >"$TMPDIR/quicklisp/local-init/cclsh.lisp"

          export CCLSH_CCL=${patchedCcl}/share/ccl-installation/lx86cl64
          export CCLSH_CCL_IMAGE=${patchedCcl}/share/ccl-installation/lx86cl64.image
          export CCLSH_BUILD_COMMIT=${lib.escapeShellArg buildCommit}
          export CCLSH_CLINEDI_SOURCE="$out/share/cclsh/clinedi"
          export CCLSH_STRUCTLISP_SOURCE="$out/share/cclsh/structlisp"
          export CCLSH_CL_COLORIST_SOURCE="$out/share/cclsh/cl-colorist"
          export CCLSH_COLORLISP_SOURCE="$out/share/cclsh/colorlisp"
          export CCLSH_COLORDIFF_SOURCE="$out/share/cclsh/colordiff"
          export CCLSH_PROJECTS_SOURCE="$out/share/cclsh"
          export CCLSH_QUICKLISP_SETUP="$TMPDIR/quicklisp/setup.lisp"
          export CCLSH_PACKAGED_QUICKLISP_TEMPLATE="$out/share/cclsh/quicklisp"
          export ASDF_OUTPUT_TRANSLATIONS="$PWD/:$TMPDIR/cclsh-fasl/"
          scripts/build
          scripts/build-fast

          runHook postBuild
        '';

        installPhase = ''
          runHook preInstall

          mkdir -p "$out/bin" "$out/share/cclsh/quicklisp"
          cp -L cclsh "$out/bin/cclsh"
          cp -L cclsh.image "$out/bin/cclsh.image"
          cp -L cclsh-fast "$out/bin/cclsh-fast"
          cp -R ${quicklispTemplate}/. "$out/share/cclsh/quicklisp/"
          rm -rf "$out/share/cclsh/clinedi/.git"
          rm -rf "$out/share/cclsh/structlisp/.git"
          rm -rf "$out/share/cclsh/cl-colorist/.git"
          rm -rf "$out/share/cclsh/colorlisp/.git"
          rm -rf "$out/share/cclsh/colordiff/.git"

          runHook postInstall
        '';

        dontStrip = true;

        meta = {
          description = "System shell running inside Clozure CL";
          homepage = "https://github.com/lambda-symbolics/cclsh";
          license = lib.licenses.isc;
          mainProgram = "cclsh";
          platforms = [ "x86_64-linux" ];
        };
      };
    in
    {
      packages.${system} = {
        default = cclsh;
        cclsh = cclsh;
        patched-ccl = patchedCcl;
      };

      apps.${system}.default = {
        type = "app";
        program = "${lib.getExe cclsh}";
      };

      checks.${system} = {
        ccl-patch-provenance = cclPatchProvenance;
        installed = pkgs.runCommand "cclsh-installed-check" { } ''
          export HOME="$TMPDIR/home"
          export XDG_CACHE_HOME="$TMPDIR/cache"
          export XDG_CONFIG_HOME="$TMPDIR/config"
          export XDG_DATA_HOME="$TMPDIR/data"
          export XDG_RUNTIME_DIR="$TMPDIR/runtime"
          mkdir -p "$HOME" "$XDG_RUNTIME_DIR"
          chmod 700 "$XDG_RUNTIME_DIR"

          ${lib.getExe cclsh} --version >version
          grep -F "cclsh 1.4.0" version
          grep -F "clinedi ${clinediRev}" version
          grep -F "cl-colorist ${clColoristRev}" version
          ${lib.getExe cclsh} -c 'exit 0'
          ${cclsh}/bin/cclsh-fast --version >fast-version
          cmp version fast-version
          ${cclsh}/bin/cclsh-fast daemon start
          ${cclsh}/bin/cclsh-fast daemon status | grep -F 'ready=1 active=0'
          ${cclsh}/bin/cclsh-fast daemon stop
          ${lib.getExe cclsh} -c \
            '(progn (unless (and (probe-file (merge-pathnames "setup.lisp" ql-setup:*quicklisp-home*)) (ql-dist:find-dist "quicklisp") (uiop:subpathp asdf:*user-cache* (uiop:ensure-directory-pathname (pathname (cclsh:getenv "XDG_CACHE_HOME")))) (member #P"${cclsh}/share/cclsh/" ql::*local-project-directories* :test (function equal))) (error "packaged Quicklisp is not writable and initialized")) (values))'
          test -f "$XDG_DATA_HOME/cclsh/quicklisp/setup.lisp"

          cp -R "$XDG_DATA_HOME/cclsh/quicklisp" "$HOME/quicklisp"
          mkdir -p "$HOME/quicklisp/local-init" "$TMPDIR/personal-projects"
          printf '%s\n' \
            '(in-package #:quicklisp-client)' \
            "(pushnew #P\"$TMPDIR/personal-projects/\" *local-project-directories* :test #'equal)" \
            >"$HOME/quicklisp/local-init/cclsh-check.lisp"

          ${lib.getExe cclsh} -c \
            "(when (member #P\"$TMPDIR/personal-projects/\" ql::*local-project-directories* :test (function equal)) (error \"plain command loaded Quicklisp local-init\"))"
          ${lib.getExe cclsh} -lc \
            "(unless (member #P\"$TMPDIR/personal-projects/\" ql::*local-project-directories* :test (function equal)) (error \"configured command skipped Quicklisp local-init\"))"

          export CCLSH_QUICKLISP_HOME="$TMPDIR/incomplete"
          mkdir -p "$CCLSH_QUICKLISP_HOME"
          ${lib.getExe cclsh} -c \
            '(when (quicklisp-setup) (error "incomplete Quicklisp override was accepted"))' \
            2>incomplete-error
          grep -F "exists without setup.lisp" incomplete-error
          test ! -e "$CCLSH_QUICKLISP_HOME/setup.lisp"

          export CCLSH_QUICKLISP_HOME=/dev/null/quicklisp
          ${lib.getExe cclsh} -c \
            '(when (quicklisp-setup) (error "unwritable Quicklisp override was accepted"))' \
            2>unwritable-error
          grep -F "packaged Quicklisp unavailable" unwritable-error

          set +e
          argument_output=$(${lib.getExe cclsh} -c --no-avx 2>&1)
          argument_status=$?
          set -e
          test "$argument_status" -eq 127
          printf '%s\n' "$argument_output" | grep -F -- --no-avx

          touch "$out"
        '';
      };

      devShells.${system}.default = pkgs.mkShell {
        packages = [
          patchedCcl
          pkgs.git
          pkgs.gnumake
          pkgs.util-linux
        ];
      };

      formatter.${system} = pkgs.nixfmt;
    };
}
