let
  reflex-platform = import ./reflex-platform {};
  pkgs = reflex-platform.nixpkgs;
  app = (import ./. {}).ghcjs.edit-log-editor;

  html =
    let
      index =
        pkgs.writeText "index.html" ''
          <!DOCTYPE html>
          <html>
            <head>
              <meta charset="UTF-8">
              <script language="javascript" src="all.min.js"></script>
            </head>
            <body>
            </body>
          </html>
        '';
    in
      pkgs.stdenv.mkDerivation {
        name = "html";
        unpackPhase = "true";
        buildPhase = "true";
        installPhase = ''
          mkdir -p $out
          cp ${index} $out/index.html
        '';
      };

  js =
    pkgs.stdenv.mkDerivation {
      name = "js";
      src = app;
      buildInputs = [
        pkgs.closurecompiler
      ];
      buildPhase = ''
        cd bin/editor.jsexe
        closure-compiler \
          --externs "${reflex-platform.ghcjsExternsJs}" \
          -O ADVANCED \
          -W QUIET \
          --jscomp_warning=checkVars \
          --create_source_map="all.js.map" \
          --source_map_format=V3 \
          --js_output_file="all.min.js" \
          all.js
        ls -lh all.min.js
        echo "//# sourceMappingURL=all.js.map" >> all.min.js
      '';
      installPhase = ''
        mkdir -p $out
        cp all.js all.min.js all.js.map $out
      '';
    };
in
  pkgs.symlinkJoin {
    name = "minified";
    paths = [ html js ];
  }
