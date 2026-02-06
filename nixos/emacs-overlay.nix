final: prev:
{
  # Keep pkgs.emacs as a real Emacs package (used by other derivations / emacsPackages),
  # but provide an "auto" wrapper for interactive use that selects the right GUI backend.
  emacs = prev."emacs30-pgtk".override {
    withNativeCompilation = true;
    withTreeSitter = true;
  };

  emacs-wayland = final.emacs;

  emacs-x11 = prev.emacs30.override {
    withNativeCompilation = true;
    withTreeSitter = true;
  };

  # Runtime launcher that chooses:
  # - Wayland session: pgtk build with GDK_BACKEND=wayland (avoids XWayland)
  # - X11 session: X11 build with GDK_BACKEND=x11
  # - No display: falls back to -nw
  #
  # Override with EMACS_AUTO_BACKEND=wayland|x11|tty.
  emacs-auto = final.stdenvNoCC.mkDerivation {
    pname = "emacs-auto";
    version = final.emacs.version or "unknown";
    dontUnpack = true;
    nativeBuildInputs = [ final.makeWrapper ];

    installPhase =
      let
        emacsWayland = final.emacs-wayland;
        emacsX11 = final.emacs-x11;
      in
      ''
        mkdir -p "$out/bin" "$out/share/applications" "$out/share/icons" "$out/share/pixmaps"

        # Ensure desktop integrations (icons) exist even though we don't install the raw emacs packages.
        if [ -d "${emacsWayland}/share/icons" ]; then
          ln -s "${emacsWayland}/share/icons/hicolor" "$out/share/icons/hicolor"
        fi
        if [ -d "${emacsWayland}/share/pixmaps" ]; then
          ln -s "${emacsWayland}/share/pixmaps/"* "$out/share/pixmaps/" || true
        fi

        # Convenience explicit launchers.
        makeWrapper ${emacsWayland}/bin/emacs "$out/bin/emacs-wayland" \
          --set GDK_BACKEND wayland
        makeWrapper ${emacsX11}/bin/emacs "$out/bin/emacs-x11" \
          --set GDK_BACKEND x11

        # Main launcher.
        cat > "$out/bin/emacs" <<'EOF_EMACS_WRAPPER'
#!${final.runtimeShell}
set -eu

backend="''${EMACS_AUTO_BACKEND:-}"
tty=0
for a in "$@"; do
  case "$a" in
    -nw|--nw|--tty|--terminal|--no-window-system) tty=1 ;;
  esac
done

if [ "$backend" = "wayland" ] || [ "$backend" = "pgtk" ]; then
  exec "@out@/bin/emacs-wayland" "$@"
fi
if [ "$backend" = "x11" ]; then
  exec "@out@/bin/emacs-x11" "$@"
fi
if [ "$backend" = "tty" ]; then
  exec "@emacsX11@/bin/emacs" -nw "$@"
fi

if [ "$tty" -eq 1 ]; then
  # Respect the user's explicit -nw, but still run a consistent binary.
  exec "@emacsX11@/bin/emacs" "$@"
fi

# Prefer Wayland if it looks like a Wayland session.
if [ -n "''${WAYLAND_DISPLAY:-}" ] || [ "''${XDG_SESSION_TYPE:-}" = "wayland" ] || [ -n "''${HYPRLAND_INSTANCE_SIGNATURE:-}" ]; then
  exec "@out@/bin/emacs-wayland" "$@"
fi

# Otherwise, if X is available, use the X11 build.
if [ -n "''${DISPLAY:-}" ] || [ "''${XDG_SESSION_TYPE:-}" = "x11" ]; then
  exec "@out@/bin/emacs-x11" "$@"
fi

# Headless fallback.
exec "@emacsX11@/bin/emacs" -nw "$@"
EOF_EMACS_WRAPPER

        substituteInPlace "$out/bin/emacs" \
          --subst-var out \
          --replace-fail "@emacsX11@" "${emacsX11}"
        chmod +x "$out/bin/emacs"

        # emacsclient is compatible across builds as long as versions match; use one.
        ln -s ${emacsX11}/bin/emacsclient "$out/bin/emacsclient"

        cat > "$out/share/applications/emacs.desktop" <<'EOF'
[Desktop Entry]
Name=Emacs
GenericName=Text Editor
Comment=Edit text
Exec=emacs %F
TryExec=emacs
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;
MimeType=text/plain;
StartupWMClass=Emacs
EOF

        cat > "$out/share/applications/emacsclient.desktop" <<'EOF'
[Desktop Entry]
Name=Emacs (Client)
GenericName=Text Editor
Comment=Edit text using a running Emacs server
Exec=emacsclient -c -a emacs %F
TryExec=emacsclient
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;
MimeType=text/plain;
StartupWMClass=Emacs
EOF
      '';
  };
}
