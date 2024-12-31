final: prev: {
  nvidia-container-toolkit = final.stdenv.mkDerivation {
    pname = "nvidia-container-toolkit-debug";
    version = prev.nvidia-container-toolkit.version;

    # This is key: multiple outputs, so you can reference them later as:
    #   $out   -> for main
    #   $tools -> for the 'tools' output in your new derivation
    outputs = [ "out" "tools" ];

    # No source required for a pure wrap
    src = null;
    dontUnpack = true;
    dontPatchShell = true;
    nativeBuildInputs = [ final.installShellFiles ] ++ final.lib.optionals final.stdenv.hostPlatform.isLinux [ final.makeWrapper ];

    # Create wrapper scripts for each set of binaries
    buildPhase = ''
      # --- Wrap binaries from the main output of the original toolkit ---
      mkdir -p wrapper-out
      if [ -d "${prev.nvidia-container-toolkit}/bin" ]; then
      for exe in $(ls ${prev.nvidia-container-toolkit}/bin); do
      cat > wrapper-out/$exe <<EOF
      #!${final.bash}/bin/bash

      if [ "\$(id -u)" -eq 0 ]; then
      mkdir -p /var/log/nvidia-container-toolkit
      chown root:users /var/log/nvidia-container-toolkit
      chmod 2777 /var/log/nvidia-container-toolkit
      fi

      # --- STARTUP LOG ---
      echo "\$(date '+%Y-%m-%d %H:%M:%S') - $exe started" \
      >> /var/log/nvidia-container-toolkit/$exe.startup.log 2>/dev/null || true

      # --- COMMAND INVOCATION LOG ---
      echo "\$(date '+%Y-%m-%d %H:%M:%S') - Executing $exe with args: \$@" \
      >> /var/log/nvidia-container-toolkit/$exe.log 2>/dev/null || true

      # --- Run the real tool, piping stdout+stderr to tee ---
      ${prev.nvidia-container-toolkit}/bin/$exe "\$@" > \
      >(tee -a /var/log/nvidia-container-toolkit/$exe.stdout.log) \
      2> >(tee -a /var/log/nvidia-container-toolkit/$exe.stderr.log >&2)

      exit_code=\$?

      # --- FINISHED LOG ---
      echo "\$(date '+%Y-%m-%d %H:%M:%S') - Finished $exe with exit code: \$exit_code" >> \
      /var/log/nvidia-container-toolkit/$exe.log 2>/dev/null || true
      echo "\$(date '+%Y-%m-%d %H:%M:%S') - $exe finished" \
      >> /var/log/nvidia-container-toolkit/$exe.startup.log 2>/dev/null || true
      EOF
      chmod +x wrapper-out/$exe
      done
      fi

      # --- Wrap binaries from the 'tools' output of the original toolkit ---
      mkdir -p wrapper-tools
      if [ -d "${prev.nvidia-container-toolkit.tools}/bin" ]; then
      for exe in $(ls ${prev.nvidia-container-toolkit.tools}/bin); do
      cat > wrapper-tools/$exe <<EOF
      #!${final.bash}/bin/bash

      if [ "\$(id -u)" -eq 0 ]; then
      mkdir -p /var/log/nvidia-container-toolkit
      chown root:users /var/log/nvidia-container-toolkit
      chmod 2777 /var/log/nvidia-container-toolkit
      fi

      # --- STARTUP LOG ---
      echo "\$(date '+%Y-%m-%d %H:%M:%S') - $exe started" \
      >> /var/log/nvidia-container-toolkit/$exe.startup.log 2>/dev/null || true

      # --- COMMAND INVOCATION LOG ---
      echo "\$(date '+%Y-%m-%d %H:%M:%S') - Executing $exe with args: \$@" \
      >> /var/log/nvidia-container-toolkit/$exe.log 2>/dev/null || true

      debug_flag="$([[ $exe = "nvidia-cdi-hook" ]] && echo "--debug")"

      if [[ $exe = "nvidia-cdi-hook" ]]; then
         ldd ${prev.nvidia-container-toolkit.tools}/bin/$exe
      fi

      # --- Run the real tool, piping stdout+stderr to tee ---
      ${prev.nvidia-container-toolkit.tools}/bin/$exe $debug_flag "\$@" > \
      >(tee -a /var/log/nvidia-container-toolkit/$exe.stdout.log) \
      2> >(tee -a /var/log/nvidia-container-toolkit/$exe.stderr.log >&2) || true

      exit_code=\$?

      # --- FINISHED LOG ---
      echo "\$(date '+%Y-%m-%d %H:%M:%S') - Finished $exe with exit code: \$exit_code" >> \
      /var/log/nvidia-container-toolkit/$exe.log 2>/dev/null || true
      echo "\$(date '+%Y-%m-%d %H:%M:%S') - $exe finished" \
      >> /var/log/nvidia-container-toolkit/$exe.startup.log 2>/dev/null || true
      EOF
      chmod +x wrapper-tools/$exe
      done
      fi
    '';

    installPhase = ''
      # For the main output of our wrapper derivation
      mkdir -p $out/bin

      if [ -d wrapper-out ]; then
      cp wrapper-out/* $out/bin/
      fi

      # For the 'tools' output of our wrapper derivation
      mkdir -p $tools/bin

      if [ -d wrapper-tools ]; then
      cp wrapper-tools/* $tools/bin/
      fi
    '';

    # If you need runtime dependencies, inherit them from the original package
    propagatedBuildInputs = prev.nvidia-container-toolkit.propagatedBuildInputs or [];
    propagatedUserEnvPkgs = prev.nvidia-container-toolkit.propagatedUserEnvPkgs or [];

    meta = {
      description = "Debug-wrapped NVIDIA Container Toolkit with separate tools output.";
      homepage = prev.nvidia-container-toolkit.meta.homepage;
      license = prev.nvidia-container-toolkit.meta.license;
      maintainers = prev.nvidia-container-toolkit.meta.maintainers;
      platforms = prev.nvidia-container-toolkit.meta.platforms;
    };
  };
}
