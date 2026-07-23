{
  lib,
  python3,
  writeShellApplication,
}: let
  python = python3.withPackages (ps:
    [ps.python-roborock]
    ++ ps.python-roborock.optional-dependencies.cli);
in
  writeShellApplication {
    name = "roborock-control";

    runtimeInputs = [python];

    text = ''
      export ROBOROCK_CONTROL_RUNNER=direct
      exec python ${../../../dotfiles/lib/bin/roborock-control} "$@"
    '';

    meta = {
      description = "Command-line controller for Roborock vacuums";
      license = lib.licenses.mit;
      maintainers = with lib.maintainers; [imalison];
      mainProgram = "roborock-control";
    };
  }
