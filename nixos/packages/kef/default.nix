{
  lib,
  python3,
  python3Packages,
  writeShellApplication,
}:

let
  pykefcontrol = python3Packages.callPackage ../pykefcontrol {};
  python = python3.withPackages (ps: [
    pykefcontrol
    ps.zeroconf
  ]);
in
writeShellApplication {
  name = "kef";

  runtimeInputs = [ python ];

  text = ''
    exec python ${./kef.py} "$@"
  '';

  meta = {
    description = "Command-line controller for KEF W2 speakers";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ imalison ];
    mainProgram = "kef";
  };
}
