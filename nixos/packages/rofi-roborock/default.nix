{
  lib,
  rofi,
  libnotify,
  roborock-control,
  writeShellApplication,
}: writeShellApplication {
  name = "rofi_roborock.sh";

  runtimeInputs = [
    libnotify
    roborock-control
    rofi
  ];

  text = ''
    exec ${../../../dotfiles/lib/bin/rofi_roborock.sh} "$@"
  '';

  meta = {
    description = "Rofi menu for dispatching Roborock room cleaning";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [imalison];
    mainProgram = "rofi_roborock.sh";
  };
}
