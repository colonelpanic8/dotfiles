{
  lib,
  rofi,
  libnotify,
  roborock-control,
  writeShellApplication,
}: writeShellApplication {
  name = "menu_roborock.sh";

  runtimeInputs = [
    libnotify
    roborock-control
    rofi
  ];

  text = ''
    exec ${../../../dotfiles/lib/bin/menu_roborock.sh} "$@"
  '';

  meta = {
    description = "Menu for dispatching Roborock room cleaning";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [imalison];
    mainProgram = "menu_roborock.sh";
  };
}
