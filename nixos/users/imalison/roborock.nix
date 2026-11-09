# roborock-control settings and the sealed Roborock session, so the vacuum
# menu works on every full host. The session holds the device list as well as
# the login; reseal it after a re-login or a device change (from nixos/secrets):
#   roborock-control export-session \
#     | age -R <(nix eval --json -f secrets.nix '"roborock-session.json.age".publicKeys' | jq -r '.[]') \
#       -o roborock-session.json.age
{
  config,
  lib,
  ...
}: {
  config = lib.mkIf config.myUsers.imalison.has.full {
    home-manager.users.imalison = {config, ...}: {
      age.secrets.roborock-session.file = ../../secrets/roborock-session.json.age;

      xdg.configFile."roborock-control/config.json".text = builtins.toJSON {
        device_id = "1rnaJe3CL8ZIiEQsFSkJL7";
        email = "IvanMalison@gmail.com";
        session_file = config.age.secrets.roborock-session.path;
        segment_aliases = {
          bathroom = 3;
          bedroom = 1;
          bedroom-closet = 6;
          closet = 6;
          dining = 4;
          entryway = 2;
          entryway-hallway = 2;
          hallway = 2;
          kitchen = 4;
          kitchen-dining = 4;
          living = 7;
          living-room = 7;
          office = 5;
          office-closet = 5;
        };
      };
    };
  };
}
