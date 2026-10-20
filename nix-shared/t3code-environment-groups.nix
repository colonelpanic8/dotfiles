builtins.mapAttrs (_: member: {
  inherit (member) label;
  environmentIds = ["fleet:${member.hostName}"];
  defaultVisible = false;
}) (import ./t3code-family.nix)
