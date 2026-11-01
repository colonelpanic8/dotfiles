builtins.mapAttrs (_: member: {
  inherit (member) label;
  environmentIds = map (hostName: "fleet:${hostName}") (builtins.attrNames member.hosts);
  defaultVisible = false;
}) (import ./t3code-family.nix)
