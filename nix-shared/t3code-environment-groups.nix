builtins.mapAttrs (_: member: {
  inherit (member) label;
  environmentIds = builtins.attrValues (builtins.mapAttrs (hostName: host: host.environmentId or "fleet:${hostName}") member.hosts);
  defaultVisible = false;
}) (import ./t3code-family.nix)
