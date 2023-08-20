lib: config: pathStr: default: configAttrs:
let

  pathToAttrSet = str: value:
  let
    parts = lib.splitString "." str;
  in
  if lib.length parts == 1 then
  { ${lib.head parts} = value; }
  else
  { ${lib.head parts} = pathToAttrSet (lib.concatStringsSep "." (lib.tail parts)) value; };

  optionsSet = pathToAttrSet pathStr {
    enable = lib.mkOption {
      inherit default;
      type = lib.types.bool;
    };
  };

  cfg = lib.attrByPath (lib.splitString "." pathStr) { enable = false; } config;

  # Extract 'imports' from configAttrs, if it exists
  importsAttr = if configAttrs ? imports then configAttrs.imports else [];
  # Remove 'imports' from configAttrs
  configAttrsWithoutImports = lib.attrsets.removeAttrs configAttrs ["imports"];

in
{
  options = optionsSet;
  config = lib.mkIf cfg.enable configAttrsWithoutImports;
  imports = importsAttr;
}
