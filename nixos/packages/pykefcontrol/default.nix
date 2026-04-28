{
  lib,
  buildPythonPackage,
  fetchPypi,
  hatchling,
  aiohttp,
  requests,
}:

buildPythonPackage rec {
  pname = "pykefcontrol";
  version = "0.9.2";
  pyproject = true;

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-3kGhN+E7driiE6ePyF0EZOEnUhTm07sxHCKdzrn/MxM=";
  };

  build-system = [
    hatchling
  ];

  dependencies = [
    aiohttp
    requests
  ];

  pythonImportsCheck = [ "pykefcontrol" ];

  meta = {
    description = "Python library for controlling KEF LS50WII, LSX II, and LS60 speakers";
    homepage = "https://github.com/N0ciple/pykefcontrol";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ imalison ];
  };
}
