{
  lib,
  buildPythonPackage,
  fetchFromGitHub,
  hatchling,
  aiohttp,
  requests,
}:
buildPythonPackage rec {
  pname = "pykefcontrol";
  version = "0.9.3-pr17";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "colonelpanic8";
    repo = "pykefcontrol";
    rev = "1eb4418ce39d9d368d4f195702215a7854790633";
    hash = "sha256-aqa0LP0cjT2F/vtoxKvCFPeBAWwReaF47QLT4KsOhjA=";
  };

  build-system = [
    hatchling
  ];

  dependencies = [
    aiohttp
    requests
  ];

  pythonImportsCheck = ["pykefcontrol"];

  meta = {
    description = "Python library for controlling KEF LS50WII, LSX II, and LS60 speakers";
    homepage = "https://github.com/N0ciple/pykefcontrol";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [imalison];
  };
}
