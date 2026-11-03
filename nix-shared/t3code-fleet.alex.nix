# Alex's T3 Code fleet on his own tailnet, published as a separate enrollment
# bundle (gen-gh-pages/t3code/mint-enrollment.sh --fleet alex). Members keep
# the environment IDs their servers already report, so phones that paired
# before enrollment see the same environments.
#
# alexanders-macbook-air joins once its server is reachable over the tailnet;
# it currently listens only on loopback.
[
  {
    environmentId = "60c44129-b35f-42bf-bb1f-346511391eee";
    label = "msi-elliott";
    httpBaseUrl = "http://msi-elliott.tail40f15e.ts.net:3773/";
    wsBaseUrl = "ws://msi-elliott.tail40f15e.ts.net:3773/";
  }
]
