# Alex's T3 Code fleet on his own tailnet, published as a separate enrollment
# bundle (gen-gh-pages/t3code/mint-enrollment.sh --fleet alex). Members keep
# the environment IDs their servers already report, so phones that paired
# before enrollment see the same environments.
[
  {
    environmentId = "60c44129-b35f-42bf-bb1f-346511391eee";
    label = "msi-elliott";
    httpBaseUrl = "http://msi-elliott.tail40f15e.ts.net:3773/";
    wsBaseUrl = "ws://msi-elliott.tail40f15e.ts.net:3773/";
  }
  # Reached through a Tailscale Serve forward configured by hand on the Mac
  # (tailnet :3774 -> 127.0.0.1:3774), not by services.t3code.tailscaleServe.
  {
    environmentId = "fleet:alexanders-macbook-air";
    label = "alexanders-macbook-air";
    httpBaseUrl = "http://alexanders-macbook-air.tail40f15e.ts.net:3774/";
    wsBaseUrl = "ws://alexanders-macbook-air.tail40f15e.ts.net:3774/";
  }
  {
    environmentId = "937c3197-50ed-4379-b382-5110cc2d7a73";
    label = "malisonlaptop";
    httpBaseUrl = "http://malisonlaptop.tail40f15e.ts.net:3773/";
    wsBaseUrl = "ws://malisonlaptop.tail40f15e.ts.net:3773/";
    # Exported from Windows and retained when the SSH hosts are refreshed.
    reuseEnrollment = true;
  }
]
