{
  config,
  inputs,
  lib,
  pkgs,
  makeEnable,
  ...
}: let
  system = pkgs.stdenv.hostPlatform.system;
  bridgePackage = inputs.chrome-favicon-dbus.packages.${system}.default;
  extensionSource = "${inputs.chrome-favicon-dbus}/extension";
  extensionKey = pkgs.writeText "chrome-favicon-dbus-extension.pem" ''
    -----BEGIN PRIVATE KEY-----
    MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDF0IL1wJ4JyaYc
    c3vcixly8UEYUxo6TEwpPFIj/9bTGsIVCzVMxeirqn3979o9DUxZGgn6Ktm6LTnb
    h1MK5FZxO3PDdiiotmejbWd79c7PWvGU/Vlf3WuSeej1ksEpNw9GEE/PJ6J9Q0nA
    8kpbJ6sicAY0S0ZzdL7XKNRKGdy6t2BrsO2MBBGcMh/EpkoxzgPBx3/mefcebUnl
    7nIStYX+59w27epsZOLOS/7FPJhx8rda0Lfw3GtFtWef7vWW2DrDuPV1+dDbp8ZH
    jspqGGncBJJ7yuUwMpDNEunBOkb/t75LCL+ISn4i5EvMZbQjvRgZOht6jHbVSJeG
    eBIVHSG1AgMBAAECggEAGtx10A+0FiGo+g+Pd/L2hXZvr7EnLFfsdqP/hm5o+jkV
    tjxTpEVykw7/dVJS2Pv7jFTeGnejoaWfB+mgE+hOOamITng2zLQaLiW+yeUmzF7t
    05y0fHFJeHYv9VrUeneEzi+MWtWnuak/tGDiSm8ATNiX6xV2r1pgyMA9VYOeBsd/
    Q7aCpMgtD9fP3Vo3b8PousmZRB1c3xMf3XIcxdxEppFlUVlGTSi2kuFoEwM6i2fr
    5ptfM8JrSoPZwB6neTg959vpezviYJviDufbCaH777Uvrvr+i5Dzifrjo94yhtp4
    +sv9jVDqL4gUCIkEs3EtQJH57tZtti+mQ8hTsFh6iQKBgQDkcqnQjkCuuIffrzgX
    dnRXwCHWptsDqwJaEnyxUoGZhP8tUcOfX8cXkn9AKZCMq/eyzXR4qIWIaKevcTUN
    s35YMoXCUZ/Xmj98/9A9rZ8Z/sJnJPvpqctNer59NhCjyIMdh4k3NZAfSoBNgTXt
    kz0ir3A0pZTUDeDSXNQe7kVUPQKBgQDdrApydIm4oJIlLq2KctHv/YYpCZRSebmS
    Wr+CdTFtjIMl5SBoVeCA+joovLOd+3VYdpHHp0eWkcvjD08K+shLWJ1JtIEMuTDO
    KJ1C6iPYRPVD6HZhjP2xtwO1gnGiyF5O5qMkfqQXP0B8iDaUTaoaZloJNpNw2fa2
    UF1ZvDVC2QKBgCqjXB6Y7mmt925Afe+jJEPckHRH4Ejm+iFlofBWvemxcQ88W467
    iaVUqnuHRnt6/VhaEIsUqRe0sNeGAaj0/3fmtiB2q6bKG19xP6TDaxam/hHSTVJn
    ZpKnfjZQiPqIyjAQxAOR87xjY18T5sTY3mhC5mZtIUIS9QtOWCkNv9LhAoGBALys
    YxP6FwKbQOj+6tytT/zpl5g0PBda3dUxAlHit9hbQYEunT/Q4E5HLNthiawMzFso
    SHpb6fXBUH2JmShqPbLL0MxcI38V3PGL5hZ/PVUllHBhWuk63O+3KyQBzADWdFbJ
    vdAhLj9PKe8U7Qebdw4TUc0Hs8TY8EFK87meFOM5AoGAZyCQvbXR0nbJuXmahhrT
    sKFlAiM3ID+0LN137qeKegxbEOit5sTBjCkdFSovsqyoKR5mF5g3J3Zq/JK4naHD
    l/7vEZbO+yR/rn/exH3Aj+GIIy83kvsRsPbAbW8aII7MhRZKEa/W3bdtHfczyI6r
    apyjjEZEct41A23iQkkbytQ=
    -----END PRIVATE KEY-----
  '';
  runtimePath = lib.makeBinPath [
    pkgs.coreutils
    pkgs.hyprland
    pkgs.xdotool
    pkgs.which
  ];
in
  makeEnable config "myModules.chrome-favicon-dbus" false {
    myModules.chromeExtensions.forceInstalled.chrome-favicon-dbus = {
      id = "lidilckiohndlmcnhndjcjpicnhfhfmh";
      src = extensionSource;
      key = extensionKey;
    };

    home-manager.sharedModules = [
      {
        home.packages = [bridgePackage];

        systemd.user.services.chrome-favicon-dbus = {
          Unit = {
            Description = "Chrome favicon metadata over D-Bus";
            # This supersedes the manually installed bridge used before this
            # module existed.  Stop it before claiming the same D-Bus name.
            Conflicts = ["chrome-window-info-bridge.service"];
            After = ["graphical-session.target" "chrome-window-info-bridge.service"];
            PartOf = ["graphical-session.target"];
            StartLimitIntervalSec = 60;
            StartLimitBurst = 3;
          };

          Service = {
            ExecStart = "${bridgePackage}/bin/chrome-favicon-dbus --host 127.0.0.1 --port 38933 --path /update";
            Environment = "PATH=${runtimePath}:/run/current-system/sw/bin";
            Restart = "on-failure";
            RestartSec = 2;
          };

          Install = {
            WantedBy = ["graphical-session.target"];
          };
        };
      }
    ];
  }
