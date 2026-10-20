"""Private HLS gateway. Only Tailscale Serve may connect to this loopback listener."""

import argparse
import http.client
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlsplit


class CameraGateway(BaseHTTPRequestHandler):
    def do_GET(self):
        self.forward()

    def do_HEAD(self):
        self.forward()

    def forward(self):
        identities = self.headers.get_all("Tailscale-User-Login", [])
        if identities != [self.server.viewer]:
            self.send_error(
                403, "This feed is restricted to its configured Tailscale user"
            )
            return

        path = urlsplit(self.path)
        if path.scheme or path.netloc or not path.path.startswith("/camera/"):
            if self.path == "/":
                self.send_response(302)
                self.send_header("Location", "/camera/")
                self.send_header("Cache-Control", "no-store")
                self.end_headers()
            else:
                self.send_error(404)
            return

        connection = http.client.HTTPConnection(
            "127.0.0.1", self.server.upstream_port, timeout=30
        )
        try:
            headers = {}
            if cookie := self.headers.get("Cookie"):
                headers["Cookie"] = cookie
            connection.request(self.command, self.path, headers=headers)
            response = connection.getresponse()
            self.send_response(response.status)
            for name in ("Content-Type", "Content-Length", "Location"):
                value = response.getheader(name)
                if value is not None:
                    self.send_header(name, value)
            for name, value in response.getheaders():
                if name.lower() == "set-cookie":
                    self.send_header(name, value)
            self.send_header("Cache-Control", "no-store")
            self.send_header("X-Content-Type-Options", "nosniff")
            self.send_header("Referrer-Policy", "no-referrer")
            self.send_header("Content-Security-Policy", "frame-ancestors 'none'")
            self.end_headers()
            if self.command != "HEAD":
                while chunk := response.read(65536):
                    self.wfile.write(chunk)
        except (OSError, http.client.HTTPException):
            self.close_connection = True
        finally:
            connection.close()


def serve(viewer, port, upstream_port):
    server = ThreadingHTTPServer(("127.0.0.1", port), CameraGateway)
    server.viewer = viewer
    server.upstream_port = upstream_port
    return server


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--viewer", required=True)
    parser.add_argument("--port", type=int, default=18381)
    parser.add_argument("--upstream-port", type=int, default=18380)
    args = parser.parse_args()
    serve(args.viewer, args.port, args.upstream_port).serve_forever()
