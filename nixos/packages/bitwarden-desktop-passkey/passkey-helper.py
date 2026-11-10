"""Answer one Bitwarden desktop WebAuthn assertion request with a USB security key.

Reads {"origin", "publicKey"} (WebAuthn JSON) on stdin and writes the assertion as
WebAuthn JSON on stdout. The PIN, when the key asks for one, comes from pinentry.
"""

import argparse
import ctypes
import json
import resource
import subprocess
import sys
from collections.abc import Mapping
from urllib.parse import unquote

from fido2.client import DefaultClientDataCollector, Fido2Client, UserInteraction
from fido2.ctap2 import AssertionResponse, Ctap2
from fido2.ctap2.base import args
from fido2.hid import CtapHidDevice
from fido2.utils import websafe_encode


def get_assertion(
    self,
    rp_id,
    client_data_hash,
    allow_list=None,
    extensions=None,
    options=None,
    pin_uv_param=None,
    pin_uv_protocol=None,
    *,
    event=None,
    on_keepalive=None,
):
    """Like Ctap2.get_assertion, but accepts a response that omits the credential.

    CTAP allows that when the allow list names exactly one credential (WearAuthn
    does so), and python-fido2 would otherwise fail to parse the response.
    """
    response = self.send_cbor(
        Ctap2.CMD.GET_ASSERTION,
        args(
            rp_id,
            client_data_hash,
            allow_list,
            extensions,
            options,
            pin_uv_param,
            pin_uv_protocol,
        ),
        event=event,
        on_keepalive=on_keepalive,
    )
    if allow_list and len(allow_list) == 1:
        response = {1: allow_list[0], **response}
    return AssertionResponse.from_dict(response)


Ctap2.get_assertion = get_assertion


class OwnChannel:
    """Drops HID reports addressed to other CTAPHID channels.

    Every client of a hidraw device sees every report, and CTAPHID clients must
    ignore channels they don't own; python-fido2 instead fails with "Wrong
    channel" when another client or a late reply to an aborted request is
    active.
    """

    def __init__(self, device, connection):
        self.device = device
        self.connection = connection

    def read_packet(self):
        while True:
            packet = self.connection.read_packet()
            if int.from_bytes(packet[:4], "big") == self.device._channel_id:
                return packet

    def write_packet(self, packet):
        self.connection.write_packet(packet)

    def close(self):
        self.connection.close()


class HidDevice(CtapHidDevice):
    def __init__(self, descriptor, connection):
        self._channel_id = 0xFFFFFFFF
        super().__init__(descriptor, OwnChannel(self, connection))


def assuan_escape(text):
    return text.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")


class Pinentry:
    def __init__(self, command):
        self.process = subprocess.Popen(
            [command],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
        )
        self.read_reply()

    def read_reply(self):
        data = []
        for line in self.process.stdout:
            line = line.rstrip("\n")
            if line.startswith("D "):
                data.append(unquote(line[2:]))
            elif line == "OK" or line.startswith("OK "):
                return "".join(data)
            elif line.startswith("ERR"):
                raise RuntimeError(f"pinentry: {line}")
        raise RuntimeError("pinentry exited unexpectedly")

    def command(self, line):
        self.process.stdin.write(line + "\n")
        self.process.stdin.flush()
        return self.read_reply()

    def close(self):
        self.process.stdin.close()
        self.process.wait()


class Interaction(UserInteraction):
    def __init__(self, pinentry, rp_id):
        self.pinentry = pinentry
        self.rp_id = rp_id

    def request_pin(self, permissions, rp_id):
        pinentry = Pinentry(self.pinentry)
        try:
            pinentry.command("SETTITLE Bitwarden")
            pinentry.command(
                "SETDESC "
                + assuan_escape(
                    f"Enter your security key PIN to sign in to {self.rp_id}."
                )
            )
            pinentry.command("SETPROMPT PIN:")
            return pinentry.command("GETPIN") or None
        finally:
            pinentry.close()

    def prompt_up(self):
        pass


def encode(value):
    if isinstance(value, (bytes, bytearray, memoryview)):
        return websafe_encode(bytes(value))
    if isinstance(value, Mapping):
        return {key: encode(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [encode(item) for item in value]
    return value


def assert_credential(request, pinentry):
    devices = list(HidDevice.list_devices())
    try:
        if not devices:
            raise ValueError("No security key found. Connect one and try again.")
        if len(devices) > 1:
            raise ValueError("Connect only one security key, then try again.")
        options = request["publicKey"]
        client = Fido2Client(
            devices[0],
            DefaultClientDataCollector(request["origin"]),
            user_interaction=Interaction(pinentry, options.get("rpId")),
        )
        selection = client.get_assertion(options)
        if len(selection.get_assertions()) != 1:
            raise ValueError(
                "Several passkeys on this key match; remove the extras to continue."
            )
        return encode(dict(selection.get_response(0)))
    finally:
        for device in devices:
            device.close()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--pinentry", required=True)
    args = parser.parse_args()

    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    if ctypes.CDLL(None).prctl(4, 0, 0, 0, 0) != 0:
        raise RuntimeError("Cannot disable tracing for the passkey helper")

    raw = sys.stdin.buffer.read(131073)
    if len(raw) > 131072:
        raise ValueError("Passkey request is too large")
    response = assert_credential(json.loads(raw), args.pinentry)
    json.dump(response, sys.stdout)
    sys.stdout.write("\n")


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(
            error
            if isinstance(error, ValueError)
            else f"{type(error).__name__}: {error}",
            file=sys.stderr,
        )
        raise SystemExit(1) from None
