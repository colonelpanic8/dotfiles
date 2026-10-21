"""Add an owner page and flake preparation to the pinned NixOS install job."""

import json
from pathlib import Path
import re
import sys


def replace_once(text, old, new):
    if text.count(old) != 1:
        raise ValueError(f"Expected one Calamares integration point: {old!r}")
    return text.replace(old, new, 1)


def patch(source, helper, target_release):
    if not re.fullmatch(r"\d{2}\.\d{2}", target_release):
        raise ValueError("The target state version must be a NixOS release such as 26.05")
    settings = source / "config/settings.conf"
    text = settings.read_text()
    text = replace_once(
        text,
        "instances:\n",
        "instances:\n- id: owner\n  module: packagechooser\n  config: family-owner.conf\n",
    )
    text = replace_once(
        text, "  - keyboard\n  - users\n", "  - keyboard\n  - packagechooser@owner\n  - users\n"
    )
    text = replace_once(
        text, "- exec:\n  - partition\n", "- exec:\n  - family-validate\n  - partition\n"
    )
    settings.write_text(text)

    users = source / "config/modules/users.conf"
    text = replace_once(users.read_text(), "  location: None\n", "  location: EtcFile\n")
    text = replace_once(
        text, "  forbidden_names: [ localhost ]", "  forbidden_names: [ localhost, installer ]"
    )
    users.write_text(text)

    job = source / "modules/nixos/main.py"
    text = replace_once(
        job.read_text(),
        '    version = ".".join(subprocess.getoutput(["nixos-version"]).split(".")[:2])[:5]',
        f"    version = {json.dumps(target_release)}",
    )
    preparation = f"""    try:
        family_flake = subprocess.check_output(
            ["pkexec", {json.dumps(helper)}, "--root", root_mount_point,
             "--username", gs.value("username"), "--hostname", gs.value("hostname"),
             "--owner", gs.value("packagechooser_owner")],
            stderr=subprocess.STDOUT,
        ).decode().strip()
    except subprocess.CalledProcessError as error:
        return (_("Family setup failed"), error.output.decode(errors="replace"))

"""
    text = replace_once(
        text,
        "    # build nixos-install command\n",
        preparation + "    # build nixos-install command\n",
    )
    text = replace_once(
        text,
        '            "--no-root-passwd",\n',
        '            "--no-root-passwd",\n            "--flake",\n            family_flake,\n',
    )
    text = replace_once(
        text,
        "    libcalamares.job.setprogress(INSTALL_PROGRESS_END)\n",
        f"""    try:
        subprocess.check_output(
            ["pkexec", {json.dumps(helper)}, "--root", root_mount_point, "--finish"],
            stderr=subprocess.STDOUT,
        )
    except subprocess.CalledProcessError as error:
        return (_("Finishing family setup failed"), error.output.decode(errors="replace"))
    libcalamares.job.setprogress(INSTALL_PROGRESS_END)
""",
    )
    compile(text, str(job), "exec")
    job.write_text(text)


if __name__ == "__main__":
    patch(Path(sys.argv[1]), sys.argv[2], sys.argv[3])
