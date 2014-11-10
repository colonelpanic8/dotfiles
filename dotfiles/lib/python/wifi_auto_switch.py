import argparse
import logging
import re
import subprocess
import time

from lxml import etree

from xpath import dxpb, xpb
import log_util


log = logging.getLogger(__name__)


def get_stdout_from_command(command):
    return subprocess.Popen(command, stdout=subprocess.PIPE).stdout.read()


def below_threshold_trigger(threshold):
    return lambda status_info: int(status_info['RSSI']) < threshold


class Network(object):

    def __init__(self, ssid, password,
                 should_switch=below_threshold_trigger(-68)):
        self.ssid = ssid
        self.password = password
        self.should_switch = should_switch

    @property
    def login_command(self):
        return ["networksetup", "-setairportnetwork", "en0",
                self.ssid, self.password]

    def login(self):
        log.debug("Reponse from connect: {0}".format(
            get_stdout_from_command(self.login_command)
        ))


class OSXXMLStatusRetriever(object):

    def _get_status_xml(self):
        return get_stdout_from_command(['airport', '-I', '--xml'])

    def _get_status_tree(self):
        return etree.fromstring(self._get_status_xml())

    _signal_strength_key_xpb = xpb.dict.key.text_contains_("RSSI_CTL_LIST")

    def get_status_dict(self):
        status_tree = self._get_status_tree()
        signal_strength_array = self._signal_strength_key_xpb.one_(status_tree).getnext()
        signal_strengths = xpb.integer.text_.apply_(signal_strength_array)
        return sum([int(ss) for ss in signal_strengths]) / len(signal_strengths)

    __call__ = get_status_dict


class OSXStatusRetriever(object):

    KEY_REMAP = {
        'agrCtlRSSI': 'RSSI',
        'maxRate': 'max_rate',
    }

    status_output_line_regex = re.compile("^([^\n]*?): ([^\n]*?)$")

    def _get_status_text(self):
        return get_stdout_from_command(['airport', '-I'])

    @classmethod
    def _remap_key(cls, key):
        return cls.KEY_REMAP.get(key, key)

    def get_status_dict(self):
        return {self._remap_key(match.group(1).strip()): match.group(2)
                for match in [self.status_output_line_regex.match(line.strip())
                              for line in self._get_status_text().split('\n')]
                if match is not None}

    __call__ = get_status_dict


class OSXSSIDToRSSI(object):

    def _get_scan_xml(self):
        return get_stdout_from_command(['airport', '--scan', '--xml'])

    def _get_scan_tree(self):
        return etree.fromstring(self._get_scan_xml())

    _network_xpb = dxpb.array.dict
    _ssid_xpb = xpb.key.text_contains_("SSID_STR")
    _rssi_xpb = xpb.key.text_contains_("RSSI")

    def _network_elements(self):
        return self._network_xpb.apply_(self._get_scan_tree())

    def get(self):
        network_elements = self._network_elements()
        ssid_to_rssi = {}
        for network_element in network_elements:
            ssid = self._get_ssid(network_element)
            rssi = self._get_rssi(network_element)
            if ssid not in ssid_to_rssi or rssi > ssid_to_rssi[ssid]:
                ssid_to_rssi[ssid] = rssi
        return ssid_to_rssi

    def _get_ssid(self, network_element):
        try:
            return self._ssid_xpb.one_(network_element).getnext().text
        except:
            return None

    def _get_rssi(self, network_element):
        try:
            return int(self._rssi_xpb.one_(network_element).getnext().text)
        except:
            return 0

    __call__ = get


class WiFiAutoSwitcher(object):

    def __init__(self, networks, status_getter=OSXStatusRetriever(),
                 ssid_to_rssi_getter=OSXSSIDToRSSI()):
        self._networks = {network.ssid: network for network in networks}
        self._get_status = status_getter
        self._ssid_to_rssi = ssid_to_rssi_getter

    def switch_if_necessary(self):
        status_dict = self._get_status()
        log.debug(status_dict)
        network = None
        if 'SSID' in status_dict:
            network = self._networks.get(status_dict['SSID'])
            if network is None:
                # Don't do anything if the current network is not recognized
                return
        if not network or network.should_switch(status_dict):
            log.debug("Attempting to switch networks from {0}, ".format(
                network.ssid if network else "(Not conneted to network)"
            ))
            new_network = self.select_known_network_with_best_rssi()
            if new_network:
                if network and new_network.ssid == network.ssid:
                    log.debug("Switch triggered but connected network is still best.")
                else:
                    new_network.login()
        else:
            log.debug("No switch deemed necessary.")

    def select_known_network_with_best_rssi(self):
        ssid_to_rssi = self._ssid_to_rssi()
        log.debug("Selecting best network using: {0}".format(ssid_to_rssi))
        network = max(
            self._networks.values(),
            key=lambda network: ssid_to_rssi.get(network.ssid, -1000000)
        )
        if network.ssid in ssid_to_rssi:
            log.debug("selected: {0}".format(network.ssid))
            return network
        else:
            log.debug("No matching networks were found.")


if __name__ == '__main__':
    log_util.enable_logger(__name__)
    parser = argparse.ArgumentParser()
    parser.add_argument('-n', '--network', nargs='+', type=str, action='append', dest='networks')
    parser.add_argument('-s', '--sleep-time', type=int, dest='sleep_time')
    args = parser.parse_args()
    network_pairs = args.networks
    for network_pair in network_pairs:
        assert len(network_pair) == 2
    auto_switcher = WiFiAutoSwitcher(
        [Network(*ssid_password) for ssid_password in network_pairs]
    )
    while True:
         time.sleep(args.sleep_time)
         auto_switcher.switch_if_necessary()
