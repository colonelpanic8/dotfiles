alias i="ipython tools/interactive.py"
alias apperror="tools/scribereader -e prod -f apperror | tools/pretty_error_log"
alias environ=". ~/.pgconf-$USER/environ.sh"
alias ym="cd ~/pg/yelp-main"
alias live="cd /nail/live/yelp"
alias sb="sandbox -vv --minimal"

function fix_environment_script() {
    sed -i 's/export YELP_CONFIG:yelp_conn:replication_delay_params.*$//' $YELP_SANDBOX_ROOT/environment.sh
}

function get_sandbox_identifier() {
    echo $YELP_SANDBOX_ROOT | gawk 'match($0, /pgconf-.*-(.*)/, matched) {print matched[1]}'
}

function is_proddb() {
python 2>/dev/null - <<END
import os
import sys
import yaml


def is_proddb_from_topology_info(topology_info):
    for cluster_info in topology_info['topology']:
        if cluster_info['cluster'] == 'primary':
            return cluster_info['entries'][0]['host'] != '127.0.0.1'

def is_proddb_from_topology_filename(topology_filename):
    with open(topology_filename) as topology_file:
        return is_proddb_from_topology_info(yaml.load(topology_file))

def is_proddb():
    try:
        topology_filename =  os.environ.get('TOPOLOGY_FILE')
        if topology_filename:
            return is_proddb_from_topology_filename(topology_filename)
        return False
    except:
        pass

sys.exit(0 if is_proddb() else 1)
END
}

function sandbox_prompt_info() {
    local sandbox_string=''
    if is_proddb
    then
        sandbox_string="proddb "
    fi
    if [ "$YELP_IN_SANDBOX" ];
    then
        sandbox_string=$sandbox_string"sandbox-$(get_sandbox_identifier)"
    else
        sandbox_string="no sandbox"
    fi
    echo $sandbox_string
}

function bash_sandbox_color() {
    if [ $YELP_IN_SANDBOX ]
    then
	sandbox_color='\e[0;33m'
    else
	sandbox_color='\e[0;31m'
    fi
}

function zsh_sandbox_color() {
    if [ $YELP_IN_SANDBOX ]
    then
	sandbox_color="%{$FG[149]%}"
    else
	sandbox_color="%{$FG[001]%}"
    fi
    echo $sandbox_color
}

function colored_sandbox_string() {
    if [ is_zsh ]
    then
	sandbox_color=$(zsh_sandbox_color)
    else
	sandbox_color=$(bash_sandbox_color)
    fi
    echo $sandbox_color$(sandbox_prompt_info)
}
