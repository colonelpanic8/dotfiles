alias tmux="tmux -2"
alias reload_tmux_conf="tmux source-file ~/.tmux.conf"
alias tmux_cb_to_remote_cb='tmux saveb - | linux_nc_paste_to_remote_clipboard'
alias fn='find . -name '
alias prj='cd ~/Projects'
alias t27='tox -e py27 -- '
alias tvenv='tox -e venv -- '
alias reload_tmux='tmux source-file ~/.tmux.conf'
alias ssh='ssh -A '
alias dusage='du -ch -d 0'
alias subl='reattach-to-user-namespace subl'

# enables the sudoing of aliases.
alias sudo='sudo '

# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
    colorflag="--color"
else # OS X `ls`
    colorflag="-G"
fi
alias ls="command ls ${colorflag}"

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*' | grep -v '127.0.0.1'"

alias whois="whois -h whois-servers.net"

# View HTTP traffic
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""
command -v greadlink > /dev/null && alias readlink="greadlink"

# Merge PDF files
# Usage: `mergepdf -o output.pdf input{1,2,3}.pdf`
alias mergepdf='/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'

# Disable Spotlight
alias spotoff="sudo mdutil -a -i off"
# Enable Spotlight
alias spoton="sudo mdutil -a -i on"

# One of @janmoesen’s ProTip™s
for method in GET HEAD POST PUT DELETE TRACE OPTIONS; do
    alias "$method"="lwp-request -m '$method'"
done

alias stfu="osascript -e 'set volume output muted true'"
alias pumpitup="osascript -e 'set volume 7'"
alias pip-upgrade="pip freeze --local | get_cols -F "=" 1 | xargs -n1 sudo pip install -U"

[[ -e /usr/local/Cellar/macvim ]] && alias vim="$(find /usr/local/Cellar/macvim -depth 1 | grep "[0-9]$")/MacVim.app/Contents/MacOS/Vim"

alias only_json='grep -E "^{\"" | jq .'
