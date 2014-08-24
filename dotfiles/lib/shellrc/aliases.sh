alias emacs="emacsclient -t"
alias xemacs="\emacs --daemon > /dev/null 2&> /dev/null; emacsclient -c &"
alias tmux="tmux -2"
alias tmux_cb_to_remote_cb='tmux saveb - | linux_nc_paste_to_remote_clipboard'
alias timestamp='date +%s'
alias fn='find . -name '
alias prj='cd ~/Projects'
alias reload_tmux='tmux source-file ~/.tmux.conf'

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
alias pip-upgrap="pip freeze --local | get_cols -F "=" 1 | xargs -n1 sudo pip install -U"
