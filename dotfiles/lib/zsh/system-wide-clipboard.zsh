pb-kill-line () {
  zle kill-line
  echo -n $CUTBUFFER | clipboard
}

pb-kill-whole-line () {
  zle kill-whole-line
  echo -n $CUTBUFFER | clipboard
}

pb-backward-kill-word () {
  zle backward-kill-word
  echo -n $CUTBUFFER | clipboard
}

pb-kill-word () {
  zle kill-word
  echo -n $CUTBUFFER | clipboard
}

pb-kill-buffer () {
  zle kill-buffer
  echo -n $CUTBUFFER | clipboard
}

pb-copy-region-as-kill-deactivate-mark () {
  zle copy-region-as-kill
  zle set-mark-command -n -1
  echo -n $CUTBUFFER | clipboard
}

pb-yank () {
  CUTBUFFER=$(pbpaste)
  zle yank
}

zle -N pb-kill-line
zle -N pb-kill-whole-line
zle -N pb-backward-kill-word
zle -N pb-kill-word
zle -N pb-kill-buffer
zle -N pb-copy-region-as-kill-deactivate-mark
zle -N pb-yank

bindkey '^K'   pb-kill-line
bindkey '^U'   pb-kill-whole-line
bindkey '\ew'  pb-copy-region-as-kill-deactivate-mark
bindkey '\eW'  pb-copy-region-as-kill-deactivate-mark