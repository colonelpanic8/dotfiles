pb-kill-line () {
  zle kill-line
  echo -n $CUTBUFFER | oscopy
}

pb-kill-whole-line () {
  zle kill-whole-line
  echo -n $CUTBUFFER | oscopy
}

pb-backward-kill-word () {
  zle backward-kill-word
  echo -n $CUTBUFFER | oscopy
}

pb-kill-word () {
  zle kill-word
  echo -n $CUTBUFFER | oscopy
}

pb-kill-buffer () {
  zle kill-buffer
  echo -n $CUTBUFFER | oscopy
}

pb-copy-region-as-kill-deactivate-mark () {
  zle copy-region-as-kill
  zle set-mark-command -n -1
  echo -n $CUTBUFFER | oscopy
}

pb-yank () {
  CUTBUFFER=$(ospaste)
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