#!/usr/bin/env zsh

function switch_sink_applications()
{
  echo switching applications
  pacmd list-sink-inputs |
    awk '/index:/{print $2}' |
    xargs -r -I{} pacmd move-sink-input {} $1 ||
      echo failed
}

current_default=$(pahelper.sh list | grep '*' | all_after_char ":" | xargs)
environment_variable_exists USE_ONLY_O_PASINK && pahelper.sh "$current_default"

# XXX: this does not prevent volumes higher than 100
pulsemixer "$@"
pashowvolume
