#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set mange.pc_app_to_hyper 1
/bin/echo -n .
$cli set repeat.initial_wait 300
/bin/echo -n .
$cli set repeat.wait 11
/bin/echo -n .
$cli set imalison.right_command_to_hyper 1
/bin/echo -n .
/bin/echo
