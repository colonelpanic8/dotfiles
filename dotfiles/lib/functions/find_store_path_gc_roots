#!/usr/bin/env sh

function find_store_path_gc_roots {
    store_path="$(realpath $1)"
    while IFS=' ' read -r gcroot derivation; do
        if [[ ! $gcroot =~ ^/proc ]]; then
            if nix-store -qR "$derivation" 2>/dev/null | grep -q "$store_path"; then
				echo $gcroot
            fi
        fi
    done < <(nix-store --gc --print-roots | awk '{print $1, $3}')
}


find_store_path_gc_roots "$@"
