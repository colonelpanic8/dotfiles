for filename in ~/.lib/shellrc/*; do
    source $filename
done
# Source everything twice just in case there were things that depended
# on each other.
for filename in ~/.lib/shellrc/*; do
    source $filename
done

