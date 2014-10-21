tell application "Terminal"
    try
	-- we look for <= 2 because Emacs --daemon seems to always have an entry in visibile-frame-list even if there isn't
	set frameVisible to do shell script "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -e '(<= 2 (length (visible-frame-list)))'"
	if frameVisible is not "t" then
	    -- there is a not a visible frame, launch one
	    do shell script "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n"
	end if
    on error
	-- daemon is not running, start the daemon and open a frame		
	do shell script "/Applications/Emacs.app/Contents/MacOS/Emacs --daemon"
	do shell script "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n"
    end try
end tell

-- bring the visible frame to the front
tell application "Emacs" to activate