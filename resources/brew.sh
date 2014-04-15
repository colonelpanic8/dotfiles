# Make sure we’re using the latest Homebrew
brew update

# Upgrade any already-installed formulae
brew upgrade

# Install GNU core utilities (those that come with OS X are outdated)
brew install coreutils
echo "Don’t forget to add $(brew --prefix coreutils)/libexec/gnubin to \$PATH."
# Install GNU `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
brew install findutils
# Install Bash 4
brew install bash
brew install scala
brew install sbt
brew install greadlink

# Install wget with IRI support
brew install wget --enable-iri

# Install more recent versions of some OS X tools
brew tap homebrew/dupes
brew install homebrew/dupes/grep

# Install everything else
brew install emacs
brew install git
brew install tmux
brew install nmap
brew install readline
brew install netcat
brew install reattach-to-user-namespace
brew install daemonize
ln -s /usr/local/Cellar/daemonize/1.7.4/sbin/daemonize /usr/local/bin/daemonize
brew install ngrep

# Remove outdated versions from the cellar
brew cleanup
