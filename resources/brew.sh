# Make sure we’re using the latest Homebrew
brew update

# Upgrade any already-installed formulae
brew upgrade

# Install GNU core utilities (those that come with OS X are outdated)
brew install coreutils 

# Install GNU `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
brew install findutils

# Install Bash 4
brew install bash

# Install wget with IRI support
brew install wget --enable-iri

# Install more recent versions of some OS X tools
brew tap homebrew/dupes
brew install homebrew/dupes/grep

# Important tools
brew install emacs
brew install git
brew install tmux
brew install python
brew install htop
brew link python
brew install scala
brew install sbt
brew install node
brew install npm

# Install everything else
brew install watch
brew install greadlink
brew install nmap
brew install readline
brew install netcat
brew install reattach-to-user-namespace
brew install daemonize
brew link daemonize
brew install ngrep
brew install gist

# Remove outdated versions from the cellar
brew cleanup
# htop wont display all process information if the owner is not root
fix_brew_htop
