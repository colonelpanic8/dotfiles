#!/bin/bash

#!/bin/bash

# Exit on any error
set -e

# Define variables
USERNAME="gitearunner"

# Function to check if script is run as root
check_root() {
    if [ "$(id -u)" != "0" ]; then
        echo "This script must be run as root" 1>&2
        exit 1
    fi
}

# Function to create system user
create_system_user() {
    # Generate a unique ID (you may need to adjust this logic)
    UNIQUE_ID=$(dscl . -list /Users UniqueID | awk '{print $2}' | sort -n | tail -1)
    UNIQUE_ID=$((UNIQUE_ID+1))

    dscl . -create /Users/$USERNAME
    dscl . -create /Users/$USERNAME RealName "Gitea Runner"
    dscl . -create /Users/$USERNAME UniqueID $UNIQUE_ID
    dscl . -create /Users/$USERNAME PrimaryGroupID 20  # 20 is the 'staff' group
    dscl . -create /Users/$USERNAME NFSHomeDirectory /var/lib/gitea-runner/nix
    dscl . -create /Users/$USERNAME IsHidden 1
    /usr/bin/dscl . -create /Users/$USERNAME Password "*"

    echo "System user $USERNAME created with UID $UNIQUE_ID."
}

# Main execution
check_root
create_system_user

echo "Setup complete. The $USERNAME system user has been created."
