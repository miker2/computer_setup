#! /bin/bash

# Modify computer settings:
defaults write com.apple.finder AppleShowAllFiles -string YES
defaults write com.apple.finder ShowPathbar -bool true
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Kill affected apps
for app in "Dock" "Finder"; do
	killall "${app}" > /dev/null 2>&1
done

echo "Done. Some of these changes might require a logout/restart to take effect."
