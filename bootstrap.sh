#!/usr/bin/env sh

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

defaults write com.apple.dock orientation -string left
defaults write com.apple.dock tilesize -integer 32
defaults write com.apple.dock largesize -int 50
defaults write com.apple.dock autohide -bool YES 
defaults write -g com.apple.swipescrolldirection -bool false
killall Dock

defaults write com.apple.finder AppleShowAllFiles YES
killall Finder

brew bundle install

mkdir -p ~/.config/fish
mkdir -p ~/.config/nvim
ln -s ~/git/dotfiles/config.fish ~/.config/fish/config.fish
ln -s ~/git/dotfiles/init.vim ~/.config/nvim/init.vim
ln -s ~/git/dotfiles/init.el ~/.emacs

echo '/opt/homebrew/bin/fish' | sudo tee -a /etc/shells; cat /etc/shells
chsh -s /opt/homebrew/bin/fish

curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
rm quicklisp.lisp

curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
