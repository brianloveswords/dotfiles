#!/bin/sh
mv ~/.zshrc ~/.zshrc.original
git clone git@github.com:brianlovesdata/oh-my-zsh.git ~/.oh-my-zsh
ln -s ./zshrc ~/.zshrc
