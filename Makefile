.PHONY: all setup submodules symlink

SHELL = /bin/sh

all: setup

setup: submodules symlink

DOTFILES = \
	.agignore \
	.bashrc \
	.bash_profile \
	.gemrc \
	.gitconfig \
	.gitexclude \
	.guile \
	.profile \
	.spacemacs \
	.zshenv \
	.zshrc \
	.tigrc \
	.vimrc

submodules:
	git submodule update --init --recursive

symlink:
	@set -eu; \
	for file in $(DOTFILES); do \
		ln -sfv "$$(pwd)/$$file" "$$HOME"; \
	done
