.PHONY: all setup submodules symlink config-symlink

SHELL = /bin/sh

all: setup

setup: submodules symlink config-symlink

DOTFILES = \
	.agignore \
	.bashrc \
	.bash_profile \
	.emacs.d \
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
		ln -sfnv "$$(pwd)/$$file" "$$HOME/$$file"; \
	done

config-symlink:
	@set -eu; \
	mkdir -p "$$HOME/.config"; \
	for dir in "$$(pwd)/.config"/*; do \
		[ -d "$$dir" ] || continue; \
		name=$${dir##*/}; \
		ln -sfnv "$$dir" "$$HOME/.config/$$name"; \
	done
