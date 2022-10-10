# Fig pre block. Keep at the top of this file.
[[ -f "$HOME/.fig/shell/bash_profile.pre.bash" ]] && builtin source "$HOME/.fig/shell/bash_profile.pre.bash"
PATH=$HOME/local/bin:$PATH
if [ -e /Users/megurine/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/megurine/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# Fig post block. Keep at the bottom of this file.
[[ -f "$HOME/.fig/shell/bash_profile.post.bash" ]] && builtin source "$HOME/.fig/shell/bash_profile.post.bash"
