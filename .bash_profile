PATH=$HOME/local/bin:$HOME/.local/share/mise/shims:$PATH

if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
elif [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix.sh'
fi
