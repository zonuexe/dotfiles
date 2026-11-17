{
  description = "Example nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:nix-darwin/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs }:
  let
    configuration = { pkgs, ... }: {
      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment.systemPackages =
        [ pkgs.vim
        ];

      # Nix configuration (migrated from the previous manual /etc/nix/nix.conf).
      nix.settings = {
        experimental-features = "nix-command flakes";
        build-users-group = "nixbld";
        trusted-users = [ "root" "megurine" ];
        extra-substituters = [ "https://emacs-ci.cachix.org" ];
        extra-trusted-public-keys = [
          "emacs-ci.cachix.org-1:B5FVOrxhXXrOL0S+tQ7USrhjMT5iOPH+QN9q0NItom4="
        ];
        extra-platforms = [ "x86_64-darwin" ];
      };

      # Automatically garbage-collect the Nix store so unused paths
      # (from nix build / nix develop) don't accumulate without bound.
      nix.gc = {
        automatic = true;
        interval = { Hour = 3; Minute = 0; };
        # Keep last 7 days of generations, then delete unreachable paths.
        options = "--delete-older-than 7d";
      };

      # Enable alternative shell support in nix-darwin.
      # programs.fish.enable = true;

      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Used for backwards compatibility, please read the changelog before changing.
      # $ darwin-rebuild changelog
      system.stateVersion = 6;

      # The platform the configuration will be used on.
      nixpkgs.hostPlatform = "aarch64-darwin";
    };
  in
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#MegurinenoMacBook-Pro
    darwinConfigurations."MegurinenoMacBook-Pro" = nix-darwin.lib.darwinSystem {
      modules = [ configuration ];
    };
    # Mac mini (LocalHostName: tadsan-mini)
    darwinConfigurations."tadsan-mini" = nix-darwin.lib.darwinSystem {
      modules = [ configuration ];
    };
  };
}
