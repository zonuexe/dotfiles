# -*- Coding: utf-8 -*-

HOME = ENV['HOME']
PWD  = Dir.pwd

dotfiles = %w(
  .agignore
  .bashrc
  .bash_profile
  .gemrc
  .gitconfig
  .gitexclude
  .guile
  .profile
  .spacemacs
  .zshenv
  .zshrc
  .tigrc
  .vimrc
)

desc "Make symbolic link"
task "symlink" do
  dotfiles.each do |file|
    sh "ln -sfv #{PWD}/#{file} #{HOME}"
  end
end
