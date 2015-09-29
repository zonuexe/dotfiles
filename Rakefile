# -*- Coding: utf-8 -*-

HOME = ENV['HOME']
PWD  = Dir.pwd

dotfiles = %w(
  .agignore
  .bashrc
  .gemrc
  .gitconfig
  .gitexclude
  .profile
  .zshenv
  .zshrc
  .tigrc
)

desc "Make symbolic link"
task "symlink" do
  dotfiles.each do |file|
    sh "ln -sfv #{PWD}/#{file} #{HOME}"
  end
end
