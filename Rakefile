# -*- Coding: utf-8 -*-

HOME = ENV['HOME']
PWD  = Dir.pwd

dotfiles = %w(
  .gitconfig
  .gitexclude
  .profile
  .zshenv
  .zshrc
)

desc "Make symbolic link"
task "symlink" do
  dotfiles.each do |file|
    sh "ln -sfv #{PWD}/#{file} #{HOME}"
  end
end
