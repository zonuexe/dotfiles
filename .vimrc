"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

let s:vimrc_path = resolve(expand('<sfile>:p'))
let s:dotfiles_dir = fnamemodify(s:vimrc_path, ':h')
let s:dein_repo_dir = s:dotfiles_dir . '/.vim/dein.vim'
let s:dein_cache_dir = expand('~/.vim/dein')

if isdirectory(s:dein_repo_dir)
  execute 'set runtimepath^=' . fnameescape(s:dein_repo_dir)
else
  echohl WarningMsg
  echom 'dein.vim submodule is missing. Run: git submodule update --init --recursive'
  echohl None
endif

if isdirectory(s:dein_repo_dir) && dein#load_state(s:dein_cache_dir)
   " Required:
   call dein#begin(s:dein_cache_dir)

   " Let dein manage dein
   " Required:
   call dein#add(s:dein_repo_dir)

   " Add or remove your plugins here:
   call dein#add('Shougo/neosnippet.vim')
   call dein#add('Shougo/neosnippet-snippets')
   call dein#add('Shougo/echodoc.vim')
   call dein#add('itchyny/calendar.vim')
   call dein#add('editorconfig/editorconfig-vim')
   call dein#add('scrooloose/nerdtree')
   call dein#add('mattn/emmet-vim')

   " You can specify revision/branch/tag.
   call dein#add('Shougo/vimproc.vim', { 'build': 'make' })
   call dein#add('Shougo/vimshell')

   call dein#add('tpope/vim-speeddating')
   call dein#add('jceb/vim-orgmode')

   call dein#add('phpactor/phpactor', { 'build': 'composer install' })
   call dein#add('w0rp/ale')
   call dein#add('ujihisa/repl.vim')

   " Required:
   call dein#end()
   call dein#save_state()
endif

if isdirectory(s:dein_repo_dir) && dein#check_install()
  call dein#install()
endif

" Required:
filetype plugin indent on

nnoremap ZZ <Nop>
nnoremap ZQ <Nop>

nnoremap <silent><C-e> :NERDTreeToggle<CR>

" Coding defaults

set tabstop=4
set shiftwidth=4
set expandtab
set softtabstop=1
set backspace=indent,eol,start

" PHP
let php_sql_query = 1
let php_baselib = 1
let php_htmlInStrings = 1
let php_noShortTags = 1
let php_parent_error_close = 1
" g:php_syntax_extensions_enabled


" Terminal titlebar
let &t_ti .= "\e[22;0t"
let &t_te .= "\e[23;0t"

" https://qiita.com/Linda_pp/items/9e0c94eb82b18071db34
if has('vim_starting')
   " 挿入モード時に非点滅の縦棒タイプのカーソル
   let &t_SI .= "\e[6 q"
   " ノーマルモード時に非点滅のブロックタイプのカーソル
   let &t_EI .= "\e[2 q"
   " 置換モード時に非点滅の下線タイプのカーソル
   let &t_SR .= "\e[4 q"
endif

" SQL
let g:sql_type_default='mysql'

syntax on
