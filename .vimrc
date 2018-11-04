"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=~/.vim/dein/repos/github.com/Shougo/dein.vim

if dein#load_state(expand('~/.vim/dein'))
   " Required:
   call dein#begin(expand('~/.vim/dein'))

   " Let dein manage dein
   " Required:
   call dein#add('Shougo/dein.vim')

   " Add or remove your plugins here:
   call dein#add('Shougo/neosnippet.vim')
   call dein#add('Shougo/neosnippet-snippets')
   call dein#add('Shougo/echodoc.vim')
   call dein#add('itchyny/calendar.vim')
   call dein#add('editorconfig/editorconfig-vim')
   call dein#add('scrooloose/nerdtree')
   call dein#add('mattn/emmet-vim')
   call dein#add('StanAngeloff/php.vim')

   " You can specify revision/branch/tag.
   call dein#add('Shougo/vimproc.vim', { 'build': 'make' })
   call dein#add('Shougo/vimshell')

   call dein#add('tpope/vim-speeddating')
   call dein#add('jceb/vim-orgmode')

   call dein#add('phpactor/phpactor', { 'build': 'composer install' })
   call dein#add('w0rp/ale')

   " Required:
   call dein#end()
   call dein#save_state()
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

function! PhpSyntaxOverride()
  hi! def link phpDocTags  phpDefine
  hi! def link phpDocParam phpType
endfunction

augroup phpSyntaxOverride
  autocmd!
  autocmd FileType php call PhpSyntaxOverride()
augroup END

" Put these lines at the very end of your vimrc file.

" Load all plugins now.
" Plugins need to be added to runtimepath before helptags can be generated.
packloadall
" Load all of the helptags now, after plugins have been loaded.
" All messages and errors will be ignored.
silent! helptags ALL
