set number
set cursorline
set hlsearch
" yanking text goes to the system clipboard
set clipboard=unnamedplus
if has("gui_running")
    set bg=light
else
    set bg=dark
endif
" For using screen/byobu
if match($TERM, "screen*")!=-1
    set term=xterm
endif
" REQUIRED. This makes vim invoke Latex-Suite when you open a tex file.
filetype plugin on

"INDENTATION
"========================================================
"Tabsize
let tabsize=4

"Sets the size of <TAB>
execute "set tabstop=".tabsize
"Sets the size of shift operator << or >> done on lines
execute "set shiftwidth=".tabsize

"Soft tabs
"When set will use soft tabs with number of spaces equal
"to softtabstop when softtabstop is defined, otherwise
"tabstop number of spaces
set expandtab
":set "softtabstop=".tabsize

set autoindent

" Key Bindings
"
" map CTRL-E to end-of-line (insert mode)
imap <C-e> <esc>$i<right>
" map CTRL-A to beginning-of-line (insert mode)
imap <C-a> <esc>0i
" map CTRL-L to right (insert mode)
imap <C-l> <right>
" map CTRL-H to left (insert mode)
imap <C-h> <left>
" map CTRL-J to down (insert mode)
inoremap <C-j> <down>
" map CTRL-K to up (insert mode)
imap <C-k> <up>
" map CTRL-B to word-back (insert mode)
imap <C-b> <esc>B<left>i
imap <C-left> <esc>B<left>i
" map CTRL-F to word-forward (insert mode)
imap <C-f> <esc><right>E<right>i
imap <C-right> <esc><right>E<right>i
" map CTRL-E to end-of-line (normal mode)
nmap <C-e> $
" map CTRL-A to beginning-of-line (normal mode)
nmap <C-a> ^
" get rid of search highlight in vim
nmap <C-h> :let @/=""<CR>
" Write as root after having opened the file as normal user
command! W w !sudo tee % >/dev/null

" IMPORTANT: win32 users will need to have 'shellslash' set so that latex
" " can be called correctly.
set shellslash
"
" " IMPORTANT: grep will sometimes skip displaying the file name if you
" " search in a singe file. This will confuse Latex-Suite. Set your grep
" " program to always generate a file-name.
set grepprg=grep\ -nH\ $*
"
"
" " OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults
" to
" " 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" " The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'
let g:Imap_FreezeImap=1
