"Highlight the columns from 80 to 999
execute "set colorcolumn=" . join(range(80,999),",")
highlight ColorColumn ctermbg=235 guibg=#2c2d27

"Add line numbers
set number
highlight LineNr ctermbg=235 guibg=#2c2d27

"Enable syntax highliting
syntax on

"Tab options: convert tabs to spaces, 2 spaces indent
set tabstop=2
set shiftwidth=2
set softtabstop=2
set smarttab
set expandtab

"See spaces and tabs
set lcs=tab:»·,space:·
set list
highlight NonText ctermfg=240 guifg=#2c2d27
highlight SpecialKey ctermfg=240 guifg=#2c2d27

"Autoindent
set autoindent
set smartindent
set cindent

