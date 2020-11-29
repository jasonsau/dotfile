source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/keys/mappings.vim
source $HOME/.config/nvim/plug-config/commentary.vim
"source $HOME/.config/nvim/themes/onedark.vim
" source $HOME/.config/nvim/plug-config/alvan.vim
" source $HOME/.config/nvim/plug-config/bracketsrainbox.vim
" source $HOME/.config/nvim/plug-config/nerdtree-syntax.vim
" source $HOME/.config/nvim/plug-config/raimbows.vim
"    source $HOME/.config/nvim/themes/dracula.vim

"luafile $HOME/.config/nvim/lua/plug-colorizer.lua

if exists('g:vscode')
"    source $HOME/.config/nvim/vscode/settings.vim
else 
    source $HOME/.config/nvim/plug-vim/plugins.vim
    source $HOME/.config/nvim/plug-config/coc.vim
    source $HOME/.config/nvim/plug-config/airline.vim
    source $HOME/.config/nvim/plug-config/rnvimr.vim
    source $HOME/.config/nvim/themes/gruvbox.vim
endif

