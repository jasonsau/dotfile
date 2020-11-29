source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/keys/mappings.vim

if exists('g:vscode') 

else
    source $HOME/.config/nvim/plug-vim/plugins.vim
    source $HOME/.config/nvim/plug-config/coc.vim
    source $HOME/.config/nvim/plug-config/airline.vim
    source $HOME/.config/nvim/plug-config/rnvimr.vim
    source $HOME/.config/nvim/plug-config/commentary.vim
    source $HOME/.config/nvim/themes/dracula.vim
    source $HOME/.config/nvim/themes/gruvbox.vim
    source $HOME/.config/nvim/plug-config/nerdtree.vim
    luafile $HOME/.config/nvim/lua/colorizer.lua
    source $HOME/.config/nvim/plug-config/autopairs.vim
    source $HOME/.config/nvim/plug-config/kite.vim
    "source $HOME/.config/nvim/themes/onedark.vim
endif

