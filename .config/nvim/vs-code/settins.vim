let mapleader = " "
nnoremap <silent> <C-j> :call VSCodeNotify('workbench.action.navigateDown')<CR>
nnoremap <silent> <C-k> :call VSCodeNotify('workbench.action.navigateUp')<CR>
nnoremap <silent> <S-TAB> :call VSCodeNotify('workbench.action.navigateBack')<CR>
nnoremap <silent> <TAB> :call VSCodeNotify('workbench.action.navigateBack')<CR>
nnoremap <silent> <C-l> :call VSCodeNotify('workbench.action.navigateRight')<CR>
nnoremap <silent> <C-h> :call VSCodeNotify('workbench.action.navigateLeft')<CR>

nnoremap <silent> <Leader>w :call VSCodeNotify('workbench.action.closeWindow')<CR>

nnoremap <silent> <Leader>nt :call VSCodeNotify('workbench.view.explorer')<CR>
