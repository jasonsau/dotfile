set PATH $PATH /home/js/.local/bin
set PATH $PATH /usr/local/go/bin

set TERM "xterm-256color"
export EDITOR=nvim

set DOCUMENTS '/run/media/Almacenamiento/Users/Jason/Documents/'




alias n='nvim'
alias l='exa -lha'
alias ..='cd ..'
alias update='sudo pacman -Syu'
alias install='sudo pacman -S'
alias sysenable='sudo systemctl enable'
alias sysstart='sudo systemctl start'
alias sysstop='sudo systemctl stop'
alias sysrestart='sudo systemctl restart'
alias t='touch'




starship init fish | source
