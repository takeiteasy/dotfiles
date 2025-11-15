export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH
export ZSH="$HOME/.oh-my-zsh"
# zstyle ':omz:update' mode disabled  # disable automatic updates
zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"
# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"
# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"
# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(
	git
	macos
	aliases
	alias-finder
	brew
	docker
	docker-compose
	fancy-ctrl-z
	fzf
	gem
	git-auto-fetch
	git
	iterm2
	pip
	rust
	safe-paste
	sudo
	thefuck
	vi-mode
	zsh-interactive-cd
	zsh-navigation-tools
	zsh-autosuggestions
	fast-syntax-highlighting
	zsh-autopair
)
source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8
export EDITOR='emacs -nw'

eval $(thefuck --alias)

export OS=Darwin
export PATH="/opt/homebrew/bin:$HOME/.local/bin:$HOME/.qlot/bin:/opt/homebrew/opt/llvm/bin:$HOME/.qlot/bin:$PATH"
export DOCKER_HOST="unix://$HOME/.colima/docker.sock"
export OLLAMA_ORIGINS="*"

alias bake="bear -- make"
alias xcodebuild="xcodebuild -arch arm64"
alias sbcl="rlwrap sbcl"
if [ -d "/Applications/Emacs.app/Contents/MacOS/bin" ]; then
  export PATH="/Applications/Emacs.app/Contents/MacOS/bin:$PATH"
  alias emacs="emacs -nw" # Always launch "emacs" in terminal mode.
fi

bindkey -v
setopt PROMPT_SUBST

function zle-keymap-select {
    case $KEYMAP in
        vicmd)
            MODE_INDICATOR='%F{red}%B N%b%f'
            ;;
        main|viins)
            MODE_INDICATOR='%F{green}%B I%b%f'
            ;;
        visual)
            MODE_INDICATOR='%F{magenta}%B V%b%f'
            ;;
        *)
            MODE_INDICATOR='%F{red}%B ?%b%f'
            ;;
    esac
    zle reset-prompt
}

function zle-line-init {
    MODE_INDICATOR='%F{green}%B I%b%f'
    zle reset-prompt
}

zle -N zle-keymap-select
zle -N zle-line-init

PROMPT='${MODE_INDICATOR}%(?. ⬥ . %F{red}%B⬥ %b%f) '

function rprompt_path {
    local full_path="${PWD/#$HOME/~}"
    
    if [[ "$full_path" == "~" ]]; then
        echo "~"
    else
        # Split path into array
        local -a parts
        parts=("${(@s:/:)full_path}")
        
        local result=""
        local last_idx=${#parts[@]}
        
        for i in {1..$last_idx}; do
            if [[ $i -eq $last_idx ]]; then
                # Last element - full name
                result="$result${parts[$i]}"
            elif [[ -n "${parts[$i]}" ]]; then
                # Not last - first char only
                result="$result${parts[$i]:0:1}/"
            fi
        done
        
        echo "$result"
    fi
} 

RPROMPT='$(rprompt_path)'

