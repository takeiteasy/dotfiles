# Fig pre block. Keep at the top of this file.
[[ -f "$HOME/.fig/shell/zshrc.pre.zsh" ]] && builtin source "$HOME/.fig/shell/zshrc.pre.zsh"

MYPATH=/opt/homebrew/bin/:$HOME/.bin:$HOME/.local/bin:/usr/local/bin:/opt/homebrew/opt/ruby/bin
BREW_BIN="/opt/homebrew/bin/brew"
if type "${BREW_BIN}" &> /dev/null; then
    export BREW_PREFIX="$(/opt/homebrew/bin/brew --prefix)"
    for bindir in "${BREW_PREFIX}/opt/"*"/libexec/gnubin"; do export PATH=$bindir:$MYPATH:$PATH; done
    for bindir in "${BREW_PREFIX}/opt/"*"/bin"; do export PATH=$bindir:$PATH; done
    for mandir in "${BREW_PREFIX}/opt/"*"/libexec/gnuman"; do export MANPATH=$mandir:$MANPATH; done
    for mandir in "${BREW_PREFIX}/opt/"*"/share/man/man1"; do export MANPATH=$mandir:$MANPATH; done
fi
export LDFLAGS="-L/opt/homebrew/lib -L/opt/homebrew/opt/binutils/lib -L/opt/homebrew/opt/flex/lib -L/opt/homebrew/opt/ruby/lib"
export CFLAGS="-I/opt/homebrew/opt/include -I/opt/homebrew/opt/binutils/include -I/opt/homebrew/opt/flex/include -I/opt/homebrew/opt/ruby/include"
export CPPFLAGS="-std=c++11 -lstdc++"
export PKG_CONFIG_PATH="/opt/homebrew/opt/ruby/lib/pkgconfig"

export ZSH="$HOME/.oh-my-zsh"
DISABLE_AUTO_TITLE="true"
DISABLE_AUTO_TITLE="true"
# CASE_SENSITIVE="true"
# HYPHEN_INSENSITIVE="true"
# DISABLE_MAGIC_FUNCTIONS="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
plugins=(alias-finder asdf autojump command-not-found colored-man-pages extract fig zsh-interactive-cd zsh-navigation-tools bgnotify copybuffer extract jump thefuck vi-mode history-substring-search)
source $ZSH/oh-my-zsh.sh

function emacs() {
    open -a "/Applications/Emacs.app/Contents/MacOS/Emacs" "$@"
}
alias mkdir="mkdir -p"
alias mkd="mkdir"
function mkcd() {
    mkd $1
    cd $1
}
function mkg() {
    mkcd $1
    git init .
}
alias bake="bear -- make"
alias cl="rlwrap sbcl"

function cl-eval() {
    sbcl --quit --eval "$@"
}

function cl-brew() {
    cmd=""
    for arg in "$@"
    do
        cmd="$cmd :$arg"
    done
    cl-eval "(mapc #'ql:quickload '($cmd))"
}

function git_branch_name() {
    branch=$(git symbolic-ref HEAD 2> /dev/null | awk 'BEGIN{FS="/"} {print $NF}')
    if [[ -n $branch ]]; then
        echo -ne ' - ('$branch
        if test -z "$(git status --porcelain --ignore-submodules)"; then
            echo -n '±'
        fi
        echo ')'
    fi
}
function tab_title() {
    # sets the tab title to current dir
    if [ $(git rev-parse --is-inside-work-tree 2>/dev/null) ]; then
        if [[ $(git diff --stat) != '' ]]; then
            echo -ne "\e]1;👍🏻\a"
        else
            echo -ne "\e]1;✍🏻\a"
        fi
    fi
    # echo -ne "\e]1;${PWD/#$HOME/~}$(git_branch_name)\a"
}
add-zsh-hook precmd tab_title
prompt_context() {
    if [[ `whoami` != "george" || -n "$SSH_CONNECTION" ]]; then
        print -n "%{%F{red}%}"
    else
        print -n "%{%F{white}%}"
    fi
}
PROMPT=" $(prompt_context)⬥  "

source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /opt/homebrew/opt/zsh-vi-mode/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
#source /opt/homebrew/share/zsh-autocomplete/zsh-autocomplete.plugin.zsh

test -e /Users/george/.iterm2_shell_integration.zsh && source /Users/george/.iterm2_shell_integration.zsh || true

# Fig post block. Keep at the bottom of this file.
[[ -f "$HOME/.fig/shell/zshrc.post.zsh" ]] && builtin source "$HOME/.fig/shell/zshrc.post.zsh"
