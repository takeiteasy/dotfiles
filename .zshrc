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
export PKG_CONFIG_PATH="/opt/homebrew/opt/ruby/lib/pkgconfig"

export ZSH="$HOME/.oh-my-zsh"
plugins=(alias-finder asdf autojump command-not-found colored-man-pages extract fig zsh-interactive-cd zsh-navigation-tools bgnotify copybuffer extract jump thefuck vi-mode history-substring-search)
source $ZSH/oh-my-zsh.sh
DISABLE_AUTO_TITLE="true"
# CASE_SENSITIVE="true"
# HYPHEN_INSENSITIVE="true"
# DISABLE_MAGIC_FUNCTIONS="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

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

function sbcl-eval() {
    sbcl --quit --eval "$@"
}

function quickload() {
    cmd=""
    for arg in "$@"
    do
        cmd="$cmd :$arg"
    done
    sbcl-eval "(mapc #'ql:quickload '($cmd))"
}

alias bake="bear -- make"

function emacs () {
    /Applications/Emacs.app/Contents/MacOS/Emacs "$@" &
}

PS1=" ⬥  "

source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /opt/homebrew/opt/zsh-vi-mode/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
# source /opt/homebrew/share/zsh-autocomplete/zsh-autocomplete.plugin.zsh

test -e /Users/george/.iterm2_shell_integration.zsh && source /Users/george/.iterm2_shell_integration.zsh || true

PATH="/Users/george/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/Users/george/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/george/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/george/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/george/perl5"; export PERL_MM_OPT;

# Fig post block. Keep at the bottom of this file.
[[ -f "$HOME/.fig/shell/zshrc.post.zsh" ]] && builtin source "$HOME/.fig/shell/zshrc.post.zsh"
