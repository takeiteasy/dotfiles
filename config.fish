set fish_greeting
set -Ux OS Darwin
set -p PATH $HOME/.local/bin $HOME/.qlot/bin /opt/homebrew/opt/llvm/bin 
set DOCKER_HOST "unix://$HOME/.colima/docker.sock"

alias vim="nvim"
alias bake="bear -- make"
alias xcodebuild="xcodebuild -arch arm64"
alias sbcl="rlwrap sbcl"
alias love="/Applications/love.app/Contents/MacOS/love"

function fish_prompt
    echo ' ⬥  '
end

function fish_right_prompt
    set -l last_status $status
    # Prompt status only if it's not 0
    set -l stat
    if test $last_status -ne 0
        set stat (set_color red)"[$last_status]"(set_color normal)
    end
    echo $stat
end

if status is-interactive
    [ -f /opt/homebrew/share/autojump/autojump.fish ]; and source /opt/homebrew/share/autojump/autojump.fish
    source /opt/homebrew/opt/asdf/libexec/asdf.fish
end

pyenv init - fish | source

# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
test -r '/Users/george/.opam/opam-init/init.fish' && source '/Users/george/.opam/opam-init/init.fish' > /dev/null 2> /dev/null; or true
# END opam configuration
