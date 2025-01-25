set fish_greeting
set -Ux OS Darwin
set -p PATH $HOME/.local/bin $HOME/.qlot/bin /opt/homebrew/opt/llvm/bin 
set DOCKER_HOST "unix://$HOME/.colima/docker.sock"

alias vim="nvim"
alias bake="bear -- make"
alias xcodebuild="xcodebuild -arch arm64"
alias sbcl="rlwrap sbcl"
alias love="/Applications/love.app/Contents/MacOS/love"

function fish_user_key_bindings
    fish_default_key_bindings -M insert
    fish_vi_key_bindings --no-erase insert
end

function fish_mode_prompt
    switch $fish_bind_mode
        case default
            set_color --bold red
            echo 'N'
        case insert
            set_color --bold green
            echo 'I'
        case replace_one
            set_color --bold yellow
            echo 'R'
        case visual
            set_color --bold brmagenta
            echo 'V'
        case '*'
            set_color --bold red
            echo '?'
    end
    set_color normal
end

function fish_prompt
    set -l last_status $status
    # Prompt status only if it's not 0
    set -l stat
    if test $last_status -ne 0
        set stat (set_color --bold red)' ⬥  '(set_color normal)
    else
        set stat ' ⬥  '
    end
    echo "$stat"
end

function fish_right_prompt
    echo (string split '/' (prompt_pwd --full-length-dirs=1 --dir-length=1) | tac | string join '/')
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
