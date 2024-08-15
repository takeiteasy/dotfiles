set fish_greeting

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
