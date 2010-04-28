
# Some of this file is mine, some is take from spider's zshrc, and some from
# guckes's zshrc. Some stuff is totally random.

# Load the super duper completion stuff
autoload -U compinit
compinit

autoload -U promptinit
promptinit

# run these commands.


# prompts
if [[ $TERM == "dumb" ]]; then	# in emacs
    PS1='%(?..[%?])%!:%~%# '
    # for tramp to not hang, need the following. cf:
    # http://www.emacswiki.org/emacs/TrampMode
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
else
    PROMPT=$'$cyan%n$white@$purple%m $GREEN>> $NC'
    RPROMPT=$'$NC<$yellow%.$NC>'
fi

# Exports
export PERL5LIB=/home/$USER/lib
export LC_ALL=C
export HOSTTYPE="$(uname -m)"
export COLORTERM=yes
export CLICOLOR=yes
export LINKS_XTERM=screen
export CC=gcc
export MANPAGER=less
export PAGER=less
export EDITOR=emacsclient

# Golang
export GOROOT=$HOME/src/go
export GOOS=darwin
export GOARCH=amd64
export GOBIN=$HOME/bin

export SCALA_HOME=/usr/local/share/scala

export PATH=$HOME/bin:/opt/local/bin:opt/local/sbin:/usr/local/bin:/usr/local/sbin:/bin:/usr/bin:/usr/X11R6/bin:/usr/sbin:/sbin:/usr/local/mysql/bin:$SCALA_HOME/bin:$PATH

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig/
export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=01;32:*.cmd=01;32:*.exe=01;32:*.com=01;32:*.btm=01;32:*.bat=01;32:*.sh=01;32:*.csh=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mng=01;35:*.xcf=01;35:*.pcx=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.avi=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.mov=01;35:*.qt=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.pdf=00;32:*.ps=00;32:*.txt=00;32:*.patch=00;32:*.diff=00;32:*.log=00;32:*.tex=00;32:*.doc=00;32:*.mp3=00;36:*.wav=00;36:*.mid=00;36:*.midi=00;36:*.au=00;36:*.ogg=00;36:*.flac=00;36:*.aac=00;36:'

# SCREENDIR will screw screen up
unset SCREENDIR

# Colours
# 
# I haven't actually used these yet, apart from for reference. You can use
# these when prompting. Capitalised stuff is bold.

export red=$'%{\e[0;31m%}'
export RED=$'%{\e[1;31m%}'
export green=$'%{\e[0;32m%}'
export GREEN=$'%{\e[1;32m%}'
export blue=$'%{\e[0;34m%}'
export BLUE=$'%{\e[1;34m%}'
export purple=$'%{\e[0;35m%}'
export PURPLE=$'%{\e[1;35m%}'
export cyan=$'%{\e[0;36m%}'
export CYAN=$'%{\e[1;36m%}'
export WHITE=$'%{\e[1;37m%}'
export white=$'%{\e[0;37m%}'
export NC=$'%{\e[0m%}' 
export yellow=$'%{\e[1;33m%}'

# Make sure no cores can be dumped while zsh is in charge. I don't know if
# this limit thing uses ulimit or what, but it seems to work..
limit coredumpsize 0

compctl -g '*(-/)' + -g '.*(-/)' -v cd pushd rmdir
compctl -k hosts -x 'p[2,-1]' -l '' -- rsh ssh

# completion for "man" by Gossamer <gossamer@tertius.net.au> 980827
# This is damn funky. I'm going to do something similar for pinfo,
# hopefully.
compctl -f -x 'S[1][2][3][4][5][6][7][8][9]' -k '(1 2 3 4 5 6 7 8 9)' \
  - 'R[[1-9nlo]|[1-9](|[a-z]),^*]' -K 'match-man' \
  - 's[-M],c[-1,-M]' -g '*(-/)' \
  - 's[-P],c[-1,-P]' -c \
  - 's[-S],s[-1,-S]' -k '( )' \
  - 's[-]' -k '(a d f h k t M P)' \
  - 'p[1,-1]' -c + -K 'match-man' \
  -- man


# Completition
compctl -b bindkey
compctl -v export
compctl -o setopt
compctl -v unset
compctl -o unsetopt
compctl -v vared
compctl -c which
compctl -c sudo

# History things
HISTFILE=$HOME/.zshist
SAVEHIST=500
HISTSIZE=800
TMPPREFIX=/tmp

# Key bindings, useful.
bindkey "\e[3~" delete-char
bindkey "\e[2~" yank        #insert
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[5~" history-search-backward
bindkey "\e[6~" history-search-forward 

# Aliases, how I love thee
alias ls="ls -GaphF"
alias ll="ls -lGaphF"
alias emacs="emacsclient"
alias sshfs="/Applications/sshfs/bin/mount_sshfs"
alias unix-timestamp="date +%s"
alias activate-torrents="(cd ~/Downloads && mv *torrent torrents) 2> /dev/null"
alias activate-nzb="(cd ~/Downloads && mv *.nzb nzb) 2> /dev/null"
alias update-gob="ssh root@gob.flatworldknowledge.com \
                  \"cd /var/www/bert/ && svn up && drush cache clear\""


# Set the titlebar for the window, and also the window title in screen :)
#
# I don't want to be cocky, but this is all my own stuff and I'm frikking
# proud of it.

# This function sets the window tile to user@host:/workingdir before each
# prompt. If you're using screen, it sets the window title (works
# wonderfully for hardstatus lines :)
precmd () {
  [[ -t 1 ]] || return
  case $TERM in
    *xterm*|rxvt|urxvt|rxvt-unicode|(dt|k|E|a)term) print -Pn "\e]2;%n@%m:%~\a"
    ;;
    screen*) print -Pn "\e\"%n@%m:%~\e\134"
  esac
}

# This sets the window title to the last run command.
[[ -t 1 ]] || return
case $TERM in
  *xterm*|rxvt|(dt|k|E|a)term)
    preexec () {
      print -Pn "\e]2;$1\a"
    }
  ;;
  screen*)
    preexec () {
      print -Pn "\e\"$1\e\134"
    }
  ;;
esac

# Setting HOME and END keys 
case $TERM in (xterm*|aterm|rxvt|Eterm)
        bindkey '^[Od' emacs-backward-word
        bindkey '^[Oc' emacs-forward-word
	bindkey '^[[7~' beginning-of-line
	bindkey '^[[8~' end-of-line ;;
esac

# Fixes Savage mouse problems.
export SDL_VIDEO_X11_DGAMOUSE=0

# The following lines were added by compinstall

# zstyle ':completion:*' completer _complete _approximate
# zstyle ':completion:*' format 'Completing %d'
# zstyle ':completion:*' group-name ''
# zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
# zstyle ':completion:*' matcher-list '+m:{a-z}={A-Z} r:|[._-]=* r:|=*' 'm:{a-z}={A-Z} r:|[._-]=* r:|=*' 'm:{a-z}={A-Z} r:|[._-]=* r:|=*' 'm:{a-z}={A-Z} r:|[._-]=* r:|=*'
# zstyle ':completion:*' max-errors 2
# zstyle ':completion:*' menu select=1
# zstyle ':completion:*' prompt '%{\e[0;31m%}%e%{\e[0m%}'
# zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
# zstyle ':completion:*' squeeze-slashes true
# zstyle :compinstall filename '/home/slarti/.zshrc'
# zstyle ':completion:*' expand 'yes'
# zstyle ':completion::complete:*' '\'

# Select Prompt
zstyle ':completion:*' menu select=1

# Expansion options
zstyle ':completion:*' completer _complete _prefix
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:predict:*' completer _complete

# Completion caching
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

# Expand partial paths
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'

# Include non-hidden directories in globbed file completions
# for certain commands

zstyle ':completion::complete:*' '\'

# Use menuselection for pid completion
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'

#  tag-order 'globbed-files directories' all-files 
zstyle ':completion::complete:*:tar:directories' file-patterns '*~.*(-/)'

# Don't complete backup files as executables
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '*\~'

# Separate matches into groups
zstyle ':completion:*:matches' group 'yes'

# With commands like rm, it's annoying if you keep getting offered the same
# file multiple times. This fixes it. Also good for cp, et cetera..
zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:cp:*' ignore-line yes

# Describe each match group.
zstyle ':completion:*:descriptions' format "%B---- %d%b"

# Messages/warnings format
zstyle ':completion:*:messages' format '%B%U---- %d%u%b' 
zstyle ':completion:*:warnings' format '%B%U---- no match for: %d%u%b'
 
# Describe options in full
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'

# Simulate spider's old abbrev-expand 3.0.5 patch 
zstyle ':completion:*:history-words' stop verbose
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false

# Follow GNU LS_COLORS
zmodload -i zsh/complist
#eval "`dircolors ~/.dir_colors`"
export ZLSCOLORS="${LS_COLORS}"
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:*:kill:*' list-colors '=%*=01;31'

# zsh Options. Big long lovely way of setting them.

setopt                       \
     NO_all_export           \
        always_last_prompt   \
        always_to_end        \
        append_history       \
        auto_cd              \
        auto_list            \
        auto_menu            \
        auto_name_dirs       \
        auto_param_keys      \
        auto_param_slash     \
        auto_pushd           \
        auto_remove_slash    \
     NO_auto_resume          \
        bad_pattern          \
        bang_hist            \
     NO_beep                 \
        brace_ccl            \
        correct_all          \
     NO_bsd_echo             \
        cdable_vars          \
     NO_chase_links          \
        clobber              \
        complete_aliases     \
        complete_in_word     \
        correct              \
     NO_correct_all          \
        csh_junkie_history   \
     NO_csh_junkie_loops     \
     NO_csh_junkie_quotes    \
     NO_csh_null_glob        \
        equals               \
        extended_glob        \
        extended_history     \
        function_argzero     \
        glob                 \
     NO_glob_assign          \
        glob_complete        \
     NO_glob_dots            \
        glob_subst           \
     NO_hash_cmds            \
     NO_hash_dirs            \
        hash_list_all        \
        hist_allow_clobber   \
        hist_beep            \
        hist_ignore_dups     \
        hist_ignore_space    \
     NO_hist_no_store        \
        hist_verify          \
     NO_hup                  \
     NO_ignore_braces        \
     NO_ignore_eof           \
        interactive_comments \
      	inc_append_history   \
     NO_list_ambiguous       \
     NO_list_beep            \
        list_types           \
        long_list_jobs       \
        magic_equal_subst    \
     NO_mail_warning         \
     NO_mark_dirs            \
        menu_complete        \
        multios              \
        nomatch              \
        notify               \
     NO_null_glob            \
        numeric_glob_sort    \
     NO_overstrike           \
        path_dirs            \
        posix_builtins       \
     NO_print_exit_value     \
     NO_prompt_cr            \
        prompt_subst         \
        pushd_ignore_dups    \
     NO_pushd_minus          \
        pushd_silent         \
        pushd_to_home        \
        rc_expand_param      \
     NO_rc_quotes            \
     NO_rm_star_silent       \
     NO_sh_file_expansion    \
        sh_option_letters    \
        short_loops          \
     NO_sh_word_split        \
     NO_single_line_zle      \
     NO_sun_keyboard_hack    \
        unset                \
     NO_verbose              \
        zle

unsetopt correct \
    correct_all

    