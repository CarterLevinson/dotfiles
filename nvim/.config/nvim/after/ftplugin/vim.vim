set foldmethod=expr
set foldexpr=VimFolds(v:lnum)
set foldlevel=0

function! VimFolds(lnum)
    " get content of current line and the line below
    let l:cur_line = getline(a:lnum)
    let l:next_line = getline(a:lnum+1)

    if l:cur_line =~# '^"{'
        return '>' . (matchend(l:cur_line, '"{*') - 1)
    else
        if l:cur_line ==# '' && (matchend(l:next_line, '"{*') - 1) == 1
            return 0
        else
            return '='
        endif
    endif
endfunction

function! MyFoldText()
    let line = getline(v:foldstart)
    let folded_line_num = v:foldend - v:foldstart
    let line_text = substitute(line, '^"{\+', '', 'g')
    let fillcharcount = &textwidth - len(line_text) - len(folded_line_num)
    return '+'. repeat('-', 4) . line_text . repeat('.', fillcharcount) . ' (' . folded_line_num . ' L)'
  endfunction
