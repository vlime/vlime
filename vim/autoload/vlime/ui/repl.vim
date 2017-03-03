function! vlime#ui#repl#InitREPLBuf(conn)
    let repl_buf = bufnr(vlime#ui#REPLBufName(a:conn), v:true)
    if !vlime#ui#VlimeBufferInitialized(repl_buf)
        call vlime#ui#SetVlimeBufferOpts(repl_buf, a:conn)
        call vlime#ui#WithBuffer(repl_buf, function('s:InitREPLBuf'))
    endif
    return repl_buf
endfunction

function! vlime#ui#repl#AppendOutput(repl_buf, str)
    let repl_winnr = bufwinnr(a:repl_buf)
    call setbufvar(a:repl_buf, '&modifiable', 1)
    if repl_winnr > 0
        " If the REPL buffer is visible, move to that window to enable
        " automatic scrolling
        let old_win_id = win_getid()
        try
            execute repl_winnr . 'wincmd w'
            call vlime#ui#AppendString(a:str)
        finally
            call win_gotoid(old_win_id)
        endtry
    else
        call vlime#ui#WithBuffer(a:repl_buf,
                    \ function('vlime#ui#AppendString', [a:str]))
    endif
    call setbufvar(a:repl_buf, '&modifiable', 0)
endfunction

function! vlime#ui#repl#InspectCurREPLPresentation()
    let p_coord = s:FindCurCoord(
                \ getcurpos(), getbufvar('%', 'vlime_repl_coords', {}))
    if type(p_coord) == v:t_none
        return
    endif

    if p_coord['type'] == 'PRESENTATION'
        call b:vlime_conn.InspectPresentation(
                    \ p_coord['id'], v:true,
                    \ {c, r -> c.ui.OnInspect(c, r, v:null, v:null)})
    endif
endfunction

function! vlime#ui#repl#YankCurREPLPresentation()
    let p_coord = s:FindCurCoord(
                \ getcurpos(), getbufvar('%', 'vlime_repl_coords', {}))
    if type(p_coord) == v:t_none
        return
    endif

    if p_coord['type'] == 'PRESENTATION'
        let @" = '(swank:lookup-presented-object ' . p_coord['id'] . ')'
        echom 'Presented object yanked.'
    endif
endfunction

function! vlime#ui#repl#ClearREPLBuffer()
    setlocal modifiable
    1,$delete _
    unlet b:vlime_repl_coords
    call s:ShowREPLBanner(b:vlime_conn)
    setlocal nomodifiable
endfunction

function! s:ShowREPLBanner(conn)
    let banner = 'SWANK'
    if has_key(a:conn.cb_data, 'version')
        let banner .= ' version ' . a:conn.cb_data['version']
    endif
    if has_key(a:conn.cb_data, 'pid')
        let banner .= ', pid ' . a:conn.cb_data['pid']
    endif
    let banner_len = len(banner)
    let banner .= ("\n" . repeat('=', banner_len) . "\n")
    call vlime#ui#AppendString(banner)
endfunction

function! s:FindCurCoord(cur_pos, coords)
    for k in keys(a:coords)
        let c = a:coords[k]
        if vlime#ui#MatchCoord(c, a:cur_pos[1], a:cur_pos[2])
            return c
        endif
    endfor
    return v:null
endfunction

function! s:InitREPLBuf()
    setlocal modifiable
    call s:ShowREPLBanner(b:vlime_conn)
    setlocal nomodifiable

    nnoremap <buffer> <silent> <c-c>
                \ :call b:vlime_conn.Interrupt(
                    \ {'name': 'REPL-THREAD', 'package': 'KEYWORD'})<cr>
    nnoremap <buffer> <silent> <LocalLeader>I
                \ :call vlime#ui#repl#InspectCurREPLPresentation()<cr>
    nnoremap <buffer> <silent> <LocalLeader>y
                \ :call vlime#ui#repl#YankCurREPLPresentation()<cr>
    nnoremap <buffer> <silent> <LocalLeader>C
                \ :call vlime#ui#repl#ClearREPLBuffer()<cr>
endfunction
