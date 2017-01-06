function! vlime#ui#repl#InitREPLBuf(conn)
    let repl_buf = vlime#ui#OpenBuffer(
                \ vlime#ui#REPLBufName(a:conn), v:true, v:false)
    if repl_buf > 0
        if !getbufvar(repl_buf, 'vlime_buffer_initialized', v:false)
            call setbufvar(repl_buf, 'vlime_buffer_initialized', v:true)
            call vlime#ui#SetVlimeBufferOpts(repl_buf, a:conn)
            let old_win_id = win_getid()
            try
                call vlime#ui#OpenBuffer(repl_buf, v:false, 'botright split')
                call s:ShowREPLBanner(a:conn)
                nnoremap <buffer> <c-c>
                            \ :call b:vlime_conn.Interrupt(
                                \ {'name': 'REPL-THREAD', 'package': 'KEYWORD'})<cr>
                nnoremap <buffer> <LocalLeader>I
                            \ :call vlime#ui#repl#InspectCurREPLPresentation()<cr>
            finally
                call win_gotoid(old_win_id)
            endtry
            call setbufvar(repl_buf, '&modifiable', 0)
        endif
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
    let cur_pos = getcurpos()
    let coords = getbufvar('%', 'vlime_repl_coords', {})
    let p_coord = v:null
    for k in keys(coords)
        let c = coords[k]
        if vlime#ui#MatchCoord(c, cur_pos[1], cur_pos[2])
            let p_coord = c
            break
        endif
    endfor

    if type(p_coord) == v:t_none
        return
    endif

    if p_coord['type'] == 'PRESENTATION'
        call b:vlime_conn.Send(b:vlime_conn.EmacsRex(
                        \ [{'package': 'SWANK', 'name': 'INSPECT-PRESENTATION'}, p_coord['id'], v:true]),
                    \ function('vlime#SimpleSendCB',
                        \ [b:vlime_conn,
                            \ {c, r -> c.ui.OnInspect(c, r, v:null, v:null)},
                            \ 'vlime#contrib#SwankPresentationsInit']))
    endif
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

