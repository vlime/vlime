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
            finally
                call win_gotoid(old_win_id)
            endtry
        endif
    endif
    return repl_buf
endfunction

function! vlime#ui#repl#AppendOutput(repl_buf, str)
    let repl_winnr = bufwinnr(a:repl_buf)
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

