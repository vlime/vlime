function! vlime#ui#mrepl#InitMREPLBuf(conn, chan_obj)
    let mrepl_buf = bufnr(vlime#ui#MREPLBufName(a:conn, a:chan_obj), v:true)
    if !vlime#ui#VlimeBufferInitialized(mrepl_buf)
        call vlime#ui#SetVlimeBufferOpts(mrepl_buf, a:conn)
        call setbufvar(mrepl_buf, 'vlime_mrepl_channel', a:chan_obj)
        call setbufvar(mrepl_buf, '&filetype', 'vlime_mrepl')
        call vlime#ui#WithBuffer(mrepl_buf, function('s:InitMREPLBuf'))
    endif
    return mrepl_buf
endfunction

function! s:InitMREPLBuf()
    " TODO
endfunction
