if !exists('g:vlime_contrib_initializers')
    let g:vlime_contrib_initializers = {
                \ 'SWANK-REPL': function('vlime#contrib#InitSwankREPL'),
                \ 'SWANK-PRESENTATIONS': function('vlime#contrib#InitSwankPresentations'),
                \ }
endif

" vlime#contrib#CallInitializers(conn[, callback])
function! vlime#contrib#CallInitializers(conn, ...)
    let Callback = vlime#GetNthVarArg(a:000, 0, v:null)

    let contribs = get(a:conn.cb_data, 'contribs', [])
    for c in contribs
        let InitFunc = get(g:vlime_contrib_initializers, c, v:null)
        if type(InitFunc) != v:t_func && exists('g:vlime_user_contrib_initializers')
            let InitFunc = get(g:vlime_user_contrib_initializers, c, v:null)
        endif
        if type(InitFunc) == v:t_func
            let ToCall = function(InitFunc, [a:conn])
            call ToCall()
        endif
    endfor

    if type(Callback) == v:t_func
        let ToCall = function(Callback, [a:conn])
        call ToCall()
    endif
endfunction

function! vlime#contrib#InitSwankPresentations(conn)
    let a:conn['server_event_handlers']['PRESENTATION-START'] =
                \ function('s:OnPresentationStart')
    let a:conn['server_event_handlers']['PRESENTATION-END'] =
                \ function('s:OnPresentationEnd')
    call a:conn.Send(a:conn.EmacsRex(
                    \ [{'package': 'SWANK', 'name': 'INIT-PRESENTATIONS'}]),
                \ function('vlime#SimpleSendCB',
                    \ [a:conn, v:null, 'vlime#contrib#InitSwankPresentations']))
endfunction

function! vlime#contrib#InitSwankREPL(conn)
    call a:conn.CreateREPL(v:null)
endfunction

function! s:OnPresentationStart(conn, msg)
    let repl_buf = bufnr(vlime#ui#REPLBufName(a:conn))
    if repl_buf < 0
        return
    endif

    let coords = getbufvar(repl_buf, 'vlime_repl_coords', {})
    let begin_pos = vlime#ui#WithBuffer(repl_buf, function('s:GetBufLastPos'))
    let coords[a:msg[1]] = {
                \ 'begin': begin_pos,
                \ 'type': 'PRESENTATION',
                \ 'id': a:msg[1],
                \ }
    call setbufvar(repl_buf, 'vlime_repl_coords', coords)
endfunction

function! s:OnPresentationEnd(conn, msg)
    let repl_buf = bufnr(vlime#ui#REPLBufName(a:conn))
    if repl_buf < 0
        return
    endif

    let coords = getbufvar(repl_buf, 'vlime_repl_coords', {})
    let c = get(coords, a:msg[1], v:null)
    if type(c) == v:t_none
        return
    endif

    let end_pos = vlime#ui#WithBuffer(repl_buf, function('s:GetBufLastPos'))
    let c['end'] = end_pos
    call setbufvar(repl_buf, 'vlime_repl_coords', coords)
endfunction

function! s:GetBufLastPos()
    let old_pos = getcurpos()
    try
        normal! G$
        let pos = getcurpos()
        return [pos[1], pos[2]]
    finally
        call setpos('.', old_pos)
    endtry
endfunction

