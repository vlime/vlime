""
" @dict VlimeConnection.InspectPresentation
" @usage {pres_id} {reset} [callback]
" @public
"
" Start inspecting an object saved by SWANK-PRESENTATIONS.
" {pres_id} should be a valid ID presented by PRESENTATION-START messages.
" If {reset} is |TRUE|, the inspector will be reset first.
"
" This method needs the SWANK-PRESENTATIONS contrib module. See
" @function(VlimeConnection.SwankRequire).
function! vlime#contrib#presentations#InspectPresentation(pres_id, reset, ...) dict
    let Callback = get(a:000, 0, v:null)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK', 'INSPECT-PRESENTATION'), a:pres_id, a:reset]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#presentations#InspectPresentation']))
endfunction

function! vlime#contrib#presentations#Init(conn)
    let a:conn['InspectPresentation'] =
                \ function('vlime#contrib#presentations#InspectPresentation')
    let a:conn['server_event_handlers']['PRESENTATION-START'] =
                \ function('s:OnPresentationStart')
    let a:conn['server_event_handlers']['PRESENTATION-END'] =
                \ function('s:OnPresentationEnd')
    call a:conn.Send(a:conn.EmacsRex(
                    \ [vlime#SYM('SWANK', 'INIT-PRESENTATIONS')]),
                \ function('vlime#SimpleSendCB',
                    \ [a:conn, v:null, 'vlime#contrib#presentations#Init']))
endfunction

function! s:OnPresentationStart(conn, msg)
    let repl_buf = bufnr(vlime#ui#REPLBufName(a:conn))
    if repl_buf < 0
        return
    endif

    let coords = getbufvar(repl_buf, 'vlime_repl_coords', {})
    let begin_pos = vlime#ui#WithBuffer(repl_buf,
                \ function('vlime#ui#GetEndOfFileCoord'))
    let c_list = get(coords, a:msg[1], [])
    call add(c_list, {
                \ 'begin': begin_pos,
                \ 'type': 'PRESENTATION',
                \ 'id': a:msg[1],
                \ })
    let coords[a:msg[1]] = c_list
    call setbufvar(repl_buf, 'vlime_repl_coords', coords)
endfunction

function! s:OnPresentationEnd(conn, msg)
    let repl_buf = bufnr(vlime#ui#REPLBufName(a:conn))
    if repl_buf < 0
        return
    endif

    let coords = getbufvar(repl_buf, 'vlime_repl_coords', {})
    let c_list = get(coords, a:msg[1], [])
    let c_pending = v:null
    for c in c_list
        if type(get(c, 'end', v:null)) == type(v:null)
            let c_pending = c
            break
        endif
    endfor

    if type(c_pending) == type(v:null)
        return
    endif

    let end_pos = vlime#ui#WithBuffer(repl_buf,
                \ function('vlime#ui#GetEndOfFileCoord'))
    let c_pending['end'] = end_pos
endfunction
