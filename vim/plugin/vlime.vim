if !exists('g:vlime_ui')
    let g:vlime_ui = vlime#ui#New()
endif

if !exists('g:vlime_connections')
    let g:vlime_connections = {}
endif

if !exists('g:vlime_next_conn_id')
    let g:vlime_next_conn_id = 1
endif

" VlimeNewConnection([name])
function! VlimeNewConnection(...)
    if a:0 > 0
        let conn_name = a:1
    else
        let conn_name = 'Vlime Connection ' . g:vlime_next_conn_id
    endif
    let conn = vlime#New(
                \ {
                    \ 'id': g:vlime_next_conn_id,
                    \ 'name': conn_name
                \ },
                \ g:vlime_ui)
    let g:vlime_connections[g:vlime_next_conn_id] = conn
    let g:vlime_next_conn_id += 1
    return conn
endfunction

function! VlimeCloseConnection(conn)
    let conn_id = s:NormalizeConnectionID(a:conn)
    let r_conn = remove(g:vlime_connections, conn_id)
    call r_conn.Close()
endfunction

function! VlimeRenameConnection(conn, new_name)
    let conn_id = s:NormalizeConnectionID(a:conn)
    let r_conn = g:vlime_connections[conn_id]
    let r_conn.cb_data.name = a:new_name
endfunction

" VlimeConnectREPL([name])
function! VlimeConnectREPL(host, port, ...)
    if a:0 > 0
        let conn = VlimeNewConnection(a:1)
    else
        let conn = VlimeNewConnection()
    endif
    call conn.Connect(a:host, a:port)
    call conn.SwankRequire(['SWANK-REPL'], function('s:OnSwankRequireComplete', [conn]))
endfunction

function! VlimeGetConnection()
    if !exists('b:vlime_conn') || !b:vlime_conn.IsConnected()
        if len(g:vlime_connections) == 0
            throw 'VlimeGetConnection: Not connected'
        elseif len(g:vlime_connections) == 1 && !exists('b:vlime_conn')
            let b:vlime_conn = g:vlime_connections[keys(g:vlime_connections)[0]]
        else
            let conn_names = []
            for k in sort(keys(g:vlime_connections), 'n')
                let conn = g:vlime_connections[k]
                let chan_info = ch_info(conn.channel)
                call add(conn_names, k . '. ' . conn.cb_data.name .
                            \ ' (' . chan_info['hostname'] . ':' . chan_info['port'] . ')')
            endfor

            echohl Question
            echom 'Which connection to use?'
            echohl None
            let conn_nr = inputlist(conn_names)
            if conn_nr == 0
                throw 'VlimeGetConnection: canceled'
            else
                let conn = get(g:vlime_connections, conn_nr, v:null)
                if type(conn) == v:t_none
                    throw 'VlimeGetConnection: Invalid connection: ' . conn_nr
                else
                    let b:vlime_conn = conn
                endif
            endif
        endif
    endif
    return b:vlime_conn
endfunction

function! VlimeSendCurExprToREPL()
    let expr = vlime#ui#CurExpr()
    if len(expr) > 0
        let conn = VlimeGetConnection()
        call conn.WithThread({'name': 'REPL-THREAD', 'package': 'KEYWORD'},
                    \ function(conn.ListenerEval, [expr]))
    endif
endfunction

function! VlimeSendCurAtomToREPL()
    let atom = vlime#ui#CurAtom()
    if len(atom) > 0
        let conn = VlimeGetConnection()
        call conn.WithThread({'name': 'REPL-THREAD', 'package': 'KEYWORD'},
                    \ function(conn.ListenerEval, [atom]))
    endif
endfunction

function! s:NormalizeConnectionID(id)
    if type(a:id) == v:t_dict
        return a:id.cb_data.id
    else
        return a:id
    endif
endfunction

function! s:OnCreateREPLComplete(conn, result)
    echom '-- OnCreateREPLComplete -------------------------'
    echom string(a:result)
    echom a:conn.cb_data.name . ' established.'
endfunction

function! s:OnSwankRequireComplete(conn, result)
    echom '-- OnSwankRequireComplete -------------------------'
    echom string(a:result)
    call a:conn.CreateREPL(v:null, function('s:OnCreateREPLComplete', [a:conn]))
endfunction
