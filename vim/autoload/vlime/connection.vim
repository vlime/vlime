if !exists('g:vlime_connections')
    let g:vlime_connections = {}
endif

if !exists('g:vlime_next_conn_id')
    let g:vlime_next_conn_id = 1
endif

" vlime#connection#New([name])
function! vlime#connection#New(...)
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
                \ vlime#ui#GetUI())
    let g:vlime_connections[g:vlime_next_conn_id] = conn
    let g:vlime_next_conn_id += 1
    return conn
endfunction

function! vlime#connection#Close(conn)
    let conn_id = s:NormalizeConnectionID(a:conn)
    let r_conn = remove(g:vlime_connections, conn_id)
    call r_conn.Close()
endfunction

function! vlime#connection#Rename(conn, new_name)
    let conn_id = s:NormalizeConnectionID(a:conn)
    let r_conn = g:vlime_connections[conn_id]
    let r_conn.cb_data['name'] = a:new_name
endfunction

function! vlime#connection#Select(quiet)
    if len(g:vlime_connections) == 0
        if !a:quiet
            call vlime#ui#ErrMsg('Vlime not connected.')
        endif
        return v:null
    else
        let cur_conn = getbufvar('%', 'vlime_conn', v:null)
        let cur_conn_id = (type(cur_conn) == type(v:null)) ? -1 : cur_conn.cb_data['id']

        let conn_names = []
        for k in sort(keys(g:vlime_connections), 'n')
            let conn = g:vlime_connections[k]
            let chan_info = vlime#compat#ch_info(conn.channel)
            let disp_name = k . '. ' . conn.cb_data['name'] .
                        \ ' (' . chan_info['hostname'] . ':' . chan_info['port'] . ')'
            if cur_conn_id == conn.cb_data['id']
                let disp_name .= ' *'
            endif
            call add(conn_names, disp_name)
        endfor

        echohl Question
        echom 'Which connection to use?'
        echohl None
        let conn_nr = inputlist(conn_names)
        if conn_nr == 0
            if !a:quiet
                call vlime#ui#ErrMsg('Canceled.')
            endif
            return v:null
        else
            let conn = get(g:vlime_connections, conn_nr, v:null)
            if type(conn) == type(v:null)
                if !a:quiet
                    call vlime#ui#ErrMsg('Invalid connection ID: ' . conn_nr)
                endif
                return v:null
            else
                return conn
            endif
        endif
    endif
endfunction

" vlime#connection#Get([quiet])
function! vlime#connection#Get(...) abort
    let quiet = get(a:000, 0, v:false)

    if !exists('b:vlime_conn') ||
                \ (type(b:vlime_conn) != type(v:null) &&
                    \ !b:vlime_conn.IsConnected()) ||
                \ (type(b:vlime_conn) == type(v:null) && !quiet)
        if len(g:vlime_connections)
            let b:vlime_conn = g:vlime_connections[keys(g:vlime_connections)[0]]
        else
            let conn = vlime#connection#Select(quiet)
            if type(conn) == type(v:null)
                if quiet
                    " No connection found. Set this variable to v:null to
                    " make it 'quiet'
                    let b:vlime_conn = conn
                else
                    return conn
                endif
            else
                let b:vlime_conn = conn
            endif
        endif
    endif
    return b:vlime_conn
endfunction

function! s:NormalizeConnectionID(id)
    if type(a:id) == v:t_dict
        return a:id.cb_data['id']
    else
        return a:id
    endif
endfunction
