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
                \ vlime#ui#GetUI())
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
    let r_conn.cb_data['name'] = a:new_name
endfunction

function! VlimeSelectConnection(quiet)
    if len(g:vlime_connections) == 0
        if !a:quiet
            call vlime#ui#ErrMsg('Vlime not connected.')
        endif
        return v:null
    else
        let conn_names = []
        for k in sort(keys(g:vlime_connections), 'n')
            let conn = g:vlime_connections[k]
            let chan_info = ch_info(conn.channel)
            call add(conn_names, k . '. ' . conn.cb_data['name'] .
                        \ ' (' . chan_info['hostname'] . ':' . chan_info['port'] . ')')
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
            if type(conn) == v:t_none
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

" VlimeGetConnection([quiet])
function! VlimeGetConnection(...) abort
    let quiet = vlime#GetNthVarArg(a:000, 0, v:false)

    if !exists('b:vlime_conn') ||
                \ (type(b:vlime_conn) != v:t_none &&
                    \ !b:vlime_conn.IsConnected()) ||
                \ (type(b:vlime_conn) == v:t_none && !quiet)
        if len(g:vlime_connections) == 1 && !exists('b:vlime_conn')
            let b:vlime_conn = g:vlime_connections[keys(g:vlime_connections)[0]]
        else
            let conn = VlimeSelectConnection(quiet)
            if type(conn) == v:t_none
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

let s:vlime_home = expand('<sfile>:p:h:h:h')

function! VlimeNewServer()
    let job = job_start(
                \ 'sbcl --load /home/l_amee/workspace/ql_env/dummy/setup.lisp --load ' .
                    \ s:vlime_home . '/lisp/load-vlime.lisp --eval (vlime:main)',
                \ {'in_io': 'pipe',
                    \ 'out_io': 'buffer',
                    \ 'err_io': 'buffer',
                    \ 'out_name': 'sbcl_test_2',
                    \ 'err_name': 'sbcl_test_2',
                    \ 'in_mode': 'nl',
                    \ 'out_mode': 'nl',
                    \ 'err_mode': 'nl',
                    \ 'out_modifiable': 0,
                    \ 'err_modifiable': 0})
    let lisp_buf = ch_getbufnr(job, 'out')
    call vlime#ui#OpenBuffer(lisp_buf, v:false, 'botright split')
    call timer_start(500, function('s:CheckServerPort', [lisp_buf]), {'repeat': -1})
    return job
endfunction

function! s:CheckServerPort(lisp_buf, timer)
    function! s:DoCheckServerPort()
        let pattern = 'Server created: (#([[:digit:][:blank:]]\+)\s\+\(\d\+\))'
        let old_pos = getcurpos()
        try
            call setpos('.', [0, 1, 1, 0, 1])
            let port_line_nr = search(pattern, 'n')
        finally
            call setpos('.', old_pos)
        endtry
        if port_line_nr > 0
            let port_line = getline(port_line_nr)
            let matched = matchlist(port_line, pattern)
            return str2nr(matched[1])
        else
            return v:null
        endif
    endfunction

    let port = vlime#ui#WithBuffer(a:lisp_buf, function('s:DoCheckServerPort'))
    if type(port) == v:t_none
        return
    else
        call timer_stop(a:timer)
        echom 'Vlime server listening on port ' . port
        call VlimeConnectREPL('127.0.0.1', port)
    endif
endfunction

function! s:NormalizeConnectionID(id)
    if type(a:id) == v:t_dict
        return a:id.cb_data['id']
    else
        return a:id
    endif
endfunction
