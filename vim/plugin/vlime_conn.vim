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

if !exists('g:vlime_servers')
    let g:vlime_servers = {}
endif

if !exists('g:vlime_next_server_id')
    let g:vlime_next_server_id = 1
endif

let s:vlime_home = fnamemodify(resolve(expand('<sfile>:p')), ':h:h:h')

" VlimeNewServer([name])
function! VlimeNewServer(...)
    if a:0 > 0
        let server_name = a:1
    else
        let server_name = 'Vlime Server ' . g:vlime_next_server_id
    endif

    let server_job = job_start(
                \ 'sbcl --load ' .
                    \ s:vlime_home . '/lisp/load-vlime.lisp --eval (vlime:main)',
                \ {'in_io': 'pipe',
                    \ 'out_io': 'buffer',
                    \ 'err_io': 'buffer',
                    \ 'out_name': vlime#ui#ServerBufName(server_name),
                    \ 'err_name': vlime#ui#ServerBufName(server_name),
                    \ 'in_mode': 'nl',
                    \ 'out_mode': 'nl',
                    \ 'err_mode': 'nl',
                    \ 'out_modifiable': 0,
                    \ 'err_modifiable': 0})
    if job_status(server_job) != 'run'
        throw 'VlimeNewServer: failed to start server job'
    endif

    let server_obj = {
                \ 'id': g:vlime_next_server_id,
                \ 'name': server_name,
                \ 'job': server_job,
                \ }
    let g:vlime_servers[g:vlime_next_server_id] = server_obj
    let g:vlime_next_server_id += 1

    let lisp_buf = ch_getbufnr(server_job, 'out')
    call vlime#ui#OpenBuffer(lisp_buf, v:false, 'botright split')
    let server_obj['timer'] = timer_start(500,
                \ function('s:CheckServerPort', [server_obj, lisp_buf]),
                \ {'repeat': -1})

    return server_obj
endfunction

function! VlimeStopServer(server)
    let server_id = s:NormalizeConnectionID(a:server)
    let r_server = g:vlime_servers[server_id]

    let timer = get(r_server, 'timer', v:null)
    if type(timer) != v:t_none
        call timer_stop(timer)
    endif
    if !job_stop(r_server['job'])
        call vlime#ui#ErrMsg('VlimeStopServer: failed to stop ' . r_server['name'])
    else
        let r_server['timer'] = timer_start(500,
                    \ function('s:CheckServerStopped', [r_server]),
                    \ {'repeat': -1})
    endif
endfunction

function! s:MatchServerCreatedPort()
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

function! s:CheckServerPort(server, lisp_buf, timer)
    let port = vlime#ui#WithBuffer(a:lisp_buf,
                \ function('s:MatchServerCreatedPort'))
    if type(port) == v:t_none
        let timer_count = get(a:server, 'port_timer_count', 0)
        if timer_count >= 20
            call timer_stop(a:timer)
            if get(a:server, 'timer', -1) == a:timer
                call remove(a:server, 'timer')
            endif
            call vlime#ui#ErrMsg('VlimeNewServer: failed to wait for ' .
                        \ a:server['name'] . '. Please inspect server output.')
        else
            let a:server['port_timer_count'] = timer_count + 1
        endif
    else
        call timer_stop(a:timer)
        if get(a:server, 'timer', -1) == a:timer
            call remove(a:server, 'timer')
        endif
        let a:server['port'] = port
        echom 'Vlime server listening on port ' . port
        call VlimeConnectREPL('127.0.0.1', port)
    endif
endfunction

function! s:CheckServerStopped(server, timer)
    if job_status(a:server['job']) == 'run'
        let timer_count = get(a:server, 'stop_timer_count', 0)
        if timer_count >= 20
            if get(a:server, 'timer', -1) == a:timer
                call remove(a:server, 'timer')
            endif
            call timer_stop(a:timer)
            call vlime#ui#ErrMsg('VlimeStopServer: failed to stop ' .
                        \ a:server['name'])
        else
            let a:server['stop_timer_count'] = timer_count + 1
        endif
    else
        if get(a:server, 'timer', -1) == a:timer
            call remove(a:server, 'timer')
        endif
        call timer_stop(a:timer)
        call remove(g:vlime_servers, a:server['id'])
        echom a:server['name'] . ' stopped.'
    endif
endfunction

function! s:NormalizeConnectionID(id)
    if type(a:id) == v:t_dict
        return a:id.cb_data['id']
    else
        return a:id
    endif
endfunction

function! s:NormalizeServerID(id)
    if type(a:id) == v:t_dict
        return a:id['id']
    else
        return a:id
    endif
endfunction
