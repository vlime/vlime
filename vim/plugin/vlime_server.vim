if !exists('g:vlime_cl_wait_time')
    let g:vlime_cl_wait_time = 10 " seconds
endif

if !exists('g:vlime_cl_wait_interval')
    let g:vlime_cl_wait_interval = 500 " milliseconds
endif

if !exists('g:vlime_servers')
    let g:vlime_servers = {}
endif

if !exists('g:vlime_next_server_id')
    let g:vlime_next_server_id = 1
endif

let s:cur_src_path = resolve(expand('<sfile>:p'))
let s:vlime_home = fnamemodify(s:cur_src_path, ':h:h:h')
let s:path_sep = s:cur_src_path[len(s:vlime_home)]

" VlimeNewServer([auto_connect[, name]])
function! VlimeNewServer(...)
    let auto_connect = vlime#GetNthVarArg(a:000, 0, v:true)
    let name = vlime#GetNthVarArg(a:000, 1, v:null)

    if type(name) != type(v:null)
        let server_name = name
    else
        let server_name = 'Vlime Server ' . g:vlime_next_server_id
    endif

    let server_job = vlime#compat#job_start(
                \ VlimeBuildServerCommand(),
                \ vlime#ui#ServerBufName(server_name))
    if vlime#compat#job_status(server_job) != 'run'
        throw 'VlimeNewServer: failed to start server job'
    endif

    let server_obj = {
                \ 'id': g:vlime_next_server_id,
                \ 'name': server_name,
                \ 'job': server_job,
                \ }
    let g:vlime_servers[g:vlime_next_server_id] = server_obj
    let g:vlime_next_server_id += 1

    let lisp_buf = vlime#compat#job_getbufnr(server_job)
    call setbufvar(lisp_buf, '&filetype', 'vlime_server')
    call vlime#ui#OpenBufferWithWinSettings(lisp_buf, v:false, 'server')

    call vlime#ui#EnsureKeyMapped('n', '<LocalLeader>c', ':call VlimeConnectToCurServer()<cr>')
    call vlime#ui#EnsureKeyMapped('n', '<LocalLeader>s', ':call VlimeStopCurServer()<cr>')

    let server_obj['timer'] = timer_start(g:vlime_cl_wait_interval,
                \ function('s:CheckServerPort',
                    \ [server_obj, lisp_buf, auto_connect]),
                \ {'repeat': -1})
    call setbufvar(lisp_buf, 'vlime_server', server_obj)

    return server_obj
endfunction

function! VlimeStopServer(server)
    let server_id = s:NormalizeServerID(a:server)
    let r_server = g:vlime_servers[server_id]

    let timer = get(r_server, 'timer', v:null)
    if type(timer) != type(v:null)
        call timer_stop(timer)
    endif
    if !vlime#compat#job_stop(r_server['job'])
        call vlime#ui#ErrMsg('VlimeStopServer: failed to stop ' . r_server['name'])
    else
        let r_server['timer'] = timer_start(g:vlime_cl_wait_interval,
                    \ function('s:CheckServerStopped', [r_server]),
                    \ {'repeat': -1})
    endif
endfunction

function! VlimeRenameServer(server, new_name)
    let server_id = s:NormalizeServerID(a:server)
    let r_server = g:vlime_servers[server_id]
    let old_buf_name = vlime#ui#ServerBufName(r_server['name'])
    let r_server['name'] = a:new_name
    let old_buf = bufnr(old_buf_name)
    call vlime#ui#WithBuffer(old_buf,
                \ function('s:RenameBuffer',
                    \ [vlime#ui#ServerBufName(a:new_name)]))
endfunction

function! VlimeShowServer(server)
    let server_id = s:NormalizeServerID(a:server)
    let r_server = g:vlime_servers[server_id]
    let buf = vlime#compat#job_getbufnr(r_server['job'])
    call vlime#ui#OpenBuffer(buf, v:false, 'botright')
endfunction

function! VlimeSelectServer()
    if len(g:vlime_servers) == 0
        call vlime#ui#ErrMsg('No server started.')
        return v:null
    endif

    let server_names = []
    for k in sort(keys(g:vlime_servers), 'n')
        let server = g:vlime_servers[k]
        let port = get(server, 'port', 0)
        call add(server_names, k . '. ' . server['name'] .
                    \ ' (' . port . ')')
    endfor

    echohl Question
    echom 'Select server:'
    echohl None
    let server_nr = inputlist(server_names)
    if server_nr == 0
        call vlime#ui#ErrMsg('Canceled.')
        return v:null
    else
        let server = get(g:vlime_servers, server_nr, v:null)
        if type(server) == type(v:null)
            call vlime#ui#ErrMsg('Invalid server ID: ' . server_nr)
            return v:null
        else
            return server
        endif
    endif
endfunction

function! VlimeConnectToCurServer()
    let port = v:null
    if vlime#compat#job_status(b:vlime_server['job']) == 'run'
        let port = get(b:vlime_server, 'port', v:null)
        if type(port) == type(v:null)
            " the server is not ready yet, search for the port again
            let port = s:MatchServerCreatedPort()
            if type(port) == type(v:null)
                call vlime#ui#ErrMsg(b:vlime_server['name'] . ' is not ready.')
            else
                let b:vlime_server['port'] = port
            endif
        endif
    else
        call vlime#ui#ErrMsg(b:vlime_server['name'] . ' is not running.')
    endif

    if type(port) == type(v:null)
        return
    endif

    let conn = VlimeConnectREPL('127.0.0.1', port)
    if type(conn) != type(v:null)
        let conn.cb_data['server'] = b:vlime_server
        let conn_list = get(b:vlime_server, 'connections', {})
        let conn_list[conn.cb_data['id']] = conn
        let b:vlime_server['connections'] = conn_list
    endif
endfunction

function! VlimeStopCurServer()
    if type(get(g:vlime_servers, b:vlime_server['id'], v:null)) == type(v:null)
        call vlime#ui#ErrMsg(b:vlime_server['name'] . ' is not running.')
        return
    endif

    let answer = input('Stop server ' . string(b:vlime_server['name']) . '? (y/n) ')
    if tolower(answer) == 'y' || tolower(answer) == 'yes'
        call VlimeStopServer(b:vlime_server)
    else
        call vlime#ui#ErrMsg('Canceled.')
    endif
endfunction

function! VlimeBuildServerCommandFor_sbcl(vlime_loader, vlime_eval)
    return join(['sbcl', '--load', a:vlime_loader, '--eval', a:vlime_eval], ' ')
endfunction

function! VlimeBuildServerCommandFor_ccl(vlime_loader, vlime_eval)
    return join(['ccl', '--load', a:vlime_loader, '--eval', a:vlime_eval], ' ')
endfunction

function! VlimeBuildServerCommand()
    let cl_impl = exists('g:vlime_cl_impl') ? g:vlime_cl_impl : 'sbcl'
    let vlime_loader = join([s:vlime_home, 'lisp', 'load-vlime.lisp'], s:path_sep)

    try
        let Builder = function('VlimeBuildServerCommandFor_' . cl_impl)
    catch /^Vim\%((\a\+)\)\=:E700/  " Unknown function
        throw 'VlimeBuildServerCommand: implementation ' .
                    \ string(cl_impl) . ' not supported'
    endtry

    return Builder(vlime_loader, '(vlime:main)')
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

function! s:CheckServerPort(server, lisp_buf, auto_connect, timer)
    let port = vlime#ui#WithBuffer(a:lisp_buf,
                \ function('s:MatchServerCreatedPort'))
    if type(port) == type(v:null)
        let timer_count = get(a:server, 'port_timer_count', 1)
        if timer_count >= s:CalcServerCheckTimesLimit()
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

        if a:auto_connect
            let auto_conn = VlimeConnectREPL('127.0.0.1', port)
            if type(auto_conn) != type(v:null)
                let auto_conn.cb_data['server'] = a:server
                let a:server['connections'] =
                            \ {auto_conn.cb_data['id']: auto_conn}
            endif
        endif
    endif
endfunction

function! s:CheckServerStopped(server, timer)
    if vlime#compat#job_status(a:server['job']) == 'run'
        let timer_count = get(a:server, 'stop_timer_count', 1)
        if timer_count >= s:CalcServerCheckTimesLimit()
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

        let conn_dict = get(a:server, 'connections', {})
        for conn_id in keys(conn_dict)
            call VlimeCloseConnection(conn_dict[conn_id])
        endfor
        let a:server['connections'] = {}
    endif
endfunction

function! s:NormalizeServerID(id)
    if type(a:id) == v:t_dict
        return a:id['id']
    else
        return a:id
    endif
endfunction

function! s:RenameBuffer(new_name)
    " Use silent! to supress the 'Cannot rename swapfile' message on Windows
    silent! 0file
    silent! execute 'file ' . escape(a:new_name, ' |\''"')
endfunction

function! s:CalcServerCheckTimesLimit()
    let wait_time_ms = g:vlime_cl_wait_time * 1000
    let times_limit = wait_time_ms / g:vlime_cl_wait_interval
    if wait_time_ms % g:vlime_cl_wait_interval > 0
        let times_limit += 1
    endif
    return times_limit
endfunction
