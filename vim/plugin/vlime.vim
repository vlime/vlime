if !exists('g:buffer_package_map')
    let g:buffer_package_map = {}
endif

if !exists('g:buffer_thread_map')
    let g:buffer_thread_map = {}
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
                \ function('s:BufferPackageGetter'),
                \ function('s:BufferPackageSetter'),
                \ function('s:BufferThreadGetter'),
                \ function('s:BufferThreadSetter'))
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
    let expr = CurExpr()
    if len(expr) > 0
        let conn = VlimeGetConnection()
        call conn.WithThread({'name': 'REPL-THREAD', 'package': 'KEYWORD'},
                    \ conn.ListenerEval, [expr])
    endif
endfunction

function! VlimeSendCurAtomToREPL()
    let atom = CurAtom()
    if len(atom) > 0
        let conn = VlimeGetConnection()
        call conn.WithThread({'name': 'REPL-THREAD', 'package': 'KEYWORD'},
                    \ conn.ListenerEval, [atom])
    endif
endfunction

function! CurChar()
    return matchstr(getline('.'), '\%' . col('.') . 'c.')
endfunction

function! CurAtom()
    let old_kw = &iskeyword
    setlocal iskeyword+=+,-,*,/,%,<,=,>,:,$,?,!,@-@,94,~,#,\|,&,.,{,},[,]
    let atom = expand('<cword>')
    let &l:iskeyword = old_kw
    return atom
endfunction

function! CurExpr()
    let cur_char = CurChar()
    if cur_char == '('
        let [s_line, s_col] = searchpairpos('(', '', ')', 'cbnW')
        let [e_line, e_col] = searchpairpos('(', '', ')', 'nW')
    elseif cur_char == ')'
        let [s_line, s_col] = searchpairpos('(', '', ')', 'bnW')
        let [e_line, e_col] = searchpairpos('(', '', ')', 'cnW')
    else
        let [s_line, s_col] = searchpairpos('(', '', ')', 'bnW')
        let [e_line, e_col] = searchpairpos('(', '', ')', 'nW')
    endif
    let lines = getline(s_line, e_line)
    if len(lines) == 1
        let lines[0] = strpart(lines[0], s_col - 1, e_col - s_col + 1)
    elseif len(lines) > 1
        let lines[0] = strpart(lines[0], s_col - 1)
        let lines[-1] = strpart(lines[-1], 0, e_col)
    endif
    return join(lines, ' ')
endfunction

function! CurInPackage()
    let pattern = '^\s*(\_s*in-package\_s\+\(.\+\)\_s*)'
    let old_cur_pos = getcurpos()
    let package_line = search(pattern, 'bcW')
    if package_line <= 0
        let package_line = search(pattern, 'cW')
    endif
    if package_line > 0
        let matches = matchlist(CurExpr(), pattern)
        let package = s:NormalizePackageName(matches[1])
    else
        let package = ''
    endif
    call setpos('.', old_cur_pos)
    return package
endfunction

function! s:NormalizePackageName(name)
    let pattern1 = '^\(\(#\?:\)\|''\)\(.\+\)'
    let pattern2 = '"\(.\+\)"'
    let matches = matchlist(a:name, pattern1)
    let r_name = ''
    if len(matches) > 0
        let r_name = matches[3]
    else
        let matches = matchlist(a:name, pattern2)
        if len(matches) > 0
            let r_name = matches[1]
        endif
    endif
    return toupper(r_name)
endfunction

function! s:NormalizeConnectionID(id)
    if type(a:id) == v:t_dict
        return a:id.cb_data.id
    else
        return a:id
    endif
endfunction

function! s:BufferPackageGetter() dict
    let cur_buf = bufnr('%')
    let buf_pkg = get(g:buffer_package_map, cur_buf, v:null)
    if type(buf_pkg) != v:t_list
        let in_pkg = CurInPackage()
        if len(in_pkg) > 0
            let buf_pkg = [in_pkg, in_pkg]
        else
            let buf_pkg = ['COMMON-LISP-USER', 'CL-USER']
        endif
    endif
    return buf_pkg
endfunction

function! s:BufferPackageSetter(pkg) dict
    let cur_buf = bufnr('%')
    let g:buffer_package_map[cur_buf] = a:pkg
endfunction

function! s:BufferThreadGetter() dict
    let cur_buf = bufnr('%')
    let buf_thread = get(g:buffer_thread_map, cur_buf, v:null)
    if type(buf_thread) == v:t_none
        let buf_thread = v:true
    endif
    return buf_thread
endfunction

function! s:BufferThreadSetter(thread) dict
    let cur_buf = bufnr('%')
    let g:buffer_thread_map[cur_buf] = a:thread
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
