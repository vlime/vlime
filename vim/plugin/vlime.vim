if !exists('g:buffer_package_map')
    let g:buffer_package_map = {}
endif

if !exists('g:buffer_thread_map')
    let g:buffer_thread_map = {}
endif

if !exists('g:vlime_connections')
    let g:vlime_connections = []
endif


function! s:NormalizePackageName(name)
    let i = 0
    while i < len(a:name) && (a:name[i] == '#' || a:name[i] == ':')
        let i += 1
    endwhile
    let r_name = toupper(strpart(a:name, i))
    return r_name
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
    echom 'buf_thread = ' . string(buf_thread)
    return buf_thread
endfunction

function! s:BufferThreadSetter(thread) dict
    let cur_buf = bufnr('%')
    let g:buffer_thread_map[cur_buf] = a:thread
endfunction

"call v.DescribeSymbol('vlime', function('vlime#DummyCB'))
"
"call v.SLDBAbort(6, function('vlime#DummyCB'))
"call v.InvokeNthRestartForEmacs(2, 1, 0, function('vlime#DummyCB'))
"
"call v.SetPackage('vlime', function('vlime#DummyCB'))

function! s:OnCreateREPLComplete(conn, result)
    echom '-- OnCreateREPLComplete -------------------------'
    echom string(a:result)
    call add(g:vlime_connections, a:conn)
endfunction

function! s:OnSwankRequireComplete(conn, result)
    echom '-- OnSwankRequireComplete -------------------------'
    echom string(a:result)
    call a:conn.CreateREPL(v:null, function('s:OnCreateREPLComplete', [a:conn]))
endfunction

function! VlimeConnectREPL()
    let vlime = vlime#New(
                \ function('s:BufferPackageGetter'),
                \ function('s:BufferPackageSetter'),
                \ function('s:BufferThreadGetter'),
                \ function('s:BufferThreadSetter'))
    call vlime.Connect('127.0.0.1', 7002)
    call vlime.SwankRequire(['SWANK-REPL'], function('s:OnSwankRequireComplete', [vlime]))
endfunction

function! GetVlimeConnection()
    if !exists('b:vlime_conn')
        if len(g:vlime_connections) == 0
            throw 'GetVlimeConnection: not connected'
        elseif len(g:vlime_connections) == 1
            let b:vlime_conn = g:vlime_connections[0]
        else
            conn_nr = input('Which connection to use? ')
            let b:vlime_conn = g:vlime_connections[conn_nr]
        endif
    endif
    return b:vlime_conn
endfunction

function! CurChar()
    return matchstr(getline('.'), '\%' . col('.') . 'c.')
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

function! SendCurExprToREPL()
    let expr = CurExpr()
    if len(expr) > 0
        let conn = GetVlimeConnection()
        call conn.WithThread({'name': 'REPL-THREAD', 'package': 'KEYWORD'},
                    \ conn.ListenerEval, [expr])
    endif
endfunction
