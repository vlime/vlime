function! VlimeCloseCurConnection()
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    let server = get(conn.cb_data, 'server', v:null)
    if type(server) == type(v:null)
        call VlimeCloseConnection(conn)
        echom conn.cb_data['name'] . ' disconnected.'
    else
        let answer = input('Also stop server ' . string(server['name']) . '? (y/n) ')
        if tolower(answer) == 'y' || tolower(answer) == 'yes'
            call VlimeStopServer(server)
        elseif tolower(answer) == 'n' || tolower(answer) == 'no'
            call VlimeCloseConnection(conn)
            echom conn.cb_data['name'] . ' disconnected.'
            call remove(server['connections'], conn.cb_data['id'])
        else
            call vlime#ui#ErrMsg('Canceled.')
        endif
    endif
endfunction

function! VlimeRenameCurConnection()
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif
    let new_name = input('New name: ', conn.cb_data['name'])
    call VlimeRenameConnection(conn, new_name)
endfunction

function! VlimeShowSelectedServer()
    let server = VlimeSelectServer()
    if type(server) == type(v:null)
        return
    endif
    call VlimeShowServer(server)
endfunction

function! VlimeStopSelectedServer()
    let server = VlimeSelectServer()
    if type(server) == type(v:null)
        return
    endif

    let answer = input('Stop server ' . string(server['name']) . '? (y/n) ')
    if tolower(answer) == 'y' || tolower(answer) == 'yes'
        call VlimeStopServer(server)
    else
        call vlime#ui#ErrMsg('Canceled.')
    endif
endfunction

function! VlimeRenameSelectedServer()
    let server = VlimeSelectServer()
    if type(server) == type(v:null)
        return
    endif
    let new_name = input('New name: ', server['name'])
    call VlimeRenameServer(server, new_name)
endfunction

" VlimeConnectREPL([host[, port[, remote_prefix[, name]]]])
function! VlimeConnectREPL(...)
    let [def_host, def_port] = exists('g:vlime_address') ?
                \ g:vlime_address : ['127.0.0.1', 7002]

    let host = vlime#GetNthVarArg(a:000, 0, def_host)
    let port = vlime#GetNthVarArg(a:000, 1, def_port)
    let remote_prefix = vlime#GetNthVarArg(a:000, 2, '')
    let name = vlime#GetNthVarArg(a:000, 3, v:null)

    if type(name) == type(v:null)
        let conn = VlimeNewConnection()
    else
        let conn = VlimeNewConnection(name)
    endif
    try
        call conn.Connect(host, port, remote_prefix)
    catch
        call VlimeCloseConnection(conn)
        call vlime#ui#ErrMsg(v:exception)
        return v:null
    endtry
    call s:CleanUpNullBufConnections()

    let contribs = exists('g:vlime_contribs') ?
                \ g:vlime_contribs : [
                    \ 'SWANK-ASDF', 'SWANK-PACKAGE-FU',
                    \ 'SWANK-PRESENTATIONS', 'SWANK-FANCY-INSPECTOR',
                    \ 'SWANK-C-P-C', 'SWANK-ARGLISTS', 'SWANK-REPL',
                    \ 'SWANK-FUZZY']

    call vlime#ChainCallbacks(
                \ function(conn.ConnectionInfo, [v:true]),
                \ function('s:OnConnectionInfoComplete'),
                \ function(conn.SwankRequire, [contribs]),
                \ function('s:OnSwankRequireComplete'),
                \ function('vlime#contrib#CallInitializers', [conn]),
                \ function('s:OnCallInitializersComplete'))
    return conn
endfunction

function! VlimeSelectCurConnection()
    let conn = VlimeSelectConnection(v:false)
    if type(conn) != type(v:null)
        " XXX: Cleanup buffers & windows for the old connection?
        let b:vlime_conn = conn
    endif
endfunction

function! VlimeSendCurThingToREPL(thing)
    if a:thing == 'thing'
        let str = vlime#ui#CurExpr()
        if len(str) <= 0
            let str = vlime#ui#CurAtom()
        endif
    elseif a:thing == 'expr'
        let str = vlime#ui#CurExpr()
    elseif a:thing == 'top_expr'
        let str = vlime#ui#CurTopExpr()
    elseif a:thing == 'atom'
        let str = vlime#ui#CurAtom()
    elseif a:thing == 'selection'
        let str = vlime#ui#CurSelection()
    endif

    if len(str) <= 0
        return
    endif

    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call conn.ui.OnWriteString(conn, "--\n", {'name': 'REPL-SEP', 'package': 'KEYWORD'})
    call conn.WithThread({'name': 'REPL-THREAD', 'package': 'KEYWORD'},
                \ function(conn.ListenerEval, [str]))
endfunction

function! VlimeCompileCurThing(thing)
    if a:thing == 'expr'
        let [str, s_pos, e_pos] = vlime#ui#CurExpr(v:true)
    elseif a:thing == 'top_expr'
        let [str, s_pos, e_pos] = vlime#ui#CurTopExpr(v:true)
    elseif a:thing == 'selection'
        let [str, s_pos, e_pos] = vlime#ui#CurSelection(v:true)
    endif
    if len(str) <= 0
        return
    endif

    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    let [str_line, str_col] = s_pos
    let cur_buf = bufnr('%')
    let cur_byte = line2byte(str_line) + str_col - 1
    let cur_file = expand('%:p')

    call conn.ui.OnWriteString(conn, "--\n", {'name': 'REPL-SEP', 'package': 'KEYWORD'})
    if exists('g:vlime_compiler_policy')
        let policy = g:vlime_compiler_policy
    else
        let policy = v:null
    endif
    call conn.CompileStringForEmacs(
                \ str, cur_buf, cur_byte, cur_file,
                \ policy, function('s:OnCompilationComplete'))
endfunction

function! VlimeInspectCurThing(thing)
    if a:thing == 'thing'
        let str = vlime#ui#CurExpr()
        if len(str) <= 0
            let str = vlime#ui#CurAtom()
        endif
    elseif a:thing == 'expr'
        let str = vlime#ui#CurExpr()
    elseif a:thing == 'top_expr'
        let str = vlime#ui#CurTopExpr()
    elseif a:thing == 'atom'
        let str = vlime#ui#CurAtom()
    elseif a:thing == 'selection'
        let str = vlime#ui#CurSelection()
    endif

    if len(str) <= 0
        return
    endif

    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call conn.InitInspector(str,
                \ {c, r -> c.ui.OnInspect(c, r, v:null, v:null)})
endfunction

function! VlimeCompileCurFile()
    let fname = expand('%:p')
    if len(fname) <= 0
        return
    endif

    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    if exists('g:vlime_compiler_policy')
        let policy = g:vlime_compiler_policy
    else
        let policy = v:null
    endif
    call conn.ui.OnWriteString(conn, "--\n", {'name': 'REPL-SEP', 'package': 'KEYWORD'})
    call conn.CompileFileForEmacs(fname, v:true, policy,
                \ function('s:OnCompilationComplete'))
endfunction

function! VlimeExpandCurMacro(expand_all)
    let expr = vlime#ui#CurExpr()
    if len(expr) <= 0
        return
    endif

    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    if a:expand_all
        call conn.SwankMacroExpandAll(expr, function('s:ShowAsyncResult'))
    else
        call conn.SwankMacroExpandOne(expr, function('s:ShowAsyncResult'))
    endif
endfunction

function! VlimeDisassembleCurForm()
    let expr = vlime#ui#CurExpr()
    if len(expr) <= 0
        return
    endif

    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call conn.DisassembleForm(expr, function('s:ShowAsyncResult'))
endfunction

function! VlimeLoadCurFile()
    let fname = expand('%:p')
    if len(fname) > 0
        let conn = VlimeGetConnection()
        if type(conn) == type(v:null)
            return
        endif

        call conn.LoadFile(fname, function('s:OnLoadFileComplete', [fname]))
    endif
endfunction

function! VlimeSetCurPackage()
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    let pkg = conn.GetCurrentPackage()
    let pkg = input('Set package: ', pkg[0])
    if len(pkg) <= 0
        call vlime#ui#ErrMsg('Canceled.')
        return
    endif
    call conn.SetPackage(pkg)
endfunction

function! VlimeSwankRequire(contribs)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif
    call conn.SwankRequire(a:contribs, function('s:OnSwankRequireComplete'))
endfunction

function! VlimeShowOperatorArgList(op)
    if len(a:op) <= 0
        return
    endif

    let conn = VlimeGetConnection(v:true)
    if type(conn) == type(v:null)
        return
    endif

    call conn.OperatorArgList(a:op, function('s:OnOperatorArgListComplete', [a:op]))
endfunction

function! VlimeDescribeCurSymbol(sym_type)
    if a:sym_type == 'operator'
        let sym = vlime#ui#CurOperator()
    elseif a:sym_type == 'atom'
        let sym = vlime#ui#CurAtom()
    endif
    if len(sym) > 0
        let conn = VlimeGetConnection()
        if type(conn) == type(v:null)
            return
        endif
        call conn.DescribeSymbol(sym, function('s:ShowAsyncResult'))
    endif
endfunction

function! VlimeXRefCurSymbol(sym_type, ref_type)
    if a:sym_type == 'operator'
        let sym = vlime#ui#CurOperator()
    elseif a:sym_type == 'atom'
        let sym = vlime#ui#CurAtom()
    endif
    if len(sym) > 0
        let conn = VlimeGetConnection()
        if type(conn) == type(v:null)
            return
        endif
        call conn.XRef(a:ref_type, sym,
                    \ function('s:OnXRefComplete', [win_getid()]))
    endif
endfunction

function! VlimeFindCurDefinition(sym_type)
    if a:sym_type == 'operator'
        let sym = vlime#ui#CurOperator()
    elseif a:sym_type == 'atom'
        let sym = vlime#ui#CurAtom()
    endif
    if len(sym) > 0
        let conn = VlimeGetConnection()
        if type(conn) == type(v:null)
            return
        endif
        call conn.FindDefinitionsForEmacs(sym,
                    \ function('s:OnXRefComplete', [win_getid()]))
    endif
endfunction

function! VlimeAproposList()
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif
    call vlime#ui#InputFromMiniBuffer(
                \ conn, 'Apropos search:',
                \ v:null,
                \ 'call VlimeAproposListInputComplete() \| bunload!')
endfunction

function! VlimeAproposListInputComplete()
    let content = vlime#ui#CurBufferContent()
    call b:vlime_conn.AproposListForEmacs(
                \ content, v:false, v:false, v:null,
                \ function('s:OnAproposListComplete'))
endfunction

function! VlimeDocumentationSymbol(sym_type)
    if a:sym_type == 'operator'
        let sym = vlime#ui#CurOperator()
    elseif a:sym_type == 'atom'
        let sym = vlime#ui#CurAtom()
    endif
    if len(sym) > 0
        let conn = VlimeGetConnection()
        if type(conn) == type(v:null)
            return
        endif
        call conn.DocumentationSymbol(sym, function('s:ShowAsyncResult'))
    endif
endfunction

function! VlimeSetBreakpoint()
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#InputFromMiniBuffer(
                \ conn, 'Set breakpoint at function:',
                \ v:null,
                \ 'call VlimeSetBreakpointInputComplete() \| bunload!')
endfunction

function! VlimeSetBreakpointInputComplete()
    let content = vlime#ui#CurBufferContent()
    call b:vlime_conn.SLDBBreak(content, function('s:OnSLDBBreakComplete'))
endfunction

function! VlimeListThreads()
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call conn.ListThreads(function('s:OnListThreadsComplete'))
endfunction

function! VlimeUndefineCurFunction()
    let sym = vlime#ui#CurAtom()
    if len(sym) <= 0
        return
    endif

    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call conn.UndefineFunction(sym, function('s:OnUndefineFunctionComplete'))
endfunction

function! VlimeUninternCurSymbol()
    let sym = vlime#ui#CurAtom()
    if len(sym) <= 0
        return
    endif

    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    let matched = matchlist(sym, '\(\([^:]\+\)\?::\?\)\?\(\k\+\)')
    if len(matched) > 0
        let sym_name = matched[3]
        let sym_pkg = matched[2]
        if matched[1] == ':'
            let sym_pkg = 'KEYWORD'
        elseif matched[1] == ''
            " Use the current package
            let sym_pkg = v:null
        endif
        call conn.UninternSymbol(sym_name, sym_pkg,
                    \ function('s:OnUninternSymbolComplete'))
    endif
endfunction

" VlimeCloseWindow([win_name])
function! VlimeCloseWindow(...)
    let win_name = vlime#GetNthVarArg(a:000, 0, v:null)
    if type(win_name) == type(v:null)
        let win_list = vlime#ui#GetWindowList(v:null, '')
        if len(win_list) <= 0
            call vlime#ui#ErrMsg('Cannot find any Vlime window.')
            return
        endif

        let win_choices = []
        let idx = 1
        for [winid, bufname] in win_list
            call add(win_choices, idx . '. ' . bufname . ' (' . winid . ')')
            let idx += 1
        endfor

        echohl Question
        echom 'Which window to close?'
        echohl None
        let idx = inputlist(win_choices)
        if idx <= 0
            call vlime#ui#ErrMsg('Canceled.')
        else
            let idx -= 1
            if idx >= len(win_list)
                call vlime#ui#ErrMsg('Invalid window number: ' . idx)
            else
                let winnr = win_id2win(win_list[idx][0])
                execute winnr . 'wincmd c'
            endif
        endif
    else
        call vlime#ui#CloseWindow(v:null, win_name)
    endif
endfunction

function! VlimeCompleteFunc(findstart, base)
    let start_col = s:CompleteFindStart()
    if a:findstart
        return start_col
    endif

    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return -1
    endif

    if s:ConnHasContrib(conn, 'SWANK-FUZZY')
        call conn.FuzzyCompletions(a:base,
                    \ function('s:OnFuzzyCompletionsComplete', [start_col + 1]))
    else
        call conn.SimpleCompletions(a:base,
                    \ function('s:OnSimpleCompletionsComplete', [start_col + 1]))
    endif
    " Actual completions are found in s:OnFuzzyCompletionsComplete(...)
    " XXX: The refresh option doesn't work, why?
    return {'words': [], 'refresh': 'always'}
endfunction

function! VlimeKey(key)
    if tolower(a:key) == 'space'
        let op = vlime#ui#SurroundingOperator()
        if s:NeedToShowArgList(op)
            call VlimeShowOperatorArgList(op)
        endif
    elseif tolower(a:key) == 'cr'
        let op = vlime#ui#SurroundingOperator()
        if s:NeedToShowArgList(op)
            call VlimeShowOperatorArgList(op)
        endif
    elseif tolower(a:key) == 'tab'
        let line = getline('.')
        let spaces = vlime#ui#CalcLeadingSpaces(line, v:true)
        let col = virtcol('.')
        if col <= spaces + 1
            let indent = VlimeCalcCurIndent()
            if spaces < indent
                call vlime#ui#IndentCurLine(indent)
            else
                return "\<tab>"
            endif
        else
            return "\<c-x>\<c-o>"
        endif
    else
        throw 'VlimeKey: Unknown key: ' . a:key
    endif
    return ''
endfunction

function! VlimeCalcCurIndent()
    let line_no = line('.')

    let conn = VlimeGetConnection(v:true)
    if type(conn) == type(v:null)
        return lispindent(line_no)
    endif

    let [s_line, s_col] = searchpairpos('(', '', ')', 'bnW')
    if s_line <= 0 || s_col <= 0
        return lispindent(line_no)
    endif

    let s_op = vlime#ui#SurroundingOperator()
    let old_cur = getcurpos()
    try
        call setpos('.', [0, s_line, s_col, 0])
        let vs_col = virtcol('.')
    finally
        call setpos('.', old_cur)
    endtry

    let matches = matchlist(s_op, '\(\([^:|]\+\||[^|]\+|\):\{1,2}\)\?\([^:|]\+\||[^|]\+|\)$')
    if len(matches) == 0
        return lispindent(line_no)
    endif

    let op_pkg = toupper(s:NormalizeIdentifierForIndentInfo(matches[2]))
    let op = tolower(s:NormalizeIdentifierForIndentInfo(matches[3]))

    if len(op_pkg) == 0
        let op_pkg = conn.GetCurrentPackage()
        if type(op_pkg) == v:t_list
            let op_pkg = op_pkg[0]
        endif
    endif

    let indent_info = get(conn.cb_data, 'indent_info', {})
    if has_key(indent_info, op) && index(indent_info[op][1], op_pkg) >= 0
        if vlime#ui#CurArgPosForIndent([s_line, s_col]) >= (indent_info[op][0] + 1)
            return vs_col + 1
        else
            return lispindent(line_no)
        endif
    else
        return lispindent(line_no)
    endif
endfunction

" VlimeSetup([force])
function! VlimeSetup(...)
    let force = vlime#GetNthVarArg(a:000, 0, v:false)

    if !force && exists('b:vlime_setup') && b:vlime_setup
        return
    endif
    let b:vlime_setup = v:true

    setlocal omnifunc=VlimeCompleteFunc
    setlocal indentexpr=VlimeCalcCurIndent()

    call vlime#ui#MapBufferKeys('lisp')
endfunction

function! VlimeInteractionMode()
    if getbufvar(bufnr('%'), 'vlime_interaction_mode', v:false)
        let b:vlime_interaction_mode = v:false
        nnoremap <buffer> <cr> <cr>
        vnoremap <buffer> <cr> <cr>
        echom 'Interaction mode disabled.'
    else
        let b:vlime_interaction_mode = v:true
        nnoremap <buffer> <silent> <cr> :call VlimeSendCurThingToREPL('thing')<cr>
        vnoremap <buffer> <silent> <cr> :<c-u>call VlimeSendCurThingToREPL('selection')<cr>
        echom 'Interaction mode enabled.'
    endif
endfunction

function! s:NormalizeIdentifierForIndentInfo(ident)
    let ident_len = len(a:ident)
    if ident_len >= 2 && a:ident[0] == '|' && a:ident[ident_len-1] == '|'
        return strpart(a:ident, 1, ident_len - 2)
    else
        return a:ident
    endif
endfunction

function! s:CompleteFindStart()
    let col = col('.') - 1
    let line = getline('.')
    while col > 0 && match(line[col-1], '\_s\|[()#;"'']') < 0
        let col -= 1
    endwhile
    return col
endfunction

function! s:ConnHasContrib(conn, contrib)
    return has_key(a:conn.cb_data, 'contribs') &&
                \ index(a:conn.cb_data['contribs'], a:contrib) >= 0
endfunction

function! s:OnCallInitializersComplete(conn)
    echom a:conn.cb_data['name'] . ' established.'
endfunction

function! s:OnSwankRequireComplete(conn, result)
    let a:conn.cb_data['contribs'] = a:result
endfunction

function! s:OnConnectionInfoComplete(conn, result)
    let a:conn.cb_data['version'] = a:result['VERSION']
    let a:conn.cb_data['pid'] = a:result['PID']
endfunction

function! s:OnFuzzyCompletionsComplete(col, conn, result)
    let comps = a:result[0]
    if type(comps) == type(v:null)
        let comps = []
    endif
    let r_comps = []
    for c in comps
        let cobj = {'word': c[0],'menu': c[3]}
        call add(r_comps, cobj)
    endfor
    call complete(a:col, r_comps)
endfunction

function! s:OnSimpleCompletionsComplete(col, conn, result)
    let comps = a:result[0]
    if type(comps) == type(v:null)
        let comps = []
    endif
    call complete(a:col, comps)
endfunction

function! s:OnOperatorArgListComplete(sym, conn, result)
    if type(a:result) == type(v:null)
        return
    endif
    call vlime#ui#ShowArgList(a:conn, a:result)
    let s:last_imode_arglist_op = a:sym
endfunction

function! s:OnLoadFileComplete(fname, conn, result)
    echom 'Loaded: ' . a:fname
endfunction

function! s:OnXRefComplete(orig_win, conn, result)
    if type(a:conn.ui) != type(v:null)
        call a:conn.ui.OnXRef(a:conn, a:result, a:orig_win)
    endif
endfunction

function! s:OnAproposListComplete(conn, result)
    if type(a:result) == type(v:null)
        call vlime#ui#ShowPreview(a:conn, 'No result found.', v:false)
    else
        let content = ''
        for item in a:result
            let item_dict = vlime#PListToDict(item)
            let content .= item_dict['DESIGNATOR']
            let flags = map(filter(keys(item_dict), {f -> f != 'DESIGNATOR'}), {i, f -> tolower(f)})
            if len(flags) > 0
                let content .= ' ('
                let content .= join(flags, ', ')
                let content .= ')'
            endif
            let content .= "\n"
        endfor
        call vlime#ui#ShowPreview(a:conn, content, v:false)
    endif
endfunction

function! s:OnSLDBBreakComplete(conn, result)
    echom 'Breakpoint set.'
endfunction

function! s:OnCompilationComplete(conn, result)
    let [_msg_type, notes, successp, duration, loadp, faslfile] = a:result
    if successp
        echom 'Compilation finished in ' . string(duration) . ' second(s)'
        if loadp && type(faslfile) != v:null
            call a:conn.LoadFile(faslfile, function('s:OnLoadFileComplete', [faslfile]))
        endif
    else
        call vlime#ui#ErrMsg('Compilation failed.')
    endif

    if type(a:conn.ui) != type(v:null)
        call a:conn.ui.OnCompilerNotes(a:conn, notes)
    endif
endfunction

function! s:OnListThreadsComplete(conn, result)
    if type(a:conn.ui) != type(v:null)
        call a:conn.ui.OnThreads(a:conn, a:result)
    endif
endfunction

function! s:OnUndefineFunctionComplete(conn, result)
    echom 'Undefined function ' . a:result
endfunction

function! s:OnUninternSymbolComplete(conn, result)
    echom a:result
endfunction

function! s:ShowAsyncResult(conn, result)
    call vlime#ui#ShowPreview(a:conn, a:result, v:false)
endfunction

function! s:CleanUpNullBufConnections()
    let old_buf = bufnr('%')
    try
        bufdo! if exists('b:vlime_conn') && type(b:vlime_conn) == type(v:null)
                    \ | unlet b:vlime_conn | endif
    finally
        execute 'hide buffer ' . old_buf
    endtry
endfunction

if !exists('s:last_imode_arglist_op')
    let s:last_imode_arglist_op = ''
endif

function! s:NeedToShowArgList(op)
    if len(a:op) > 0
        let arglist_buf = bufnr(vlime#ui#ArgListBufName())
        let arglist_win_nr = bufwinnr(arglist_buf)
        let arglist_visible = (arglist_win_nr >= 0)
        if !arglist_visible || a:op != s:last_imode_arglist_op
            return !!v:true
        else
            let conn = VlimeGetConnection(v:true)
            if type(conn) == type(v:null)
                " The current buffer doesn't have an active connection.
                " Close the arglist window explicitly, to avoid confusion.
                execute arglist_win_nr . 'wincmd c'
                return !!v:false
            else
                " If the current connection is different with the connection
                " used in arglist_buf, the arglist needs a refresh.
                let arglist_conn = getbufvar(
                            \ arglist_buf, 'vlime_conn',
                            \ {'cb_data': {'id': -1}})
                return conn.cb_data['id'] != arglist_conn.cb_data['id']
            endif
        endif
    else
        return !!v:false
    endif
endfunction
