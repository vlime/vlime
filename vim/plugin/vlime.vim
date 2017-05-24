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

" VlimeSendToREPL([content])
function! VlimeSendToREPL(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#MaybeInput(
                \ vlime#GetNthVarArg(a:000, 0, v:null),
                \ function('s:SendToREPLInputComplete', [conn]),
                \ 'Send to REPL: ',
                \ v:null,
                \ conn)
endfunction

" VlimeCompile([content[, buf]])
function! VlimeCompile(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    let content = vlime#GetNthVarArg(a:000, 0, v:null)
    let buf = vlime#GetNthVarArg(a:000, 1, -1)
    call vlime#ui#MaybeInput(
                \ content,
                \ function('s:CompileInputComplete', [conn, buf]),
                \ 'Compile: ',
                \ v:null,
                \ conn)
endfunction

" VlimeInspect([content])
function! VlimeInspect(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#MaybeInput(
                \ vlime#GetNthVarArg(a:000, 0, v:null),
                \ { str ->
                    \ conn.InitInspector(str,
                        \ {c, r -> c.ui.OnInspect(c, r, v:null, v:null)})},
                \ 'Inspect: ',
                \ v:null,
                \ conn)
endfunction

" VlimeCompileFile([file_name[, policy[, win]]])
function! VlimeCompileFile(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    let file_name = vlime#GetNthVarArg(a:000, 0, v:null)
    let policy = vlime#GetNthVarArg(a:000, 1, v:null)
    let win = vlime#GetNthVarArg(a:000, 2, 0)
    call vlime#ui#MaybeInput(
                \ file_name,
                \ function('s:CompileFileInputComplete', [conn, win, policy]),
                \ 'Compile file: ',
                \ '',
                \ v:null,
                \ 'file')
endfunction

" VlimeExpandMacro([expr[, expand_all]])
function! VlimeExpandMacro(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    let expr = vlime#GetNthVarArg(a:000, 0, v:null)
    let expand_all = vlime#GetNthVarArg(a:000, 1, v:false)

    if expand_all
        let CB = { e -> conn.SwankMacroExpandAll(e, function('s:ShowAsyncResult'))}
    else
        let CB = { e -> conn.SwankMacroExpandOne(e, function('s:ShowAsyncResult'))}
    endif

    call vlime#ui#MaybeInput(expr, CB, 'Expand macro: ', v:null, conn)
endfunction

" VlimeDisassembleForm([content])
function! VlimeDisassembleForm(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#MaybeInput(
                \ vlime#GetNthVarArg(a:000, 0, v:null),
                \ { expr ->
                    \ conn.DisassembleForm(expr, function('s:ShowAsyncResult'))},
                \ 'Disassemble: ',
                \ v:null,
                \ conn)
endfunction

" VlimeLoadFile([file_name])
function! VlimeLoadFile(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#MaybeInput(
                \ vlime#GetNthVarArg(a:000, 0, v:null),
                \ { fname ->
                    \ conn.LoadFile(fname, function('s:OnLoadFileComplete', [fname]))},
                \ 'Load file: ',
                \ '',
                \ v:null,
                \ 'file')
endfunction

" VlimeSetPackage([pkg])
function! VlimeSetPackage(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    let pkg = vlime#GetNthVarArg(a:000, 0, v:null)
    let cur_pkg = conn.GetCurrentPackage()
    call vlime#ui#MaybeInput(
                \ pkg,
                \ { p -> conn.SetPackage(p)},
                \ 'Set package: ',
                \ cur_pkg[0],
                \ conn)
endfunction

function! VlimeSwankRequire(contribs)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif
    call conn.SwankRequire(a:contribs, function('s:OnSwankRequireComplete'))
endfunction

" VlimeShowOperatorArgList([op])
function! VlimeShowOperatorArgList(...)
    let conn = VlimeGetConnection(v:true)
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#MaybeInput(
                \ vlime#GetNthVarArg(a:000, 0, v:null),
                \ { op ->
                    \ conn.OperatorArgList(op, function('s:OnOperatorArgListComplete', [op]))},
                \ 'Arglist for operator: ',
                \ v:null,
                \ conn)
endfunction

" VlimeDescribeSymbol([symbol])
function! VlimeDescribeSymbol(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#MaybeInput(
                \ vlime#GetNthVarArg(a:000, 0, v:null),
                \ { sym ->
                    \ conn.DescribeSymbol(sym, function('s:ShowAsyncResult'))},
                \ 'Describe symbol: ',
                \ v:null,
                \ conn)
endfunction

" VlimeXRefSymbol(ref_type[, sym[, win]])
function! VlimeXRefSymbol(ref_type, ...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    let sym = vlime#GetNthVarArg(a:000, 0, v:null)
    let win = vlime#GetNthVarArg(a:000, 1, 0)
    call vlime#ui#MaybeInput(
                \ sym,
                \ { s ->
                    \ conn.XRef(a:ref_type, s, function('s:OnXRefComplete', [win]))},
                \ 'XRef symbol: ',
                \ v:null,
                \ conn)
endfunction

" VlimeFindDefinition([sym[, win]])
function! VlimeFindDefinition(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    let sym = vlime#GetNthVarArg(a:000, 0, v:null)
    let win = vlime#GetNthVarArg(a:000, 1, 0)
    call vlime#ui#MaybeInput(
                \ sym,
                \ { s ->
                    \ conn.FindDefinitionsForEmacs(s, function('s:OnXRefComplete', [win]))},
                \ 'Definition of symbol: ',
                \ v:null,
                \ conn)
endfunction

" VlimeAproposList([pattern])
function! VlimeAproposList(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#MaybeInput(
                \ vlime#GetNthVarArg(a:000, 0, v:null),
                \ { pattern ->
                    \ conn.AproposListForEmacs(
                        \ pattern, v:false, v:false, v:null,
                        \ function('s:OnAproposListComplete'))},
                \ 'Apropos search: ',
                \ v:null,
                \ conn)
endfunction

" VlimeDocumentationSymbol([symbol])
function! VlimeDocumentationSymbol(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#MaybeInput(
                \ vlime#GetNthVarArg(a:000, 0, v:null),
                \ { sym ->
                    \ conn.DocumentationSymbol(sym, function('s:ShowAsyncResult'))},
                \ 'Documentation for symbol: ',
                \ v:null,
                \ conn)
endfunction

" VlimeSetBreakpoint([sym])
function! VlimeSetBreakpoint(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#MaybeInput(
                \ vlime#GetNthVarArg(a:000, 0, v:null),
                \ { sym ->
                    \ conn.SLDBBreak(sym, function('s:OnSLDBBreakComplete'))},
                \ 'Set breakpoint at function: ',
                \ v:null,
                \ conn)
endfunction

function! VlimeListThreads()
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call conn.ListThreads(function('s:OnListThreadsComplete'))
endfunction

" VlimeUndefineFunction([sym])
function! VlimeUndefineFunction(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#MaybeInput(
                \ vlime#GetNthVarArg(a:000, 0, v:null),
                \ { sym ->
                    \ conn.UndefineFunction(sym, function('s:OnUndefineFunctionComplete'))},
                \ 'Undefine function: ',
                \ v:null,
                \ conn)
endfunction

" VlimeUninternSymbol([sym])
function! VlimeUninternSymbol(...)
    let conn = VlimeGetConnection()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#MaybeInput(
                \ vlime#GetNthVarArg(a:000, 0, v:null),
                \ function('s:UninternSymbolInputComplete', [conn]),
                \ 'Unintern symbol: ',
                \ v:null,
                \ conn)
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
        nnoremap <buffer> <silent> <cr> :call VlimeSendToREPL(vlime#ui#CurExprOrAtom())<cr>
        vnoremap <buffer> <silent> <cr> :<c-u>call VlimeSendToREPL(vlime#ui#CurSelection())<cr>
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

function! s:OnCompilationComplete(orig_win, conn, result)
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
        call a:conn.ui.OnCompilerNotes(a:conn, notes, a:orig_win)
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

function! s:SendToREPLInputComplete(conn, content)
    call a:conn.ui.OnWriteString(a:conn, "--\n", {'name': 'REPL-SEP', 'package': 'KEYWORD'})
    call a:conn.WithThread({'name': 'REPL-THREAD', 'package': 'KEYWORD'},
                \ function(a:conn.ListenerEval, [a:content]))
endfunction

function! s:CompileInputComplete(conn, buf, content)
    if type(a:content) == v:t_list
        let str = a:content[0]
        let [str_line, str_col] = a:content[1]
    elseif type(a:content) == v:t_string
        let str = a:content
        let str_line = 0
        let str_col = 0
    endif

    if a:buf > 0
        let cinfo = vlime#ui#WithBuffer(a:buf,
                    \ function('s:GetInfoForCompiling', [str_line, str_col, {}]))
    else
        let cinfo = {'byte': 0, 'file': '', 'win': 0}
    endif

    if exists('g:vlime_compiler_policy')
        let policy = g:vlime_compiler_policy
    else
        let policy = v:null
    endif

    call a:conn.ui.OnWriteString(a:conn, "--\n", {'name': 'REPL-SEP', 'package': 'KEYWORD'})
    call a:conn.CompileStringForEmacs(
                \ str, a:buf, cinfo['byte'], cinfo['file'],
                \ policy, function('s:OnCompilationComplete', [cinfo['win']]))
endfunction

function! s:CompileFileInputComplete(conn, win, policy, file_name)
    if type(a:policy) != type(v:null)
        let policy = a:policy
    elseif exists('g:vlime_compiler_policy')
        let policy = g:vlime_compiler_policy
    else
        let policy = v:null
    endif

    call a:conn.ui.OnWriteString(a:conn, "--\n", {'name': 'REPL-SEP', 'package': 'KEYWORD'})
    call a:conn.CompileFileForEmacs(a:file_name, v:true, policy,
                \ function('s:OnCompilationComplete', [a:win]))
endfunction

function! s:UninternSymbolInputComplete(conn, sym)
    let matched = matchlist(a:sym, '\(\([^:]\+\)\?::\?\)\?\(\k\+\)')
    if len(matched) > 0
        let sym_name = matched[3]
        let sym_pkg = matched[2]
        if matched[1] == ':'
            let sym_pkg = 'KEYWORD'
        elseif matched[1] == ''
            " Use the current package
            let sym_pkg = v:null
        endif
        call a:conn.UninternSymbol(sym_name, sym_pkg,
                    \ function('s:OnUninternSymbolComplete'))
    endif
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

function! s:GetInfoForCompiling(line, col, ret)
    let a:ret['byte'] = line2byte(a:line) + a:col - 1
    let a:ret['file'] = expand('%:p')
    let a:ret['win'] = win_getid()
    return a:ret
endfunction
