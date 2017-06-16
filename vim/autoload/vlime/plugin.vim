""
" @public
"
" Close the connection bound to the current buffer. If no connection is bound,
" show a menu to choose one.
function! vlime#plugin#CloseCurConnection()
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    let server = get(conn.cb_data, 'server', v:null)
    if type(server) == type(v:null)
        call vlime#connection#Close(conn)
        echom conn.cb_data['name'] . ' disconnected.'
    else
        let answer = input('Also stop server ' . string(server['name']) . '? (y/n) ')
        if tolower(answer) == 'y' || tolower(answer) == 'yes'
            call vlime#server#Stop(server)
        elseif tolower(answer) == 'n' || tolower(answer) == 'no'
            call vlime#connection#Close(conn)
            echom conn.cb_data['name'] . ' disconnected.'
            call remove(server['connections'], conn.cb_data['id'])
        else
            call vlime#ui#ErrMsg('Canceled.')
        endif
    endif
endfunction

""
" @public
"
" Rename the connection bound to the current buffer. If no connection is
" bound, show a menu to choose one.
function! vlime#plugin#RenameCurConnection()
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif
    let new_name = input('New name: ', conn.cb_data['name'])
    if len(new_name) > 0
        call vlime#connection#Rename(conn, new_name)
    else
        call vlime#ui#ErrMsg('Canceled.')
    endif
endfunction

""
" @public
"
" Show the output buffer for a server started by Vlime.
function! vlime#plugin#ShowSelectedServer()
    let server = vlime#server#Select()
    if type(server) == type(v:null)
        return
    endif
    call vlime#server#Show(server)
endfunction

""
" @public
"
" Stop a server started by Vlime.
function! vlime#plugin#StopSelectedServer()
    let server = vlime#server#Select()
    if type(server) == type(v:null)
        return
    endif

    let answer = input('Stop server ' . string(server['name']) . '? (y/n) ')
    if tolower(answer) == 'y' || tolower(answer) == 'yes'
        call vlime#server#Stop(server)
    else
        call vlime#ui#ErrMsg('Canceled.')
    endif
endfunction

""
" @public
"
" Rename a server started by Vlime. Prompt for the new server name.
function! vlime#plugin#RenameSelectedServer()
    let server = vlime#server#Select()
    if type(server) == type(v:null)
        return
    endif
    let new_name = input('New name: ', server['name'])
    if len(new_name) > 0
        call vlime#server#Rename(server, new_name)
    else
        call vlime#ui#ErrMsg('Canceled.')
    endif
endfunction

""
" @usage [host] [port] [remote_prefix] [timeout] [name]
" @public
"
" Connect to a server, and return a connection object (see
" @dict(VlimeConnection)).
"
" [host] and [port] specify the server to connect to. This function will use
" the value in |g:vlime_address| if they are omitted.
" [remote_prefix], if specified, is an SFTP URL prefix, to tell Vlime to open
" remote files via SFTP (see |vlime-remote-server|).
" [timeout] is the time to wait for the connection to be made, in
" milliseconds.
" [name] gives the new connection a name. Omit this argument to use an
" automatically generated name.
function! vlime#plugin#ConnectREPL(...)
    let [def_host, def_port] = exists('g:vlime_address') ?
                \ g:vlime_address : ['127.0.0.1', 7002]
    let def_timeout = exists('g:vlime_connect_timeout') ?
                \ g:vlime_connect_timeout : v:null

    let host = get(a:000, 0, def_host)
    let port = get(a:000, 1, def_port)
    let remote_prefix = get(a:000, 2, '')
    let timeout = get(a:000, 3, def_timeout)
    let name = get(a:000, 4, v:null)

    if type(name) == type(v:null)
        let conn = vlime#connection#New()
    else
        let conn = vlime#connection#New(name)
    endif
    try
        call conn.Connect(host, port, remote_prefix, timeout)
    catch
        call vlime#connection#Close(conn)
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

""
" @public
"
" Show a menu to let you choose a connection, and bind this connection to the
" current buffer.
function! vlime#plugin#SelectCurConnection()
    let conn = vlime#connection#Select(v:false)
    if type(conn) != type(v:null)
        " XXX: Cleanup buffers & windows for the old connection?
        let b:vlime_conn = conn
    endif
endfunction

""
" @usage [content]
" @public
"
" Evaluate [content] in the REPL and show the result in the REPL buffer. If
" [content] is omitted, show an input buffer.
function! vlime#plugin#SendToREPL(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#input#MaybeInput(
                \ get(a:000, 0, v:null),
                \ function('s:SendToREPLInputComplete', [conn]),
                \ 'Send to REPL: ',
                \ v:null,
                \ conn)
endfunction

""
" @usage [content] [policy]
" @public
"
" Compile [content], with the specified [policy], and show the result in the
" REPL buffer. If [content] is omitted or v:null, show an input buffer. If
" [policy] is omitted, try to use |g:vlime_compiler_policy|. Open the compiler
" notes window when there are warnings or errors etc.
function! vlime#plugin#Compile(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    let content = get(a:000, 0, v:null)
    let policy = get(a:000, 1, v:null)
    let win = win_getid()
    call vlime#ui#input#MaybeInput(
                \ content,
                \ function('s:CompileInputComplete', [conn, win, policy]),
                \ 'Compile: ',
                \ v:null,
                \ conn)
endfunction

""
" @usage [content]
" @public
"
" Evaluate [content] and launch the inspector with the evaluation result
" loaded. If [content] is omitted, show an input buffer.
function! vlime#plugin#Inspect(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#input#MaybeInput(
                \ get(a:000, 0, v:null),
                \ { str ->
                    \ conn.InitInspector(str,
                        \ {c, r -> c.ui.OnInspect(c, r, v:null, v:null)})},
                \ 'Inspect: ',
                \ v:null,
                \ conn)
endfunction

""
" @usage [file_name] [policy]
" @public
"
" Compile a file named [file_name], with the specified [policy], and show the
" result in the REPL buffer. If [file_name] is omitted or v:null, prompt for
" the file name. If [policy] is omitted, try to use |g:vlime_compiler_policy|.
" Open the compiler notes window when there are warnings or errors etc.
function! vlime#plugin#CompileFile(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    let file_name = get(a:000, 0, v:null)
    let policy = get(a:000, 1, v:null)
    let win = win_getid()
    call vlime#ui#input#MaybeInput(
                \ file_name,
                \ function('s:CompileFileInputComplete', [conn, win, policy]),
                \ 'Compile file: ',
                \ '',
                \ v:null,
                \ 'file')
endfunction

""
" @usage [expr] [expand_all]
" @public
"
" Perform macro expansion on [expr] and show the result in the preview window.
" If [expr] is omitted, show an input buffer. If [expand_all] is present and
" |TRUE|, recursively expand all macros in [expr].
function! vlime#plugin#ExpandMacro(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    let expr = get(a:000, 0, v:null)
    let expand_all = get(a:000, 1, v:false)

    if expand_all
        let CB = { e -> conn.SwankMacroExpandAll(e, function('s:ShowAsyncResult'))}
    else
        let CB = { e -> conn.SwankMacroExpandOne(e, function('s:ShowAsyncResult'))}
    endif

    call vlime#ui#input#MaybeInput(expr, CB, 'Expand macro: ', v:null, conn)
endfunction

""
" @usage [content]
" @public
"
" Compile and disassemble [content]. Show the result in the preview window. If
" [content] is omitted, show an input buffer.
function! vlime#plugin#DisassembleForm(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#input#MaybeInput(
                \ get(a:000, 0, v:null),
                \ { expr ->
                    \ conn.DisassembleForm(expr, function('s:ShowAsyncResult'))},
                \ 'Disassemble: ',
                \ v:null,
                \ conn)
endfunction

""
" @usage [file_name]
" @public
"
" Load a file named [file_name]. If [file_name] is omitted, prompt for the
" file name.
function! vlime#plugin#LoadFile(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#input#MaybeInput(
                \ get(a:000, 0, v:null),
                \ { fname ->
                    \ conn.LoadFile(fname, function('s:OnLoadFileComplete', [fname]))},
                \ 'Load file: ',
                \ '',
                \ v:null,
                \ 'file')
endfunction

""
" @usage [pkg]
" @public
"
" Bind a Common Lisp package [pkg] to the current buffer. If [pkg] is omitted,
" show an input buffer.
function! vlime#plugin#SetPackage(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    let pkg = get(a:000, 0, v:null)
    let cur_pkg = conn.GetCurrentPackage()
    call vlime#ui#input#MaybeInput(
                \ pkg,
                \ { p -> conn.SetPackage(p)},
                \ 'Set package: ',
                \ cur_pkg[0],
                \ conn)
endfunction

""
" @public
"
" Require Swank contrib modules. {contribs} should be a plain string or a list
" of strings. Each string is a contrib module name. These names are
" case-sensitive. Normally you should use uppercase.
function! vlime#plugin#SwankRequire(contribs)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif
    call conn.SwankRequire(a:contribs, function('s:OnSwankRequireComplete'))
endfunction

""
" @usage [op]
" @public
"
" Show the arglist description for operator [op] in the arglist window. If
" [op] is omitted, show an input buffer.
function! vlime#plugin#ShowOperatorArgList(...)
    let conn = vlime#connection#Get(v:true)
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#input#MaybeInput(
                \ get(a:000, 0, v:null),
                \ { op ->
                    \ conn.OperatorArgList(op, function('s:OnOperatorArgListComplete', [op]))},
                \ 'Arglist for operator: ',
                \ v:null,
                \ conn)
endfunction

""
" @usage [symbol]
" @public
"
" Show a description for [symbol] in the preview window. If [symbol] is
" omitted, show an input buffer.
function! vlime#plugin#DescribeSymbol(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#input#MaybeInput(
                \ get(a:000, 0, v:null),
                \ { sym ->
                    \ conn.DescribeSymbol(sym, function('s:ShowAsyncResult'))},
                \ 'Describe symbol: ',
                \ v:null,
                \ conn)
endfunction

""
" @usage {ref_type} [sym]
" @public
"
" Lookup cross references for [sym], and show the results in the xref window.
" If [sym] is omitted, show an input buffer. See
" @function(VlimeConnection.XRef) for possible values for {ref_type}.
function! vlime#plugin#XRefSymbol(ref_type, ...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    let sym = get(a:000, 0, v:null)
    let win = win_getid()
    call vlime#ui#input#MaybeInput(
                \ sym,
                \ { s ->
                    \ conn.XRef(a:ref_type, s, function('s:OnXRefComplete', [win]))},
                \ 'XRef symbol: ',
                \ v:null,
                \ conn)
endfunction

""
" @public
"
" A wrapper function for @function(vlime#plugin#XRefSymbol) and
" @function(vlime#plugin#FindDefinition). Pick the type of cross reference
" interactively.
function! vlime#plugin#XRefSymbolWrapper()
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    let ref_types = ['calls', 'calls-who', 'references', 'binds', 'sets', 'macroexpands', 'specializes', 'definition']

    if v:count > 0
        let answer = v:count
    else
        let options = []
        let i = 0
        while i < len(ref_types)
            call add(options, string(i + 1) . '. ' . ref_types[i])
            let i += 1
        endwhile

        echohl Question
        echom 'What kind of xref?'
        echohl None
        let answer = inputlist(options)
    endif

    if answer <= 0
        call vlime#ui#ErrMsg('Canceled.')
        return
    elseif answer > len(ref_types)
        call vlime#ui#ErrMsg('Invalid xref type: ' . answer)
        return
    endif

    let rtype = ref_types[answer - 1]
    if rtype == 'definition'
        call vlime#plugin#FindDefinition()
    else
        call vlime#plugin#XRefSymbol(toupper(rtype))
    endif
endfunction

""
" @usage [sym]
" @public
"
" Find the definition for [sym], and show the results in the xref window. If
" [sym] is omitted, show an input buffer.
function! vlime#plugin#FindDefinition(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    let sym = get(a:000, 0, v:null)
    let win = win_getid()
    call vlime#ui#input#MaybeInput(
                \ sym,
                \ { s ->
                    \ conn.FindDefinitionsForEmacs(s, function('s:OnXRefComplete', [win]))},
                \ 'Definition of symbol: ',
                \ v:null,
                \ conn)
endfunction

""
" @usage [pattern]
" @public
"
" Apropos search for [pattern]. Show the results in the preview window. If
" [pattern] is omitted, show an input buffer.
function! vlime#plugin#AproposList(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#input#MaybeInput(
                \ get(a:000, 0, v:null),
                \ { pattern ->
                    \ conn.AproposListForEmacs(
                        \ pattern, v:false, v:false, v:null,
                        \ function('s:OnAproposListComplete'))},
                \ 'Apropos search: ',
                \ v:null,
                \ conn)
endfunction

""
" @usage [symbol]
" @public
"
" Show the documentation for [symbol] in the preview window. If [symbol] is
" omitted, show an input buffer.
function! vlime#plugin#DocumentationSymbol(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#input#MaybeInput(
                \ get(a:000, 0, v:null),
                \ { sym ->
                    \ conn.DocumentationSymbol(sym, function('s:ShowAsyncResult'))},
                \ 'Documentation for symbol: ',
                \ v:null,
                \ conn)
endfunction

""
" @usage [sym]
" @public
"
" Set a breakpoint at entry to a function with the name [sym]. If [sym] is
" omitted, show an input buffer.
function! vlime#plugin#SetBreakpoint(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#input#MaybeInput(
                \ get(a:000, 0, v:null),
                \ { sym ->
                    \ conn.SLDBBreak(sym, function('s:OnSLDBBreakComplete'))},
                \ 'Set breakpoint at function: ',
                \ v:null,
                \ conn)
endfunction

""
" @public
"
" Show the thread list window.
function! vlime#plugin#ListThreads()
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call conn.ListThreads(function('s:OnListThreadsComplete'))
endfunction

""
" @usage [sym]
" @public
"
" Undefine a function with the name [sym]. If [sym] is omitted, show an input
" buffer.
function! vlime#plugin#UndefineFunction(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#input#MaybeInput(
                \ get(a:000, 0, v:null),
                \ { sym ->
                    \ conn.UndefineFunction(sym, function('s:OnUndefineFunctionComplete'))},
                \ 'Undefine function: ',
                \ v:null,
                \ conn)
endfunction

""
" @usage [sym]
" @public
"
" Unintern a symbol [sym]. If [sym] is omitted, show an input buffer.
function! vlime#plugin#UninternSymbol(...)
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call vlime#ui#input#MaybeInput(
                \ get(a:000, 0, v:null),
                \ function('s:UninternSymbolInputComplete', [conn]),
                \ 'Unintern symbol: ',
                \ v:null,
                \ conn)
endfunction

""
" @public
"
" A wrapper function for @function(vlime#plugin#UndefineFunction) and
" @function(vlime#plugin#UninternSymbol). Pick the type of action to perform
" interactively.
function! vlime#plugin#UndefineUninternWrapper()
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    if v:count > 0
        let answer = v:count
    else
        let options = ['1. Undefine a function', '2. Unintern a symbol']
        echohl Question
        echom 'What to do?'
        echohl None
        let answer = inputlist(options)
    endif

    if answer <= 0
        call vlime#ui#ErrMsg('Canceled.')
    elseif answer == 1
        call vlime#plugin#UndefineFunction()
    elseif answer == 2
        call vlime#plugin#UninternSymbol()
    else
        call vlime#ui#ErrMsg('Invalid action: ' . answer)
    endif
endfunction

""
" @usage [win_name]
" @public
"
" Close Vlime special windows. [win_name] is the type of windows to close. See
" @function(vlime#ui#GetWindowList) for valid values for [win_name]. If
" [win_name] is omitted, show a menu to let you choose which window to close.
function! vlime#plugin#CloseWindow(...)
    let win_name = get(a:000, 0, v:null)
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

""
" @public
"
" The completion function. This function is meant to be used as |omnifunc| or
" |completefunc|. It is asynchronous, and will NOT return the completion list
" immediately.
function! vlime#plugin#CompleteFunc(findstart, base)
    let start_col = s:CompleteFindStart()
    if a:findstart
        return start_col
    endif

    let conn = vlime#connection#Get()
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

function! vlime#plugin#VlimeKey(key)
    if tolower(a:key) == 'space'
        let op = vlime#ui#SurroundingOperator()
        if s:NeedToShowArgList(op)
            call vlime#plugin#ShowOperatorArgList(op)
        endif
    elseif tolower(a:key) == 'cr'
        let op = vlime#ui#SurroundingOperator()
        if s:NeedToShowArgList(op)
            call vlime#plugin#ShowOperatorArgList(op)
        endif
    elseif tolower(a:key) == 'tab'
        let line = getline('.')
        let spaces = vlime#ui#CalcLeadingSpaces(line, v:true)
        let col = virtcol('.')
        if col <= spaces + 1
            let indent = vlime#plugin#CalcCurIndent()
            if spaces < indent
                call vlime#ui#IndentCurLine(indent)
            else
                return "\<tab>"
            endif
        else
            return "\<c-x>\<c-o>"
        endif
    else
        throw 'vlime#plugin#VlimeKey: Unknown key: ' . a:key
    endif
    return ''
endfunction

""
" @public
"
" Calculate the indent size for the current line, in number of <space>
" characters.
function! vlime#plugin#CalcCurIndent()
    let line_no = line('.')

    let conn = vlime#connection#Get(v:true)
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

""
" @usage [force]
" @public
"
" Set up Vlime for the current buffer. Do nothing if the current buffer is
" already initialized. If [force] is present and |TRUE|, always perform the
" initialization.
function! vlime#plugin#Setup(...)
    let force = get(a:000, 0, v:false)

    if !force && exists('b:vlime_setup') && b:vlime_setup
        return
    endif
    let b:vlime_setup = v:true

    setlocal omnifunc=vlime#plugin#CompleteFunc
    setlocal indentexpr=vlime#plugin#CalcCurIndent()

    call vlime#ui#MapBufferKeys('lisp')
endfunction

""
" @public
"
" Toggle interaction mode.
function! vlime#plugin#InteractionMode()
    if getbufvar(bufnr('%'), 'vlime_interaction_mode', v:false)
        let b:vlime_interaction_mode = v:false
        nnoremap <buffer> <cr> <cr>
        vnoremap <buffer> <cr> <cr>
        echom 'Interaction mode disabled.'
    else
        let b:vlime_interaction_mode = v:true
        nnoremap <buffer> <silent> <cr> :call vlime#plugin#SendToREPL(vlime#ui#CurExprOrAtom())<cr>
        vnoremap <buffer> <silent> <cr> :<c-u>call vlime#plugin#SendToREPL(vlime#ui#CurSelection())<cr>
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
    let a:conn.cb_data['contribs'] =
                \ (type(a:result) == v:t_list) ? a:result : []
endfunction

function! s:OnConnectionInfoComplete(conn, result)
    let a:conn.cb_data['version'] = get(a:result, 'VERSION', '<unkown version>')
    let a:conn.cb_data['pid'] = get(a:result, 'PID', '<unknown pid>')
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

function! s:OnListenerEvalComplete(conn, result)
    if type(a:result) == v:t_list && len(a:result) > 0 &&
                \ type(a:result[0]) == v:t_dict && a:result[0]['name'] == 'VALUES' &&
                \ type(a:conn.ui) != type(v:null)
        let values = a:result[1:]
        if len(values) > 0
            for val in values
                call a:conn.ui.OnWriteString(
                            \ a:conn,
                            \ val . "\n",
                            \ {'name': 'REPL-RESULT', 'package': 'KEYWORD'})
            endfor
        else
            call a:conn.ui.OnWriteString(
                        \ a:conn,
                        \ "; No value\n",
                        \ {'name': 'REPL-RESULT', 'package': 'KEYWORD'})
        endif
    endif
endfunction

function! s:ShowAsyncResult(conn, result)
    call vlime#ui#ShowPreview(a:conn, a:result, v:false)
endfunction

function! s:SendToREPLInputComplete(conn, content)
    call a:conn.ui.OnWriteString(a:conn, "--\n", {'name': 'REPL-SEP', 'package': 'KEYWORD'})
    call a:conn.WithThread({'name': 'REPL-THREAD', 'package': 'KEYWORD'},
                \ function(a:conn.ListenerEval, [a:content, function('s:OnListenerEvalComplete')]))
endfunction

function! s:CompileInputComplete(conn, win, policy, content)
    if type(a:content) == v:t_list
        let str = a:content[0]
        let [str_line, str_col] = a:content[1]

        let buf = bufnr('%')
        let cur_byte = line2byte(str_line) + str_col - 1
        let cur_file = expand('%:p')
    elseif type(a:content) == v:t_string
        let str = a:content
    endif

    if type(a:policy) != type(v:null)
        let policy = a:policy
    elseif exists('g:vlime_compiler_policy')
        let policy = g:vlime_compiler_policy
    else
        let policy = v:null
    endif

    call a:conn.ui.OnWriteString(a:conn, "--\n", {'name': 'REPL-SEP', 'package': 'KEYWORD'})

    if type(a:content) == v:t_string
        call a:conn.CompileStringForEmacs(
                    \ str, v:null, 1, v:null,
                    \ policy, function('s:OnCompilationComplete', [a:win]))
    else
        call a:conn.CompileStringForEmacs(
                    \ str, buf, cur_byte, cur_file,
                    \ policy, function('s:OnCompilationComplete', [a:win]))
    endif
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
            let conn = vlime#connection#Get(v:true)
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
