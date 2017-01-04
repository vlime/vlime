" Vlime Connection constructor.
" vlime#New([cb_data[, ui]])
function! vlime#New(...)
    let cb_data = s:GetNthVarArg(a:000, 0)
    let ui = s:GetNthVarArg(a:000, 1)
    let obj = {
                \ 'cb_data': cb_data,
                \ 'channel': v:null,
                \ 'ui': ui,
                \ 'Connect': function('vlime#Connect'),
                \ 'IsConnected': function('vlime#IsConnected'),
                \ 'Close': function('vlime#Close'),
                \ 'Call': function('vlime#Call'),
                \ 'Send': function('vlime#Send'),
                \ 'GetCurrentPackage': function('vlime#GetCurrentPackage'),
                \ 'SetCurrentPackage': function('vlime#SetCurrentPackage'),
                \ 'GetCurrentThread': function('vlime#GetCurrentThread'),
                \ 'SetCurrentThread': function('vlime#SetCurrentThread'),
                \ 'WithPackage': function('vlime#WithPackage'),
                \ 'WithThread': function('vlime#WithThread'),
                \ 'EmacsRex': function('vlime#EmacsRex'),
                \ 'Pong': function('vlime#Pong'),
                \ 'ConnectionInfo': function('vlime#ConnectionInfo'),
                \ 'SwankRequire': function('vlime#SwankRequire'),
                \ 'CreateREPL': function('vlime#CreateREPL'),
                \ 'ListenerEval': function('vlime#ListenerEval'),
                \ 'SetPackage': function('vlime#SetPackage'),
                \ 'DescribeSymbol': function('vlime#DescribeSymbol'),
                \ 'OperatorArgList': function('vlime#OperatorArgList'),
                \ 'SimpleCompletions': function('vlime#SimpleCompletions'),
                \ 'FuzzyCompletions': function('vlime#FuzzyCompletions'),
                \ 'ReturnString': function('vlime#ReturnString'),
                \ 'Return': function('vlime#Return'),
                \ 'SwankMacroExpandOne': function('vlime#SwankMacroExpandOne'),
                \ 'SwankMacroExpand': function('vlime#SwankMacroExpand'),
                \ 'SwankMacroExpandAll': function('vlime#SwankMacroExpandAll'),
                \ 'DisassembleForm': function('vlime#DisassembleForm'),
                \ 'CompileStringForEmacs': function('vlime#CompileStringForEmacs'),
                \ 'CompileFileForEmacs': function('vlime#CompileFileForEmacs'),
                \ 'LoadFile': function('vlime#LoadFile'),
                \ 'Interrupt': function('vlime#Interrupt'),
                \ 'SLDBAbort': function('vlime#SLDBAbort'),
                \ 'SLDBContinue': function('vlime#SLDBContinue'),
                \ 'SLDBStep': function('vlime#SLDBStep'),
                \ 'SLDBNext': function('vlime#SLDBNext'),
                \ 'SLDBOut': function('vlime#SLDBOut'),
                \ 'InvokeNthRestartForEmacs': function('vlime#InvokeNthRestartForEmacs'),
                \ 'RestartFrame': function('vlime#RestartFrame'),
                \ 'FrameLocalsAndCatchTags': function('vlime#FrameLocalsAndCatchTags'),
                \ 'FrameSourceLocation': function('vlime#FrameSourceLocation'),
                \ 'InitInspector': function('vlime#InitInspector'),
                \ 'InspectNthPart': function('vlime#InspectNthPart'),
                \ 'InspectorCallNthAction': function('vlime#InspectorCallNthAction'),
                \ 'OnServerEvent': function('vlime#OnServerEvent'),
                \ 'server_event_handlers': {
                    \ 'PING': function('vlime#OnPing'),
                    \ 'NEW-PACKAGE': function('vlime#OnNewPackage'),
                    \ 'DEBUG': function('vlime#OnDebug'),
                    \ 'DEBUG-ACTIVATE': function('vlime#OnDebugActivate'),
                    \ 'DEBUG-RETURN': function('vlime#OnDebugReturn'),
                    \ 'WRITE-STRING': function('vlime#OnWriteString'),
                    \ 'READ-STRING': function('vlime#OnReadString'),
                    \ 'READ-FROM-MINIBUFFER': function('vlime#OnReadFromMiniBuffer'),
                    \ 'INDENTATION-UPDATE': function('vlime#OnIndentationUpdate'),
                    \ 'INVALID-RPC': function('vlime#OnInvalidRPC'),
                    \ 'INSPECT': function('vlime#OnInspect'),
                    \ }
                \ }
    return obj
endfunction

" ================== methods for vlime connections ==================

function! vlime#Connect(host, port) dict
    let self.channel = ch_open(
                \ a:host . ':' . string(a:port),
                \ {'mode': 'json',
                \  'callback': {chan, msg -> self.OnServerEvent(chan, msg)}})
    if ch_status(self.channel) != 'open'
        call self.Close()
        throw 'vlime#Connect: failed to open channel'
    endif
    return self
endfunction

function! vlime#IsConnected() dict
    return type(self.channel) == v:t_channel &&
                \ ch_status(self.channel) == 'open'
endfunction

function! vlime#Close() dict
    if type(self.channel) == v:t_channel
        try
            call ch_close(self.channel)
        catch /^Vim\%((\a\+)\)\=:E906/  " Not an open channel
        endtry
        let self.channel = v:null
    endif
    return self
endfunction

function! vlime#Call(msg) dict
    return ch_evalexpr(self.channel, a:msg)
endfunction

" vlime#Send(msg[, callback])
function! vlime#Send(msg, ...) dict
    let options = {}
    if a:0 == 1
        let options['callback'] = a:1
    elseif a:0 != 0
        throw 'vlime#Send: wrong # of arguments'
    endif

    if len(options) > 0
        call ch_sendexpr(self.channel, a:msg, options)
    else
        call ch_sendexpr(self.channel, a:msg)
    endif
endfunction

function! vlime#GetCurrentPackage() dict
    if type(self.ui) != v:t_none
        return self.ui.GetCurrentPackage()
    else
        return v:null
    endif
endfunction

function! vlime#SetCurrentPackage(package) dict
    if type(self.ui) != v:t_none
        call self.ui.SetCurrentPackage(a:package)
    endif
endfunction

function! vlime#GetCurrentThread() dict
    if type(self.ui) != v:t_none
        return self.ui.GetCurrentThread()
    else
        return v:true
    endif
endfunction

function! vlime#SetCurrentThread(thread) dict
    if type(self.ui) != v:t_none
        call self.ui.SetCurrentThread(a:thread)
    endif
endfunction

function! vlime#WithThread(thread, Func) dict
    let old_thread = self.GetCurrentThread()
    try
        call self.SetCurrentThread(a:thread)
        return a:Func()
    finally
        call self.SetCurrentThread(old_thread)
    endtry
endfunction

function! vlime#WithPackage(package, Func) dict
    let old_package = self.GetCurrentPackage()
    try
        call self.SetCurrentPackage([a:package, a:package])
        return a:Func()
    finally
        call self.SetCurrentPackage(old_package)
    endtry
endfunction

function! vlime#EmacsRex(cmd) dict
    let pkg_info = self.GetCurrentPackage()
    if type(pkg_info) != v:t_list
        let pkg = v:null
    else
        let pkg = pkg_info[0]
    endif
    return s:EmacsRex(a:cmd, pkg, self.GetCurrentThread())
endfunction

function! vlime#Pong(thread, ttag) dict
    call self.Send([s:KW('EMACS-PONG'), a:thread, a:ttag])
endfunction

" vlime#ConnectionInfo([return_dict[, callback]])
function! vlime#ConnectionInfo(...) dict
    " We pass local variables as extra arguments instead of
    " using the 'closure' flag on inner functions, to prevent
    " messed-up variable values caused by calling the outer
    " function more than once.
    function! s:ConnectionInfoCB(conn, Callback, return_dict, chan, msg) abort
        call s:CheckReturnStatus(a:msg, 'vlime#ConnectionInfo')
        if a:return_dict
            call s:TryToCall(a:Callback, [a:conn, vlime#PListToDict(a:msg[1][1])])
        else
            call s:TryToCall(a:Callback, [a:conn, a:msg[1][1]])
        endif
    endfunction

    let return_dict = s:GetNthVarArg(a:000, 0, v:true)
    let Callback = s:GetNthVarArg(a:000, 1)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'CONNECTION-INFO')]),
                \ function('s:ConnectionInfoCB', [self, Callback, return_dict]))
endfunction

" vlime#SwankRequire(contrib[, callback])
function! vlime#SwankRequire(contrib, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    if type(a:contrib) == v:t_list
        let required = [s:CL('QUOTE'), map(a:contrib, {k, v -> s:KW(v)})]
    else
        let required = s:KW(a:contrib)
    endif

    call self.Send(self.EmacsRex([s:SYM('SWANK', 'SWANK-REQUIRE'), required]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#SwankRequire']))
endfunction

" vlime#CreateREPL([coding_system[, callback]])
function! vlime#CreateREPL(...) dict
    function! s:CreateREPL_CB(conn, Callback, chan, msg) abort
        call s:CheckReturnStatus(a:msg, 'vlime#CreateREPL')
        " The package for the REPL defaults to ['COMMON-LISP-USER', 'CL-USER'],
        " so SetCurrentPackage(...) is not necessary.
        "call a:conn.SetCurrentPackage(a:msg[1][1])
        call s:TryToCall(a:Callback, [a:conn, a:msg[1][1]])
    endfunction

    let cmd = [s:SYM('SWANK-REPL', 'CREATE-REPL'), v:null]
    let coding_system = s:GetNthVarArg(a:000, 0)
    if coding_system != v:null
        let cmd += [s:KW('CODING-SYSTEM'), coding_system]
    endif
    let Callback = s:GetNthVarArg(a:000, 1)
    call self.Send(self.EmacsRex(cmd),
                \ function('s:CreateREPL_CB', [self, Callback]))
endfunction

" vlime#ListenerEval(expr[, callback])
function! vlime#ListenerEval(expr, ...) dict
    function! s:ListenerEvalCB(conn, Callback, chan, msg) abort
        let stat = s:CheckAndReportReturnStatus(a:conn, a:msg, 'vlime#ListenerEval')
        if stat
            call s:TryToCall(a:Callback, [a:conn, a:msg[1][1]])
        endif
    endfunction

    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK-REPL', 'LISTENER-EVAL'), a:expr]),
                \ function('s:ListenerEvalCB', [self, Callback]))
endfunction

function! vlime#Interrupt(thread) dict
    call self.Send([s:KW('EMACS-INTERRUPT'), a:thread])
endfunction

" vlime#SLDBAbort([callback])
function! vlime#SLDBAbort(...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'SLDB-ABORT')]),
                \ function('s:SLDBSendCB', [self, Callback, 'vlime#SLDBAbort']))
endfunction

" vlime#SLDBContinue([callback])
function! vlime#SLDBContinue(...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'SLDB-CONTINUE')]),
                \ function('s:SLDBSendCB', [self, Callback, 'vlime#SLDBContinue']))
endfunction

" vlime#SLDBStep(frame[, callback]) dict
function! vlime#SLDBStep(frame, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'SLDB-STEP'), a:frame]),
                \ function('s:SLDBSendCB', [self, Callback, 'vlime#SLDBStep']))
endfunction

" vlime#SLDBNext(frame[, callback])
function! vlime#SLDBNext(frame, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'SLDB-NEXT'), a:frame]),
                \ function('s:SLDBSendCB', [self, Callback, 'vlime#SLDBNext']))
endfunction

" vlime#SLDBOut(frame[, callback])
function! vlime#SLDBOut(frame, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'SLDB-OUT'), a:frame]),
                \ function('s:SLDBSendCB', [self, Callback, 'vlime#SLDBOut']))
endfunction

" vlime#InvokeNthRestartForEmacs(level, restart[, callback])
function! vlime#InvokeNthRestartForEmacs(level, restart, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INVOKE-NTH-RESTART-FOR-EMACS'), a:level, a:restart]),
                \ function('s:SLDBSendCB', [self, Callback, 'vlime#InvokeNthRestartForEmacs']))
endfunction

" vlime#RestartFrame(frame[, callback])
function! vlime#RestartFrame(frame, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'RESTART-FRAME'), a:frame]),
                \ function('s:SLDBSendCB',
                    \ [self, Callback, 'vlime#RestartFrame']))
endfunction

" vlime#FrameLocalsAndCatchTags(frame[, callback])
function! vlime#FrameLocalsAndCatchTags(frame, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'FRAME-LOCALS-AND-CATCH-TAGS'), a:frame]),
                \ function('s:SimpleSendCB',
                    \ [self, Callback, 'vlime#FrameLocalsAndCatchTags']))
endfunction

" vlime#FrameSourceLocation(frame[, callback])
function! vlime#FrameSourceLocation(frame, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'FRAME-SOURCE-LOCATION'), a:frame]),
                \ function('s:SimpleSendCB',
                    \ [self, Callback, 'vlime#FrameSourceLocation']))
endfunction

" vlime#InitInspector(thing[, callback])
function! vlime#InitInspector(thing, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INIT-INSPECTOR'), a:thing]),
                \ function('s:SimpleSendCB',
                    \ [self, Callback, 'vlime#InitInspector']))
endfunction

" vlime#InspectNthPart(nth[, callback])
function! vlime#InspectNthPart(nth, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INSPECT-NTH-PART'), a:nth]),
                \ function('s:SimpleSendCB',
                    \ [self, Callback, 'vlime#InspectNthPart']))
endfunction

" vlime#InspectorCallNthAction(nth[, callback])
function! vlime#InspectorCallNthAction(nth, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INSPECTOR-CALL-NTH-ACTION'), a:nth]),
                \ function('s:SimpleSendCB',
                    \ [self, Callback, 'vlime#InspectorCallNthAction']))
endfunction

" vlime#SetPackage(package[, callback])
function! vlime#SetPackage(package, ...) dict
    function! s:SetPackageCB(conn, Callback, chan, msg) abort
        call s:CheckReturnStatus(a:msg, 'vlime#SetPackage')
        call a:conn.SetCurrentPackage(a:msg[1][1])
        call s:TryToCall(a:Callback, [a:conn, a:msg[1][1]])
    endfunction

    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'SET-PACKAGE'), a:package]),
                \ function('s:SetPackageCB', [self, Callback]))
endfunction

" vlime#DescribeSymbol(symbol[, callback])
function! vlime#DescribeSymbol(symbol, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'DESCRIBE-SYMBOL'), a:symbol]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#DescribeSymbol']))
endfunction

" vlime#OperatorArgList(operator[, callback])
function! vlime#OperatorArgList(operator, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'OPERATOR-ARGLIST'), a:operator, self.GetCurrentPackage()[0]]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#OperatorArgList']))
endfunction

" vlime#SimpleCompletions(symbol[, callback])
function! vlime#SimpleCompletions(symbol, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'SIMPLE-COMPLETIONS'), a:symbol, self.GetCurrentPackage()[0]]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#SimpleCompletions']))
endfunction

" vlime#FuzzyCompletions(symbol[, callback])
function! vlime#FuzzyCompletions(symbol, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'FUZZY-COMPLETIONS'), a:symbol, self.GetCurrentPackage()[0]]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#FuzzyCompletions']))
endfunction

function! vlime#ReturnString(thread, ttag, str) dict
    call self.Send([s:KW('EMACS-RETURN-STRING'), a:thread, a:ttag, a:str])
endfunction

function! vlime#Return(thread, ttag, val) dict
    call self.Send([s:KW('EMACS-RETURN'), a:thread, a:ttag, a:val])
endfunction

" vlime#SwankMacroExpandOne(expr[, callback])
function! vlime#SwankMacroExpandOne(expr, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'SWANK-MACROEXPAND-1'), a:expr]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#SwankMacroExpandOne']))
endfunction

" vlime#SwankMacroExpand(expr[, callback])
function! vlime#SwankMacroExpand(expr, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'SWANK-MACROEXPAND'), a:expr]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#SwankMacroExpand']))
endfunction

" vlime#SwankMacroExpandAll(expr[, callback])
function! vlime#SwankMacroExpandAll(expr, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'SWANK-MACROEXPAND-ALL'), a:expr]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#SwankMacroExpandAll']))
endfunction

" vlime#DisassembleForm(expr[, callback])
function! vlime#DisassembleForm(expr, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'DISASSEMBLE-FORM'), a:expr]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#DisassembleForm']))
endfunction

" vlime#CompileStringForEmacs(expr, buffer, position, filename[, callback])
function! vlime#CompileStringForEmacs(expr, buffer, position, filename, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'COMPILE-STRING-FOR-EMACS'),
                        \ a:expr, a:buffer,
                        \ [s:CL('QUOTE'), [[s:KW('POSITION'), a:position]]],
                        \ a:filename, v:null]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#CompileStringForEmacs']))
endfunction


" vlime#CompileFileForEmacs(filename[, load[, callback]]) dict
function! vlime#CompileFileForEmacs(filename, ...) dict
    let load = s:GetNthVarArg(a:000, 0, v:true)
    let Callback = s:GetNthVarArg(a:000, 1)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'COMPILE-FILE-FOR-EMACS'), a:filename, load]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#CompileFileForEmacs']))
endfunction

" vlime#LoadFile(filename[, callback])
function! vlime#LoadFile(filename, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'LOAD-FILE'), a:filename]),
                \ function('s:SimpleSendCB', [self, Callback, 'vlime#LoadFile']))
endfunction

" ------------------ server event handlers ------------------

function! vlime#OnPing(conn, msg)
    let [_msg_type, thread, ttag] = a:msg
    call a:conn.Pong(thread, ttag)
endfunction

function! vlime#OnNewPackage(conn, msg)
    call a:conn.SetCurrentPackage([a:msg[1], a:msg[2]])
endfunction

function! vlime#OnDebug(conn, msg)
    if type(a:conn.ui) != v:t_none
        let [_msg_type, thread, level, condition, restarts, frames, conts] = a:msg
        call a:conn.ui.OnDebug(a:conn, thread, level, condition, restarts, frames, conts)
    endif
endfunction

function! vlime#OnDebugActivate(conn, msg)
    if type(a:conn.ui) != v:t_none
        let [_msg_type, thread, level, select] = a:msg
        call a:conn.ui.OnDebugActivate(a:conn, thread, level, select)
    endif
endfunction

function! vlime#OnDebugReturn(conn, msg)
    if type(a:conn.ui) != v:t_none
        let [_msg_type, thread, level, stepping] = a:msg
        call a:conn.ui.OnDebugReturn(a:conn, thread, level, stepping)
    endif
endfunction

function! vlime#OnWriteString(conn, msg)
    if type(a:conn.ui) != v:t_none
        let str = a:msg[1]
        let str_type = (len(a:msg) >= 3) ? a:msg[2] : v:null
        call a:conn.ui.OnWriteString(a:conn, str, str_type)
    endif
endfunction

function! vlime#OnReadString(conn, msg)
    if type(a:conn.ui) != v:t_none
        let [_msg_type, thread, ttag] = a:msg
        call a:conn.ui.OnReadString(a:conn, thread, ttag)
    endif
endfunction

function! vlime#OnReadFromMiniBuffer(conn, msg)
    if type(a:conn.ui) != v:t_none
        let [_msg_type, thread, ttag, prompt, init_val] = a:msg
        call a:conn.ui.OnReadFromMiniBuffer(
                    \ a:conn, thread, ttag, prompt, init_val)
    endif
endfunction

function! vlime#OnIndentationUpdate(conn, msg)
    if type(a:conn.ui) != v:t_none
        let [_msg_type, indent_info] = a:msg
        call a:conn.ui.OnIndentationUpdate(a:conn, indent_info)
    endif
endfunction

function! vlime#OnInvalidRPC(conn, msg)
    if type(a:conn.ui) != v:t_none
        let [_msg_type, id, err_msg] = a:msg
        call a:conn.ui.OnInvalidRPC(a:conn, id, err_msg)
    endif
endfunction

function! vlime#OnInspect(conn, msg)
    if type(a:conn.ui) != v:t_none
        let [_msg_type, i_content, i_thread, i_tag] = a:msg
        call a:conn.ui.OnInspect(a:conn, i_content, i_thread, i_tag)
    endif
endfunction

" ------------------ end of server event handlers ------------------

function! vlime#OnServerEvent(chan, msg) dict
    let chan_info = ch_info(self.channel)
    echom '==========================='
    echom chan_info['hostname'] . ':' . chan_info['port'] .
                \ ' -> ' . string(a:msg)
    let msg_type = a:msg[0]
    let Handler = get(self.server_event_handlers, msg_type['name'], v:null)
    if type(Handler) == v:t_func
        call Handler(self, a:msg)
    endif
endfunction

" ================== end of methods for vlime connections ==================

function! vlime#GetNthVarArg(args, n, ...)
    let n_args = [a:args, a:n]
    call extend(n_args, a:000)
    let Ref = function('s:GetNthVarArg', n_args)
    return Ref()
endfunction

function! s:SimpleSendCB(conn, Callback, caller, chan, msg) abort
    call s:CheckReturnStatus(a:msg, a:caller)
    call s:TryToCall(a:Callback, [a:conn, a:msg[1][1]])
endfunction

function! s:SLDBSendCB(conn, Callback, caller, chan, msg) abort
    let status = a:msg[1][0]
    if status['name'] != 'ABORT' && status['name'] != 'OK'
        throw caller . ' returned: ' . string(a:msg[1])
    endif
    call s:TryToCall(a:Callback, [a:conn, a:msg[1][1]])
endfunction

function! s:GetNthVarArg(args, n, ...)
    let def_val = v:null
    if a:0 == 1
        let def_val = a:1
    elseif a:0 != 0
        throw 's:GetNthVarArg: wrong # of arguments'
    endif

    if a:n >= len(a:args)
        return def_val
    else
        return a:args[a:n]
    endif
endfunction

function! vlime#PListToDict(plist)
    let d = {}
    let i = 0
    while i < len(a:plist)
        let d[a:plist[i]['name']] = a:plist[i+1]
        let i += 2
    endwhile
    return d
endfunction

function! vlime#ChainCallbacks(...)
    let cbs = a:000
    if len(cbs) <= 0
        return
    endif

    function! s:ChainCallbackCB(cbs, ...)
        if len(a:cbs) < 1
            return
        endif
        let CB = function(a:cbs[0], a:000)
        call CB()

        if len(a:cbs) < 2
            return
        endif
        let NextFunc = a:cbs[1]
        call NextFunc(function('s:ChainCallbackCB', [a:cbs[2:]]))
    endfunction

    let FirstFunc = cbs[0]
    call FirstFunc(function('s:ChainCallbackCB', [cbs[1:]]))
endfunction

function! s:SearchPList(plist, name)
    let i = 0
    while i < len(a:plist)
        if a:plist[i]['name'] == a:name
            return a:plist[i+1]
        endif
        let i += 2
endfunction

function! s:CheckReturnStatus(return_msg, caller)
    let status = a:return_msg[1][0]
    if status['name'] != 'OK'
        throw a:caller . ' returned: ' . string(a:return_msg[1])
    endif
endfunction

function! s:CheckAndReportReturnStatus(conn, return_msg, caller)
    let status = a:return_msg[1][0]
    if status['name'] == 'OK'
        return v:true
    elseif status['name'] == 'ABORT'
        call a:conn.ui.OnWriteString(a:conn, a:return_msg[1][1] . "\n",
                    \ {'name': 'ABORT-REASON', 'package': 'KEYWORD'})
        return v:false
    else
        call a:conn.ui.OnWriteString(a:conn, string(a:return_msg[1]),
                    \ {'name': 'UNKNOWN-ERROR', 'package': 'KEYWORD'})
        return v:false
    endif
endfunction

function! s:SYM(package, name)
    return {'name': a:name, 'package': a:package}
endfunction

function! s:KW(name)
    return s:SYM('KEYWORD', a:name)
endfunction

function! s:CL(name)
    return s:SYM('COMMON-LISP', a:name)
endfunction

function! s:EmacsRex(cmd, pkg, thread)
    return [s:KW('EMACS-REX'), a:cmd, a:pkg, a:thread]
endfunction

function! s:TryToCall(Callback, args)
    if type(a:Callback) == v:t_func
        let CB = function(a:Callback, a:args)
        call CB()
    endif
endfunction

function! vlime#DummyCB(conn, result)
    echom '---------------------------'
    echom string(a:result)
endfunction
