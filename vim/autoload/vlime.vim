" Vlime Connection constructor.
" vlime#New([cb_data[, ui]])
function! vlime#New(...)
    let cb_data = s:GetNthVarArg(a:000, 0)
    let ui = s:GetNthVarArg(a:000, 1)
    let obj = {
                \ 'cb_data': cb_data,
                \ 'channel': v:null,
                \ 'remote_prefix': '',
                \ 'ping_tag': 1,
                \ 'ui': ui,
                \ 'Connect': function('vlime#Connect'),
                \ 'IsConnected': function('vlime#IsConnected'),
                \ 'Close': function('vlime#Close'),
                \ 'Call': function('vlime#Call'),
                \ 'Send': function('vlime#Send'),
                \ 'FixRemotePath': function('vlime#FixRemotePath'),
                \ 'FixLocalPath': function('vlime#FixLocalPath'),
                \ 'GetCurrentPackage': function('vlime#GetCurrentPackage'),
                \ 'SetCurrentPackage': function('vlime#SetCurrentPackage'),
                \ 'GetCurrentThread': function('vlime#GetCurrentThread'),
                \ 'SetCurrentThread': function('vlime#SetCurrentThread'),
                \ 'WithPackage': function('vlime#WithPackage'),
                \ 'WithThread': function('vlime#WithThread'),
                \ 'EmacsRex': function('vlime#EmacsRex'),
                \ 'Ping': function('vlime#Ping'),
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
                \ 'XRef': function('vlime#XRef'),
                \ 'FindDefinitionsForEmacs': function('vlime#FindDefinitionsForEmacs'),
                \ 'AproposListForEmacs': function('vlime#AproposListForEmacs'),
                \ 'DocumentationSymbol': function('vlime#DocumentationSymbol'),
                \ 'Interrupt': function('vlime#Interrupt'),
                \ 'SLDBAbort': function('vlime#SLDBAbort'),
                \ 'SLDBBreak': function('vlime#SLDBBreak'),
                \ 'SLDBContinue': function('vlime#SLDBContinue'),
                \ 'SLDBStep': function('vlime#SLDBStep'),
                \ 'SLDBNext': function('vlime#SLDBNext'),
                \ 'SLDBOut': function('vlime#SLDBOut'),
                \ 'SLDBReturnFromFrame': function('vlime#SLDBReturnFromFrame'),
                \ 'SLDBDisassemble': function('vlime#SLDBDisassemble'),
                \ 'InvokeNthRestartForEmacs': function('vlime#InvokeNthRestartForEmacs'),
                \ 'RestartFrame': function('vlime#RestartFrame'),
                \ 'FrameLocalsAndCatchTags': function('vlime#FrameLocalsAndCatchTags'),
                \ 'FrameSourceLocation': function('vlime#FrameSourceLocation'),
                \ 'EvalStringInFrame': function('vlime#EvalStringInFrame'),
                \ 'InitInspector': function('vlime#InitInspector'),
                \ 'InspectorReinspect': function('vlime#InspectorReinspect'),
                \ 'InspectorRange': function('vlime#InspectorRange'),
                \ 'InspectNthPart': function('vlime#InspectNthPart'),
                \ 'InspectorCallNthAction': function('vlime#InspectorCallNthAction'),
                \ 'InspectorPop': function('vlime#InspectorPop'),
                \ 'InspectCurrentCondition': function('vlime#InspectCurrentCondition'),
                \ 'InspectInFrame': function('vlime#InspectInFrame'),
                \ 'InspectPresentation': function('vlime#InspectPresentation'),
                \ 'ListThreads': function('vlime#ListThreads'),
                \ 'KillNthThread': function('vlime#KillNthThread'),
                \ 'DebugNthThread': function('vlime#DebugNthThread'),
                \ 'UndefineFunction': function('vlime#UndefineFunction'),
                \ 'UninternSymbol': function('vlime#UninternSymbol'),
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

" vlime#Connect(host, port[, remote_prefix])
function! vlime#Connect(host, port, ...) dict
    let self.channel = vlime#compat#ch_open(a:host, a:port,
                \ {chan, msg -> self.OnServerEvent(chan, msg)})
    " XXX: There should be a better way to wait for ncat
    sleep 500m
    if vlime#compat#ch_status(self.channel) != 'open'
        call self.Close()
        throw 'vlime#Connect: failed to open channel'
    endif

    let remote_prefix = vlime#GetNthVarArg(a:000, 0, '')
    let self['remote_prefix'] = remote_prefix

    return self
endfunction

function! vlime#IsConnected() dict
    return type(self.channel) == vlime#compat#ch_type() &&
                \ vlime#compat#ch_status(self.channel) == 'open'
endfunction

function! vlime#Close() dict
    if type(self.channel) == vlime#compat#ch_type()
        try
            call vlime#compat#ch_close(self.channel)
        catch /^vlime#compat#.*not an open channel.*/
        endtry
        let self.channel = v:null
    endif
    return self
endfunction

function! vlime#Call(msg) dict
    return vlime#compat#ch_evalexpr(self.channel, a:msg)
endfunction

" vlime#Send(msg[, callback])
function! vlime#Send(msg, ...) dict
    let Callback = vlime#GetNthVarArg(a:000, 0, v:null)
    if type(Callback) != type(v:null)
        call vlime#compat#ch_sendexpr(self.channel, a:msg, Callback)
    else
        call vlime#compat#ch_sendexpr(self.channel, a:msg)
    endif
endfunction

function! vlime#FixRemotePath(path) dict
    if type(a:path) == v:t_string
        return self['remote_prefix'] . a:path
    elseif type(a:path) == v:t_list && type(a:path[0]) == v:t_dict
                \ && a:path[0]['name'] == 'LOCATION'
        if a:path[1][0]['name'] == 'FILE'
            let a:path[1][1] = self['remote_prefix'] . a:path[1][1]
        elseif a:path[1][0]['name'] == 'BUFFER-AND-FILE'
            let a:path[1][2] = self['remote_prefix'] . a:path[1][2]
        endif
        return a:path
    else
        throw 'vlime#FixRemotePath: unknown path: ' . string(a:path)
    endif
endfunction

function! vlime#FixLocalPath(path) dict
    let prefix_len = len(self['remote_prefix'])
    if prefix_len > 0 && a:path[0:prefix_len-1] == self['remote_prefix']
        return a:path[prefix_len:]
    else
        return a:path
    endif
endfunction

function! vlime#GetCurrentPackage() dict
    if type(self.ui) != type(v:null)
        return self.ui.GetCurrentPackage()
    else
        return v:null
    endif
endfunction

function! vlime#SetCurrentPackage(package) dict
    if type(self.ui) != type(v:null)
        call self.ui.SetCurrentPackage(a:package)
    endif
endfunction

function! vlime#GetCurrentThread() dict
    if type(self.ui) != type(v:null)
        return self.ui.GetCurrentThread()
    else
        return v:true
    endif
endfunction

function! vlime#SetCurrentThread(thread) dict
    if type(self.ui) != type(v:null)
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

function! vlime#Ping() dict
    let cur_tag = self.ping_tag
    let self.ping_tag = (self.ping_tag >= 65536) ? 1 : (self.ping_tag + 1)

    let result = self.Call(self.EmacsRex([s:SYM('SWANK', 'PING'), cur_tag]))
    if type(result) == v:t_string && len(result) == 0
        " Error or timeout
        throw 'vlime#Ping: failed'
    endif

    call s:CheckReturnStatus(result, 'vlime#Ping')
    if result[1][1] != cur_tag
        throw 'vlime#Ping: bad tag'
    endif
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
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#SwankRequire']))
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

" vlime#SLDBBreak(func_name[, callback])
function! vlime#SLDBBreak(func_name, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'SLDB-BREAK'), a:func_name]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#SLDBBreak']))
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

" vlime#SLDBReturnFromFrame(frame, str[, callback]) dict
function! vlime#SLDBReturnFromFrame(frame, str, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'SLDB-RETURN-FROM-FRAME'), a:frame, a:str]),
                \ function('s:SLDBSendCB',
                    \ [self, Callback, 'vlime#SLDBReturnFromFrame']))
endfunction

" vlime#SLDBDisassemble(frame[, callback]) dict
function! vlime#SLDBDisassemble(frame, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'SLDB-DISASSEMBLE'), a:frame]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#SLDBDisassemble']))
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
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#FrameLocalsAndCatchTags']))
endfunction

" vlime#FrameSourceLocation(frame[, callback])
function! vlime#FrameSourceLocation(frame, ...) dict
    function! s:FrameSourceLocationCB(conn, Callback, chan, msg)
        call s:CheckReturnStatus(a:msg,  'vlime#FrameSourceLocation')
        if a:msg[1][1][0]['name'] == 'LOCATION'
            let fixed_loc = a:conn.FixRemotePath(a:msg[1][1])
        else
            let fixed_loc = a:msg[1][1]
        endif
        call s:TryToCall(a:Callback, [a:conn, fixed_loc])
    endfunction

    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'FRAME-SOURCE-LOCATION'), a:frame]),
                \ function('s:FrameSourceLocationCB', [self, Callback]))
endfunction

" vlime#EvalStringInFrame(str, frame, package[, callback]) dict
function! vlime#EvalStringInFrame(str, frame, package, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'EVAL-STRING-IN-FRAME'),
                        \ a:str, a:frame, a:package]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#EvalStringInFrame']))
endfunction

" vlime#InitInspector(thing[, callback])
function! vlime#InitInspector(thing, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INIT-INSPECTOR'), a:thing]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#InitInspector']))
endfunction

" vlime#InspectorReinspect([callback])
function! vlime#InspectorReinspect(...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INSPECTOR-REINSPECT')]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#InspectorReinspect']))
endfunction

" vlime#InspectorRange(r_start, r_end[, callback])
function! vlime#InspectorRange(r_start, r_end, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INSPECTOR-RANGE'), a:r_start, a:r_end]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#InspectorRange']))
endfunction

" vlime#InspectNthPart(nth[, callback])
function! vlime#InspectNthPart(nth, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INSPECT-NTH-PART'), a:nth]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#InspectNthPart']))
endfunction

" vlime#InspectorCallNthAction(nth[, callback])
function! vlime#InspectorCallNthAction(nth, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INSPECTOR-CALL-NTH-ACTION'), a:nth]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#InspectorCallNthAction']))
endfunction

" vlime#InspectorPop([callback])
function! vlime#InspectorPop(...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INSPECTOR-POP')]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#InspectorPop']))
endfunction

" vlime#InspectCurrentCondition([callback])
function! vlime#InspectCurrentCondition(...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INSPECT-CURRENT-CONDITION')]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#InspectCurrentCondition']))
endfunction

" vlime#InspectInFrame(thing, frame[, callback])
function! vlime#InspectInFrame(thing, frame, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INSPECT-IN-FRAME'), a:thing, a:frame]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#InspectInFrame']))
endfunction

" vlime#InspectPresentation(pres_id, reset[, callback])
function! vlime#InspectPresentation(pres_id, reset, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'INSPECT-PRESENTATION'), a:pres_id, a:reset]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#InspectPresentation']))
endfunction

" vlime#ListThreads([callback])
function! vlime#ListThreads(...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'LIST-THREADS')]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#ListThreads']))
endfunction

" vlime#KillNthThread(nth[, callback])
function! vlime#KillNthThread(nth, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'KILL-NTH-THREAD'), a:nth]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#KillNthThread']))
endfunction

" vlime#DebugNthThread(nth[, callback])
function! vlime#DebugNthThread(nth, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'DEBUG-NTH-THREAD'), a:nth]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#DebugNthThread']))
endfunction

" vlime#UndefineFunction(func_name[, callback])
function! vlime#UndefineFunction(func_name, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'UNDEFINE-FUNCTION'), a:func_name]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#UndefineFunction']))
endfunction

"vlime#UninternSymbol(sym_name[, package[, callback]])
function! vlime#UninternSymbol(sym_name, ...) dict
    let pkg = vlime#GetNthVarArg(a:000, 0, v:null)
    let Callback = vlime#GetNthVarArg(a:000, 1, v:null)
    if type(pkg) == type(v:null)
        let pkg_info = self.GetCurrentPackage()
        if type(pkg_info) == v:t_list
            let pkg = pkg_info[0]
        endif
    endif

    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'UNINTERN-SYMBOL'), a:sym_name, pkg]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#UninternSymbol']))
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
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#DescribeSymbol']))
endfunction

" vlime#OperatorArgList(operator[, callback])
function! vlime#OperatorArgList(operator, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    let cur_package = self.GetCurrentPackage()
    if type(cur_package) != type(v:null)
        let cur_package = cur_package[0]
    endif
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'OPERATOR-ARGLIST'), a:operator, cur_package]),
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#OperatorArgList']))
endfunction

" vlime#SimpleCompletions(symbol[, callback])
function! vlime#SimpleCompletions(symbol, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    let cur_package = self.GetCurrentPackage()
    if type(cur_package) != type(v:null)
        let cur_package = cur_package[0]
    endif
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'SIMPLE-COMPLETIONS'), a:symbol, cur_package]),
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#SimpleCompletions']))
endfunction

" vlime#FuzzyCompletions(symbol[, callback])
function! vlime#FuzzyCompletions(symbol, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    let cur_package = self.GetCurrentPackage()
    if type(cur_package) != type(v:null)
        let cur_package = cur_package[0]
    endif
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'FUZZY-COMPLETIONS'), a:symbol, cur_package]),
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#FuzzyCompletions']))
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
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#SwankMacroExpandOne']))
endfunction

" vlime#SwankMacroExpand(expr[, callback])
function! vlime#SwankMacroExpand(expr, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'SWANK-MACROEXPAND'), a:expr]),
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#SwankMacroExpand']))
endfunction

" vlime#SwankMacroExpandAll(expr[, callback])
function! vlime#SwankMacroExpandAll(expr, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'SWANK-MACROEXPAND-ALL'), a:expr]),
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#SwankMacroExpandAll']))
endfunction

" vlime#DisassembleForm(expr[, callback])
function! vlime#DisassembleForm(expr, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'DISASSEMBLE-FORM'), a:expr]),
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#DisassembleForm']))
endfunction

" vlime#CompileStringForEmacs(expr, buffer, position, filename[, policy[, callback]])
function! vlime#CompileStringForEmacs(expr, buffer, position, filename, ...) dict
    let policy = s:TransformCompilerPolicy(s:GetNthVarArg(a:000, 0))
    let Callback = s:GetNthVarArg(a:000, 1)
    let fixed_filename = self.FixLocalPath(a:filename)
    call self.Send(self.EmacsRex(
                    \ [s:SYM('SWANK', 'COMPILE-STRING-FOR-EMACS'),
                        \ a:expr, a:buffer,
                        \ [s:CL('QUOTE'), [[s:KW('POSITION'), a:position]]],
                        \ fixed_filename, policy]),
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#CompileStringForEmacs']))
endfunction


" vlime#CompileFileForEmacs(filename[, load[, policy[, callback]]]) dict
function! vlime#CompileFileForEmacs(filename, ...) dict
    let load = s:GetNthVarArg(a:000, 0, v:true)
    let policy = s:TransformCompilerPolicy(s:GetNthVarArg(a:000, 1))
    let Callback = s:GetNthVarArg(a:000, 2)
    let fixed_filename = self.FixLocalPath(a:filename)
    let cmd = [s:SYM('SWANK', 'COMPILE-FILE-FOR-EMACS'), fixed_filename, load]
    if type(policy) != type(v:null)
        let cmd += [s:KW('POLICY'), policy]
    endif
    call self.Send(self.EmacsRex(cmd),
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#CompileFileForEmacs']))
endfunction

" vlime#LoadFile(filename[, callback])
function! vlime#LoadFile(filename, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    let fixed_filename = self.FixLocalPath(a:filename)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'LOAD-FILE'), fixed_filename]),
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#LoadFile']))
endfunction

" vlime#XRef(ref_type, name[, callback])
function! vlime#XRef(ref_type, name, ...) dict
    function! s:XRefCB(conn, Callback, chan, msg)
        call s:CheckReturnStatus(a:msg,  'vlime#XRef')
        call s:FixXRefListPaths(a:conn, a:msg[1][1])
        call s:TryToCall(a:Callback, [a:conn, a:msg[1][1]])
    endfunction

    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'XREF'), s:KW(a:ref_type), a:name]),
                \ function('s:XRefCB', [self, Callback]))
endfunction

" vlime#FindDefinitionsForEmacs(name[, callback])
function! vlime#FindDefinitionsForEmacs(name, ...) dict
    function! s:FindDefinitionsForEmacsCB(conn, Callback, chan, msg)
        call s:CheckReturnStatus(a:msg, 'vlime#FindDefinitionsForEmacs')
        call s:FixXRefListPaths(a:conn, a:msg[1][1])
        call s:TryToCall(a:Callback, [a:conn, a:msg[1][1]])
    endfunction

    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'FIND-DEFINITIONS-FOR-EMACS'), a:name]),
                \ function('s:FindDefinitionsForEmacsCB', [self, Callback]))
endfunction

" vlime#AproposListForEmacs(name, external_only, case_sensitive, package[, callback])
function! vlime#AproposListForEmacs(name, external_only, case_sensitive, package, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'APROPOS-LIST-FOR-EMACS'),
                    \ a:name, a:external_only, a:case_sensitive, a:package]),
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#AproposListForEmacs']))
endfunction

" vlime#DocumentationSymbol(sym_name[, callback])
function! vlime#DocumentationSymbol(sym_name, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(self.EmacsRex([s:SYM('SWANK', 'DOCUMENTATION-SYMBOL'), a:sym_name]),
                \ function('vlime#SimpleSendCB', [self, Callback, 'vlime#DocumentationSymbol']))
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
    if type(a:conn.ui) != type(v:null)
        let [_msg_type, thread, level, condition, restarts, frames, conts] = a:msg
        call a:conn.ui.OnDebug(a:conn, thread, level, condition, restarts, frames, conts)
    endif
endfunction

function! vlime#OnDebugActivate(conn, msg)
    if type(a:conn.ui) != type(v:null)
        let [_msg_type, thread, level, select] = a:msg
        call a:conn.ui.OnDebugActivate(a:conn, thread, level, select)
    endif
endfunction

function! vlime#OnDebugReturn(conn, msg)
    if type(a:conn.ui) != type(v:null)
        let [_msg_type, thread, level, stepping] = a:msg
        call a:conn.ui.OnDebugReturn(a:conn, thread, level, stepping)
    endif
endfunction

function! vlime#OnWriteString(conn, msg)
    if type(a:conn.ui) != type(v:null)
        let str = a:msg[1]
        let str_type = (len(a:msg) >= 3) ? a:msg[2] : v:null
        call a:conn.ui.OnWriteString(a:conn, str, str_type)
    endif
endfunction

function! vlime#OnReadString(conn, msg)
    if type(a:conn.ui) != type(v:null)
        let [_msg_type, thread, ttag] = a:msg
        call a:conn.ui.OnReadString(a:conn, thread, ttag)
    endif
endfunction

function! vlime#OnReadFromMiniBuffer(conn, msg)
    if type(a:conn.ui) != type(v:null)
        let [_msg_type, thread, ttag, prompt, init_val] = a:msg
        call a:conn.ui.OnReadFromMiniBuffer(
                    \ a:conn, thread, ttag, prompt, init_val)
    endif
endfunction

function! vlime#OnIndentationUpdate(conn, msg)
    if type(a:conn.ui) != type(v:null)
        let [_msg_type, indent_info] = a:msg
        call a:conn.ui.OnIndentationUpdate(a:conn, indent_info)
    endif
endfunction

function! vlime#OnInvalidRPC(conn, msg)
    if type(a:conn.ui) != type(v:null)
        let [_msg_type, id, err_msg] = a:msg
        call a:conn.ui.OnInvalidRPC(a:conn, id, err_msg)
    endif
endfunction

function! vlime#OnInspect(conn, msg)
    if type(a:conn.ui) != type(v:null)
        let [_msg_type, i_content, i_thread, i_tag] = a:msg
        call a:conn.ui.OnInspect(a:conn, i_content, i_thread, i_tag)
    endif
endfunction

" ------------------ end of server event handlers ------------------

function! vlime#OnServerEvent(chan, msg) dict
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

function! vlime#SimpleSendCB(conn, Callback, caller, chan, msg) abort
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
    if type(a:plist) == type(v:null)
        return {}
    endif

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

function! vlime#ParseSourceLocation(loc)
    if type(a:loc[0]) != v:t_dict || a:loc[0]['name'] != 'LOCATION'
        throw 'vlime#ParseSourceLocation: invalid location: ' . string(a:loc)
    endif

    let loc_obj = {}

    for p in a:loc[1:]
        if type(p) != v:t_list
            continue
        endif

        if len(p) == 1
            let loc_obj[p[0]['name']] = v:null
        elseif len(p) == 2
            let loc_obj[p[0]['name']] = p[1]
        elseif len(p) > 2
            let loc_obj[p[0]['name']] = p[1:]
        endif
    endfor

    return loc_obj
endfunction

function! vlime#GetValidSourceLocation(loc)
    let loc_file = get(a:loc, 'FILE', v:null)
    let loc_buffer = get(a:loc, 'BUFFER', v:null)
    let loc_buf_and_file = get(a:loc, 'BUFFER-AND-FILE', v:null)

    if type(loc_file) != type(v:null)
        let loc_pos = get(a:loc, 'POSITION', v:null)
        let valid_loc = [loc_file, loc_pos]
    elseif type(loc_buffer) != type(v:null)
        let loc_offset = get(a:loc, 'OFFSET', v:null)
        if type(loc_offset) != type(v:null)
            let loc_offset = loc_offset[0] + loc_offset[1]
        endif
        let valid_loc = [loc_buffer, loc_offset]
    elseif type(loc_buf_and_file) != type(v:null)
        let loc_offset = get(a:loc, 'OFFSET', v:null)
        if type(loc_offset) != type(v:null)
            let loc_offset = loc_offset[0] + loc_offset[1]
        endif
        let valid_loc = [loc_buf_and_file[0], loc_offset]
    else
        let valid_loc = []
    endif

    return valid_loc
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

function! s:FixXRefListPaths(conn, xref_list)
    if type(a:xref_list) != v:t_list
        return
    endif

    for spec in a:xref_list
        if type(spec[0]) == v:t_string && spec[1][0]['name'] == 'LOCATION'
            let spec[1] = a:conn.FixRemotePath(spec[1])
        endif
    endfor
endfunction

function! s:TransformCompilerPolicy(policy)
    if type(a:policy) == v:t_dict
        let plc_list = []
        for [key, val] in items(a:policy)
            call add(plc_list, {'head': [s:CL(key)], 'tail': val})
        endfor
        return [s:CL('QUOTE'), plc_list]
    else
        return a:policy
    endif
endfunction

function! vlime#DummyCB(conn, result)
    echom '---------------------------'
    echom string(a:result)
endfunction
