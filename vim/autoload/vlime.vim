function! vlime#New()
    let obj = {
                \ 'channel': v:null,
                \ 'repl_package': v:null,
                \ 'repl_prompt': v:null,
                \ 'Connect': function('vlime#Connect'),
                \ 'Close': function('vlime#Close'),
                \ 'Call': function('vlime#Call'),
                \ 'Send': function('vlime#Send'),
                \ 'ConnectionInfo': function('vlime#ConnectionInfo'),
                \ 'SwankRequire': function('vlime#SwankRequire'),
                \ 'CreateREPL': function('vlime#CreateREPL'),
                \ 'ListenerEval': function('vlime#ListenerEval'),
                \ 'SetPackage': function('vlime#SetPackage'),
                \ 'DescribeSymbol': function('vlime#DescribeSymbol'),
                \ 'OperatorArgList': function('vlime#OperatorArgList'),
                \ 'Interrupt': function('vlime#Interrupt'),
                \ 'SLDBAbort': function('vlime#SLDBAbort'),
                \ 'SLDBContinue': function('vlime#SLDBContinue'),
                \ 'InvokeNthRestartForEmacs': function('vlime#InvokeNthRestartForEmacs'),
                \ 'OnServerEvent': function('vlime#OnServerEvent'),
                \ 'server_event_handlers': {
                    \ 'NEW-PACKAGE': function('vlime#OnNewPackage')
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

" vlime#ConnectionInfo([return_dict[, callback]])
function! vlime#ConnectionInfo(...) dict
    " We pass local variables as extra arguments instead of
    " using the 'closure' flag on inner functions, to prevent
    " messed-up variable values caused by calling the outer
    " function more than once.
    function! s:ConnectionInfoCB(Callback, return_dict, chan, msg) abort
        call s:CheckReturnStatus(a:msg, 'vlime#ConnectionInfo')
        if a:return_dict
            call s:TryToCall(a:Callback, [s:PListToDict(a:msg[1][1])])
        else
            call s:TryToCall(a:Callback, [a:msg[1][1]])
        endif
    endfunction

    let return_dict = s:GetNthVarArg(a:000, 0, v:true)
    let Callback = s:GetNthVarArg(a:000, 1)
    call self.Send(s:EmacsRex(
                    \ [s:SYM('SWANK', 'CONNECTION-INFO')], v:null, v:true),
                \ function('s:ConnectionInfoCB', [Callback, return_dict]))
endfunction

" vlime#SwankRequire(contrib[, callback])
function! vlime#SwankRequire(contrib, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(s:EmacsRex(
                    \ [s:SYM('SWANK', 'SWANK-REQUIRE'), s:KW(a:contrib)],
                    \ v:null, v:true),
                \ function('s:SimpleSendCB', [Callback, 'vlime#SwankRequire']))
endfunction

" vlime#CreateREPL([coding_system[, callback]])
function! vlime#CreateREPL(...) dict
    function! s:CreateREPL_CB(Callback, conn, chan, msg) abort
        call s:CheckReturnStatus(a:msg, 'vlime#CreateREPL')
        let a:conn.repl_package = a:msg[1][1][0]
        let a:conn.repl_prompt = a:msg[1][1][1]
        call s:TryToCall(a:Callback, [a:msg[1][1]])    " [PACKAGE_NAME, PROMPT]
    endfunction

    let cmd = [s:SYM('SWANK-REPL', 'CREATE-REPL'), v:null]
    let coding_system = s:GetNthVarArg(a:000, 0)
    if coding_system != v:null
        let cmd += [s:KW('CODING-SYSTEM'), coding_system]
    endif
    let Callback = s:GetNthVarArg(a:000, 1)
    call self.Send(s:EmacsRex(cmd, v:null, v:true),
                \ function('s:CreateREPL_CB', [Callback, self]))
endfunction

" vlime#ListenerEval(expr[, callback])
function! vlime#ListenerEval(expr, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(s:EmacsRex(
                    \ [s:SYM('SWANK-REPL', 'LISTENER-EVAL'), a:expr],
                    \ self.repl_package, s:KW('REPL-THREAD')),
                \ function('s:SimpleSendCB', [Callback, 'vlime#ListenerEval']))
endfunction

function! vlime#Interrupt(thread) dict
    call self.Send([s:KW('EMACS-INTERRUPT'), a:thread])
endfunction

" vlime#SLDBAbort(thread[, callback])
function! vlime#SLDBAbort(thread, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(s:EmacsRex(
                    \ [s:SYM('SWANK', 'SLDB-ABORT')], v:null, a:thread),
                \ function('s:SLDBSendCB', [Callback, 'vlime#SLDBAbort']))
endfunction

" vlime#SLDBContinue(thread[, callback])
function! vlime#SLDBContinue(thread, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(s:EmacsRex(
                    \ [s:SYM('SWANK', 'SLDB-CONTINUE')], v:null, a:thread),
                \ function('s:SLDBSendCB', [Callback, 'vlime#SLDBContinue']))
endfunction

" vlime#InvokeNthRestartForEmacs(thread, level, restart[, callback])
function! vlime#InvokeNthRestartForEmacs(thread, level, restart, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(s:EmacsRex(
                    \ [s:SYM('SWANK', 'INVOKE-NTH-RESTART-FOR-EMACS'), a:level, a:restart],
                    \ v:null, a:thread),
                \ function('s:SLDBSendCB', [Callback, 'vlime#InvokeNthRestartForEmacs']))
endfunction

" vlime#SetPackage(package[, callback])
function! vlime#SetPackage(package, ...) dict
    function! s:SetPackageCB(Callback, conn, chan, msg) abort
        call s:CheckReturnStatus(a:msg, 'vlime#SetPackage')
        let a:conn.repl_package = a:msg[1][1][0]
        let a:conn.repl_prompt = a:msg[1][1][1]
        call s:TryToCall(a:Callback, [a:msg[1][1]])     " [PACKAGE_NAME, PROMPT]
    endfunction

    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(s:EmacsRex(
                    \ [s:SYM('SWANK', 'SET-PACKAGE'), a:package],
                    \ self.repl_package, s:KW('REPL-THREAD')),
                \ function('s:SetPackageCB', [Callback, self]))
endfunction

" vlime#DescribeSymbol(symbol[, callback])
function! vlime#DescribeSymbol(symbol, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(s:EmacsRex(
                    \ [s:SYM('SWANK', 'DESCRIBE-SYMBOL'), a:symbol],
                    \ self.repl_package, v:true),
                \ function('s:SimpleSendCB', [Callback, 'vlime#DescribeSymbol']))
endfunction

" vlime#OperatorArgList(operator[, callback])
function! vlime#OperatorArgList(operator, ...) dict
    let Callback = s:GetNthVarArg(a:000, 0)
    call self.Send(s:EmacsRex(
                    \ [s:SYM('SWANK', 'OPERATOR-ARGLIST'), a:operator, self.repl_package],
                    \ self.repl_package, v:true),
                \ function('s:SimpleSendCB', [Callback, 'vlime#OperatorArgList']))
endfunction

" ------------------ server event handlers ------------------

function! vlime#OnNewPackage(conn, msg)
    let a:conn.repl_package = a:msg[1]
    let a:conn.repl_prompt = a:msg[2]
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

function! s:SimpleSendCB(Callback, caller, chan, msg) abort
    call s:CheckReturnStatus(a:msg, a:caller)
    call s:TryToCall(a:Callback, [a:msg[1][1]])
endfunction

function! s:SLDBSendCB(Callback, caller, chan, msg) abort
    let status = a:msg[1][0]
    if status['name'] != 'ABORT' && status['name'] != 'OK'
        throw caller . ' returned: ' . string(a:msg[1])
    endif
    call s:TryToCall(a:Callback, [a:msg[1][1]])
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

function! s:PListToDict(plist)
    let d = {}
    let i = 0
    while i < len(a:plist)
        let d[a:plist[i]['name']] = a:plist[i+1]
        let i += 2
    endwhile
    return d
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

function! vlime#DummyCB(result)
    echom '---------------------------'
    echom string(a:result)
endfunction
