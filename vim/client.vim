
let s:vlime_channel = v:null

function! OnVlimeEvent(chan, msg)
    echom string(a:msg)
endfunction

function! NewVlimeConnection(host, port)
    return ch_open(a:host . ':' . a:port, {'mode': 'json', 'callback': 'OnVlimeEvent'})
endfunction

function! VlimeConnect(host, port)
    if type(s:vlime_channel) != v:t_channel
        let s:vlime_channel = NewVlimeConnection(a:host, a:port)
    elseif ch_status(s:vlime_channel) != 'open'
        call VlimeClose()
        let s:vlime_channel = NewVlimeConnection(a:host, a:port)
    endif
    return s:vlime_channel
endfunction

function! VlimeClose()
    if type(s:vlime_channel) == v:t_channel
        try
            call ch_close(s:vlime_channel)
        catch /^Vim\%((\a\+)\)\=:E906/  "Not an open channel
        endtry
    endif
endfunction

function! SYM(package, name)
    return {'name': a:name, 'package': a:package}
endfunction

function! KW(name)
    return SYM('KEYWORD', a:name)
endfunction

function! CL(name)
    return SYM('COMMON-LISP', a:name)
endfunction

function! EmacsRex(cmd, pkg, thread)
    return [KW('EMACS-REX'), a:cmd, a:pkg, a:thread]
endfunction

function! VlimeCall(msg)
    return ch_evalexpr(s:vlime_channel, a:msg)
endfunction

function! VlimeConnectionInfo()
    let raw = VlimeCall(EmacsRex([SYM('SWANK', 'CONNECTION-INFO')], v:null, v:true))
    call VlimeCheckReturnStatus(raw, 'VlimeConnectionInfo')
    return PListToDict(raw[1][1])
endfunction

function! VlimeCreateREPL(...)
    " TODO: Cache the current package
    let pkg_info = PListToDict(VlimeConnectionInfo()['PACKAGE'])
    if a:0 == 0
        let raw = VlimeCall(EmacsRex([SYM('SWANK-REPL', 'CREATE-REPL'), v:null], pkg_info['NAME'], v:true))
    elseif a:0 == 1
        let raw = VlimeCall(EmacsRex([SYM('SWANK-REPL', 'CREATE-REPL'), v:null, KW('CODING-SYSTEM'), a:1], pkg_info['NAME'], v:true))
    endif
    call VlimeCheckReturnStatus(raw, 'VlimeConnectionInfo')
    return raw[1][1]    " [PACKAGE_NAME, PROMPT]
endfunction

function! VlimeListenerEval(expr)
    " TODO: Cache the current package
    let pkg_info = PListToDict(VlimeConnectionInfo()['PACKAGE'])
    let raw = VlimeCall(EmacsRex([SYM('SWANK-REPL', 'LISTENER-EVAL'), a:expr], pkg_info['NAME'], v:true))
    call VlimeCheckReturnStatus(raw, 'VlimeListenerEval')
    return raw[1][1]
endfunction

function! VlimeSLDBAbort(thread)
    return VlimeCall(EmacsRex([SYM('SWANK', 'SLDB-ABORT')], v:null, a:thread))
endfunction

function! VlimeCheckReturnStatus(return_msg, caller)
    let status = a:return_msg[1][0]
    if status['name'] != 'OK'
        throw a:caller . ': ' . a:return_msg[1]
    endif
endfunction

function! PListToDict(plist)
    let d = {}
    let i = 0
    while i < len(a:plist)
        let d[a:plist[i]['name']] = a:plist[i+1]
        let i += 2
    endwhile
    return d
endfunction
