function! s:BuildImpl(impl, impl_functions)
    let impl_name = has('nvim') ? 'neovim' : 'vim'
    for impl_func in a:impl_functions
        let a:impl[impl_func] = function('vlime#compat#' . impl_name . '#' . impl_func)
    endfor
endfunction

if !exists('s:ch_impl')
    let s:ch_impl = {}
    call s:BuildImpl(s:ch_impl,
                \ [
                    \ 'ch_type',
                    \ 'ch_open',
                    \ 'ch_status',
                    \ 'ch_info',
                    \ 'ch_close',
                    \ 'ch_evalexpr',
                    \ 'ch_sendexpr',
                    \ 'ch_sendraw',
                \ ])
endif

if !exists('s:job_impl')
    let s:job_impl = {}
    call s:BuildImpl(s:job_impl,
                \ [
                    \ 'job_start',
                    \ 'job_stop',
                    \ 'job_status',
                    \ 'job_getbufnr',
                \ ])
endif


function! vlime#compat#ch_type()
    return s:ch_impl.ch_type()
endfunction

" vlime#compat#ch_open(host, port[, callback[, timeout]])
function! vlime#compat#ch_open(host, port, ...)
    let Callback = get(a:000, 0, v:null)
    let timeout = get(a:000, 1, v:null)
    return s:ch_impl.ch_open(a:host, a:port, Callback, timeout)
endfunction

function! vlime#compat#ch_status(chan)
    return s:ch_impl.ch_status(a:chan)
endfunction

function! vlime#compat#ch_info(chan)
    return s:ch_impl.ch_info(a:chan)
endfunction

function! vlime#compat#ch_close(chan)
    return s:ch_impl.ch_close(a:chan)
endfunction

function! vlime#compat#ch_evalexpr(chan, expr)
    return s:ch_impl.ch_evalexpr(a:chan, a:expr)
endfunction

" vlime#compat#ch_sendexpr(chan, expr[, callback[, raw-or-tag]])
function! vlime#compat#ch_sendexpr(chan, expr, ...)
    let Callback = get(a:000, 0, v:null)
    let raw_or_tag = get(a:000, 1, -1)

    let msg = a:expr
    if raw_or_tag == -1
        call add(msg, a:chan.next_msg_id)
    elseif  raw_or_tag > 0
        call add(msg, raw_or_tag)
    endif

    let json = json_encode(msg) . "\n"
    call vlime#compat#ch_sendraw(a:chan, json)
    if type(Callback) != type(v:null)
        let a:chan.msg_callbacks[a:chan.next_msg_id] = Callback
        "echomsg  "set idx " a:chan.next_msg_id " for " msg
        " cb " Callback
    endif
    call s:IncMsgID(a:chan)
endfunction

function! s:IncMsgID(chan)
    if a:chan.next_msg_id >= 65535
        let a:chan.next_msg_id = 1
    else
        let a:chan.next_msg_id += 1
    endif
endfunction

function! vlime#compat#ch_sendraw(chan, msg)
    let l_str = printf("%06x", len(a:msg))
    let msg = l_str . a:msg
    let ret = s:ch_impl.ch_sendraw(a:chan, msg)
    if ret == 0
        throw 'vlime#compat#ch_sendraw failed [ret= ' . ret . ']'
    endif
endfunction

function! vlime#compat#job_start(cmd, opts)
    return s:job_impl.job_start(a:cmd, a:opts)
endfunction

function! vlime#compat#job_stop(job)
    return s:job_impl.job_stop(a:job)
endfunction

function! vlime#compat#job_status(job)
    return s:job_impl.job_status(a:job)
endfunction

function! vlime#compat#job_getbufnr(job)
    return s:job_impl.job_getbufnr(a:job)
endfunction
