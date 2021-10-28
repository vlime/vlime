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
    return s:ch_impl.ch_sendexpr(a:chan, a:expr, get(a:000, 0, v:null), get(a:000, 1, -1))
endfunction

function! vlime#compat#ch_sendraw(chan, msg)
    let l_str = printf("%06x", len(a:msg))
    let msg = l_str . a:msg
    return s:ch_impl.ch_sendraw(a:chan, msg)
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
