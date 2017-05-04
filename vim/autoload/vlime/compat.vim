if !exists('s:ch_impl')
    let s:ch_impl = {}
    let s:ch_impl_functions = ['ch_open', 'ch_status', 'ch_info', 'ch_close', 'ch_evalexpr', 'ch_sendexpr']

    if has('nvim')
        let s:ch_impl_name = 'neovim'
    else
        let s:ch_impl_name = 'vim'
    endif

    for impl_func in s:ch_impl_functions
        let s:ch_impl[impl_func] = function('vlime#compat#' . s:ch_impl_name . '#' . impl_func)
    endfor
endif


" vlime#compat#ch_open(host, port[, callback])
function! vlime#compat#ch_open(host, port, ...)
    let callback = vlime#GetNthVarArg(a:000, 0, v:null)
    return s:ch_impl.ch_open(a:host, a:port, callback)
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

" vlime#compat#ch_sendexpr(chan, expr[, callback])
function! vlime#compat#ch_sendexpr(chan, expr, ...)
    let callback = vlime#GetNthVarArg(a:000, 0, v:null)
    return s:ch_impl.ch_sendexpr(a:chan, a:expr, callback)
endfunction
