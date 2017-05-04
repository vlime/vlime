function! vlime#compat#vim#ch_open(host, port, callback)
    let opts = {'mode': 'json'}
    if type(a:callback) != type(v:null)
        let opts['callback'] = a:callback
    endif
    return ch_open(a:host . ':' . string(a:port), opts)
endfunction

function! vlime#compat#vim#ch_status(chan)
    let stat = ch_status(a:chan)
    return (stat == 'open') ? 'open' : 'closed'
endfunction

function! vlime#compat#vim#ch_info(chan)
    let info = ch_info(a:chan)
    return {'hostname': info['hostname'], 'port': info['port']}
endfunction

function! vlime#compat#vim#ch_close(chan)
    try
        return ch_close(a:chan)
    catch /^Vim\%((\a\+)\)\=:E906/  " Not an open channel
        throw 'vlime#compat#vim#ch_close: not an open channel'
    endtry
endfunction

function! vlime#compat#vim#ch_evalexpr(chan, expr)
    return ch_evalexpr(a:chan, a:expr)
endfunction

" vlime#compat#vim#ch_sendexpr(chan, expr, callback)
function! vlime#compat#vim#ch_sendexpr(chan, expr, callback)
    if type(a:callback) == type(v:null)
        return ch_sendexpr(a:chan, a:expr)
    else
        return ch_sendexpr(a:chan, a:expr, {'callback': a:callback})
    endif
endfunction
