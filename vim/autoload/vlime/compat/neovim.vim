function! vlime#compat#neovim#ch_open(host, port, callback)
    let chan_obj = {
                \ 'hostname': a:host,
                \ 'port': a:port,
                \ 'on_stdout': function('s:ChanInputCB'),
                \ 'next_msg_id': 1,
                \ 'msg_callbacks': {},
                \ }
    if type(a:callback) != type(v:null)
        let chan_obj['chan_callback'] = a:callback
    endif

    let job_id = jobstart(['ncat', a:host, string(a:port)], chan_obj)
    let chan_obj['job_id'] = job_id
    return chan_obj
endfunction

function! vlime#compat#neovim#ch_status(chan)
    try
        let job_pid = jobpid(a:chan.job_id)
    catch /^Vim\%((\a\+)\)\=:E900/  " Invalid job id
        let job_pid = 0
    endtry

    return (job_pid > 0) ? 'open' : 'closed'
endfunction

function! vlime#compat#neovim#ch_info(chan)
    return {'hostname': a:chan.hostname, 'port': a:chan.port}
endfunction

function! vlime#compat#neovim#ch_close(chan)
    try
        return jobstop(a:chan.job_id)
    catch /^Vim\%((\a\+)\)\=:E900/  " Invalid job id
        " The job already stopped
        throw 'vlime#compat#neovim#ch_close: not an open channel'
    endtry
endfunction

function! vlime#compat#neovim#ch_evalexpr(chan, expr)
    throw 'vlime#compat#neovim#ch_evalexpr: not supported'
endfunction

" vlime#compat#neovim#ch_sendexpr(chan, expr, callback)
function! vlime#compat#neovim#ch_sendexpr(chan, expr, callback) 
    let msg = [a:chan.next_msg_id, a:expr]

    if jobsend(a:chan.job_id, json_encode(msg) . "\n")
        if type(a:callback) != type(v:null)
            let a:chan.msg_callbacks[a:chan.next_msg_id] = a:callback
        endif
        call s:IncMsgID(a:chan)
    else
        throw 'vlime#compat#neovim#ch_sendexpr: jobsend() failed'
    endif
endfunction

function! s:ChanInputCB(job_id, data, source) dict
    echom '==========================='
    echom 'job_id: ' . string(a:job_id)
    echom 'data: ' . string(a:data)
    echom 'source: ' . string(a:source)
    echom 'self: ' . string(self)
endfunction

function! s:IncMsgID(chan)
    if a:chan.next_msg_id >= 65535
        let a:chan.next_msg_id = 1
    else
        let a:chan.next_msg_id += 1
    endif
endfunction
