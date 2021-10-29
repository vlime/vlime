function! vlime#compat#vim#ch_type()
    return v:t_channel
endfunction

function! vlime#compat#vim#ch_open(host, port, callback, timeout)
    let chan_obj = {
                \ 'on_data': function('vlime#compat#vim#ch_on_data'),
                \ 'callback': a:callback,
                \ }

    let opts = {
                \ 'mode': 'raw',
                \ 'callback': chan_obj.on_data,
                \ }
    if type(a:timeout) != type(v:null)
        let opts['waittime'] = a:timeout
    endif
    let chan_obj["ch"] = ch_open(a:host . ':' . string(a:port), opts)
    return chan_obj
endfunction

function! vlime#compat#vim#ch_on_data(ch, msg) dict
    let CB = get(self, 'callback')
    call CB(self, a:msg)
endfunction

function! vlime#compat#vim#ch_status(chan)
    let stat = ch_status(a:chan.ch)
    return (stat == 'open') ? 'open' : 'closed'
endfunction

function! vlime#compat#vim#ch_info(chan)
    let info = ch_info(a:chan.ch)
    return {'hostname': info['hostname'], 'port': info['port']}
endfunction

function! vlime#compat#vim#ch_close(chan)
    try
        return ch_close(a:chan.ch)
    catch /^Vim\%((\a\+)\)\=:E906/  " Not an open channel
        throw 'vlime#compat#vim#ch_close: not an open channel'
    endtry
endfunction

function! vlime#compat#vim#ch_evalexpr(chan, expr)
    return ch_evalexpr(a:chan.ch, a:expr)
endfunction

function! vlime#compat#vim#ch_sendraw(chan, msg)
    call ch_sendraw(a:chan.ch, a:msg)
    return 1 "XXX better error management?!
endfunction

function! vlime#compat#vim#job_start(cmd, opts)
    let buf_name = a:opts['buf_name']
    let Callback = a:opts['callback']
    let ExitCB = a:opts['exit_cb']
    let use_terminal = a:opts['use_terminal']

    if use_terminal
        let term_opts = {
                    \ 'out_cb': function('s:JobOutputCB', [Callback]),
                    \ 'err_cb': function('s:JobOutputCB', [Callback]),
                    \ 'exit_cb': function('s:JobExitCB', [ExitCB]),
                    \ 'curwin': v:true,
                    \ }
        let term_buf = term_start(a:cmd, term_opts)
        return term_getjob(term_buf)
    else
        let job_opts = {
                    \ 'in_io': 'pipe',
                    \ 'out_io': 'buffer',
                    \ 'err_io': 'buffer',
                    \ 'out_name': buf_name,
                    \ 'err_name': buf_name,
                    \ 'in_mode': 'nl',
                    \ 'out_mode': 'nl',
                    \ 'err_mode': 'nl',
                    \ 'out_modifiable': 0,
                    \ 'err_modifiable': 0,
                    \ 'out_cb': function('s:JobOutputCB', [Callback]),
                    \ 'err_cb': function('s:JobOutputCB', [Callback]),
                    \ 'exit_cb': function('s:JobExitCB', [ExitCB]),
                    \ }
        return job_start(a:cmd, job_opts)
    endif
endfunction

function! vlime#compat#vim#job_stop(job)
    return job_stop(a:job)
endfunction

function! vlime#compat#vim#job_status(job)
    return (job_status(a:job) == 'run') ? 'run' : 'dead'
endfunction

function! vlime#compat#vim#job_getbufnr(job)
    return ch_getbufnr(a:job, 'out')
endfunction

" XXX 99% copy pasta from vlime#compat#neovmim

function! s:JobOutputCB(user_cb, chan, data)
    let ToCall = function(a:user_cb, [[a:data]])
    call ToCall()
endfunction

function! s:JobExitCB(user_exit_cb, job, exit_status)
    let ToCall = function(a:user_exit_cb, [a:exit_status])
    call ToCall()
endfunction
