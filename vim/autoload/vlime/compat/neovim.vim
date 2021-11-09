function! vlime#compat#neovim#ch_type()
    return v:t_dict
endfunction

function! vlime#compat#neovim#ch_open(host, port, callback, timeout)
    " Avoid duplicates with initialization messages
    let chan_obj = {
                \ 'hostname': a:host,
                \ 'port': a:port,
                \ 'on_data': function('vlime#compat#neovim#ch_on_data'),
                \ 'callback': a:callback,
                \ }

    try
        let ch_id = sockconnect('tcp', a:host . ':' . a:port, chan_obj)
        let chan_obj['ch_id'] = ch_id
        let chan_obj['is_connected'] = v:true
    catch
        let chan_obj['ch_id'] = v:null
        let chan_obj['is_connected'] = v:false
    endtry

    " XXX: There should be a better way to wait for the channel is ready
    let waittime = (type(a:timeout) != type(v:null)) ? (a:timeout + 500) : 500
    execute 'sleep' waittime 'm'

    return chan_obj
endfunction

function! vlime#compat#neovim#ch_on_data(job_id, data, source) dict
    let CB = get(self, 'callback')
    call CB(self, join(a:data, ""))
endfunction

function! vlime#compat#neovim#ch_status(chan)
    return a:chan['is_connected'] ? 'open' : 'closed'
endfunction

function! vlime#compat#neovim#ch_info(chan)
    return {'hostname': a:chan.hostname, 'port': a:chan.port}
endfunction

function! vlime#compat#neovim#ch_close(chan)
    try
        if a:chan.ch_id
            return chanclose(a:chan.ch_id)
        endif
    catch /^Vim\%((\a\+)\)\=:E900/  " Invalid ch id
        " The channel already closed
        throw 'vlime#compat#neovim#ch_close: not an open channel'
    endtry
endfunction

function! vlime#compat#neovim#ch_evalexpr(chan, expr)
    throw 'vlime#compat#neovim#ch_evalexpr: not supported'
endfunction

function! vlime#compat#neovim#ch_sendraw(chan, msg)
    let ret = chansend(a:chan.ch_id, a:msg)
    if ret == 0
        let a:chan['is_connected'] = v:false
    endif
    return ret
endfunction

function! vlime#compat#neovim#job_start(cmd, opts)
    let buf_name = a:opts['buf_name']
    let Callback = a:opts['callback']
    let ExitCB = a:opts['exit_cb']
    let use_terminal = a:opts['use_terminal']

    if use_terminal
        let job_obj = {
                    \ 'on_stdout': function('s:JobOutputCB', [Callback]),
                    \ 'on_stderr': function('s:JobOutputCB', [Callback]),
                    \ 'on_exit': function('s:JobExitCB', [ExitCB]),
                    \ 'use_terminal': v:true,
                    \ }
        let job_id = termopen(a:cmd, job_obj)
        let job_obj['job_id'] = job_id
        let job_obj['out_buf'] = bufnr('%')
        return job_obj
    else
        let buf = bufnr(buf_name, v:true)
        call setbufvar(buf, '&buftype', 'nofile')
        call setbufvar(buf, '&bufhidden', 'hide')
        call setbufvar(buf, '&swapfile', 0)
        call setbufvar(buf, '&buflisted', 1)
        call setbufvar(buf, '&modifiable', 0)

        let job_obj = {
                    \ 'on_stdout': function('s:JobOutputCB', [Callback]),
                    \ 'on_stderr': function('s:JobOutputCB', [Callback]),
                    \ 'on_exit': function('s:JobExitCB', [ExitCB]),
                    \ 'out_name': buf_name,
                    \ 'err_name': buf_name,
                    \ 'out_buf': buf,
                    \ 'err_buf': buf,
                    \ 'use_terminal': v:false,
                    \ }

        let job_id = jobstart(a:cmd, job_obj)
        let job_obj['job_id'] = job_id
        return job_obj
    endif
endfunction

function! vlime#compat#neovim#job_stop(job)
    call jobstop(a:job.job_id)
    return !!v:true
endfunction

function! vlime#compat#neovim#job_status(job)
    try
        let job_pid = jobpid(a:job.job_id)
    catch /^Vim\%((\a\+)\)\=:E900/  " Invalid job id
        let job_pid = 0
    endtry

    return (job_pid > 0) ? 'run' : 'dead'
endfunction

function! vlime#compat#neovim#job_getbufnr(job)
    return get(a:job, 'out_buf', 0)
endfunction

function! s:JobOutputCB(user_cb, job_id, data, source) dict
    let ToCall = function(a:user_cb, [a:data])
    call ToCall()

    if !self.use_terminal
        let buf = (a:source == 'stdout') ? self.out_buf : self.err_buf
        call vlime#ui#WithBuffer(buf, function('s:AppendToJobBuffer', [a:data]))
    endif
endfunction

function! s:JobExitCB(user_exit_cb, job_id, exit_status, source) dict
    let ToCall = function(a:user_exit_cb, [a:exit_status])
    call ToCall()
endfunction

function! s:AppendToJobBuffer(data)
    call setbufvar('%', '&modifiable', 1)
    try
        for line in a:data
            if len(line) > 0
                call append(line('$'), line)
            endif
        endfor
    finally
        call setbufvar('%', '&modifiable', 0)
    endtry
endfunction
