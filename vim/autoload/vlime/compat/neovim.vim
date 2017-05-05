function! vlime#compat#neovim#ch_type()
    return v:t_dict
endfunction

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


function! vlime#compat#neovim#job_start(cmd, buf_name)
    let buf = bufnr(a:buf_name, v:true)
    call setbufvar(buf, '&buftype', 'nofile')
    call setbufvar(buf, '&bufhidden', 'hide')
    call setbufvar(buf, '&swapfile', 0)
    call setbufvar(buf, '&buflisted', 1)

    call vlime#ui#WithBuffer(buf, { -> append(0, 'Reading from channel output...')})

    call setbufvar(buf, '&modifiable', 0)

    let job_obj = {
                \ 'on_stdout': function('s:JobInputCB'),
                \ 'on_stderr': function('s:JobInputCB'),
                \ 'out_name': a:buf_name,
                \ 'err_name': a:buf_name,
                \ 'out_buf': buf,
                \ 'err_buf': buf,
                \ }

    let job_id = jobstart(a:cmd, job_obj)
    let job_obj['job_id'] = job_id
    return job_obj
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


function! s:ChanInputCB(job_id, data, source) dict
    let obj_list = []
    let buffered = get(self, 'recv_buffer', '')
    for frag in a:data
        let buffered .= frag
        try
            " XXX: what about E488: Trailing characters?
            let json_obj = json_decode(buffered)
            call add(obj_list, json_obj)
            let buffered = ''
        catch /^Vim\%((\a\+)\)\=:E474/  " Invalid argument
        endtry
    endfor

    let self['recv_buffer'] = buffered

    for json_obj in obj_list
        if json_obj[0] == 0
            let CB = get(self, 'chan_callback', v:null)
        else
            try
                let CB = remove(self.msg_callbacks, json_obj[0])
            catch /^Vim\%((\a\+)\)\=:E716/  " Key not present in Dictionary
                let CB = v:null
            endtry
        endif

        if type(CB) != type(v:null)
            try
                call CB(self, json_obj[1])
            catch /.*/
                call vlime#ui#ErrMsg('vlime: callback failed: ' . v:exception)
            endtry
        endif
    endfor
endfunction

function! s:IncMsgID(chan)
    if a:chan.next_msg_id >= 65535
        let a:chan.next_msg_id = 1
    else
        let a:chan.next_msg_id += 1
    endif
endfunction

function! s:JobInputCB(job_id, data, source) dict
    let buf = (a:source == 'stdout') ? self.out_buf : self.err_buf
    call vlime#ui#WithBuffer(buf, function('s:AppendToJobBuffer', [a:data]))
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
