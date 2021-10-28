function! vlime#compat#vim#ch_type()
    return v:t_channel
endfunction

function! vlime#compat#vim#ch_open(host, port, callback, timeout)
    let chan_obj = {
                \ 'on_data': function('s:ChanInputCB'),
                \ 'next_msg_id': 10,
                \ 'msg_callbacks': {},
                \ }
    if type(a:callback) != type(v:null)
        let chan_obj['chan_callback'] = a:callback
    endif

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

" XXX 99% copy pasta from vlime#compat#neovmim
" vlime#compat#vim#ch_sendexpr(chan, expr, callback, r_or_tagaw)
function! vlime#compat#vim#ch_sendexpr(chan, expr, callback, raw_or_tag)
    let msg = a:expr
    if a:raw_or_tag == -1
        call add(msg, a:chan.next_msg_id)
    elseif  a:raw_or_tag > 0
        call add(msg, a:raw_or_tag)
    endif

    let json = json_encode(msg) . "\n"
    let l_str = printf("%06x", len(json))
    let json2 = l_str . json
    let ret = ch_sendraw(a:chan.ch, json2)
    echom 'ret ' . ret
    " XXX do something with `ret`
    if type(a:callback) != type(v:null)
        let a:chan.msg_callbacks[a:chan.next_msg_id] = a:callback
        "echomsg  "set idx " a:chan.next_msg_id " for " msg
        " cb " a:callback
    endif
    call s:IncMsgID(a:chan)
endfunction

function! vlime#compat#vim#ch_sendraw(chan, msg)
    return ch_sendraw(a:chan.ch, a:msg)
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
function! s:ChanInputCB(ch, data) dict
    let obj_list = []
    let bytes_want = -1
    let buffered = get(self, 'recv_buffer', '') . a:data
    while len(buffered) > 0
        if bytes_want == -1 
            if len(buffered) >= 6
                let bytes_want = str2nr(strpart(buffered, 0, 6), 16)
                let buffered = strpart(buffered, 6)
            else
                " Not enough data
                break
            endif
        else
            if len(buffered) >= bytes_want
                let input = strpart(buffered, 0, bytes_want)
                " msgpack-special-dict - see VIM help for json_decode()
                let input = substitute(input, "\\u0000", "", "g")
                let json_obj = json_decode(input)
                call add(obj_list, json_obj)
                let buffered = strpart(buffered, bytes_want)
                let bytes_want = -1
            else
                " Not enough data
                " keep the length information for next time
                let buffered = printf("%06x", bytes_want) . buffered
                break
            endif
        endif
    endwhile

    let self['recv_buffer'] = buffered

    for json_obj in obj_list
        let type = json_obj[0]

        " Previously vlime always sent a callback index with a message
        " now we've got to read swanks data directly
        " See previous NORMALIZE-SWANK-FORM (in lisp/src/vlime-protocol.lisp)
        if type == vlime#KW("RETURN")
            let cb_index = remove(json_obj, -1)

            if cb_index == 0
                let CB = get(self, 'chan_callback', v:null)
            else
                try
                    let CB = remove(self.msg_callbacks, cb_index)
                catch /^Vim\%((\a\+)\)\=:E716/  " Key not present in Dictionary
                    let CB = v:null
                endtry
            endif
        else
            let CB = get(self, 'chan_callback', v:null)
        endif

        "echomsg "got obj " json_encode(json_obj)
        if type(CB) != type(v:null)
            try
                call CB(self, json_obj)
            catch /.*/
                call vlime#ui#ErrMsg('vlime: callback failed: ' . v:exception)
            endtry
        endif
    endfor
endfunction

" XXX 100% copy pasta from vlime#compat#neovmim
function! s:IncMsgID(chan)
    if a:chan.next_msg_id >= 65535
        let a:chan.next_msg_id = 1
    else
        let a:chan.next_msg_id += 1
    endif
endfunction

function! s:JobOutputCB(user_cb, chan, data)
    let ToCall = function(a:user_cb, [[a:data]])
    call ToCall()
endfunction

function! s:JobExitCB(user_exit_cb, job, exit_status)
    let ToCall = function(a:user_exit_cb, [a:exit_status])
    call ToCall()
endfunction
