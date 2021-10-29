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
    let chan_extra = {
                \ 'next_msg_id': 10,
                \ 'msg_callbacks': {},
                \ 'on_data': function('vlime#compat#ch_on_data'),
                \ }
    if type(Callback) != type(v:null)
        let chan_extra['chan_callback'] = Callback
    endif
    let ch = s:ch_impl.ch_open(a:host, a:port, chan_extra.on_data, timeout)
    let ch['extra'] = chan_extra
    return ch
endfunction

function! vlime#compat#ch_on_data(ch, data) dict
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
        let cb_index = -1

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

        " let json = json_encode(json_obj)
        " echomsg 'Received [index=' . cb_index . ',json=' . strpart(json, 0, 50) . '...' strpart(json, strlen(json) - 50, 50) . ']'
        if type(CB) != type(v:null)
            try
                " echomsg 'Call CB [name=' . strpart(string(CB), 0, 100) . ']'
                call CB(self, json_obj)
            catch /.*/
                call vlime#ui#ErrMsg('vlime: callback failed: ' . v:exception)
            endtry
        endif
    endfor
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
        call add(msg, a:chan.extra.next_msg_id)
    elseif  raw_or_tag > 0
        call add(msg, raw_or_tag)
    endif

    let json = json_encode(msg) . "\n"
    call vlime#compat#ch_sendraw(a:chan, json)
    if type(Callback) != type(v:null)
        let a:chan.extra.msg_callbacks[a:chan.extra.next_msg_id] = Callback
        " echomsg  "Set CB [index=" . a:chan.extra.next_msg_id .
        "             \ ",CB=" . strpart(string(Callback), 0, 100) .
        "             \ "]"
    endif
    call vlime#compat#inc_msg_id(a:chan)
endfunction

function! vlime#compat#inc_msg_id(chan)
    if a:chan.extra.next_msg_id >= 65535
        let a:chan.extra.next_msg_id = 1
    else
        let a:chan.extra.next_msg_id += 1
    endif
endfunction

function! vlime#compat#ch_sendraw(chan, msg)
    " echomsg 'Send [msg=' . strpart(a:msg, 0, 50) '...' strpart(a:msg, strlen(a:msg) - 50, 50) . ']'
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
