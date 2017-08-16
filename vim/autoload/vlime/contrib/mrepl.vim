""
" @dict VlimeConnection.CreateMREPL
" @usage [chan_id] [callback]
" @public
"
" Create an REPL listener using SWANK-MREPL. [chan_id] should be a unique
" number identifying the local channel. Use a automatically generated ID if
" [chan_id] is omitted or v:null.
"
" This method needs the SWANK-MREPL contrib module. See
" @function(VlimeConnection.SwankRequire).
function! vlime#contrib#mrepl#CreateMREPL(...) dict
    let chan_id = get(a:000, 0, v:null)
    let Callback = get(a:000, 1, v:null)
    let chan_obj = self.MakeLocalChannel(chan_id, function('s:MREPL_ChannelCB'))
    let chan_obj['mrepl'] = {}
    call self.Send(self.EmacsRex([vlime#SYM('SWANK-MREPL', 'CREATE-MREPL'), chan_obj['id']]),
                \ function('s:CreateMREPL_CB', [self, Callback, chan_obj]))
endfunction

function! vlime#contrib#mrepl#OnMREPL_WriteResult(conn, chan_obj, result) dict
    " TODO
    echom '---- from ' . string(a:chan_obj) . ': ' . string(a:result)
endfunction

function! vlime#contrib#mrepl#Init(conn)
    let a:conn['CreateMREPL'] = function('vlime#contrib#mrepl#CreateMREPL')
    let ui = vlime#ui#GetUI()
    let ui['OnMREPL_WriteResult'] = function('vlime#contrib#mrepl#OnMREPL_WriteResult')
endfunction

function! s:CreateMREPL_CB(conn, Callback, local_chan, chan, msg)
    try
        call vlime#CheckReturnStatus(a:msg, 'vlime#contrib#mrepl#CreateMREPL')
    catch
        let local_chan_id = local_chan['id']
        call a:conn.RemoveLocalChannel(local_chan_id)
        throw v:exception
    endtry
    let [chan_id, thread_id, pkg_name, pkg_prompt] = a:msg[1][1]
    let a:local_chan['mrepl']['peer'] = chan_id
    let a:local_chan['mrepl']['prompt'] = [pkg_name, pkg_prompt]
    call a:conn.MakeRemoteChannel(chan_id, thread_id)
    call vlime#TryToCall(a:Callback, [a:conn, a:msg[1][1]])
endfunction

function! s:MREPL_ChannelCB(conn, chan_obj, msg)
    let msg_type = a:msg[0]
    if msg_type['name'] == 'WRITE-RESULT'
        if type(a:conn.ui) != type(v:null)
            call a:conn.ui.OnMREPL_WriteResult(a:conn, a:chan_obj, a:msg[1])
        endif
    elseif msg_type['name'] == 'PROMPT'
        let mrepl = get(a:chan_obj, 'mrepl', {})
        let mrepl['prompt'] = a:msg[1:2]
        let a:chan_obj['mrepl'] = mrepl
    elseif get(g:, '_vlime_debug', v:false)
        echom 'Unknown message: ' . string(a:msg)
    endif
endfunction
