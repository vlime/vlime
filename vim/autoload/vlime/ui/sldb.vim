function! vlime#ui#sldb#InitSLDBBuf(ui, conn, thread, level, frames)
    let buf = bufnr(vlime#ui#SLDBBufName(a:conn, a:thread), v:true)
    call vlime#ui#SetVlimeBufferOpts(buf, a:conn)
    call setbufvar(buf, 'vlime_sldb_level', a:level)
    call setbufvar(buf, 'vlime_sldb_frames', a:frames)
    call a:ui.SetCurrentThread(a:thread, buf)
    return buf
endfunction

" Operates on current buffer. Should be called with vlime#ui#WithBuffer(...)
function! vlime#ui#sldb#FillSLDBBuf(thread, level, condition, restarts, frames)
    normal! ggVG"_d

    call vlime#ui#AppendString(
                \ 'Thread: ' . a:thread . '; Level: ' . a:level . "\n\n")

    let condition_str = ''
    for c in a:condition
        if type(c) == v:t_string
            let condition_str .= (c . "\n")
        endif
    endfor
    let condition_str .= "\n"
    call vlime#ui#AppendString(condition_str)

    let restarts_str = "Restarts:\n"
    let [max_name_len, has_star] = s:FindMaxRestartNameLen(a:restarts)
    let max_digits = len(string(len(a:restarts) - 1))
    let ri = 0
    while ri < len(a:restarts)
        let r = a:restarts[ri]
        let idx_str = vlime#ui#Pad(string(ri), '.', max_digits)
        let restart_line = s:FormatRestartLine(r, max_name_len, has_star)
        let restarts_str .= ('  ' . idx_str . restart_line . "\n")
        let ri += 1
    endwhile
    let restarts_str .= "\n"
    call vlime#ui#AppendString(restarts_str)

    let frames_str = "Frames:\n"
    let max_digits = len(string(len(a:frames) - 1))
    for f in a:frames
        let idx_str = vlime#ui#Pad(string(f[0]), '.', max_digits)
        let frames_str .= ('  ' . idx_str . f[1] . "\n")
    endfor
    call vlime#ui#AppendString(frames_str)

    " TODO: Move to a separate function?
    nnoremap <buffer> <cr> :call vlime#ui#sldb#ChooseCurRestart()<cr>
    nnoremap <buffer> d :call vlime#ui#sldb#ShowFrameDetails()<cr>
    nnoremap <buffer> S :call vlime#ui#sldb#OpenFrameSource()<cr>
    nnoremap <buffer> r :call vlime#ui#sldb#RestartCurFrame()<cr>
    nnoremap <buffer> s :call vlime#ui#sldb#StepCurOrLastFrame('step')<cr>
    nnoremap <buffer> x :call vlime#ui#sldb#StepCurOrLastFrame('next')<cr>
    nnoremap <buffer> o :call vlime#ui#sldb#StepCurOrLastFrame('out')<cr>
    nnoremap <buffer> c :call b:vlime_conn.SLDBContinue()<cr>
    nnoremap <buffer> a :call b:vlime_conn.SLDBAbort()<cr>
    nnoremap <buffer> C :call vlime#ui#sldb#InspectCurCondition()<cr>
    nnoremap <buffer> i :call vlime#ui#sldb#InspectInCurFrame()<cr>
endfunction

function! vlime#ui#sldb#ChooseCurRestart()
    let nth = s:MatchRestart()
    if nth >= 0
        call b:vlime_conn.InvokeNthRestartForEmacs(b:vlime_sldb_level, nth)
    endif
endfunction

function! vlime#ui#sldb#ShowFrameDetails()
    let nth = s:MatchFrame()
    if nth < 0
        let nth = 0
    endif
    call vlime#ChainCallbacks(
                \ function(b:vlime_conn.FrameLocalsAndCatchTags, [nth]),
                \ function('s:ShowFrameLocalsCB', [nth]),
                \ function(b:vlime_conn.FrameSourceLocation, [nth]),
                \ function('s:ShowFrameSourceLocationCB', [nth, v:true]))
endfunction

function! vlime#ui#sldb#OpenFrameSource()
    let nth = s:MatchFrame()
    if nth < 0
        let nth = 0
    endif
    call b:vlime_conn.FrameSourceLocation(nth, function('s:OpenFrameSourceCB'))
endfunction

function! vlime#ui#sldb#RestartCurFrame()
    let nth = s:MatchFrame()
    if nth >= 0
        call b:vlime_conn.RestartFrame(nth)
    endif
endfunction

function! vlime#ui#sldb#StepCurOrLastFrame(opr)
    let nth = s:MatchFrame()
    if nth < 0
        let nth = 0
    endif

    if a:opr == 'step'
        call b:vlime_conn.SLDBStep(nth)
    elseif a:opr == 'next'
        call b:vlime_conn.SLDBNext(nth)
    elseif a:opr == 'out'
        call b:vlime_conn.SLDBOut(nth)
    endif
endfunction

function! vlime#ui#sldb#InspectCurCondition()
    call b:vlime_conn.InspectCurrentCondition(
                \ {c, r -> c.ui.OnInspect(c, r, v:null, v:null)})
endfunction

function! vlime#ui#sldb#InspectInCurFrame()
    let nth = s:MatchFrame()
    if nth < 0
        let nth = 0
    endif

    let thread = b:vlime_conn.GetCurrentThread()
    call vlime#ui#InputFromMiniBuffer(
                \ b:vlime_conn, 'Inspect in frame (evaluated):',
                \ v:null,
                \ 'call vlime#ui#InspectInCurFrameInputComplete('
                    \ . nth . ', ' . thread . ') \| bunload!')
endfunction

function! s:FindMaxRestartNameLen(restarts)
    let max_name_len = 0
    let has_star = v:false
    for r in a:restarts
        if r[0][0] == '*'
            let start = 1
            let has_star = v:true
        else
            let start = 0
        endif
        if len(r[0][start:]) > max_name_len
            let max_name_len = len(r[0][start:])
        endif
    endfor
    return [max_name_len, has_star]
endfunction

function! s:FormatRestartLine(r, max_name_len, has_star)
    if a:has_star
        if a:r[0][0] == '*'
            let spc = ''
            let start = 1
        else
            let spc = ' '
            let start = 0
        endif
    else
        let spc = ''
        let start = 0
    endif
    let pad = repeat(' ', a:max_name_len + 1 - len(a:r[0][start:]))
    return spc . a:r[0] . pad . '- ' . a:r[1]
endfunction

function! s:MatchRestart()
    let line = getline('.')
    let matches = matchlist(line,
                \ '^\s*\([0-9]\+\)\.\s\+\*\?[A-Z\-]\+\s\+-\s.\+$')
    return (len(matches) > 0) ? (matches[1] + 0) : -1
endfunction

function! s:MatchFrame()
    let line = getline('.')
    let matches = matchlist(line, '^\s*\([0-9]\+\)\.\s\+(.\+)$')
    return (len(matches) > 0) ? (matches[1] + 0) : -1
endfunction

function! s:ShowFrameLocalsCB(frame, conn, result)
    let content = 'Frame: ' . a:frame . "\n"
    let locals = a:result[0]
    if type(locals) != v:t_none
        let content .= "\nLocals:\n"
        let rlocals = []
        let max_name_len = 0
        for lc in locals
            let rlc = vlime#PListToDict(lc)
            call add(rlocals, rlc)
            if len(rlc['NAME']) > max_name_len
                let max_name_len = len(rlc['NAME'])
            endif
        endfor
        for rlc in rlocals
            let content .= '  '     " Indentation
            let content .= vlime#ui#Pad(rlc['NAME'], ':', max_name_len)
            let content .= (rlc['VALUE'] . "\n")
        endfor
    endif
    let catch_tags = a:result[1]
    if type(catch_tags) != v:t_none
        let content .= "\nCatch tags:\n"
        for ct in catch_tags
            let content .= '  ' . ct . "\n"
        endfor
    endif
    call vlime#ui#ShowPreview(a:conn, content, v:false, 12)
endfunction

function! s:ShowFrameSourceLocationCB(frame, append, conn, result)
    if a:result[0]['name'] != 'LOCATION'
        call vlime#ui#ErrMsg(a:result[1])
        return
    endif

    if a:append
        let content = ''
    else
        let content = 'Frame: ' . a:frame . "\n"
    endif
    let content .= "\nLocation:\n"
    let content .= '  File: ' . a:result[1][1] . "\n"
    let content .= '  Position: ' . a:result[2][1] . "\n"

    let snippet_lines = split(a:result[3][1], "\n")
    let snippet = join(map(snippet_lines, '"    " . v:val'), "\n")
    let content .= "  Snippet:\n" . snippet . "\n"

    if a:append
        call vlime#ui#ShowPreview(a:conn, content, v:true)
    else
        call vlime#ui#ShowPreview(a:conn, content, v:false, 12)
    endif
endfunction

function! s:OpenFrameSourceCB(conn, result)
    if a:result[0]['name'] != 'LOCATION'
        call vlime#ui#ErrMsg(a:result[1])
        return
    endif
    let file_loc = a:result[1][1]
    let file_buf = bufnr(file_loc)
    let buf_exists = v:true
    if file_buf > 0
        let buf_win = bufwinnr(file_buf)
        if buf_win > 0
            execute buf_win . 'wincmd w'
        else
            let win_list = win_findbuf(file_buf)
            if len(win_list) > 0
                call win_gotoid(win_list[0])
            else
                let buf_exists = v:false
            endif
        endif
    else
        let buf_exists = v:false
    endif

    if !buf_exists
        if filereadable(file_loc)
            execute 'tabedit ' . escape(file_loc, ' \')
        else
            call vlime#ui#ErrMsg('Not readable: ' . file_loc)
            return
        endif
    endif

    let src_line = byte2line(a:result[2][1])
    execute 'normal! ' . src_line . 'gg'
endfunction
