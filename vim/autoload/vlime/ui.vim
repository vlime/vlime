function! vlime#ui#New()
    let obj = {
                \ 'buffer_package_map': {},
                \ 'buffer_thread_map': {},
                \ 'GetCurrentPackage': function('vlime#ui#GetCurrentPackage'),
                \ 'SetCurrentPackage': function('vlime#ui#SetCurrentPackage'),
                \ 'GetCurrentThread': function('vlime#ui#GetCurrentThread'),
                \ 'SetCurrentThread': function('vlime#ui#SetCurrentThread'),
                \ 'OnDebug': function('vlime#ui#OnDebug'),
                \ 'OnDebugActivate': function('vlime#ui#OnDebugActivate'),
                \ 'OnDebugReturn': function('vlime#ui#OnDebugReturn'),
                \ 'OnWriteString': function('vlime#ui#OnWriteString'),
                \ 'OnReadString': function('vlime#ui#OnReadString'),
                \ 'OnIndentationUpdate': function('vlime#ui#OnIndentationUpdate'),
                \ }
    return obj
endfunction

function! vlime#ui#GetUI()
    if !exists('g:vlime_ui')
        let g:vlime_ui = vlime#ui#New()
    endif
    return g:vlime_ui
endfunction

" vlime#ui#GetCurrentPackage([buffer])
function! vlime#ui#GetCurrentPackage(...) dict
    let buf_spec = vlime#GetNthVarArg(a:000, 0, '%')
    let cur_buf = bufnr(buf_spec)
    let buf_pkg = get(self.buffer_package_map, cur_buf, v:null)
    if type(buf_pkg) != v:t_list
        let in_pkg = vlime#ui#WithBuffer(cur_buf, function('vlime#ui#CurInPackage'))
        if len(in_pkg) > 0
            let buf_pkg = [in_pkg, in_pkg]
        else
            let buf_pkg = ['COMMON-LISP-USER', 'CL-USER']
        endif
    endif
    return buf_pkg
endfunction

" vlime#ui#SetCurrentPackage(pkg[, buffer])
function! vlime#ui#SetCurrentPackage(pkg, ...) dict
    let buf_spec = vlime#GetNthVarArg(a:000, 0, '%')
    let cur_buf = bufnr(buf_spec)
    let self.buffer_package_map[cur_buf] = a:pkg
endfunction

" vlime#ui#GetCurrentThread([buffer])
function! vlime#ui#GetCurrentThread(...) dict
    let buf_spec = vlime#GetNthVarArg(a:000, 0, '%')
    let cur_buf = bufnr(buf_spec)
    return get(self.buffer_thread_map, cur_buf, v:true)
endfunction

" vlime#ui#SetCurrentThread(thread[, buffer])
function! vlime#ui#SetCurrentThread(thread, ...) dict
    let buf_spec = vlime#GetNthVarArg(a:000, 0, '%')
    let cur_buf = bufnr(buf_spec)
    let self.buffer_thread_map[cur_buf] = a:thread
endfunction

function! vlime#ui#OnDebug(conn, thread, level, condition, restarts, frames, conts) dict
    let dbg_buf = s:InitSLDBBuf(self, a:conn, a:thread, a:level, a:frames)
    call setbufvar(dbg_buf, '&modifiable', 1)
    call vlime#ui#WithBuffer(
                \ dbg_buf,
                \ function('s:FillSLDBBuf',
                    \ [a:thread, a:level, a:condition, a:restarts, a:frames]))
    call setbufvar(dbg_buf, '&modifiable', 0)
endfunction

function! vlime#ui#OnDebugActivate(conn, thread, level, select) dict
    let dbg_buf = vlime#ui#OpenBuffer(
                \ vlime#ui#SLDBBufName(a:conn, a:thread), v:false, v:true)
    if dbg_buf > 0
        normal! gg
    endif
endfunction

function! vlime#ui#OnDebugReturn(conn, thread, level, stepping)
    let buf_name = vlime#ui#SLDBBufName(a:conn, a:thread)
    let bufnr = bufnr(buf_name)
    if bufnr > 0
        let buf_level = getbufvar(bufnr, 'vlime_sldb_level', -1)
        if buf_level == a:level
            call setbufvar(bufnr, '&buflisted', 0)
            execute 'bunload! ' . bufnr
        endif
    endif
endfunction

function! vlime#ui#OnWriteString(conn, str, str_type)
    let repl_buf = vlime#ui#OpenBuffer(
                \ vlime#ui#REPLBufName(a:conn), v:true, v:false)
    if repl_buf > 0
        if !getbufvar(repl_buf, 'vlime_buffer_initialized', v:false)
            call setbufvar(repl_buf, 'vlime_buffer_initialized', v:true)
            call s:SetVlimeBufferOpts(repl_buf, a:conn)
            let old_winnr = winnr()
            try
                call vlime#ui#OpenBuffer(repl_buf, v:false, v:true)
                call s:ShowREPLBanner(a:conn)
                nnoremap <buffer> <c-c>
                            \ :call b:vlime_conn.Interrupt(
                                \ {'name': 'REPL-THREAD', 'package': 'KEYWORD'})<cr>
            finally
                execute old_winnr . 'wincmd w'
            endtry
        endif

        let repl_winnr = bufwinnr(repl_buf)
        if repl_winnr > 0
            " If the REPL buffer is visible, move to that window to enable
            " automatic scrolling
            let old_winnr = winnr()
            try
                execute repl_winnr . 'wincmd w'
                call vlime#ui#AppendString(a:str)
            finally
                execute old_winnr . 'wincmd w'
            endtry
        else
            call vlime#ui#WithBuffer(repl_buf,
                        \ function('vlime#ui#AppendString', [a:str]))
        endif
    endif
endfunction

function! vlime#ui#OnReadString(conn, thread, ttag)
    let input_str = input('Input string: ')
    if input_str[len(input_str)-1] != "\n"
        let input_str .= "\n"
    endif
    call a:conn.ReturnString(a:thread, a:ttag, input_str)
endfunction

function! vlime#ui#OnIndentationUpdate(conn, indent_info)
    if !has_key(a:conn.cb_data, 'indent_info')
        let a:conn.cb_data['indent_info'] = {}
    endif
    for i in a:indent_info
        let a:conn.cb_data['indent_info'][i[0]] = [i[1], i[2]]
    endfor
endfunction

function! vlime#ui#WithBuffer(buf, Func)
    let old_buf = bufnr('%')
    let cur_buf = bufnr(a:buf)
    try
        execute 'hide buffer ' . cur_buf
        return a:Func()
    finally
        execute 'buffer ' . old_buf
    endtry
endfunction

function! vlime#ui#CurChar()
    return matchstr(getline('.'), '\%' . col('.') . 'c.')
endfunction

function! vlime#ui#CurAtom()
    let old_kw = &iskeyword
    try
        setlocal iskeyword+=+,-,*,/,%,<,=,>,:,$,?,!,@-@,94,~,#,\|,&,.,{,},[,]
        return expand('<cword>')
    finally
        let &l:iskeyword = old_kw
    endtry
endfunction

" vlime#ui#CurExpr([return_pos])
function! vlime#ui#CurExpr(...)
    let return_pos = vlime#GetNthVarArg(a:000, 0, v:false)

    let cur_char = vlime#ui#CurChar()
    let [s_line, s_col] = vlime#ui#CurExprPos(cur_char)
    let [e_line, e_col] = vlime#ui#CurExprEndPos(cur_char)
    let lines = getline(s_line, e_line)
    if len(lines) == 1
        let lines[0] = strpart(lines[0], s_col - 1, e_col - s_col + 1)
    elseif len(lines) > 1
        let lines[0] = strpart(lines[0], s_col - 1)
        let lines[-1] = strpart(lines[-1], 0, e_col)
    endif

    let expr = join(lines, "\n")
    return return_pos ? [expr, [s_line, s_col], [e_line, e_col]] : expr
endfunction

function! vlime#ui#CurExprPos(cur_char)
    if a:cur_char == '('
        return searchpairpos('(', '', ')', 'cbnW')
    elseif a:cur_char == ')'
        return searchpairpos('(', '', ')', 'bnW')
    else
        return searchpairpos('(', '', ')', 'bnW')
    endif
endfunction

function! vlime#ui#CurExprEndPos(cur_char)
    if a:cur_char == '('
        return searchpairpos('(', '', ')', 'nW')
    elseif a:cur_char == ')'
        return searchpairpos('(', '', ')', 'cnW')
    else
        return searchpairpos('(', '', ')', 'nW')
    endif
endfunction

function! vlime#ui#CurInPackage()
    let pattern = '^\s*(\_s*in-package\_s\+\(.\+\)\_s*)'
    let old_cur_pos = getcurpos()
    let package_line = search(pattern, 'bcW')
    if package_line <= 0
        let package_line = search(pattern, 'cW')
    endif
    if package_line > 0
        let matches = matchlist(vlime#ui#CurExpr(), pattern)
        let package = s:NormalizePackageName(matches[1])
    else
        let package = ''
    endif
    call setpos('.', old_cur_pos)
    return package
endfunction

function! vlime#ui#CurOperator()
    let expr = vlime#ui#CurExpr()
    if len(expr) > 0
        let matches = matchlist(expr, '^(\_s*\([^[:blank:]\n()]\+\)\_s*\_.*)$')
        if len(matches) > 0
            return matches[1]
        endif
    endif
    return ''
endfunction

" vlime#ui#CurSelection([return_pos])
function! vlime#ui#CurSelection(...)
    let return_pos = vlime#GetNthVarArg(a:000, 0, v:false)

    if return_pos
        let [s_pos, e_pos] = [getpos("'<")[1:2], getpos("'>")[1:2]]
    endif
    let old_reg = @x
    try
        normal! gv"xy
        if return_pos
            return [@x, s_pos, e_pos]
        else
            return @x
        endif
    finally
        let @x = old_reg
    endtry
endfunction

function! vlime#ui#ChooseCurRestart()
    let line = getline('.')
    let matches = matchlist(line, '^\s*\([0-9]\+\)\.\s\+\*\?[A-Z\-]\+\s\+-\s.\+$')
    if len(matches) > 0
        let nth = matches[1] + 0
        call b:vlime_conn.InvokeNthRestartForEmacs(b:vlime_sldb_level, nth)
    endif
endfunction

function! vlime#ui#RestartCurFrame()
    let line = getline('.')
    let matches = matchlist(line, '^\s*\([0-9]\+\)\.\s\+(.\+)$')
    if len(matches) > 0
        let nth = matches[1] + 0
        call b:vlime_conn.RestartFrame(nth)
    endif
endfunction

function! vlime#ui#StepCurOrLastFrame(opr)
    let line = getline('.')
    let matches = matchlist(line, '^\s*\([0-9]\+\)\.\s\+(.\+)$')
    if len(matches) > 0
        let nth = matches[1] + 0
    else
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

function! vlime#ui#ShowFrameSourceLocation(frame, append)
    function! s:ShowFrameSourceLocationCB(frame, append, conn, result)
        echom string(a:result)

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
        let content .= '  Snippet: ' . a:result[3][1] . "\n"
        if a:append
            call vlime#ui#ShowPreview(content, v:true)
        else
            call vlime#ui#ShowPreview(content, v:false, 12)
        endif
    endfunction

    call b:vlime_conn.FrameSourceLocation(a:frame,
                \ function('s:ShowFrameSourceLocationCB', [a:frame, a:append]))
endfunction

function! vlime#ui#ShowFrameLocals()
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
                let content .= s:Pad(rlc['NAME'], ':', max_name_len)
                let content .= (rlc['VALUE'] . "\n")
            endfor
        endif
        let catch_tags = a:result[1]
        if type(catch_tags) != v:t_none
            let content .= "\nCatch tags:\n"
            for ct in catch_tags
                let content .= '  ' . ct
            endfor
        endif
        call vlime#ui#ShowPreview(content, v:false, 12)
    endfunction

    let line = getline('.')
    let matches = matchlist(line, '^\s*\([0-9]\+\)\.\s\+(.\+)$')
    if len(matches) > 0
        let nth = matches[1] + 0
    else
        let nth = 0
    endif

    call b:vlime_conn.FrameLocalsAndCatchTags(nth,
                \ function('s:ShowFrameLocalsCB', [nth]))
endfunction

function! vlime#ui#OpenBuffer(name, create, show)
    let buf = bufnr(a:name, a:create)
    if buf > 0
        if type(a:show) == v:t_string && a:show == 'preview'
            execute 'pedit! ' . substitute(a:name, '\(\s\)', '\\\1', 'g')
        elseif type(a:show) == v:t_string && a:show[0:7] == 'preview '
            let prefix = a:show[8:]
            execute prefix . ' pedit! ' . substitute(a:name, '\(\s\)', '\\\1', 'g')
        elseif a:show
            " Found it. Try to put it in a window
            let win_nr = bufwinnr(buf)
            if win_nr < 0
                execute 'botright split #' . buf
            else
                execute win_nr . 'wincmd w'
            endif
        endif
    endif
    return buf
endfunction

" vlime#ui#ShowPreview(content, append[, win_size])
function! vlime#ui#ShowPreview(content, append, ...)
    let win_size = vlime#GetNthVarArg(a:000, 0)
    if type(win_size) != v:t_none
        let old_pwheight = &previewheight
        pclose!
        try
            let &previewheight = win_size
            let buf = vlime#ui#OpenBuffer(
                        \ vlime#ui#PreviewBufName(), v:true, 'preview topleft')
        finally
            let &previewheight = old_pwheight
        endtry
    else
        let buf = vlime#ui#OpenBuffer(
                    \ vlime#ui#PreviewBufName(), v:true, 'preview topleft')
    endif

    if buf > 0
        if !getbufvar(buf, 'vlime_buffer_initialized', v:false)
            call setbufvar(buf, 'vlime_buffer_initialized', v:true)
            call s:SetVlimeBufferOpts(buf, v:null)
        endif
        if a:append
            " XXX: Appending doesn't work and I don't know why
            call vlime#ui#WithBuffer(buf,
                        \ function('vlime#ui#AppendString', [a:content]))
        else
            call vlime#ui#WithBuffer(buf,
                        \ function('vlime#ui#ReplaceContent', [a:content]))
        endif
    endif

    return buf
endfunction

function! vlime#ui#SLDBBufName(conn, thread)
    return 'vlime / sldb / ' . a:conn.cb_data.name . ' / ' . a:thread
endfunction

function! vlime#ui#REPLBufName(conn)
    return 'vlime / repl / ' . a:conn.cb_data.name
endfunction

function! vlime#ui#PreviewBufName()
    return 'vlime / preview'
endfunction

function! vlime#ui#IndentCurLine(indent)
    if &expandtab
        let indent_str = repeat(' ', a:indent)
    else
        " Ah! So bad! Such evil!
        let indent_str = repeat("\<tab>", a:indent / &tabstop)
        let indent_str .= repeat(' ', a:indent % &tabstop)
    endif
    let line = getline('.')
    call setline('.', substitute(line, '^\(\s*\)', indent_str, ''))
    normal! ^
endfunction

function! vlime#ui#ErrMsg(msg)
    echohl ErrorMsg
    echom a:msg
    echohl None
endfunction

function! s:NormalizePackageName(name)
    let pattern1 = '^\(\(#\?:\)\|''\)\(.\+\)'
    let pattern2 = '"\(.\+\)"'
    let matches = matchlist(a:name, pattern1)
    let r_name = ''
    if len(matches) > 0
        let r_name = matches[3]
    else
        let matches = matchlist(a:name, pattern2)
        if len(matches) > 0
            let r_name = matches[1]
        endif
    endif
    return toupper(r_name)
endfunction

function! s:Pad(prefix, sep, max_len)
    return a:prefix . a:sep . repeat(' ', a:max_len + 1 - len(a:prefix))
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

function! s:SetVlimeBufferOpts(buf, conn)
    call setbufvar(a:buf, '&buftype', 'nofile')
    call setbufvar(a:buf, '&bufhidden', 'hide')
    call setbufvar(a:buf, '&swapfile', 0)
    call setbufvar(a:buf, '&buflisted', 1)
    call setbufvar(a:buf, 'vlime_conn', a:conn)
endfunction

function! s:InitSLDBBuf(ui, conn, thread, level, frames)
    let buf = bufnr(vlime#ui#SLDBBufName(a:conn, a:thread), v:true)
    call s:SetVlimeBufferOpts(buf, a:conn)
    call setbufvar(buf, 'vlime_sldb_level', a:level)
    call setbufvar(buf, 'vlime_sldb_frames', a:frames)
    call a:ui.SetCurrentThread(a:thread, buf)
    return buf
endfunction

" Operates on current buffer. Should be called with vlime#ui#WithBuffer(...)
function! s:FillSLDBBuf(thread, level, condition, restarts, frames)
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
        let idx_str = s:Pad(string(ri), '.', max_digits)
        let restart_line = s:FormatRestartLine(r, max_name_len, has_star)
        let restarts_str .= ('  ' . idx_str . restart_line . "\n")
        let ri += 1
    endwhile
    let restarts_str .= "\n"
    call vlime#ui#AppendString(restarts_str)

    let frames_str = "Frames:\n"
    let max_digits = len(string(len(a:frames) - 1))
    for f in a:frames
        let idx_str = s:Pad(string(f[0]), '.', max_digits)
        let frames_str .= ('  ' . idx_str . f[1] . "\n")
    endfor
    call vlime#ui#AppendString(frames_str)

    " TODO: Move to a separate function?
    nnoremap <buffer> <cr> :call vlime#ui#ChooseCurRestart()<cr>
    nnoremap <buffer> S :call vlime#ui#ShowFrameLocals()<cr>
    nnoremap <buffer> r :call vlime#ui#RestartCurFrame()<cr>
    nnoremap <buffer> s :call vlime#ui#StepCurOrLastFrame('step')<cr>
    nnoremap <buffer> x :call vlime#ui#StepCurOrLastFrame('next')<cr>
    nnoremap <buffer> o :call vlime#ui#StepCurOrLastFrame('out')<cr>
    nnoremap <buffer> c :call b:vlime_conn.SLDBContinue()<cr>
    nnoremap <buffer> a :call b:vlime_conn.SLDBAbort()<cr>
endfunction

function! vlime#ui#AppendString(str)
    let i = len(a:str) - 1
    let nl = 0
    while i >= 0 && a:str[i] == "\n"
        let i -= 1
        let nl += 1
    endwhile
    let r_str = (i >= 0) ? a:str[0:i] : ''

    let old_cur = getcurpos()
    let scroll = (old_cur[1] == line('$'))
    let old_reg_x = @x
    try
        if len(r_str) > 0
            let @x = r_str
            normal! G$"xp
        endif
        if nl > 0
            for n in range(nl)
                call append(line('$'), '')
            endfor
        endif
    finally
        let @x = old_reg_x
        if !scroll
            call setpos('.', old_cur)
        else
            normal! G
        endif
    endtry
endfunction

function! vlime#ui#ReplaceContent(str)
    normal! ggVG"_d
    call vlime#ui#AppendString(a:str)
    normal! gg
endfunction

function! s:ShowREPLBanner(conn)
    let banner = 'SWANK'
    if has_key(a:conn.cb_data, 'version')
        let banner .= ' version ' . a:conn.cb_data['version']
    endif
    if has_key(a:conn.cb_data, 'pid')
        let banner .= ', pid ' . a:conn.cb_data['pid']
    endif
    let banner_len = len(banner)
    let banner .= ("\n" . repeat('=', banner_len) . "\n")
    call vlime#ui#AppendString(banner)
endfunction
