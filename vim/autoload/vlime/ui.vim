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
                \ 'OnReadFromMiniBuffer': function('vlime#ui#OnReadFromMiniBuffer'),
                \ 'OnIndentationUpdate': function('vlime#ui#OnIndentationUpdate'),
                \ 'OnInvalidRPC': function('vlime#ui#OnInvalidRPC'),
                \ 'OnInspect': function('vlime#ui#OnInspect'),
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
    let dbg_buf = vlime#ui#sldb#InitSLDBBuf(self, a:conn, a:thread, a:level, a:frames)
    call setbufvar(dbg_buf, '&modifiable', 1)
    call vlime#ui#WithBuffer(
                \ dbg_buf,
                \ function('vlime#ui#sldb#FillSLDBBuf',
                    \ [a:thread, a:level, a:condition, a:restarts, a:frames]))
    call setbufvar(dbg_buf, '&modifiable', 0)
endfunction

function! vlime#ui#OnDebugActivate(conn, thread, level, select) dict
    let dbg_buf = vlime#ui#OpenBuffer(
                \ vlime#ui#SLDBBufName(a:conn, a:thread),
                \ v:false, 'botright split')
    if dbg_buf > 0
        normal! gg
    endif
endfunction

function! vlime#ui#OnDebugReturn(conn, thread, level, stepping) dict
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

function! vlime#ui#OnWriteString(conn, str, str_type) dict
    let repl_buf = vlime#ui#repl#InitREPLBuf(a:conn)
    if repl_buf > 0
        call vlime#ui#repl#AppendOutput(repl_buf, a:str)
    endif
endfunction

function! vlime#ui#OnReadString(conn, thread, ttag) dict
    call vlime#ui#InputFromMiniBuffer(
                \ a:conn, 'Input string:', v:null,
                \ 'call vlime#ui#ReadStringInputComplete('
                    \ . a:thread . ', ' . a:ttag . ') \| bunload!')
endfunction

function! vlime#ui#OnReadFromMiniBuffer(conn, thread, ttag, prompt, init_val) dict
    call vlime#ui#InputFromMiniBuffer(
                \ a:conn, a:prompt, a:init_val,
                \ 'call vlime#ui#ReturnMiniBufferContent('
                    \ . a:thread . ', ' . a:ttag . ') \| bunload!')
endfunction

function! vlime#ui#OnIndentationUpdate(conn, indent_info) dict
    if !has_key(a:conn.cb_data, 'indent_info')
        let a:conn.cb_data['indent_info'] = {}
    endif
    for i in a:indent_info
        let a:conn.cb_data['indent_info'][i[0]] = [i[1], i[2]]
    endfor
endfunction

function! vlime#ui#OnInvalidRPC(conn, rpc_id, err_msg) dict
    call vlime#ui#ErrMsg(a:err_msg)
endfunction

function! vlime#ui#OnInspect(conn, i_content, i_thread, i_tag) dict
    let insp_buf = vlime#ui#inspector#InitInspectorBuf(
                \ a:conn.ui, a:conn, a:i_thread)
    call vlime#ui#OpenBuffer(insp_buf, v:false, 'botright split')

    let r_content = vlime#PListToDict(a:i_content)
    let old_title = getline(1)
    if get(r_content, 'TITLE', v:null) == old_title
        let old_cur = getcurpos()
    else
        let old_cur = [0, 1, 1, 0, 1]
    endif

    call setbufvar(insp_buf, '&modifiable', 1)
    call vlime#ui#inspector#FillInspectorBuf(r_content, a:i_thread, a:i_tag)
    call setbufvar(insp_buf, '&modifiable', 0)
    call setpos('.', old_cur)
    " Needed for displaying the content of the current buffer correctly
    redraw
endfunction

function! vlime#ui#ReadStringInputComplete(thread, ttag)
    let content = vlime#ui#CurBufferContent()
    if content[len(content)-1] != "\n"
        let content .= "\n"
    endif
    call b:vlime_conn.ReturnString(a:thread, a:ttag, content)
endfunction

function! vlime#ui#ReturnMiniBufferContent(thread, ttag)
    let content = vlime#ui#CurBufferContent()
    call b:vlime_conn.Return(a:thread, a:ttag, content)
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

function! vlime#ui#CurBufferContent()
    let old_reg = @x
    try
        normal! ggVG"xy
        let lines = split(@x, "\n")
        return join(filter(lines, "match(v:val, '^\s*;.*$') < 0"), "\n")
    finally
        let @x = old_reg
    endtry
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

function! vlime#ui#OpenBuffer(name, create, show)
    let buf = bufnr(a:name, a:create)
    if buf > 0
        if (type(a:show) == v:t_string && len(a:show) > 0) || a:show
            " Found it. Try to put it in a window
            let win_nr = bufwinnr(buf)
            if win_nr < 0
                if type(a:show) == v:t_string
                    execute a:show . ' #' . buf
                else
                    execute 'split #' . buf
                endif
            else
                execute win_nr . 'wincmd w'
            endif
        endif
    endif
    return buf
endfunction

" vlime#ui#ShowPreview(conn, content, append[, win_size])
function! vlime#ui#ShowPreview(conn, content, append, ...)
    let win_size = vlime#GetNthVarArg(a:000, 0)
    let old_win_id = win_getid()
    try
        let buf = vlime#ui#OpenBuffer(
                    \ vlime#ui#PreviewBufName(), v:true, 'topleft split')
        if buf > 0
            " We already switched to the preview window
            if type(win_size) != v:t_none
                execute 'resize ' . win_size
                set winfixheight
                set winfixwidth
            endif

            if !getbufvar(buf, 'vlime_buffer_initialized', v:false)
                call setbufvar(buf, 'vlime_buffer_initialized', v:true)
                call vlime#ui#SetVlimeBufferOpts(buf, a:conn)
            endif
            if a:append
                call vlime#ui#AppendString(a:content)
            else
                call vlime#ui#ReplaceContent(a:content)
            endif
        endif
    finally
        call win_gotoid(old_win_id)
    endtry

    return buf
endfunction

function! vlime#ui#InputFromMiniBuffer(conn, prompt, init_val, complete_command)
    let buf = vlime#ui#OpenBuffer(
                \ vlime#ui#MiniBufName(a:prompt), v:true, 'botright split')
    call vlime#ui#SetVlimeBufferOpts(buf, a:conn)
    call setbufvar(buf, '&buflisted', 0)
    resize 4
    set winfixheight
    set winfixwidth

    call vlime#ui#AppendString('; ' . a:prompt . "\n")
    if type(a:init_val) != v:t_none
        call vlime#ui#AppendString(a:init_val)
    endif

    augroup MiniBufferLeaveAu
        autocmd!
        execute 'autocmd BufWinLeave <buffer> bunload! ' . buf
    augroup end

    execute 'nnoremap <buffer> <cr> :' . a:complete_command . '<cr>'
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

function! vlime#ui#Pad(prefix, sep, max_len)
    return a:prefix . a:sep . repeat(' ', a:max_len + 1 - len(a:prefix))
endfunction

function! vlime#ui#ErrMsg(msg)
    echohl ErrorMsg
    echom a:msg
    echohl None
endfunction

function! vlime#ui#SetVlimeBufferOpts(buf, conn)
    call setbufvar(a:buf, '&buftype', 'nofile')
    call setbufvar(a:buf, '&bufhidden', 'hide')
    call setbufvar(a:buf, '&swapfile', 0)
    call setbufvar(a:buf, '&buflisted', 1)
    call setbufvar(a:buf, 'vlime_conn', a:conn)
endfunction

function! vlime#ui#MatchCoord(coord, cur_line, cur_col)
    let c_begin = get(a:coord, 'begin', v:null)
    let c_end = get(a:coord, 'end', v:null)
    if type(c_begin) == v:t_none || type(c_end) == v:t_none
        return v:false
    endif

    if c_begin[0] == c_end[0] && a:cur_line == c_begin[0]
                \ && a:cur_col >= c_begin[1]
                \ && a:cur_col <= c_end[1]
        return v:true
    elseif c_begin[0] < c_end[0]
        if a:cur_line == c_begin[0] && a:cur_col >= c_begin[1]
            return v:true
        elseif a:cur_line == c_end[0] && a:cur_col <= c_end[1]
            return v:true
        elseif a:cur_line > c_begin[0] && a:cur_line < c_end[0]
            return v:true
        endif
    endif

    return v:false
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

function! vlime#ui#InspectorBufName()
    return 'vlime / inspect'
endfunction

function! vlime#ui#MiniBufName(prompt)
    return 'vlime / input / ' . a:prompt
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

