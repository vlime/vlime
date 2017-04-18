let g:vlime_default_window_settings = {
            \ 'sldb': {'pos': 'botright', 'size': v:null, 'vertical': v:false},
            \ 'repl': {'pos': 'botright', 'size': v:null, 'vertical': v:false},
            \ 'inspector': {'pos': 'botright', 'size': v:null, 'vertical': v:false},
            \ 'xref': {'pos': 'botright', 'size': 12, 'vertical': v:false},
            \ 'notes': {'pos': 'botright', 'size': 12, 'vertical': v:false},
            \ 'threads': {'pos': 'botright', 'size': 12, 'vertical': v:false},
            \ 'preview': {'pos': 'aboveleft', 'size': 12, 'vertical': v:false},
            \ 'arglist': {'pos': 'topleft', 'size': 2, 'vertical': v:false},
            \ 'input': {'pos': 'botright', 'size': 4, 'vertical': v:false},
            \ 'server': {'pos': 'botright', 'size': v:null, 'vertical': v:false},
            \ }

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
                \ 'OnXRef': function('vlime#ui#OnXRef'),
                \ 'OnCompilerNotes': function('vlime#ui#OnCompilerNotes'),
                \ 'OnThreads': function('vlime#ui#OnThreads'),
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
    call vlime#ui#WithBuffer(
                \ dbg_buf,
                \ function('vlime#ui#sldb#FillSLDBBuf',
                    \ [a:thread, a:level, a:condition, a:restarts, a:frames]))
endfunction

function! vlime#ui#OnDebugActivate(conn, thread, level, select) dict
    let dbg_buf = vlime#ui#OpenBufferWithWinSettings(
                \ vlime#ui#SLDBBufName(a:conn, a:thread), v:false, 'sldb')
    if dbg_buf > 0
        call setpos('.', [0, 1, 1, 0, 1])
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
    if len(win_findbuf(repl_buf)) <= 0
        call vlime#ui#OpenBufferWithWinSettings(repl_buf, v:false, 'repl')
    endif
    call vlime#ui#repl#AppendOutput(repl_buf, a:str)
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
    call vlime#ui#OpenBufferWithWinSettings(insp_buf, v:false, 'inspector')

    let r_content = vlime#PListToDict(a:i_content)
    let old_title = getline(1)
    if get(r_content, 'TITLE', v:null) == old_title
        let old_cur = getcurpos()
    else
        let old_cur = [0, 1, 1, 0, 1]
    endif

    call vlime#ui#inspector#FillInspectorBuf(r_content, a:i_thread, a:i_tag)
    call setpos('.', old_cur)
    " Needed for displaying the content of the current buffer correctly
    redraw
endfunction

function! vlime#ui#OnXRef(conn, xref_list)
    if type(a:xref_list) == v:t_none
        call vlime#ui#ErrMsg('No xref found.')
    elseif type(a:xref_list) == v:t_dict &&
                \ a:xref_list['name'] == 'NOT-IMPLEMENTED'
        call vlime#ui#ErrMsg('Not implemented.')
    else
        let xref_buf = vlime#ui#xref#InitXRefBuf(a:conn)
        call vlime#ui#OpenBufferWithWinSettings(xref_buf, v:false, 'xref')
        call vlime#ui#xref#FillXRefBuf(a:xref_list)
    endif
endfunction

function! vlime#ui#OnCompilerNotes(conn, note_list)
    let notes_buf = vlime#ui#compiler_notes#InitCompilerNotesBuffer(a:conn)
    let buf_opened = len(win_findbuf(notes_buf)) > 0
    if buf_opened || type(a:note_list) != v:t_none
        let old_win_id = win_getid()
        call vlime#ui#OpenBufferWithWinSettings(notes_buf, v:false, 'notes')
        call vlime#ui#compiler_notes#FillCompilerNotesBuf(a:note_list)
        if type(a:note_list) == v:t_none
            " There's no message. Don't stay in the notes window.
            call win_gotoid(old_win_id)
        endif
    endif
endfunction

function! vlime#ui#OnThreads(conn, thread_list)
    let threads_buf = vlime#ui#threads#InitThreadsBuffer(a:conn)
    call vlime#ui#OpenBufferWithWinSettings(threads_buf, v:false, 'threads')
    call vlime#ui#threads#FillThreadsBuf(a:thread_list)
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
    let pattern = '(\_s*in-package\_s\+\(.\+\)\_s*)'
    let old_cur_pos = getcurpos()
    try
        let package_line = search(pattern, 'bcW')
        if package_line <= 0
            let package_line = search(pattern, 'cW')
        endif
        if package_line > 0
            let matches = matchlist(vlime#ui#CurExpr(), pattern)
            " The pattern used here does not check for lone parentheses,
            " so there may not be a match.
            let package = (len(matches) > 0) ? s:NormalizePackageName(matches[1]) : ''
        else
            let package = ''
        endif
        return package
    finally
        call setpos('.', old_cur_pos)
    endtry
endfunction

function! vlime#ui#CurOperator()
    let expr = vlime#ui#CurExpr()
    if len(expr) > 0
        let matches = matchlist(expr, '^(\_s*\(\k\+\)\_s*\_.*)$')
        if len(matches) > 0
            return matches[1]
        endif
    else
        " There may be an incomplete expression
        let [s_line, s_col] = searchpairpos('(', '', ')', 'cbnW')
        if s_line > 0 && s_col > 0
            let op_line = getline(s_line)[(s_col-1):]
            let matches = matchlist(op_line, '(\s*\(\k\+\)\s*')
            if len(matches) > 0
                return matches[1]
            endif
        endif
    endif
    return ''
endfunction

function! vlime#ui#SurroundingOperator()
    let [s_line, s_col] = searchpairpos('(', '', ')', 'bnW')
    if s_line > 0 && s_col > 0
        let op_line = getline(s_line)[(s_col-1):]
        let matches = matchlist(op_line, '(\s*\(\k\+\)\s*')
        if len(matches) > 0
            return matches[1]
        endif
    endif
    return ''
endfunction

" vlime#ui#CurSelection([return_pos])
function! vlime#ui#CurSelection(...)
    let return_pos = vlime#GetNthVarArg(a:000, 0, v:false)
    let sel_start = getpos("'<")
    let sel_end = getpos("'>")
    let lines = getline(sel_start[1], sel_end[1])
    if sel_start[1] == sel_end[1]
        let lines[0] = lines[0][(sel_start[2]-1):(sel_end[2]-1)]
    else
        let lines[0] = lines[0][(sel_start[2]-1):]
        let last_idx = len(lines) - 1
        let lines[last_idx] = lines[last_idx][:(sel_end[2]-1)]
    endif

    if return_pos
        return [join(lines, "\n"), sel_start[1:2], sel_end[1:2]]
    else
        return join(lines, "\n")
    endif
endfunction

function! vlime#ui#CurBufferContent()
    let last_line_nr = line('$')
    let line_nr = 1
    let lines = getline(1, '$')
    return join(filter(lines, "match(v:val, '^\s*;.*$') < 0"), "\n")
endfunction

function! vlime#ui#GetCurWindowLayout()
    let old_win = win_getid()
    let layout = []
    let old_ei = &eventignore
    let &eventignore = 'all'
    try
        windo call add(layout,
                    \ {'id': win_getid(),
                        \ 'height': winheight(0),
                        \ 'width': winwidth(0)})
        return layout
    finally
        call win_gotoid(old_win)
        let &eventignore = old_ei
    endtry
endfunction

function! vlime#ui#RestoreWindowLayout(layout)
    let old_win = win_getid()
    let old_ei = &eventignore
    let &eventignore = 'all'
    try
        for ws in a:layout
            call win_gotoid(ws['id'])
            execute 'resize ' . ws['height']
            execute 'vertical resize ' . ws['width']
        endfor
    finally
        call win_gotoid(old_win)
        let &eventignore = old_ei
    endtry
endfunction

function! vlime#ui#WithBuffer(buf, Func)
    let buf_win = bufwinid(a:buf)
    let buf_visible = (buf_win >= 0) ? v:true : v:false

    let old_win = win_getid()

    let old_lazyredraw = &lazyredraw
    let &lazyredraw = 1

    let old_ei = &eventignore
    let &eventignore = 'all'

    try

        if buf_visible
            call win_gotoid(buf_win)
            return a:Func()
        else
            let old_layout = vlime#ui#GetCurWindowLayout()
            try
                silent call vlime#ui#OpenBuffer(a:buf, v:false, v:true)
                let tmp_win_nr = winnr()
                try
                    return a:Func()
                finally
                    execute tmp_win_nr . 'wincmd c'
                endtry
            finally
                call vlime#ui#RestoreWindowLayout(old_layout)
            endtry
        endif
    finally
        call win_gotoid(old_win)
        let &lazyredraw = old_lazyredraw
        let &eventignore = old_ei
    endtry
endfunction

" vlime#ui#OpenBuffer(name, create, show[, vertical[, initial_size]])
function! vlime#ui#OpenBuffer(name, create, show, ...)
    let vertical = vlime#GetNthVarArg(a:000, 0, v:false)
    let initial_size = vlime#GetNthVarArg(a:000, 1)
    let buf = bufnr(a:name, a:create)
    if buf > 0
        if (type(a:show) == v:t_string && len(a:show) > 0) || a:show
            " Found it. Try to put it in a window
            let win_nr = bufwinnr(buf)
            if win_nr < 0
                if type(a:show) == v:t_string
                    let split_cmd = 'split #' . buf
                    if vertical
                        let split_cmd = join(['vertical', split_cmd])
                    endif
                    " Use silent! to suppress the 'Illegal file name' message
                    " and E303: Unable to open swap file...
                    silent! execute join([a:show, split_cmd])
                else
                    silent! execute 'split #' . buf
                endif
                if type(initial_size) != v:t_none
                    let resize_cmd = join(['resize', initial_size])
                    if vertical
                        let resize_cmd = join(['vertical', resize_cmd])
                    endif
                    execute resize_cmd
                endif
            else
                execute win_nr . 'wincmd w'
            endif
        endif
    endif
    return buf
endfunction

function! vlime#ui#OpenBufferWithWinSettings(buf_name, buf_create, win_name)
    let [win_pos, win_size, win_vert] = vlime#ui#GetWindowSettings(a:win_name)
    return vlime#ui#OpenBuffer(a:buf_name, a:buf_create,
                \ win_pos, win_vert, win_size)
endfunction

" vlime#ui#ShowTransientWindow(
"       conn, content, append, buf_name, win_name[, file_type])
function! vlime#ui#ShowTransientWindow(
            \ conn, content, append, buf_name, win_name, ...)
    let file_type = vlime#GetNthVarArg(a:000, 0, v:null)
    let old_win_id = win_getid()
    try
        let buf = vlime#ui#OpenBufferWithWinSettings(
                    \ a:buf_name, v:true, a:win_name)
        if buf > 0
            " We already switched to the transient window
            setlocal winfixheight
            setlocal winfixwidth

            if !vlime#ui#VlimeBufferInitialized(buf)
                call vlime#ui#SetVlimeBufferOpts(buf, a:conn)
                if type(file_type) != v:t_none
                    call setbufvar(buf, '&filetype', file_type)
                endif
            else
                call setbufvar(buf, 'vlime_conn', a:conn)
            endif

            setlocal modifiable
            if a:append
                call vlime#ui#AppendString(a:content)
            else
                call vlime#ui#ReplaceContent(a:content)
            endif
            setlocal nomodifiable
        endif
        return buf
    finally
        call win_gotoid(old_win_id)
    endtry
endfunction

function! vlime#ui#ShowPreview(conn, content, append)
    return vlime#ui#ShowTransientWindow(
                \ a:conn, a:content, a:append,
                \ vlime#ui#PreviewBufName(), 'preview', 'vlime_preview')
endfunction

function! vlime#ui#ShowArgList(conn, content)
    return vlime#ui#ShowTransientWindow(
                \ a:conn, a:content, v:false,
                \ vlime#ui#ArgListBufName(), 'arglist', 'vlime_arglist')
endfunction

function! vlime#ui#InputFromMiniBuffer(conn, prompt, init_val, complete_command)
    let buf = vlime#ui#OpenBufferWithWinSettings(
                \ vlime#ui#MiniBufName(a:conn, a:prompt), v:true, 'input')
    call vlime#ui#SetVlimeBufferOpts(buf, a:conn)
    call setbufvar(buf, '&buflisted', 0)
    setlocal winfixheight
    setlocal winfixwidth

    call vlime#ui#AppendString('; ' . a:prompt . "\n")
    if type(a:init_val) != v:t_none
        call vlime#ui#AppendString(a:init_val)
    endif

    augroup MiniBufferLeaveAu
        autocmd!
        execute 'autocmd BufWinLeave <buffer> bunload! ' . buf
    augroup end

    execute 'nnoremap <buffer> <silent> <cr> :' . a:complete_command . '<cr>'
endfunction

function! vlime#ui#AppendString(str)
    let new_lines = split(a:str, "\n", v:true)
    let last_line_nr = line('$')
    let last_line = getline(last_line_nr)
    call setline(last_line_nr, last_line . new_lines[0])
    call append('$', new_lines[1:])

    let cur_line_nr = line('.')
    if cur_line_nr == last_line_nr
        call setpos('.', [0, line('$'), 1, 0, 1])
    endif
endfunction

function! vlime#ui#ReplaceContent(str)
    1,$delete _
    call vlime#ui#AppendString(a:str)
    call setpos('.', [0, 1, 1, 0, 1])
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
    let new_line = substitute(line, '^\(\s*\)', indent_str, '')
    call setline('.', new_line)
    let spaces = vlime#ui#CalcLeadingSpaces(new_line)
    call setpos('.', [0, line('.'), spaces + 1, 0, a:indent + 1])
endfunction

" vlime#ui#CurArgPosForIndent([pos])
function! vlime#ui#CurArgPosForIndent(...)
    let s_pos = vlime#GetNthVarArg(a:000, 0, v:null)
    let arg_pos = -1

    if type(s_pos) == v:t_none
        let [s_line, s_col] = searchpairpos('(', '', ')', 'bnW')
    else
        let [s_line, s_col] = s_pos
    endif
    if s_line <= 0 || s_col <= 0
        return arg_pos
    endif

    let cur_pos = getcurpos()
    let paren_count = 0
    let delimiter = v:false

    for ln in range(s_line, cur_pos[1])
        let line = getline(ln)
        let idx = (ln == s_line) ? (s_col - 1) : 0
        let end_idx = (ln == cur_pos[1]) ? cur_pos[2] : len(line)

        while idx < end_idx
            let ch = line[idx]
            if ch == ' ' || ch == "\<tab>"
                let delimiter = v:true
            else
                if delimiter
                    let delimiter = v:false
                    if paren_count == 1
                        let arg_pos += 1
                    endif
                endif

                if ch == '('
                    let delimiter = v:true
                    let paren_count += 1
                elseif ch == ')'
                    let delimiter = v:true
                    let paren_count -= 1
                endif
            endif

            let idx += 1
        endwhile

        let delimiter = v:true
    endfor

    return arg_pos
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

function! vlime#ui#VlimeBufferInitialized(buf)
    return type(getbufvar(a:buf, 'vlime_conn', v:null)) != v:t_none
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

function! vlime#ui#JumpToOrOpenFile(file_path, byte_pos)
    let file_buf = bufnr(a:file_path)
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
        if type(a:file_path) == v:t_number
            call vlime#ui#ErrMsg('Buffer ' . a:file_path . ' does not exist.')
        elseif a:file_path[0:6] == 'sftp://' || filereadable(a:file_path)
            execute 'tabedit ' . escape(a:file_path, ' \')
        else
            call vlime#ui#ErrMsg('Not readable: ' . a:file_path)
            return
        endif
    endif

    if type(a:byte_pos) != v:t_none
        let src_line = byte2line(a:byte_pos)
        call setpos('.', [0, src_line, 1, 0, 1])
        let cur_pos = line2byte('.') + col('.') - 1
        if a:byte_pos - cur_pos > 0
            call setpos('.', [0, src_line, a:byte_pos - cur_pos + 1, 0])
        endif
    endif
endfunction

function! vlime#ui#CalcLeadingSpaces(str, ...)
    let expand_tab = vlime#GetNthVarArg(a:000, 0, v:false)
    if expand_tab
        let n_str = substitute(a:str, "\t", repeat(' ', &tabstop), 'g')
    else
        let n_str = a:str
    endif
    let spaces = match(n_str, '[^[:blank:]]')
    if spaces < 0
        let spaces = len(n_str)
    endif
    return spaces
endfunction

function! vlime#ui#GetEndOfFileCoord()
    let last_line_nr = line('$')
    let last_line = getline(last_line_nr)
    let last_col_nr = len(last_line)
    if last_col_nr <= 0
        let last_col_nr = 1
    endif
    return [last_line_nr, last_col_nr]
endfunction

if !exists('g:vlime_buf_name_sep')
    let g:vlime_buf_name_sep = ' | '
endif

function! vlime#ui#SLDBBufName(conn, thread)
    return join(['vlime', 'sldb', a:conn.cb_data.name, a:thread],
                \ g:vlime_buf_name_sep)
endfunction

function! vlime#ui#REPLBufName(conn)
    return join(['vlime', 'repl', a:conn.cb_data.name],
                \ g:vlime_buf_name_sep)
endfunction

function! vlime#ui#PreviewBufName()
    return join(['vlime', 'preview'], g:vlime_buf_name_sep)
endfunction

function! vlime#ui#ArgListBufName()
    return join(['vlime', 'arglist'], g:vlime_buf_name_sep)
endfunction

function! vlime#ui#InspectorBufName(conn)
    return join(['vlime', 'inspect', a:conn.cb_data.name],
                \ g:vlime_buf_name_sep)
endfunction

function! vlime#ui#MiniBufName(conn, prompt)
    return join(['vlime', 'input', a:conn.cb_data.name, a:prompt],
                \ g:vlime_buf_name_sep)
endfunction

function! vlime#ui#XRefBufName(conn)
    return join(['vlime', 'xref', a:conn.cb_data.name],
                \ g:vlime_buf_name_sep)
endfunction

function! vlime#ui#CompilerNotesBufName(conn)
    return join(['vlime', 'notes', a:conn.cb_data.name],
                \ g:vlime_buf_name_sep)
endfunction

function! vlime#ui#ThreadsBufName(conn)
    return join(['vlime', 'threads', a:conn.cb_data.name],
                \ g:vlime_buf_name_sep)
endfunction

function! vlime#ui#ServerBufName(server_name)
    return join(['vlime', 'server', a:server_name],
                \ g:vlime_buf_name_sep)
endfunction

function! vlime#ui#GetWindowSettings(win_name)
    let settings = get(g:vlime_default_window_settings, a:win_name, v:null)
    if type(settings) == v:t_none
        throw 'vlime#ui#GetWindowSettings: unknown window ' . string(a:win_name)
    else
        let settings = copy(settings)
    endif

    if exists('g:vlime_window_settings')
        let UserSettings = get(g:vlime_window_settings, a:win_name, {})
        if type(UserSettings) == v:t_func
            let UserSettings = UserSettings()
        endif
        for sk in keys(UserSettings)
            let settings[sk] = UserSettings[sk]
        endfor
    endif

    return [get(settings, 'pos', 'botright'),
                \ get(settings, 'size', v:null),
                \ get(settings, 'vertical', v:false)]
endfunction

" vlime#ui#EnsureKeyMapped(mode, key, cmd[, flags])
function! vlime#ui#EnsureKeyMapped(mode, key, cmd, ...)
    let flags = vlime#GetNthVarArg(a:000, 0, '<buffer> <silent>')
    if type(a:key) != v:t_list
        let key_list = [a:key]
    else
        let key_list = a:key
    endif

    if !hasmapto(a:cmd)
        for kk in key_list
            if len(maparg(kk, a:mode)) <= 0
                execute a:mode . join(['noremap', flags, kk, a:cmd])
            endif
        endfor
    endif
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

