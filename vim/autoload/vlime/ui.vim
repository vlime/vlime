function! vlime#ui#New()
    let obj = {
                \ 'buffer_package_map': {},
                \ 'buffer_thread_map': {},
                \ 'GetCurrentPackage': function('vlime#ui#GetCurrentPackage'),
                \ 'SetCurrentPackage': function('vlime#ui#SetCurrentPackage'),
                \ 'GetCurrentThread': function('vlime#ui#GetCurrentThread'),
                \ 'SetCurrentThread': function('vlime#ui#SetCurrentThread'),
                \ 'OnDebug': function('vlime#ui#OnDebug'),
                \ 'OnDebugActivate': function('vlime#ui#OnDebugActivate')
                \ }
    return obj
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
    let dbg_buf = s:InitSLDBBuf(self, a:conn, a:thread, a:level)
    call setbufvar(dbg_buf, '&modifiable', 1)
    call vlime#ui#WithBuffer(
                \ dbg_buf,
                \ function('s:FillSLDBBuf',
                    \ [a:thread, a:level, a:condition, a:restarts, a:frames]))
    call setbufvar(dbg_buf, '&modifiable', 0)
endfunction

function! vlime#ui#OnDebugActivate(conn, thread, level, select)
    let dbg_buf = bufnr(s:SLDBBufName(a:conn, a:thread))
    let win_nr = bufwinnr(dbg_buf)
    if win_nr < 0
        execute 'botright split #' . dbg_buf
    else
        execute win_nr . 'wincmd w'
    endif
    let pos = search('^\s*[0-9]\+\.\s\+\*[A-Z]\+\s\+-\s.\+$')
    if pos <= 0
        call search('^\s*[0-9]\+\.\s\+[A-Z]\+\s\+-\s.\+$')
    endif
    redraw
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
    setlocal iskeyword+=+,-,*,/,%,<,=,>,:,$,?,!,@-@,94,~,#,\|,&,.,{,},[,]
    let atom = expand('<cword>')
    let &l:iskeyword = old_kw
    return atom
endfunction

function! vlime#ui#CurExpr()
    let cur_char = vlime#ui#CurChar()
    if cur_char == '('
        let [s_line, s_col] = searchpairpos('(', '', ')', 'cbnW')
        let [e_line, e_col] = searchpairpos('(', '', ')', 'nW')
    elseif cur_char == ')'
        let [s_line, s_col] = searchpairpos('(', '', ')', 'bnW')
        let [e_line, e_col] = searchpairpos('(', '', ')', 'cnW')
    else
        let [s_line, s_col] = searchpairpos('(', '', ')', 'bnW')
        let [e_line, e_col] = searchpairpos('(', '', ')', 'nW')
    endif
    let lines = getline(s_line, e_line)
    if len(lines) == 1
        let lines[0] = strpart(lines[0], s_col - 1, e_col - s_col + 1)
    elseif len(lines) > 1
        let lines[0] = strpart(lines[0], s_col - 1)
        let lines[-1] = strpart(lines[-1], 0, e_col)
    endif
    return join(lines, "\n")
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

function! vlime#ui#ChooseCurRestart()
    let line = getline('.')
    let matches = matchlist(line, '^\s*\([0-9]\+\)\.\s\+\*\?[A-Z]\+\s\+-\s.\+$')
    if len(matches) > 0
        let nth = matches[1] + 0
        echom 'Invoking restart ' . nth
        call b:vlime_conn.InvokeNthRestartForEmacs(b:vlime_sldb_level, nth)
        set nobuflisted
        bunload!
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

function! s:PadIdx(idx, sep, max_digits)
    return a:idx . a:sep . repeat(' ', a:max_digits + 1 - len(string(a:idx)))
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

function! s:InitSLDBBuf(ui, conn, thread, level)
    let buf = bufnr(s:SLDBBufName(a:conn, a:thread), v:true)
    call setbufvar(buf, '&buftype', 'nofile')
    call setbufvar(buf, '&bufhidden', 'hide')
    call setbufvar(buf, '&swapfile', 0)
    call setbufvar(buf, '&buflisted', 1)
    call setbufvar(buf, 'vlime_conn', a:conn)
    call setbufvar(buf, 'vlime_sldb_level', a:level)
    call a:ui.SetCurrentThread(a:thread, buf)
    return buf
endfunction

" Operates on current buffer. Should be called with vlime#ui#WithBuffer(...)
function! s:FillSLDBBuf(thread, level, condition, restarts, frames)
    normal! ggVG"_d

    call append(line('$'), 'Thread: ' . a:thread . '; Level: ' . a:level)
    call append(line('$'), '')

    call append(line('$'), a:condition[0:-2])
    call append(line('$'), '')

    call append(line('$'), 'Restarts:')
    let [max_name_len, has_star] = s:FindMaxRestartNameLen(a:restarts)
    let max_digits = len(string(len(a:restarts) - 1))
    let ri = 0
    while ri < len(a:restarts)
        let r = a:restarts[ri]
        let idx_str = s:PadIdx(ri, '.', max_digits)
        let restart_line = s:FormatRestartLine(r, max_name_len, has_star)
        call append(line('$'), '  ' . idx_str . restart_line)
        let ri += 1
    endwhile
    call append(line('$'), '')

    call append(line('$'), 'Frames:')
    let max_digits = len(string(len(a:frames) - 1))
    for f in a:frames
        let idx_str = s:PadIdx(f[0], '.', max_digits)
        call append(line('$'), '  ' . idx_str . f[1])
    endfor

    " TODO: Move to a separate function?
    nnoremap <buffer> <cr> :call vlime#ui#ChooseCurRestart()<cr>
endfunction

function! s:SLDBBufName(conn, thread)
    return 'sldb / ' . a:conn.cb_data.name . ' / ' . a:thread
endfunction
