function! vlime#ui#inspector#InitInspectorBuf(ui, conn, thread)
    let buf = bufnr(vlime#ui#InspectorBufName(), v:true)
    call vlime#ui#SetVlimeBufferOpts(buf, a:conn)
    if type(a:thread) != v:t_none
        call a:ui.SetCurrentThread(a:thread, buf)
    endif
    return buf
endfunction

function! vlime#ui#inspector#FillInspectorBuf(content, thread, itag)
    call vlime#ui#ReplaceContent(a:content['TITLE'] . "\n"
                \ . repeat('=', len(a:content['TITLE'])) . "\n\n")
    normal! G$

    let coords = []
    call vlime#ui#inspector#FillInspectorBufContent(
                \ a:content['CONTENT'], coords)
    let b:vlime_inspector_coords = coords

    augroup InspectorLeaveAu
        autocmd!
        execute 'autocmd BufWinLeave <buffer> call vlime#ui#inspector#ResetInspectorBuffer('
                    \ . bufnr('%') . ')'
        if type(a:thread) != v:t_none && type(a:itag) != v:t_none
            execute 'autocmd BufWinLeave <buffer> call b:vlime_conn.Return('
                        \ . a:thread . ', ' . a:itag . ', v:null)'
        endif
    augroup end

    nnoremap <buffer> <cr> :call vlime#ui#inspector#InspectorSelect()<cr>
    nnoremap <buffer> <space> :call vlime#ui#inspector#InspectorSelect()<cr>
    nnoremap <buffer> p :call vlime#ui#inspector#InspectorPop()<cr>
endfunction

function! vlime#ui#inspector#FillInspectorBufContent(content, coords)
    if type(a:content) == v:t_string
        call vlime#ui#AppendString(a:content)
        normal! G$
    elseif type(a:content) == v:t_list
        if len(a:content) == 3 && type(a:content[0]) == v:t_dict
            let begin_pos = getcurpos()
            if begin_pos[2] != 1 || len(getline('.')) > 0
                let begin_pos[2] += 1
            endif
            call vlime#ui#inspector#FillInspectorBufContent(
                        \ a:content[1], a:coords)
            let end_pos = getcurpos()
            call add(a:coords, {
                        \ 'begin': [begin_pos[1], begin_pos[2]],
                        \ 'end': [end_pos[1], end_pos[2]],
                        \ 'type': a:content[0]['name'],
                        \ 'id': a:content[2],
                        \ })
        else
            for c in a:content
                call vlime#ui#inspector#FillInspectorBufContent(c, a:coords)
            endfor
        endif
    endif
endfunction

function! vlime#ui#inspector#ResetInspectorBuffer(bufnr)
    call setbufvar(a:bufnr, 'vlime_conn', v:null)
    call setbufvar(a:bufnr, 'vlime_inspector_coords', [])
    execute 'bunload! ' . a:bufnr
endfunction

function! vlime#ui#inspector#InspectorSelect()
    let cur_pos = getcurpos()
    let coord = v:null
    for c in b:vlime_inspector_coords
        if c['begin'][0] == c['end'][0] && cur_pos[1] == c['begin'][0]
                    \ && cur_pos[2] >= c['begin'][1]
                    \ && cur_pos[2] <= c['end'][1]
            let coord = c
            break
        elseif c['begin'][0] < c['end'][0]
            if cur_pos[1] == c['begin'][0] && cur_pos[2] >= c['begin'][1]
                let coord = c
                break
            elseif cur_pos[1] == c['end'][0] && cur_pos[2] <= c['end'][1]
                let coord = c
                break
            elseif cur_pos[1] > c['begin'][0] && cur_pos[1] < c['end'][0]
                let coord = c
                break
            endif
        endif
    endfor

    if type(coord) == v:t_none
        return
    endif

    if coord['type'] == 'ACTION'
        call b:vlime_conn.InspectorCallNthAction(coord['id'],
                    \ {c, r -> c.ui.OnInspect(c, r, v:null, v:null)})
    elseif coord['type'] == 'VALUE'
        call b:vlime_conn.InspectNthPart(coord['id'],
                    \ {c, r -> c.ui.OnInspect(c, r, v:null, v:null)})
    endif
endfunction

function! vlime#ui#inspector#InspectorPop()
    call b:vlime_conn.InspectorPop(function('s:OnInspectorPopComplete'))
endfunction

function! s:OnInspectorPopComplete(conn, result)
    if type(a:result) == v:t_none
        call vlime#ui#ErrMsg('The inspector stack is empty.')
    else
        call a:conn.ui.OnInspect(a:conn, a:result, v:null, v:null)
    endif
endfunction

