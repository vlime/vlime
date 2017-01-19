function! vlime#ui#xref#InitXRefBuf(conn)
    let buf = bufnr(vlime#ui#XRefBufName(a:conn), v:true)
    call vlime#ui#SetVlimeBufferOpts(buf, a:conn)
    return buf
endfunction

function! vlime#ui#xref#FillXRefBuf(xref_list)
    if type(a:xref_list) == v:t_none
        call vlime#ui#ReplaceContent('No xref found.')
        return
    elseif type(a:xref_list) == v:t_dict &&
                \ a:xref_list['name'] == 'NOT-IMPLEMENTED'
        call vlime#ui#ReplaceContent('Not implemented.')
        return
    else
        let xlist = a:xref_list
    endif

    let coords = []
    normal! ggVG"_d
    let idx = 0
    for xref in xlist
        let begin_pos = getcurpos()
        call vlime#ui#AppendString(xref[0])
        normal! G$
        let end_pos = getcurpos()
        call vlime#ui#AppendString("\n")
        call add(coords, {
                    \ 'begin': [begin_pos[1], begin_pos[2]],
                    \ 'end': [end_pos[1], end_pos[2]],
                    \ 'type': 'XREF',
                    \ 'id': idx,
                    \ })
        let idx += 1
    endfor
    normal! gg

    let b:vlime_xref_coords = coords
    let b:vlime_xref_list = xlist

    nnoremap <buffer> <cr> :call vlime#ui#xref#OpenCurXref()<cr>
endfunction

function! vlime#ui#xref#OpenCurXref()
    let xref_coord = v:null
    for c in b:vlime_xref_coords
        let cur_pos = getcurpos()
        if vlime#ui#MatchCoord(c, cur_pos[1], cur_pos[2])
            let xref_coord = c
            break
        endif
    endfor

    if type(xref_coord) == v:t_none
        return
    endif

    let xref_loc = b:vlime_xref_list[xref_coord['id']][1][1:]
    let path = s:FindXRefLocationProp('FILE', xref_loc)
    let pos = s:FindXRefLocationProp('POSITION', xref_loc)

    if type(path) != v:t_none
        call vlime#ui#JumpToOrOpenFile(path, pos)
    endif
endfunction

function! s:FindXRefLocationProp(key, prop_list)
    if type(a:prop_list) != v:t_none
        for p in a:prop_list
            if p[0]['name'] == a:key
                return p[1]
            endif
        endfor
    endif
    return v:null
endfunction
