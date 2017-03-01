function! vlime#ui#compiler_notes#InitCompilerNotesBuffer(conn)
    let buf = bufnr(vlime#ui#CompilerNotesBufName(a:conn), v:true)
    if !vlime#ui#VlimeBufferInitialized(buf)
        call vlime#ui#SetVlimeBufferOpts(buf, a:conn)
    endif
    return buf
endfunction

function! vlime#ui#compiler_notes#FillCompilerNotesBuf(note_list)
    if type(a:note_list) == v:t_none
        call vlime#ui#ReplaceContent('No message from the compiler.')
        let b:vlime_compiler_note_coords = {}
        let b:vlime_compiler_note_list = []
        return
    endif

    let coords = []
    let nlist = []
    normal! ggVG"_d
    let idx = 0
    let note_count = len(a:note_list)
    for note in a:note_list
        let note_dict = vlime#PListToDict(note)
        call add(nlist, note_dict)

        let begin_pos = getcurpos()
        call vlime#ui#AppendString(note_dict['SEVERITY']['name'] . ': ' . note_dict['MESSAGE'])
        normal! G$
        let end_pos = getcurpos()
        if idx < note_count - 1
            call vlime#ui#AppendString("\n--\n")
        endif
        call add(coords, {
                    \ 'begin': [begin_pos[1], begin_pos[2]],
                    \ 'end': [end_pos[1], end_pos[2]],
                    \ 'type': 'NOTE',
                    \ 'id': idx,
                    \ })
        let idx += 1
    endfor
    normal! gg

    let b:vlime_compiler_note_coords = coords
    let b:vlime_compiler_note_list = nlist

    nnoremap <buffer> <silent> <cr> :call vlime#ui#compiler_notes#OpenCurNote()<cr>
endfunction

function! vlime#ui#compiler_notes#OpenCurNote()
    let cur_pos = getcurpos()
    let note_coord = v:null
    for c in b:vlime_compiler_note_coords
        if vlime#ui#MatchCoord(c, cur_pos[1], cur_pos[2])
            let note_coord = c
            break
        endif
    endfor

    if type(note_coord) == v:t_none
        return
    endif

    let note_loc = b:vlime_compiler_note_list[note_coord['id']]['LOCATION']

    let note_file = s:FindNoteLocationProp('FILE', note_loc)
    let note_buffer = s:FindNoteLocationProp('BUFFER', note_loc)

    if type(note_file) != v:t_none
        let note_pos = s:FindNoteLocationProp('POSITION', note_loc)
        call vlime#ui#JumpToOrOpenFile(note_file[0], note_pos[0])
    elseif type(note_buffer) != v:t_none
        let note_offset = s:FindNoteLocationProp('OFFSET', note_loc)
        let note_offset = note_offset[0] + note_offset[1]
        call vlime#ui#JumpToOrOpenFile(note_buffer[0], note_offset)
    else
        call vlime#ui#ErrMsg('No source available.')
    endif
endfunction

function! s:FindNoteLocationProp(key, loc)
    if type(a:loc) != v:t_none
        for p in a:loc[1:]
            if type(p) == v:t_list && p[0]['name'] == a:key
                return p[1:]
            endif
        endfor
    endif
    return v:null
endfunction
