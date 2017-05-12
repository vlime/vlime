function! vlime#ui#compiler_notes#InitCompilerNotesBuffer(conn)
    let buf = bufnr(vlime#ui#CompilerNotesBufName(a:conn), v:true)
    if !vlime#ui#VlimeBufferInitialized(buf)
        call vlime#ui#SetVlimeBufferOpts(buf, a:conn)
        call setbufvar(buf, '&filetype', 'vlime_notes')
        call vlime#ui#WithBuffer(buf, function('s:InitCompilerNotesBuffer'))
    endif
    return buf
endfunction

function! vlime#ui#compiler_notes#FillCompilerNotesBuf(note_list)
    setlocal modifiable

    if type(a:note_list) == type(v:null)
        call vlime#ui#ReplaceContent('No message from the compiler.')
        let b:vlime_compiler_note_coords = []
        let b:vlime_compiler_note_list = []
        return
    endif

    let coords = []
    let nlist = []
    1,$delete _
    let idx = 0
    let note_count = len(a:note_list)
    for note in a:note_list
        let note_dict = vlime#PListToDict(note)
        call add(nlist, note_dict)

        let begin_pos = getcurpos()
        call vlime#ui#AppendString(note_dict['SEVERITY']['name'] . ': ' . note_dict['MESSAGE'])
        let eof_coord = vlime#ui#GetEndOfFileCoord()
        if idx < note_count - 1
            call vlime#ui#AppendString("\n--\n")
        endif
        call add(coords, {
                    \ 'begin': [begin_pos[1], begin_pos[2]],
                    \ 'end': eof_coord,
                    \ 'type': 'NOTE',
                    \ 'id': idx,
                    \ })
        let idx += 1
    endfor
    call setpos('.', [0, 1, 1, 0, 1])

    setlocal nomodifiable

    let b:vlime_compiler_note_coords = coords
    let b:vlime_compiler_note_list = nlist
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

    if type(note_coord) == type(v:null)
        return
    endif

    let note_loc = b:vlime_compiler_note_list[note_coord['id']]['LOCATION']

    let note_file = s:FindNoteLocationProp('FILE', note_loc)
    let note_buffer = s:FindNoteLocationProp('BUFFER', note_loc)

    if type(note_file) != type(v:null)
        let note_pos = s:FindNoteLocationProp('POSITION', note_loc)
        call vlime#ui#JumpToOrOpenFile(note_file[0], note_pos[0])
    elseif type(note_buffer) != type(v:null)
        let note_offset = s:FindNoteLocationProp('OFFSET', note_loc)
        let note_offset = note_offset[0] + note_offset[1]
        call vlime#ui#JumpToOrOpenFile(note_buffer[0], note_offset)
    else
        call vlime#ui#ErrMsg('No source available.')
    endif
endfunction

function! s:FindNoteLocationProp(key, loc)
    if type(a:loc) != type(v:null)
        for p in a:loc[1:]
            if type(p) == v:t_list && p[0]['name'] == a:key
                return p[1:]
            endif
        endfor
    endif
    return v:null
endfunction

function! s:InitCompilerNotesBuffer()
    call vlime#ui#MapBufferKeys('notes')
endfunction
