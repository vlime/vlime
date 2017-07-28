let s:indent_level_width = 2

function! vlime#ui#trace_dialog#InitTraceDialogBuf(conn)
    let buf = bufnr(vlime#ui#TraceDialogBufName(a:conn), v:true)
    if !vlime#ui#VlimeBufferInitialized(buf)
        call vlime#ui#SetVlimeBufferOpts(buf, a:conn)
        call setbufvar(buf, '&filetype', 'vlime_trace')
        call vlime#ui#WithBuffer(buf, function('s:InitTraceDialogBuffer'))
    endif
    return buf
endfunction

function! vlime#ui#trace_dialog#FillTraceDialogBuf(spec_list, trace_count)
    setlocal modifiable

    let b:vlime_trace_specs_coords = []
    call s:DrawSpecList(a:spec_list, b:vlime_trace_specs_coords)

    let b:vlime_trace_entries_header_coords = []
    let cached_entries = get(b:, 'vlime_trace_cached_entries', {})
    call s:DrawTraceEntryHeader(
                \ a:trace_count, len(cached_entries),
                \ b:vlime_trace_entries_header_coords)

    setlocal nomodifiable
endfunction

function! vlime#ui#trace_dialog#Select()
    let coord = s:GetCurCoord()

    if type(coord) == type(v:null)
        return
    endif

    if coord['type'] == 'REFRESH-SPECS'
        call b:vlime_conn.ReportSpecs(
                    \ function('s:ReportSpecsComplete', [bufnr('%')]))
    elseif coord['type'] == 'UNTRACE-ALL-SPECS'
        call b:vlime_conn.DialogUntraceAll(
                    \ function('s:DialogUntraceAllComplete', [bufnr('%')]))
    elseif coord['type'] == 'UNTRACE-SPEC'
        call b:vlime_conn.DialogUntrace(coord['id'],
                    \ function('s:DialogUntraceComplete', [bufnr('%')]))
    elseif coord['type'] == 'REFRESH-TRACE-ENTRY-HEADER'
        call b:vlime_conn.ReportTotal(
                    \ function('s:ReportTotalComplete', [bufnr('%')]))
    elseif coord['type'] == 'FETCH-NEXT-TRACE-ENTRIES-BATCH'
        call b:vlime_conn.ReportPartialTree(
                    \ s:GetFetchKey(),
                    \ function('s:ReportPartialTreeComplete', [bufnr('%')]))
    elseif coord['type'] == 'CLEAR-TRACE-ENTRIES'
        call b:vlime_conn.ClearTraceTree(
                    \ function('s:ClearTraceTreeComplete', [bufnr('%')]))
    elseif coord['type'] == 'TRACE-ENTRY-ARG'
        " TODO
        echom string(coord)
    elseif coord['type'] == 'TRACE-ENTRY-RETVAL'
        " TODO
        echom string(coord)
    endif
endfunction

function! s:InitTraceDialogBuffer()
    execute 'setlocal shiftwidth=' . s:indent_level_width
    call vlime#ui#MapBufferKeys('trace')
endfunction

function! s:DrawSpecList(spec_list, coords)
    let line_range = get(b:, 'vlime_trace_specs_line_range', v:null)
    if type(line_range) == type(v:null)
        let first_line = 1
        let last_line = line('$')
    else
        let [first_line, last_line] = line_range
    endif

    let spec_list = (type(a:spec_list) == type(v:null)) ? [] : a:spec_list
    let title = 'Traced (' . len(spec_list) . ')'
    let content = title . "\n" . repeat('=', len(title)) . "\n\n"
    let cur_line = 4

    let header_buttons = s:AddButton(
                \ '', '[refresh]', 'REFRESH-SPECS', v:null, cur_line, a:coords)
    let header_buttons .= ' '
    let header_buttons = s:AddButton(
                \ header_buttons, '[untrace all]',
                \ 'UNTRACE-ALL-SPECS', v:null, cur_line, a:coords)

    let content .= (header_buttons . "\n\n")
    let cur_line += 2

    let untrace_button = '[untrace]'
    for spec in spec_list
        let untrace_button = s:AddButton(
                    \ '', '[untrace]', 'UNTRACE-SPEC', spec, cur_line, a:coords)
        let content .= (untrace_button . ' ' . s:NameObjToStr(spec) . "\n")
        let cur_line += 1
    endfor

    let content .= "\n"

    let old_cur_pos = getcurpos()
    try
        let new_lines_count = vlime#ui#ReplaceContent(content, first_line, last_line)
    finally
        call setpos('.', old_cur_pos)
    endtry

    let b:vlime_trace_specs_line_range = [first_line, first_line + new_lines_count - 1]

    let delta = s:CalcLineRangeShift(b:vlime_trace_specs_line_range, line_range)
    let b:vlime_trace_entries_header_line_range =
                \ s:ShiftLineRange(
                    \ get(b:, 'vlime_trace_entries_header_line_range', v:null),
                    \ delta)
    let b:vlime_trace_entries_line_range =
                \ s:ShiftLineRange(
                    \ get(b:, 'vlime_trace_entries_line_range', v:null),
                    \ delta)
endfunction

function! s:DrawTraceEntryHeader(entry_count, cached_entry_count, coords)
    let line_range = get(b:, 'vlime_trace_entries_header_line_range', v:null)
    if type(line_range) == type(v:null)
        let first_line = line('$')
        let last_line = line('$')
    else
        let [first_line, last_line] = line_range
    endif

    let title = 'Trace Entries (' . a:cached_entry_count . '/' . a:entry_count . ')'
    let content = title . "\n" . repeat('=', len(title)) . "\n\n"
    let cur_line = 4

    let header_buttons = s:AddButton(
                \ '', '[refresh]',
                \ 'REFRESH-TRACE-ENTRY-HEADER', v:null, cur_line, a:coords)
    let header_buttons .= ' '

    if a:cached_entry_count != a:entry_count
        let header_buttons = s:AddButton(
                    \ header_buttons, '[fetch next batch]',
                    \ 'FETCH-NEXT-TRACE-ENTRIES-BATCH', v:null, cur_line, a:coords)
        let header_buttons .= ' '
        let header_buttons = s:AddButton(
                    \ header_buttons, '[fetch all]',
                    \ 'FETCH-ALL-TRACE-ENTRIES', v:null, cur_line, a:coords)
        let header_buttons .= ' '
    endif

    let header_buttons = s:AddButton(
                \ header_buttons, '[clear]',
                \ 'CLEAR-TRACE-ENTRIES', v:null, cur_line, a:coords)
    let content .= (header_buttons . "\n\n")

    let old_cur_pos = getcurpos()
    try
        let new_lines_count = vlime#ui#ReplaceContent(content, first_line, last_line)
    finally
        call setpos('.', old_cur_pos)
    endtry

    if type(line_range) == type(v:null)
        let b:vlime_trace_entries_header_line_range =
                    \ [first_line, first_line + new_lines_count - 2]
    else
        let b:vlime_trace_entries_header_line_range =
                    \ [first_line, first_line + new_lines_count - 1]
    endif
endfunction

" s:DrawTraceEntries(toplevel, cached_entries, coords[, cur_level[, acc_content[, cur_line]]])
"
" This is a recursive function, with "isomeric" return values. The toplevel
" call returns nothing meaningful, but the nested calls return the constructed
" text content.
function! s:DrawTraceEntries(toplevel, cached_entries, coords, ...)
    let cur_level = get(a:000, 0, 0)
    let acc_content = get(a:000, 1, '')
    let cur_line = get(a:000, 2, 1)

    let line_range = get(b:, 'vlime_trace_entries_line_range', v:null)
    if type(line_range) == type(v:null)
        let first_line = line('$')
        let last_line = line('$')
    else
        let [first_line, last_line] = line_range
    endif

    let content = ''
    for tid in a:toplevel
        let entry = a:cached_entries[tid]

        let connector_char = (acc_content == '') ? ' ' : '`'
        let str_id = string(entry['id'])
        let name_line = str_id .
                    \ s:Indent(
                        \ connector_char . repeat('-', s:indent_level_width - 1) . ' ' .
                            \ s:NameObjToStr(entry['name']) . "\n",
                        \ cur_level * s:indent_level_width - len(str_id) + 1)
        let content .= name_line
        let cur_line += 1

        let arg_ret_prefix = (len(entry['children']) > 0) ? '|' : ' '

        let [arg_content, cur_line] = s:ConstructTraceEntryArgs(
                    \ entry['id'], entry['args'], arg_ret_prefix . ' > ',
                    \ 'TRACE-ENTRY-ARG', cur_level, cur_line, a:coords)
        let content .= arg_content

        let [ret_content, cur_line] = s:ConstructTraceEntryArgs(
                    \ entry['id'], entry['retvals'], arg_ret_prefix . ' < ',
                    \ 'TRACE-ENTRY-RETVAL', cur_level, cur_line, a:coords)
        let content .= ret_content

        if len(entry['children']) > 0
            let [content, cur_line] = s:DrawTraceEntries(
                        \ entry['children'], a:cached_entries,
                        \ a:coords, cur_level + 1, content, cur_line)
        endif
    endfor

    if acc_content == ''
        let old_cur_pos = getcurpos()
        try
            let new_lines_count = vlime#ui#ReplaceContent(content, first_line, last_line)
        finally
            call setpos('.', old_cur_pos)
        endtry
        let b:vlime_trace_entries_line_range =
                    \ [first_line, first_line + new_lines_count - 1]
    else
        return [acc_content . content, cur_line]
    endif
endfunction

function! s:AddButton(buttons_str, name, co_type, co_id, cur_line, coords)
    let button_begin = [a:cur_line, len(a:buttons_str) + 1]
    let buttons_str = a:buttons_str . a:name
    let button_end = [a:cur_line, len(buttons_str)]
    call add(a:coords, {
                \ 'begin': button_begin,
                \ 'end': button_end,
                \ 'type': a:co_type,
                \ 'id': a:co_id
                \})
    return buttons_str
endfunction

function! s:CalcLineRangeShift(new, old)
    if type(a:old) == type(v:null)
        return 0
    endif
    return a:new[1] - a:old[1]
endfunction

function! s:ShiftLineRange(line_range, delta)
    if type(a:line_range) == type(v:null)
        return a:line_range
    endif
    return [a:line_range[0] + a:delta, a:line_range[1] + a:delta]
endfunction

function! s:GetCurCoord()
    let cur_pos = getcurpos()

    if cur_pos[1] >= b:vlime_trace_specs_line_range[0] &&
                \ cur_pos[1] <= b:vlime_trace_specs_line_range[1]
        let line_delta = b:vlime_trace_specs_line_range[0] - 1
        let shifted_line = cur_pos[1] - line_delta
        for c in b:vlime_trace_specs_coords
            if vlime#ui#MatchCoord(c, shifted_line, cur_pos[2])
                return c
            endif
        endfor
    elseif cur_pos[1] >= b:vlime_trace_entries_header_line_range[0] &&
                \ cur_pos[1] <= b:vlime_trace_entries_header_line_range[1]
        let line_delta = b:vlime_trace_entries_header_line_range[0] - 1
        let shifted_line = cur_pos[1] - line_delta
        for c in b:vlime_trace_entries_header_coords
            if vlime#ui#MatchCoord(c, shifted_line, cur_pos[2])
                return c
            endif
        endfor
    elseif exists('b:vlime_trace_entries_line_range') &&
                \ cur_pos[1] >= b:vlime_trace_entries_line_range[0] &&
                \ cur_pos[1] <= b:vlime_trace_entries_line_range[1]
        let line_delta = b:vlime_trace_entries_line_range[0] - 1
        let shifted_line = cur_pos[1] - line_delta
        for c in b:vlime_trace_entries_coords
            if vlime#ui#MatchCoord(c, shifted_line, cur_pos[2])
                return c
            endif
        endfor
    endif

    return v:null
endfunction

function! s:ReportSpecsComplete(trace_buf, conn, result)
    let coords = []
    call setbufvar(a:trace_buf, '&modifiable', 1)
    call vlime#ui#WithBuffer(a:trace_buf,
                \ function('s:DrawSpecList', [a:result, coords]))
    call setbufvar(a:trace_buf, '&modifiable', 0)
    call setbufvar(a:trace_buf, 'vlime_trace_specs_coords', coords)
endfunction

function! s:ReportTotalComplete(trace_buf, conn, result)
    let cached_entries =
                \ getbufvar(a:trace_buf, 'vlime_trace_cached_entries', {})

    let coords = []
    call setbufvar(a:trace_buf, '&modifiable', 1)
    call vlime#ui#WithBuffer(a:trace_buf,
                \ function('s:DrawTraceEntryHeader',
                    \ [a:result, len(cached_entries), coords]))
    call setbufvar(a:trace_buf, '&modifiable', 0)
    call setbufvar(a:trace_buf, 'vlime_trace_entries_header_coords', coords)
endfunction

function! s:DialogUntraceAllComplete(trace_buf, conn, result)
    if type(a:result) != type(v:null)
        for r in a:result
            echom r
        endfor
    endif

    call b:vlime_conn.ReportSpecs(
                \ function('s:ReportSpecsComplete', [a:trace_buf]))
endfunction

function! s:DialogUntraceComplete(trace_buf, conn, result)
    echom a:result
    call b:vlime_conn.ReportSpecs(
                \ function('s:ReportSpecsComplete', [a:trace_buf]))
endfunction

function! s:ReportPartialTreeComplete(trace_buf, conn, result)
    let [entry_list, remaining, fetch_key] = a:result
    let entry_list = (type(entry_list) == type(v:null)) ? [] : entry_list

    let cached_entries =
                \ getbufvar(a:trace_buf, 'vlime_trace_cached_entries', {})
    let toplevel_entries =
                \ getbufvar(a:trace_buf, 'vlime_trace_toplevel_entries', [])

    for t_entry in entry_list
        let [id, parent, name, arg_list, retval_list] = t_entry
        let entry_obj = get(cached_entries, id, {'id': id, 'children': []})
        let entry_obj['parent'] = parent
        let entry_obj['name'] = name
        let entry_obj['args'] = s:ArgListToDict(arg_list)
        let entry_obj['retvals'] = s:ArgListToDict(retval_list)
        let parent_obj = (type(parent) == type(v:null)) ?
                    \ v:null : get(cached_entries, parent, v:null)
        if type(parent_obj) == type(v:null)
            if index(toplevel_entries, id) < 0
                call add(toplevel_entries, id)
            endif
        else
            if index(parent_obj['children'], id) < 0
                call add(parent_obj['children'], id)
            endif
        endif
        let cached_entries[id] = entry_obj
    endfor

    call setbufvar(a:trace_buf, 'vlime_trace_cached_entries', cached_entries)
    call setbufvar(a:trace_buf, 'vlime_trace_toplevel_entries', toplevel_entries)

    let coords = []
    call setbufvar(a:trace_buf, '&modifiable', 1)
    call vlime#ui#WithBuffer(a:trace_buf,
                \ function('s:DrawTraceEntryHeader',
                    \ [len(cached_entries) + remaining,
                        \ len(cached_entries),
                        \ coords]))
    call setbufvar(a:trace_buf, 'vlime_trace_entries_header_coords', coords)

    let coords = []
    call vlime#ui#WithBuffer(a:trace_buf,
                \ function('s:DrawTraceEntries',
                    \ [toplevel_entries, cached_entries, coords]))
    call setbufvar(a:trace_buf, '&modifiable', 0)
    call setbufvar(a:trace_buf, 'vlime_trace_entries_coords', coords)
endfunction

function! s:ClearTraceTreeComplete(trace_buf, conn, result)
    call vlime#ui#WithBuffer(a:trace_buf, function('s:ResetTraceEntries'))
    call b:vlime_conn.ReportTotal(
                \ function('s:ReportTotalComplete', [a:trace_buf]))
endfunction

function! s:ResetTraceEntries()
    silent! unlet b:vlime_trace_fetch_key
    silent! unlet b:vlime_trace_cached_entries
    silent! unlet b:vlime_trace_toplevel_entries
    silent! unlet b:vlime_trace_entries_coords

    let line_range = get(b:, 'vlime_trace_entries_line_range', v:null)
    silent! unlet b:vlime_trace_entries_line_range

    if type(line_range) != type(v:null)
        let [first_line, last_line] = line_range
        setlocal modifiable
        execute first_line . ',' . last_line . 'delete _'
        call append(first_line - 1, '')
        setlocal nomodifiable
    endif
endfunction

function! s:GetFetchKey()
    if !exists('b:vlime_trace_fetch_key')
        let b:vlime_trace_fetch_key = get(s:, 'next_fetch_key', 0)
        let s:next_fetch_key = b:vlime_trace_fetch_key + 1
        if s:next_fetch_key > 65535
            let s:next_fetch_key = 0
        endif
    endif
    return b:vlime_trace_fetch_key
endfunction

function! s:ArgListToDict(arg_list)
    let arg_list = (type(a:arg_list) == type(v:null)) ? [] : a:arg_list
    let args = {}
    for r in arg_list
        let args[r[0]] = r[1]
    endfor
    return args
endfunction

function! s:Indent(str, count)
    return repeat(' ', a:count) . a:str
endfunction

function! s:NameObjToStr(name)
    if type(a:name) == v:t_dict
        return a:name['package'] . '::' . a:name['name']
    elseif type(a:name) == v:t_list
        return '(' . a:name[0]['name'] . ' ' .
                    \ a:name[1]['package'] . '::' . a:name[1]['name'] . ')'
    else
        throw 'NameObjToStr: illegal name: ' . string(name)
    endif
endfunction

function! s:ConstructTraceEntryArgs(
            \ entry_id, arg_dict, prefix, button_type, cur_level, cur_line, coords)
    let content = ''
    let cur_line = a:cur_line
    for i in sort(keys(a:arg_dict), 'n')
        let line = s:Indent(
                    \ a:prefix,
                    \ (a:cur_level + 1) * s:indent_level_width)
        let line = s:AddButton(
                    \ line, a:arg_dict[i],
                    \ a:button_type, [a:entry_id, str2nr(i)],
                    \ cur_line, a:coords)
        let line .= "\n"
        let content .= line
        let cur_line += 1
    endfor
    return [content, cur_line]
endfunction
