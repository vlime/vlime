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
    call s:DrawTraceEntryHeader(
                \ a:trace_count, 0,
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
    endif
endfunction

function! s:InitTraceDialogBuffer()
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
        if type(spec) == v:t_dict
            let content .= (untrace_button . ' ' . spec['package'] . '::' . spec['name'] . "\n")
        elseif type(spec) == v:t_list
            let content .= (untrace_button .
                            \ ' (' . spec[0]['name'] . ' ' .
                            \ spec[1]['package'] . '::' . spec[1]['name'] .
                            \ ")\n")
        else
            let content .= (untrace_button . " <UNKNOWN>\n")
        endif
        let cur_line += 1
    endfor

    let content .= "\n"
    let new_lines_count = vlime#ui#ReplaceContent(content, first_line, last_line)
    let b:vlime_trace_specs_line_range = [first_line, first_line + new_lines_count - 1]

    let delta = s:CalcLineRangeShift(b:vlime_trace_specs_line_range, line_range)
    let b:vlime_trace_entries_header_line_range =
                \ s:ShiftLineRange(
                    \ get(b:, 'vlime_trace_entries_header_line_range', v:null),
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

    let new_lines_count = vlime#ui#ReplaceContent(content, first_line, last_line)

    if type(line_range) == type(v:null)
        let b:vlime_trace_entries_header_line_range =
                    \ [first_line, first_line + new_lines_count - 2]
    else
        let b:vlime_trace_entries_header_line_range =
                    \ [first_line, first_line + new_lines_count - 1]
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
    let coords = []
    call setbufvar(a:trace_buf, '&modifiable', 1)
    call vlime#ui#WithBuffer(a:trace_buf,
                \ function('s:DrawTraceEntryHeader', [a:result, 0, coords]))
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
    echom string(a:result)
endfunction

function! s:GetFetchKey()
    if !exists('b:vlime_trace_fetch_key')
        let b:vlime_trace_fetch_key = get(s:, 'next_fetch_key', 0)
        let s:next_fetch_key = b:vlime_trace_fetch_key + 1
    endif
    return b:vlime_trace_fetch_key
endfunction
