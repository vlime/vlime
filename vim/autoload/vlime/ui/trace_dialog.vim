function! vlime#ui#trace_dialog#InitTraceDialogBuffer(conn)
    let buf = bufnr(vlime#ui#TraceDialogBufName(a:conn), v:true)
    if !vlime#ui#VlimeBufferInitialized(buf)
        call vlime#ui#SetVlimeBufferOpts(buf, a:conn)
        call setbufvar(buf, '&filetype', 'vlime_trace')
        call vlime#ui#WithBuffer(buf, function('s:InitTraceDialogBuffer'))
    endif
    return buf
endfunction

function! vlime#ui#trace_dialog#FillTraceDialogBuf(spec_list, trace_count)
    let coords = []
    let specs_line_range = get(b:, 'vlime_trace_specs_line_range', v:null)
    let entries_header_line_range = get(b:, 'vlime_trace_entries_header_line_range', v:null)

    setlocal modifiable

    let b:vlime_trace_specs_line_range =
                \ s:DrawSpecList(a:spec_list, specs_line_range, coords)

    let entries_header_line_range = s:ShiftLineRange(
                \ entries_header_line_range,
                \ s:CalcLineRangeShift(
                    \ b:vlime_trace_specs_line_range, specs_line_range))

    let b:vlime_trace_entries_header_line_range =
                \ s:DrawTraceEntryHeader(
                    \ a:trace_count, 0, entries_header_line_range, coords)

    setlocal nomodifiable

    let b:vlime_trace_coords = coords
endfunction

function! s:InitTraceDialogBuffer()
    " TODO
endfunction

function! s:DrawSpecList(spec_list, line_range, coords)
    if type(a:line_range) == type(v:null)
        let first_line = 1
        let last_line = line('$')
    else
        let [first_line, last_line] = a:line_range
    endif

    let title = 'Traced (' . len(a:spec_list) . ')'
    let content = title . "\n" . repeat('=', len(title)) . "\n\n"
    let cur_line = first_line + 3

    let header_buttons = s:AddButton(
                \ '', '[refresh]', 'REFRESH-SPECS', v:null, cur_line, a:coords)
    let header_buttons .= ' '
    let header_buttons = s:AddButton(
                \ header_buttons, '[untrace all]',
                \ 'UNTRACE-ALL-SPECS', v:null, cur_line, a:coords)

    let content .= (header_buttons . "\n\n")
    let cur_line += 2

    let untrace_button = '[untrace]'
    for spec in a:spec_list
        let untrace_button = s:AddButton(
                    \ '', '[untrace]', 'UNTRACE-SPEC', spec, cur_line, a:coords)
        let content .= (untrace_button . ' ' . spec['package'] . '::' . spec['name'] . "\n")
        let cur_line += 1
    endfor

    let content .= "\n"
    let new_lines_count = vlime#ui#ReplaceContent(content, first_line, last_line)
    return [first_line, first_line + new_lines_count - 1]
endfunction

function! s:DrawTraceEntryHeader(entry_count, cached_entry_count, line_range, coords)
    if type(a:line_range) == type(v:null)
        let first_line = line('$')
        let last_line = line('$')
    else
        let [first_line, last_line] = a:line_range
    endif

    let title = 'Trace Entries (' . a:cached_entry_count . '/' . a:entry_count . ')'
    let content = title . "\n" . repeat('=', len(title)) . "\n\n"
    let cur_line = first_line + 3

    let header_buttons = s:AddButton(
                \ '', '[refresh]',
                \ 'REFRESH-TRACE-ENTRY-HEADER', v:null, cur_line, a:coords)
    let header_buttons .= ' '
    let header_buttons = s:AddButton(
                \ header_buttons, '[fetch next batch]',
                \ 'FETCH-NEXT-TRACE-ENTRIES-BATCH', v:null, cur_line, a:coords)
    let header_buttons .= ' '
    let header_buttons = s:AddButton(
                \ header_buttons, '[fetch all]',
                \ 'FETCH-ALL-TRACE-ENTRIES', v:null, cur_line, a:coords)
    let header_buttons .= ' '
    let header_buttons = s:AddButton(
                \ header_buttons, '[clear]',
                \ 'CLEAR-TRACE-ENTRIES', v:null, cur_line, a:coords)
    let content .= (header_buttons . "\n\n")

    let new_lines_count = vlime#ui#ReplaceContent(content, first_line, last_line)

    if type(a:line_range) == type(v:null)
        return [first_line, first_line + new_lines_count - 2]
    else
        return [first_line, first_line + new_lines_count - 1]
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
