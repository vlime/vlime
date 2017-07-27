function! vlime#contrib#trace_dialog#ClearTraceTree(...) dict
    let Callback = get(a:000, 0, v:null)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'CLEAR-TRACE-TREE')]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#ClearTraceTree']))
endfunction

function! vlime#contrib#trace_dialog#DialogToggleTrace(name, ...) dict
    let Callback = get(a:000, 0, v:null)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'DIALOG-TOGGLE-TRACE'),
                        \ s:TranslateFunctionSpec(a:name, self)]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#DialogToggleTrace']))
endfunction

function! vlime#contrib#trace_dialog#DialogTrace(name, ...) dict
    let Callback = get(a:000, 0, v:null)
    let pkg = s:GetCurrentPackage(self)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'DIALOG-TRACE'),
                        \ s:TranslateFunctionSpec(a:name, self)]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#DialogTrace']))
endfunction

function! vlime#contrib#trace_dialog#DialogUntrace(name, ...) dict
    let Callback = get(a:000, 0, v:null)
    let pkg = s:GetCurrentPackage(self)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'DIALOG-UNTRACE'),
                        \ s:TranslateFunctionSpec(a:name, self)]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#DialogUntrace']))
endfunction

function! vlime#contrib#trace_dialog#DialogUntraceAll(...) dict
    let Callback = get(a:000, 0, v:null)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'DIALOG-UNTRACE-ALL')]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#DialogUntraceAll']))
endfunction

function! vlime#contrib#trace_dialog#FindTrace(id, ...) dict
    let Callback = get(a:000, 0, v:null)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'FIND-TRACE'), a:id]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#FindTrace']))
endfunction

function! vlime#contrib#trace_dialog#FindTracePart(id, part_id, type, ...) dict
    let Callback = get(a:000, 0, v:null)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'FIND-TRACE-PART'),
                        \ a:id, a:part_id, vlime#KW(a:type)]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#FindTracePart']))
endfunction

function! vlime#contrib#trace_dialog#InspectTracePart(id, part_id, type, ...) dict
    let Callback = get(a:000, 0, v:null)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'INSPECT-TRACE-PART'),
                        \ a:id, a:part_id, vlime#KW(a:type)]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#InspectTracePart']))
endfunction

function! vlime#contrib#trace_dialog#ReportPartialTree(key, ...) dict
    let Callback = get(a:000, 0, v:null)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'REPORT-PARTIAL-TREE'), a:key]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#ReportPartialTree']))
endfunction

function! vlime#contrib#trace_dialog#ReportSpecs(...) dict
    let Callback = get(a:000, 0, v:null)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'REPORT-SPECS')]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#ReportSpecs']))
endfunction

function! vlime#contrib#trace_dialog#ReportTotal(...) dict
    let Callback = get(a:000, 0, v:null)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'REPORT-TOTAL')]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#ReportTotal']))
endfunction

function! vlime#contrib#trace_dialog#ReportTraceDetail(id, ...) dict
    let Callback = get(a:000, 0, v:null)
    call self.Send(self.EmacsRex(
                    \ [vlime#SYM('SWANK-TRACE-DIALOG', 'REPORT-TRACE-DETAIL'), a:id]),
                \ function('vlime#SimpleSendCB',
                    \ [self, Callback, 'vlime#contrib#trace_dialog#ReportTraceDetail']))
endfunction

function! vlime#contrib#trace_dialog#Init(conn)
    let a:conn['ClearTraceTree'] = function('vlime#contrib#trace_dialog#ClearTraceTree')
    let a:conn['DialogToggleTrace'] = function('vlime#contrib#trace_dialog#DialogToggleTrace')
    let a:conn['DialogTrace'] = function('vlime#contrib#trace_dialog#DialogTrace')
    let a:conn['DialogUntrace'] = function('vlime#contrib#trace_dialog#DialogUntrace')
    let a:conn['DialogUntraceAll'] = function('vlime#contrib#trace_dialog#DialogUntraceAll')
    let a:conn['FindTrace'] = function('vlime#contrib#trace_dialog#FindTrace')
    let a:conn['FindTracePart'] = function('vlime#contrib#trace_dialog#FindTracePart')
    let a:conn['InspectTracePart'] = function('vlime#contrib#trace_dialog#InspectTracePart')
    let a:conn['ReportPartialTree'] = function('vlime#contrib#trace_dialog#ReportPartialTree')
    let a:conn['ReportSpecs'] = function('vlime#contrib#trace_dialog#ReportSpecs')
    let a:conn['ReportTotal'] = function('vlime#contrib#trace_dialog#ReportTotal')
    let a:conn['ReportTraceDetail'] = function('vlime#contrib#trace_dialog#ReportTraceDetail')
endfunction

function! s:TranslateFunctionSpec(spec, conn)
    if type(a:spec) == v:t_string
        return [vlime#CL('QUOTE'), vlime#SYM(s:GetCurrentPackage(a:conn), a:spec)]
    elseif type(a:spec) == v:t_list &&
                \ len(a:spec) == 2 &&
                \ type(a:spec[0]) == v:t_string &&
                \ a:spec[0] == 'SETF'
        return [vlime#CL('QUOTE'),
                    \ [vlime#CL(a:spec[0]),
                        \ vlime#SYM(s:GetCurrentPackage(a:conn), a:spec[1])]]
    elseif type(a:spec) == v:t_dict ||
                \ (type(a:spec) == v:t_list &&
                    \ len(a:spec) == 2 &&
                    \ type(a:spec[0]) == v:t_dict &&
                    \ a:spec[0]['name'] == 'SETF')
        return [vlime#CL('QUOTE'), a:spec]
    endif
endfunction

function! s:GetCurrentPackage(conn)
    let pkg = a:conn.GetCurrentPackage()
    if type(pkg) == type(v:null)
        return 'COMMON-LISP-USER'
    endif
    return pkg[0]
endfunction
