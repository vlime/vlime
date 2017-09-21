function! vlime#overlay#slimv#SendToREPL(content)
    if v:register != '"' && v:register != '+'
        call setreg(v:register, a:content)
    endif
    call vlime#plugin#SendToREPL(a:content)
endfunction

function! vlime#overlay#slimv#CurTopExprOrAtom()
    let expr = vlime#ui#CurTopExpr()
    if len(expr) <= 0
        let expr = vlime#ui#CurAtom()
    endif
    return expr
endfunction

function! vlime#overlay#slimv#SendSelection()
    if v:register == '"' || v:register == '+'
        call vlime#plugin#SendToREPL(vlime#ui#CurSelection())
    else
        call vlime#plugin#SendToREPL(getreg(v:register))
    endif
endfunction

function! vlime#overlay#slimv#CompileSelection()
    if v:register == '"' || v:register == '+'
        call vlime#plugin#Compile(vlime#ui#CurSelection(v:true))
    else
        call vlime#plugin#Compile(getreg(v:register))
    endif
endfunction

function! vlime#overlay#slimv#UntraceAll()
    let conn = vlime#connection#Get()
    if type(conn) == type(v:null)
        return
    endif

    call conn.DialogUntraceAll(function('s:DialogUntraceAllComplete'))
endfunction

if !exists('g:vlime_default_mappings')
    " buf_type: [
    "   [mode, key, command, description],
    "   ...
    " ]
    let g:vlime_default_mappings = {
                \ 'lisp': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("lisp")<cr>',
                        \ 'Show this quick reference.'],
                    \
                    \ ['i', '<tab>', '<c-r>=vlime#plugin#VlimeKey("tab")<cr>',
                        \ 'Trigger omni-completion.'],
                    \ ['i', '<space>', '<space><c-r>=vlime#plugin#VlimeKey("space")<cr>',
                        \ 'Trigger the arglist hint.'],
                    \ ['i', '<cr>', '<cr><c-r>=vlime#plugin#VlimeKey("cr")<cr>',
                        \ 'Trigger the arglist hint.'],
                    \
                    \ ['n', '<LocalLeader>c', ':call vlime#plugin#ConnectREPL()<cr>',
                        \ 'Connect to a server.'],
                    \ ['n', ['<LocalLeader>Cs', '<LocalLeader>CS'], ':call vlime#plugin#SelectCurConnection()<cr>',
                        \ 'Switch connections.'],
                    \ ['n', ['<LocalLeader>Cd', '<LocalLeader>CD'], ':call vlime#plugin#CloseCurConnection()<cr>',
                        \ 'Disconnect.'],
                    \ ['n', ['<LocalLeader>Cr', '<LocalLeader>CR'], ':call vlime#plugin#RenameCurConnection()<cr>',
                        \ 'Rename the current connection.'],
                    \
                    \ ['n', '<LocalLeader>j', ':call vlime#plugin#FindDefinition(vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Show the definition for the name under the cursor.'],
                    \
                    \ ['n', '<LocalLeader>d', ':call vlime#overlay#slimv#SendToREPL(vlime#overlay#slimv#CurTopExprOrAtom())<cr>',
                        \ 'Send the top-level expression under the cursor to the REPL.'],
                    \ ['n', '<LocalLeader>e', ':call vlime#overlay#slimv#SendToREPL(vlime#ui#CurExprOrAtom())<cr>',
                        \ 'Send the expression under the cursor to the REPL.'],
                    \ ['v', '<LocalLeader>r', ':<c-u>call vlime#overlay#slimv#SendToREPL(vlime#ui#CurSelection())<cr>',
                        \ 'Send the current selection to the REPL.'],
                    \ ['n', '<LocalLeader>r', ':call vlime#overlay#slimv#SendSelection()<cr>',
                        \ 'Send the last selection, or the content in a register, to the REPL.'],
                    \ ['n', '<LocalLeader>b', ':call vlime#plugin#SendToREPL(vlime#ui#CurBufferContent(v:true))<cr>',
                        \ 'Send the current buffer to the REPL.'],
                    \ ['n', '<LocalLeader>v', ':call vlime#plugin#SendToREPL()<cr>',
                        \ 'Send a snippet to the REPL.'],
                    \ ['n', '<LocalLeader>u', ':call vlime#plugin#UndefineFunction(vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Undefine the function under the cursor.'],
                    \
                    \ ['n', '<LocalLeader>1', ':call vlime#plugin#ExpandMacro(vlime#ui#CurExpr(), "one")<cr>',
                        \ 'Expand the macro under the cursor once.'],
                    \ ['n', '<LocalLeader>m', ':call vlime#plugin#ExpandMacro(vlime#ui#CurExpr(), "all")<cr>',
                        \ 'Expand the macro under the cursor and all nested macros.'],
                    \
                    \ ['n', '<LocalLeader>t', ':call vlime#plugin#DialogToggleTrace(vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Trace/untrace the function under the cursor.'],
                    \ ['n', '<LocalLeader>T', ':call vlime#overlay#slimv#UntraceAll()<cr>',
                        \ 'Untrace all functions.'],
                    \ ['n', '<LocalLeader>o', ':call vlime#plugin#OpenTraceDialog()<cr>',
                        \ 'Show the trace dialog.'],
                    \
                    \ ['n', '<LocalLeader>B', ':call vlime#plugin#SetBreakpoint(vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Set a breakpoint at entry to a function.'],
                    \ ['n', '<LocalLeader>l', ':call vlime#plugin#DisassembleForm("''" . vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Disassemble the function under the cursor.'],
                    \ ['n', '<LocalLeader>i', ':call vlime#plugin#Inspect(vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Evaluate the atom under the cursor, and inspect the result.'],
                    \ ['v', '<LocalLeader>i', ':<c-u>call vlime#plugin#Inspect(vlime#ui#CurSelection())<cr>',
                        \ 'Evaluate the current selection, and inspect the result.'],
                    \
                    \ ['n', '<LocalLeader>H', ':call vlime#plugin#ListThreads()<cr>',
                        \ 'Show a list of the running threads.'],
                    \
                    \ ['n', '<LocalLeader>D', ':call vlime#plugin#Compile(vlime#ui#CurTopExpr(v:true))<cr>',
                        \ 'Compile the top-level expression under the cursor.'],
                    \ ['n', '<LocalLeader>L', ':call vlime#plugin#CompileFile(expand("%:p"))<cr>',
                        \ 'Compile and load the current file.'],
                    \ ['n', '<LocalLeader>F', ':call vlime#plugin#CompileFile(expand("%:p"), v:null, v:false)<cr>',
                        \ 'Compile the current file.'],
                    \ ['v', '<LocalLeader>R', ':<c-u>call vlime#plugin#Compile(vlime#ui#CurSelection())<cr>',
                        \ 'Compile the current selection.'],
                    \ ['n', '<LocalLeader>R', ':call vlime#overlay#slimv#CompileSelection()<cr>',
                        \ 'Compile the last selection, or the content of a register'],
                    \
                    \ ['n', '<LocalLeader>xr', ':call vlime#plugin#XRefSymbol("REFERENCES", vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Show references to the variable under the cursor.'],
                    \ ['n', '<LocalLeader>xs', ':call vlime#plugin#XRefSymbol("SETS", vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Show locations where the variable under the cursor is set.'],
                    \ ['n', '<LocalLeader>xb', ':call vlime#plugin#XRefSymbol("BINDS", vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Show bindings for the variable under the cursor.'],
                    \ ['n', '<LocalLeader>xm', ':call vlime#plugin#XRefSymbol("MACROEXPANDS", vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Show locations where the macro under the cursor is called.'],
                    \ ['n', '<LocalLeader>xp', ':call vlime#plugin#XRefSymbol("SPECIALIZES", vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Show specialized methods for the class under the cursor.'],
                    \ ['n', '<LocalLeader>xl', ':call vlime#plugin#XRefSymbol("CALLS", vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Show callers of the function under the cursor.'],
                    \ ['n', '<LocalLeader>xe', ':call vlime#plugin#XRefSymbol("CALLS-WHO", vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Show callees of the function under the cursor.'],
                    \ ['n', '<LocalLeader>xi', ':<c-u>call vlime#plugin#XRefSymbolWrapper()<cr>',
                        \ 'Interactively prompt for the symbol to search for cross references.'],
                    \
                    \ ['n', '<LocalLeader>s', ':call vlime#plugin#DescribeSymbol(vlime#ui#CurAtom())<cr>',
                        \ 'Describe the atom under the cursor.'],
                    \ ['n', '<LocalLeader>A', ':call vlime#plugin#AproposList(vlime#ui#CurAtom(), v:true)<cr>',
                        \ 'Apropos search.'],
                    \
                    \ ['n', '<LocalLeader>g', ':call vlime#plugin#SetPackage()<cr>',
                        \ 'Specify the package for the current buffer.'],
                    \
                    \ ['n', ['<LocalLeader>Ss', '<LocalLeader>SS'], ':call vlime#server#New()<cr>',
                        \ 'Start a new server and connect to it.'],
                    \ ['n', ['<LocalLeader>Sv', '<LocalLeader>SV'], ':call vlime#plugin#ShowSelectedServer()<cr>',
                        \ 'View the console output of a server.'],
                    \ ['n', ['<LocalLeader>St', '<LocalLeader>ST'], ':call vlime#plugin#StopSelectedServer()<cr>',
                        \ 'Stop a server.'],
                    \ ['n', ['<LocalLeader>Sr', '<LocalLeader>SR'], ':call vlime#plugin#RenameSelectedServer()<cr>',
                        \ 'Rename a server.'],
                    \
                    \ ['n', '<LocalLeader>wp', ':call vlime#plugin#CloseWindow("preview")<cr>',
                        \ 'Close all visible preview windows.'],
                    \ ['n', '<LocalLeader>wr', ':call vlime#plugin#CloseWindow("arglist")<cr>',
                        \ 'Close all visible arglist windows.'],
                    \ ['n', '<LocalLeader>wn', ':call vlime#plugin#CloseWindow("notes")<cr>',
                        \ 'Close all visible compiler notes windows.'],
                    \ ['n', '<LocalLeader>wR', ':call vlime#plugin#CloseWindow("repl")<cr>',
                        \ 'Close all visible REPL windows.'],
                    \ ['n', '<LocalLeader>wA', ':call vlime#plugin#CloseWindow("")<cr>',
                        \ 'Close all Vlime windows.'],
                    \ ['n', '<LocalLeader>wl', ':call vlime#plugin#CloseWindow()<cr>',
                        \ 'Show a list of visible Vlime windows, and choose which to close.'],
                \ ],
                \
                \ 'sldb': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("sldb")<cr>',
                        \ 'Show this quick reference.'],
                    \
                    \ ['n', '<cr>', ':call vlime#ui#sldb#ChooseCurRestart()<cr>',
                        \ 'Choose a restart.'],
                    \ ['n', 'd', ':call vlime#ui#sldb#ShowFrameDetails()<cr>',
                        \ 'Show the details of the frame under the cursor.'],
                    \ ['n', 'S', ':<c-u>call vlime#ui#sldb#OpenFrameSource()<cr>',
                        \ 'Open the source code for the frame under the cursor.'],
                    \ ['n', 'T', ':<c-u>call vlime#ui#sldb#OpenFrameSource("tabedit")<cr>',
                        \ 'Open the source code for the frame under the cursor, in a separate tab.'],
                    \ ['n', 'O', ':<c-u>call vlime#ui#sldb#FindSource()<cr>',
                        \ 'Open the source code for a local variable.'],
                    \ ['n', 'r', ':call vlime#ui#sldb#RestartCurFrame()<cr>',
                        \ 'Restart the frame under the cursor.'],
                    \ ['n', 's', ':call vlime#ui#sldb#StepCurOrLastFrame("step")<cr>',
                        \ 'Start stepping in the frame under the cursor.'],
                    \ ['n', 'x', ':call vlime#ui#sldb#StepCurOrLastFrame("next")<cr>',
                        \ 'Step over the current function call.'],
                    \ ['n', 'o', ':call vlime#ui#sldb#StepCurOrLastFrame("out")<cr>',
                        \ 'Step out of the current function.'],
                    \ ['n', 'c', ':call b:vlime_conn.SLDBContinue()<cr>',
                        \ 'Invoke the restart labeled CONTINUE.'],
                    \ ['n', 'a', ':call b:vlime_conn.SLDBAbort()<cr>',
                        \ 'Invoke the restart labeled ABORT.'],
                    \ ['n', 'C', ':call vlime#ui#sldb#InspectCurCondition()<cr>',
                        \ 'Inspect the current condition object.'],
                    \ ['n', 'i', ':call vlime#ui#sldb#InspectInCurFrame()<cr>',
                        \ 'Evaluate and inspect an expression in the frame under the cursor.'],
                    \ ['n', 'e', ':call vlime#ui#sldb#EvalStringInCurFrame()<cr>',
                        \ 'Evaluate an expression in the frame under the cursor.'],
                    \ ['n', 'E', ':call vlime#ui#sldb#SendValueInCurFrameToREPL()<cr>',
                        \ 'Evaluate an expression in the frame under the cursor, and send the result to the REPL.'],
                    \ ['n', 'D', ':call vlime#ui#sldb#DisassembleCurFrame()<cr>',
                        \ 'Disassemble the frame under the cursor.'],
                    \ ['n', 'R', ':call vlime#ui#sldb#ReturnFromCurFrame()<cr>',
                        \ 'Return a manually specified result from the frame under the cursor.'],
                \ ],
                \
                \ 'repl': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("repl")<cr>',
                        \ 'Show this quick reference.'],
                    \
                    \ ['n', '<c-c>', ':call b:vlime_conn.Interrupt({"name": "REPL-THREAD", "package": "KEYWORD"})<cr>',
                        \ 'Interrupt the REPL thread.'],
                    \ ['n', '<LocalLeader>-', ':call vlime#ui#repl#ClearREPLBuffer()<cr>',
                        \ 'Clear the REPL buffer.'],
                    \ ['n', '<LocalLeader>i', ':call vlime#ui#repl#InspectCurREPLPresentation()<cr>',
                        \ 'Inspect the evaluation result under the cursor.'],
                    \ ['n', '<LocalLeader>y', ':call vlime#ui#repl#YankCurREPLPresentation()<cr>',
                        \ 'Yank the evaluation result under the cursor.'],
                    \ ['n', ['<tab>', '<c-n>'], ':call vlime#ui#repl#NextField(v:true)<cr>',
                        \ 'Move the cursor to the next presented object.'],
                    \ ['n', '<c-p>', ':call vlime#ui#repl#NextField(v:false)<cr>',
                        \ 'Move the cursor to the previous presented object.'],
                \ ],
                \
                \ 'mrepl': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("mrepl")<cr>',
                        \ 'Show this quick reference.'],
                    \
                    \ ['i', '<space>', '<space><c-r>=vlime#plugin#VlimeKey("space")<cr>',
                        \ 'Trigger the arglist hint.'],
                    \ ['i', '<cr>', '<c-r>=vlime#ui#mrepl#Submit()<cr>',
                        \ 'Submit the last input to the REPL.'],
                    \ ['i', '<c-j>', '<cr><c-r>=vlime#plugin#VlimeKey("cr")<cr>',
                        \ 'Insert a newline, and trigger the arglist hint.'],
                    \ ['i', '<tab>', '<c-r>=vlime#plugin#VlimeKey("tab")<cr>',
                        \ 'Trigger omni-completion.'],
                    \ ['i', '<c-c>', '<c-r>=vlime#ui#mrepl#Interrupt()<cr>',
                        \ 'Interrupt the MREPL thread.'],
                    \ ['n', '<LocalLeader>-', ':call vlime#ui#mrepl#Clear()<cr>',
                        \ 'Clear the MREPL buffer.'],
                    \ ['n', '<LocalLeader>Q', ':call vlime#ui#mrepl#Disconnect()<cr>',
                        \ 'Disconnect from this REPL.'],
                \ ],
                \
                \ 'inspector': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("inspector")<cr>',
                        \ 'Show this quick reference.'],
                    \
                    \ ['n', ['<cr>', '<space>'], ':call vlime#ui#inspector#InspectorSelect()<cr>',
                        \ 'Activate the interactable field/button under the cursor.'],
                    \ ['n', 's', ':call vlime#ui#inspector#SendCurValueToREPL()<cr>',
                        \ 'Send the value of the field under the cursor, to the REPL.'],
                    \ ['n', 'S', ':call vlime#ui#inspector#SendCurInspecteeToREPL()<cr>',
                        \ 'Send the value being inspected to the REPL.'],
                    \ ['n', 'o', ':<c-u>call vlime#ui#inspector#FindSource("part")<cr>',
                        \ 'Open the source code for the value of the field under the cursor.'],
                    \ ['n', 'O', ':<c-u>call vlime#ui#inspector#FindSource("inspectee")<cr>',
                        \ 'Open the source code for the value being inspected.'],
                    \ ['n', ['<tab>', '<c-n>'], ':call vlime#ui#inspector#NextField(v:true)<cr>',
                        \ 'Select the next interactable field/button.'],
                    \ ['n', '<c-p>', ':call vlime#ui#inspector#NextField(v:false)<cr>',
                        \ 'Select the previous interactable field/button.'],
                    \ ['n', 'p', ':call vlime#ui#inspector#InspectorPop()<cr>',
                        \ 'Return to the previous inspected object.'],
                    \ ['n', 'P', ':call vlime#ui#inspector#InspectorNext()<cr>',
                        \ 'Move to the next inspected object.'],
                    \ ['n', 'R', ':call b:vlime_conn.InspectorReinspect({c, r -> c.ui.OnInspect(c, r, v:null, v:null)})<cr>',
                        \ 'Refresh the inspector.'],
                \ ],
                \
                \ 'trace': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("trace")<cr>',
                        \ 'Show this quick reference.'],
                    \
                    \ ['n', ['<cr>', '<space>'], ':call vlime#ui#trace_dialog#Select()<cr>',
                        \ 'Activate the interactable field/button under the cursor.'],
                    \ ['n', 'i', ':call vlime#ui#trace_dialog#Select("inspect")<cr>',
                        \ 'Inspect the value of the field under the cursor.'],
                    \ ['n', 's', ':call vlime#ui#trace_dialog#Select("to_repl")<cr>',
                        \ 'Send the value of the field under the cursor to the REPL.'],
                    \ ['n', 'R', ':call vlime#plugin#OpenTraceDialog()<cr>',
                        \ 'Refresh the trace dialog.'],
                    \ ['n', ['<tab>', '<c-n>'], ':call vlime#ui#trace_dialog#NextField(v:true)<cr>',
                        \ 'Select the next interactable field/button.'],
                    \ ['n', '<c-p>', ':call vlime#ui#trace_dialog#NextField(v:false)<cr>',
                        \ 'Select the previous interactable field/button.'],
                \ ],
                \
                \ 'xref': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("xref")<cr>',
                        \ 'Show this quick reference.'],
                    \
                    \ ['n', '<cr>', ':<c-u>call vlime#ui#xref#OpenCurXref()<cr>',
                        \ 'Open the selected source location.'],
                    \ ['n', 't', ':<c-u>call vlime#ui#xref#OpenCurXref(v:true, "tabedit")<cr>',
                        \ 'Open the selected source location, in a separate tab.'],
                    \ ['n', 's', ':<c-u>call vlime#ui#xref#OpenCurXref(v:true, "split")<cr>',
                        \ 'Open the selected source location, in a horizontal split window.'],
                    \ ['n', 'S', ':<c-u>call vlime#ui#xref#OpenCurXref(v:true, "vsplit")<cr>',
                        \ 'Open the selected source location, in a vertical split window.'],
                \ ],
                \
                \ 'notes': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("notes")<cr>',
                        \ 'Show this quick reference.'],
                    \
                    \ ['n', '<cr>', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote()<cr>',
                        \ 'Open the selected source location.'],
                    \ ['n', 't', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote("tabedit")<cr>',
                        \ 'Open the selected source location, in a separate tab.'],
                    \ ['n', 's', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote("split")<cr>',
                        \ 'Open the selected source location, in a horizontal split window.'],
                    \ ['n', 'S', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote("vsplit")<cr>',
                        \ 'Open the selected source location, in a vertical split window.'],
                \ ],
                \
                \ 'threads': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("threads")<cr>',
                        \ 'Show this quick reference.'],
                    \
                    \ ['n', '<c-c>', ':call vlime#ui#threads#InterruptCurThread()<cr>',
                        \ 'Interrupt the selected thread.'],
                    \ ['n', 'K', ':call vlime#ui#threads#KillCurThread()<cr>',
                        \ 'Kill the selected thread.'],
                    \ ['n', 'D', ':call vlime#ui#threads#DebugCurThread()<cr>',
                        \ 'Invoke the debugger in the selected thread.'],
                    \ ['n', 'r', ':call vlime#ui#threads#Refresh()<cr>',
                        \ 'Refresh the thread list.'],
                \ ],
                \
                \ 'server': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("server")<cr>',
                        \ 'Show this quick reference.'],
                    \
                    \ ['n', '<LocalLeader>c', ':call vlime#server#ConnectToCurServer()<cr>',
                        \ 'Connect to this server.'],
                    \ ['n', '<LocalLeader>s', ':call vlime#server#StopCurServer()<cr>',
                        \ 'Stop this server.'],
                \ ],
                \
                \ 'input': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("input")<cr>',
                        \ 'Show this quick reference.'],
                    \
                    \ ['n', '<c-p>', ':call vlime#ui#input#NextHistoryItem("backward")<cr>',
                        \ 'Show the previous item in input history.'],
                    \ ['n', '<c-n>', ':call vlime#ui#input#NextHistoryItem("forward")<cr>',
                        \ 'Show the next item in input history.'],
                \ ],
            \ }
endif


function! vlime#overlay#slimv#Init()
endfunction


function! s:DialogUntraceAllComplete(conn, result)
    if type(a:result) != type(v:null)
        for r in a:result
            echom r
        endfor
    endif
endfunction
