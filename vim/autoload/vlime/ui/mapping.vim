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
                    \ ['i', '<space>', '<space><c-r>=vlime#plugin#VlimeKey("space")<cr>',
                        \ 'Trigger the arglist hint.'],
                    \ ['i', '<cr>', '<cr><c-r>=vlime#plugin#VlimeKey("cr")<cr>',
                        \ 'Trigger the arglist hint.'],
                    \ ['i', '<tab>', '<c-r>=vlime#plugin#VlimeKey("tab")<cr>',
                        \ 'Trigger omni-completion.'],
                    \
                    \ ['n', '<LocalLeader>cc', ':call vlime#plugin#ConnectREPL()<cr>',
                        \ 'Connect to a server.'],
                    \ ['n', '<LocalLeader>cs', ':call vlime#plugin#SelectCurConnection()<cr>',
                        \ 'Switch connections.'],
                    \ ['n', '<LocalLeader>cd', ':call vlime#plugin#CloseCurConnection()<cr>',
                        \ 'Disconnect.'],
                    \ ['n', '<LocalLeader>cR', ':call vlime#plugin#RenameCurConnection()<cr>',
                        \ 'Rename the current connection.'],
                    \
                    \ ['n', '<LocalLeader>rr', ':call vlime#server#New()<cr>',
                        \ 'Run a new server and connect to it.'],
                    \ ['n', '<LocalLeader>rv', ':call vlime#plugin#ShowSelectedServer()<cr>',
                        \ 'View the console output of a server.'],
                    \ ['n', '<LocalLeader>rs', ':call vlime#plugin#StopSelectedServer()<cr>',
                        \ 'Stop a server.'],
                    \ ['n', '<LocalLeader>rR', ':call vlime#plugin#RenameSelectedServer()<cr>',
                        \ 'Rename a server.'],
                    \
                    \ ['n', '<LocalLeader>ss', ':call vlime#plugin#SendToREPL(vlime#ui#CurExprOrAtom())<cr>',
                        \ 'Send the expression/atom under the cursor to the REPL.'],
                    \ ['n', '<LocalLeader>se', ':call vlime#plugin#SendToREPL(vlime#ui#CurExpr())<cr>',
                        \ 'Send the expression under the cursor to the REPL.'],
                    \ ['n', '<LocalLeader>st', ':call vlime#plugin#SendToREPL(vlime#ui#CurTopExpr())<cr>',
                        \ 'Send the top-level expression under the cursor to the REPL.'],
                    \ ['n', '<LocalLeader>sa', ':call vlime#plugin#SendToREPL(vlime#ui#CurAtom())<cr>',
                        \ 'Send the atom under the cursor to the REPL.'],
                    \ ['n', '<LocalLeader>si', ':call vlime#plugin#SendToREPL()<cr>',
                        \ 'Send a snippet to the REPL.'],
                    \ ['v', '<LocalLeader>s', ':<c-u>call vlime#plugin#SendToREPL(vlime#ui#CurSelection())<cr>',
                        \ 'Send the current selection to the REPL.'],
                    \
                    \ ['n', '<LocalLeader>mm', ':call vlime#plugin#ExpandMacro(vlime#ui#CurExpr(), "expand")<cr>',
                        \ 'Expand the macro under the cursor.'],
                    \ ['n', '<LocalLeader>m1', ':call vlime#plugin#ExpandMacro(vlime#ui#CurExpr(), "one")<cr>',
                        \ 'Expand the macro under the cursor once.'],
                    \ ['n', '<LocalLeader>ma', ':call vlime#plugin#ExpandMacro(vlime#ui#CurExpr(), "all")<cr>',
                        \ 'Expand the macro under the cursor and all nested macros.'],
                    \
                    \ ['n', '<LocalLeader>oe', ':call vlime#plugin#Compile(vlime#ui#CurExpr(v:true))<cr>',
                        \ 'Compile the expression under the cursor.'],
                    \ ['n', '<LocalLeader>ot', ':call vlime#plugin#Compile(vlime#ui#CurTopExpr(v:true))<cr>',
                        \ 'Compile the top-level expression under the cursor.'],
                    \ ['n', '<LocalLeader>of', ':call vlime#plugin#CompileFile(expand("%:p"))<cr>',
                        \ 'Compile the current file.'],
                    \ ['v', '<LocalLeader>o', ':<c-u>call vlime#plugin#Compile(vlime#ui#CurSelection(v:true))<cr>',
                        \ 'Compile the current selection.'],
                    \
                    \ ['n', '<LocalLeader>xc', ':call vlime#plugin#XRefSymbol("CALLS", vlime#ui#CurAtom())<cr>',
                        \ 'Show callers of the function under the cursor.'],
                    \ ['n', '<LocalLeader>xC', ':call vlime#plugin#XRefSymbol("CALLS-WHO", vlime#ui#CurAtom())<cr>',
                        \ 'Show callees of the function under the cursor.'],
                    \ ['n', '<LocalLeader>xr', ':call vlime#plugin#XRefSymbol("REFERENCES", vlime#ui#CurAtom())<cr>',
                        \ 'Show references to the variable under the cursor.'],
                    \ ['n', '<LocalLeader>xb', ':call vlime#plugin#XRefSymbol("BINDS", vlime#ui#CurAtom())<cr>',
                        \ 'Show bindings for the variable under the cursor.'],
                    \ ['n', '<LocalLeader>xs', ':call vlime#plugin#XRefSymbol("SETS", vlime#ui#CurAtom())<cr>',
                        \ 'Show locations where the variable under the cursor is set.'],
                    \ ['n', '<LocalLeader>xe', ':call vlime#plugin#XRefSymbol("MACROEXPANDS", vlime#ui#CurAtom())<cr>',
                        \ 'Show locations where the macro under the cursor is called.'],
                    \ ['n', '<LocalLeader>xm', ':call vlime#plugin#XRefSymbol("SPECIALIZES", vlime#ui#CurAtom())<cr>',
                        \ 'Show specialized methods for the class under the cursor.'],
                    \ ['n', '<LocalLeader>xd', ':call vlime#plugin#FindDefinition(vlime#ui#CurAtom())<cr>',
                        \ 'Show the definition for the name under the cursor.'],
                    \ ['n', '<LocalLeader>xi', ':<c-u>call vlime#plugin#XRefSymbolWrapper()<cr>',
                        \ 'Interactively prompt for the symbol to search for cross references.'],
                    \
                    \ ['n', '<LocalLeader>do', ':call vlime#plugin#DescribeSymbol(vlime#ui#CurOperator())<cr>',
                        \ 'Describe the operator of the expression under the cursor.'],
                    \ ['n', '<LocalLeader>da', ':call vlime#plugin#DescribeSymbol(vlime#ui#CurAtom())<cr>',
                        \ 'Describe the atom under the cursor.'],
                    \ ['n', '<LocalLeader>di', ':call vlime#plugin#DescribeSymbol()<cr>',
                        \ 'Prompt for the symbol to describe.'],
                    \ ['n', '<LocalLeader>ds', ':call vlime#plugin#AproposList()<cr>',
                        \ 'Apropos search.'],
                    \ ['n', '<LocalLeader>ddo', ':call vlime#plugin#DocumentationSymbol(vlime#ui#CurOperator())<cr>',
                        \ 'Show the documentation for the operator of the expression under the cursor.'],
                    \ ['n', '<LocalLeader>dda', ':call vlime#plugin#DocumentationSymbol(vlime#ui#CurAtom())<cr>',
                        \ 'Show the documentation for the atom under the cursor.'],
                    \ ['n', '<LocalLeader>ddi', ':call vlime#plugin#DocumentationSymbol()<cr>',
                        \ 'Prompt for a symbol, and show its documentation.'],
                    \ ['n', '<LocalLeader>dr', ':call vlime#plugin#ShowOperatorArgList(vlime#ui#CurOperator())<cr>',
                        \ 'Show the arglist for the expresison under the cursor.'],
                    \
                    \ ['n', ['<LocalLeader>II', '<LocalLeader>Ii'], ':call vlime#plugin#Inspect(vlime#ui#CurExprOrAtom())<cr>',
                        \ 'Evaluate the expression/atom under the cursor, and inspect the result.'],
                    \ ['n', ['<LocalLeader>IE', '<LocalLeader>Ie'], ':call vlime#plugin#Inspect(vlime#ui#CurExpr())<cr>',
                        \ 'Evaluate the expression under the cursor, and inspect the result.'],
                    \ ['n', ['<LocalLeader>IT', '<LocalLeader>It'], ':call vlime#plugin#Inspect(vlime#ui#CurTopExpr())<cr>',
                        \ 'Evaluate the top-level expression under the cursor, and inspect the result.'],
                    \ ['n', ['<LocalLeader>IA', '<LocalLeader>Ia'], ':call vlime#plugin#Inspect(vlime#ui#CurAtom())<cr>',
                        \ 'Evaluate the atom under the cursor, and inspect the result.'],
                    \ ['n', ['<LocalLeader>IN', '<LocalLeader>In'], ':call vlime#plugin#Inspect()<cr>',
                        \ 'Evaluate a snippet, and inspect the result.'],
                    \ ['v', '<LocalLeader>I', ':<c-u>call vlime#plugin#Inspect(vlime#ui#CurSelection())<cr>',
                        \ 'Evaluate the current selection, and inspect the result.'],
                    \
                    \ ['n', ['<LocalLeader>TD', '<LocalLeader>Td'], ':call vlime#plugin#OpenTraceDialog()<cr>',
                        \ 'Show the trace dialog.'],
                    \ ['n', ['<LocalLeader>TI', '<LocalLeader>Ti'], ':call vlime#plugin#DialogToggleTrace()<cr>',
                        \ 'Prompt for a function name to trace/untrace.'],
                    \ ['n', ['<LocalLeader>TT', '<LocalLeader>Tt'], ':call vlime#plugin#DialogToggleTrace(vlime#ui#CurAtom())<cr>',
                        \ 'Trace/untrace the function under the cursor.'],
                    \
                    \ ['n', '<LocalLeader>uf', ':call vlime#plugin#UndefineFunction(vlime#ui#CurAtom())<cr>',
                        \ 'Undefine the function under the cursor.'],
                    \ ['n', '<LocalLeader>us', ':call vlime#plugin#UninternSymbol(vlime#ui#CurAtom())<cr>',
                        \ 'Unintern the symbol under the cursor.'],
                    \ ['n', '<LocalLeader>ui', ':<c-u>call vlime#plugin#UndefineUninternWrapper()<cr>',
                        \ 'Interactively prompt for the function/symbol to undefine/unintern.'],
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
                    \
                    \ ['n', '<LocalLeader>i', ':call vlime#plugin#InteractionMode()<cr>',
                        \ 'Interaction mode.'],
                    \ ['n', '<LocalLeader>l', ':call vlime#plugin#LoadFile(expand("%:p"))<cr>',
                        \ 'Load the current file.'],
                    \ ['n', '<LocalLeader>a', ':call vlime#plugin#DisassembleForm(vlime#ui#CurExpr())<cr>',
                        \ 'Disassemble the form under the cursor.'],
                    \ ['n', '<LocalLeader>p', ':call vlime#plugin#SetPackage()<cr>',
                        \ 'Specify the package for the current buffer.'],
                    \ ['n', '<LocalLeader>b', ':call vlime#plugin#SetBreakpoint()<cr>',
                        \ 'Set a breakpoint at entry to a function.'],
                    \ ['n', '<LocalLeader>t', ':call vlime#plugin#ListThreads()<cr>',
                        \ 'Show a list of the running threads.'],
                \ ],
                \
                \ 'sldb': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("sldb")<cr>', 'Show this quick reference.'],
                    \
                    \ ['n', '<cr>', ':call vlime#ui#sldb#ChooseCurRestart()<cr>', ''],
                    \ ['n', 'd', ':call vlime#ui#sldb#ShowFrameDetails()<cr>', ''],
                    \ ['n', 'S', ':<c-u>call vlime#ui#sldb#OpenFrameSource()<cr>', ''],
                    \ ['n', 'T', ':<c-u>call vlime#ui#sldb#OpenFrameSource("tabedit")<cr>', ''],
                    \ ['n', 'O', ':<c-u>call vlime#ui#sldb#FindSource()<cr>', ''],
                    \ ['n', 'r', ':call vlime#ui#sldb#RestartCurFrame()<cr>', ''],
                    \ ['n', 's', ':call vlime#ui#sldb#StepCurOrLastFrame("step")<cr>', ''],
                    \ ['n', 'x', ':call vlime#ui#sldb#StepCurOrLastFrame("next")<cr>', ''],
                    \ ['n', 'o', ':call vlime#ui#sldb#StepCurOrLastFrame("out")<cr>', ''],
                    \ ['n', 'c', ':call b:vlime_conn.SLDBContinue()<cr>', ''],
                    \ ['n', 'a', ':call b:vlime_conn.SLDBAbort()<cr>', ''],
                    \ ['n', 'C', ':call vlime#ui#sldb#InspectCurCondition()<cr>', ''],
                    \ ['n', 'i', ':call vlime#ui#sldb#InspectInCurFrame()<cr>', ''],
                    \ ['n', 'e', ':call vlime#ui#sldb#EvalStringInCurFrame()<cr>', ''],
                    \ ['n', 'E', ':call vlime#ui#sldb#SendValueInCurFrameToREPL()<cr>', ''],
                    \ ['n', 'D', ':call vlime#ui#sldb#DisassembleCurFrame()<cr>', ''],
                    \ ['n', 'R', ':call vlime#ui#sldb#ReturnFromCurFrame()<cr>', ''],
                \ ],
                \
                \ 'repl': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("repl")<cr>', 'Show this quick reference.'],
                    \
                    \ ['n', '<c-c>', ':call b:vlime_conn.Interrupt({"name": "REPL-THREAD", "package": "KEYWORD"})<cr>', ''],
                    \ ['n', '<LocalLeader>I', ':call vlime#ui#repl#InspectCurREPLPresentation()<cr>', ''],
                    \ ['n', '<LocalLeader>y', ':call vlime#ui#repl#YankCurREPLPresentation()<cr>', ''],
                    \ ['n', '<LocalLeader>C', ':call vlime#ui#repl#ClearREPLBuffer()<cr>', ''],
                    \ ['n', ['<c-n>', '<tab>'], ':call vlime#ui#repl#NextField(v:true)<cr>', ''],
                    \ ['n', '<c-p>', ':call vlime#ui#repl#NextField(v:false)<cr>', ''],
                \ ],
                \ 'mrepl': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("mrepl")<cr>', 'Show this quick reference.'],
                    \
                    \ ['i', '<space>', '<space><c-r>=vlime#plugin#VlimeKey("space")<cr>', ''],
                    \ ['i', '<cr>', '<c-r>=vlime#ui#mrepl#Submit()<cr>', ''],
                    \ ['i', '<c-j>', '<cr><c-r>=vlime#plugin#VlimeKey("cr")<cr>', ''],
                    \ ['i', '<tab>', '<c-r>=vlime#plugin#VlimeKey("tab")<cr>', ''],
                    \ ['i', '<c-c>', '<c-r>=vlime#ui#mrepl#Interrupt()<cr>', ''],
                    \ ['n', '<LocalLeader>C', ':call vlime#ui#mrepl#Clear()<cr>', ''],
                    \ ['n', '<LocalLeader>D', ':call vlime#ui#mrepl#Disconnect()<cr>', ''],
                \ ],
                \
                \ 'inspector': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("inspector")<cr>', 'Show this quick reference.'],
                    \
                    \ ['n', ['<cr>', '<space>'], ':call vlime#ui#inspector#InspectorSelect()<cr>', ''],
                    \ ['n', 's', ':call vlime#ui#inspector#SendCurValueToREPL()<cr>', ''],
                    \ ['n', 'S', ':call vlime#ui#inspector#SendCurInspecteeToREPL()<cr>', ''],
                    \ ['n', 'o', ':<c-u>call vlime#ui#inspector#FindSource("part")<cr>', ''],
                    \ ['n', 'O', ':<c-u>call vlime#ui#inspector#FindSource("inspectee")<cr>', ''],
                    \ ['n', ['<c-n>', '<tab>'], ':call vlime#ui#inspector#NextField(v:true)<cr>', ''],
                    \ ['n', '<c-p>', ':call vlime#ui#inspector#NextField(v:false)<cr>', ''],
                    \ ['n', 'p', ':call vlime#ui#inspector#InspectorPop()<cr>', ''],
                    \ ['n', 'P', ':call vlime#ui#inspector#InspectorNext()<cr>', ''],
                    \ ['n', 'R', ':call b:vlime_conn.InspectorReinspect({c, r -> c.ui.OnInspect(c, r, v:null, v:null)})<cr>', ''],
                \ ],
                \
                \ 'trace': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("trace")<cr>', 'Show this quick reference.'],
                    \
                    \ ['n', ['<cr>', '<space>'], ':call vlime#ui#trace_dialog#Select()<cr>', ''],
                    \ ['n', 'i', ':call vlime#ui#trace_dialog#Select("inspect")<cr>', ''],
                    \ ['n', 's', ':call vlime#ui#trace_dialog#Select("to_repl")<cr>', ''],
                    \ ['n', 'R', ':call vlime#plugin#OpenTraceDialog()<cr>', ''],
                    \ ['n', ['<c-n>', '<tab>'], ':call vlime#ui#trace_dialog#NextField(v:true)<cr>', ''],
                    \ ['n', '<c-p>', ':call vlime#ui#trace_dialog#NextField(v:false)<cr>', ''],
                \ ],
                \
                \ 'xref': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("xref")<cr>', 'Show this quick reference.'],
                    \
                    \ ['n', '<cr>', ':<c-u>call vlime#ui#xref#OpenCurXref()<cr>', ''],
                    \ ['n', 't', ':<c-u>call vlime#ui#xref#OpenCurXref(v:true, "tabedit")<cr>', ''],
                    \ ['n', 's', ':<c-u>call vlime#ui#xref#OpenCurXref(v:true, "split")<cr>', ''],
                    \ ['n', 'S', ':<c-u>call vlime#ui#xref#OpenCurXref(v:true, "vsplit")<cr>', ''],
                \ ],
                \
                \ 'notes': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("notes")<cr>', 'Show this quick reference.'],
                    \
                    \ ['n', '<cr>', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote()<cr>', ''],
                    \ ['n', 't', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote("tabedit")<cr>', ''],
                    \ ['n', 's', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote("split")<cr>', ''],
                    \ ['n', 'S', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote("vsplit")<cr>', ''],
                \ ],
                \
                \ 'threads': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("threads")<cr>', 'Show this quick reference.'],
                    \
                    \ ['n', '<c-c>', ':call vlime#ui#threads#InterruptCurThread()<cr>', ''],
                    \ ['n', 'K', ':call vlime#ui#threads#KillCurThread()<cr>', ''],
                    \ ['n', 'D', ':call vlime#ui#threads#DebugCurThread()<cr>', ''],
                    \ ['n', 'r', ':call vlime#ui#threads#Refresh()<cr>', ''],
                \ ],
                \
                \ 'server': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("server")<cr>', 'Show this quick reference.'],
                    \
                    \ ['n', '<LocalLeader>c', ':call vlime#server#ConnectToCurServer()<cr>', ''],
                    \ ['n', '<LocalLeader>s', ':call vlime#server#StopCurServer()<cr>', ''],
                \ ],
                \
                \ 'input': [
                    \ ['n', '<LocalLeader>?', ':call vlime#ui#ShowQuickRef("input")<cr>', 'Show this quick reference.'],
                    \
                    \ ['n', '<c-p>', ':call vlime#ui#input#NextHistoryItem("backward")<cr>', ''],
                    \ ['n', '<c-n>', ':call vlime#ui#input#NextHistoryItem("forward")<cr>', ''],
                \ ],
            \ }
endif


function! vlime#ui#mapping#GetBufferMappings(buf_type)
    return g:vlime_default_mappings[a:buf_type]
endfunction
