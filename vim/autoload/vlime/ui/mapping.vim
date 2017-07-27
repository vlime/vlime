if !exists('g:vlime_default_mappings')
    " buf_type: [
    "   [mode, key, command],
    "   ...
    " ]
    let g:vlime_default_mappings = {
                \ 'lisp': [
                    \ ['i', '<space>', '<space><c-r>=vlime#plugin#VlimeKey("space")<cr>'],
                    \ ['i', '<cr>', '<cr><c-r>=vlime#plugin#VlimeKey("cr")<cr>'],
                    \ ['i', '<tab>', '<c-r>=vlime#plugin#VlimeKey("tab")<cr>'],
                    \
                    \ ['n', '<LocalLeader>cc', ':call vlime#plugin#ConnectREPL()<cr>'],
                    \ ['n', '<LocalLeader>cs', ':call vlime#plugin#SelectCurConnection()<cr>'],
                    \ ['n', '<LocalLeader>cd', ':call vlime#plugin#CloseCurConnection()<cr>'],
                    \ ['n', '<LocalLeader>cR', ':call vlime#plugin#RenameCurConnection()<cr>'],
                    \
                    \ ['n', '<LocalLeader>rr', ':call vlime#server#New()<cr>'],
                    \ ['n', '<LocalLeader>rv', ':call vlime#plugin#ShowSelectedServer()<cr>'],
                    \ ['n', '<LocalLeader>rs', ':call vlime#plugin#StopSelectedServer()<cr>'],
                    \ ['n', '<LocalLeader>rR', ':call vlime#plugin#RenameSelectedServer()<cr>'],
                    \
                    \ ['n', '<LocalLeader>ss', ':call vlime#plugin#SendToREPL(vlime#ui#CurExprOrAtom())<cr>'],
                    \ ['n', '<LocalLeader>se', ':call vlime#plugin#SendToREPL(vlime#ui#CurExpr())<cr>'],
                    \ ['n', '<LocalLeader>st', ':call vlime#plugin#SendToREPL(vlime#ui#CurTopExpr())<cr>'],
                    \ ['n', '<LocalLeader>sa', ':call vlime#plugin#SendToREPL(vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>si', ':call vlime#plugin#SendToREPL()<cr>'],
                    \ ['v', '<LocalLeader>s', ':<c-u>call vlime#plugin#SendToREPL(vlime#ui#CurSelection())<cr>'],
                    \
                    \ ['n', '<LocalLeader>mm', ':call vlime#plugin#ExpandMacro(vlime#ui#CurExpr(), "expand")<cr>'],
                    \ ['n', '<LocalLeader>m1', ':call vlime#plugin#ExpandMacro(vlime#ui#CurExpr(), "one")<cr>'],
                    \ ['n', '<LocalLeader>ma', ':call vlime#plugin#ExpandMacro(vlime#ui#CurExpr(), "all")<cr>'],
                    \
                    \ ['n', '<LocalLeader>oe', ':call vlime#plugin#Compile(vlime#ui#CurExpr(v:true))<cr>'],
                    \ ['n', '<LocalLeader>ot', ':call vlime#plugin#Compile(vlime#ui#CurTopExpr(v:true))<cr>'],
                    \ ['n', '<LocalLeader>of', ':call vlime#plugin#CompileFile(expand("%:p"))<cr>'],
                    \ ['v', '<LocalLeader>o', ':<c-u>call vlime#plugin#Compile(vlime#ui#CurSelection(v:true))<cr>'],
                    \
                    \ ['n', '<LocalLeader>xc', ':call vlime#plugin#XRefSymbol("CALLS", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xC', ':call vlime#plugin#XRefSymbol("CALLS-WHO", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xr', ':call vlime#plugin#XRefSymbol("REFERENCES", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xb', ':call vlime#plugin#XRefSymbol("BINDS", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xs', ':call vlime#plugin#XRefSymbol("SETS", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xe', ':call vlime#plugin#XRefSymbol("MACROEXPANDS", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xm', ':call vlime#plugin#XRefSymbol("SPECIALIZES", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xd', ':call vlime#plugin#FindDefinition(vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xi', ':<c-u>call vlime#plugin#XRefSymbolWrapper()<cr>'],
                    \
                    \ ['n', '<LocalLeader>do', ':call vlime#plugin#DescribeSymbol(vlime#ui#CurOperator())<cr>'],
                    \ ['n', '<LocalLeader>da', ':call vlime#plugin#DescribeSymbol(vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>di', ':call vlime#plugin#DescribeSymbol()<cr>'],
                    \ ['n', '<LocalLeader>ds', ':call vlime#plugin#AproposList()<cr>'],
                    \ ['n', '<LocalLeader>ddo', ':call vlime#plugin#DocumentationSymbol(vlime#ui#CurOperator())<cr>'],
                    \ ['n', '<LocalLeader>dda', ':call vlime#plugin#DocumentationSymbol(vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>ddi', ':call vlime#plugin#DocumentationSymbol()<cr>'],
                    \ ['n', '<LocalLeader>dr', ':call vlime#plugin#ShowOperatorArgList(vlime#ui#CurOperator())<cr>'],
                    \
                    \ ['n', ['<LocalLeader>II', '<LocalLeader>Ii'], ':call vlime#plugin#Inspect(vlime#ui#CurExprOrAtom())<cr>'],
                    \ ['n', ['<LocalLeader>IE', '<LocalLeader>Ie'], ':call vlime#plugin#Inspect(vlime#ui#CurExpr())<cr>'],
                    \ ['n', ['<LocalLeader>IT', '<LocalLeader>It'], ':call vlime#plugin#Inspect(vlime#ui#CurTopExpr())<cr>'],
                    \ ['n', ['<LocalLeader>IA', '<LocalLeader>Ia'], ':call vlime#plugin#Inspect(vlime#ui#CurAtom())<cr>'],
                    \ ['n', ['<LocalLeader>IN', '<LocalLeader>In'], ':call vlime#plugin#Inspect()<cr>'],
                    \ ['v', '<LocalLeader>I', ':<c-u>call vlime#plugin#Inspect(vlime#ui#CurSelection())<cr>'],
                    \
                    \ ['n', '<LocalLeader>uf', ':call vlime#plugin#UndefineFunction(vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>us', ':call vlime#plugin#UninternSymbol(vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>ui', ':<c-u>call vlime#plugin#UndefineUninternWrapper()<cr>'],
                    \
                    \ ['n', '<LocalLeader>wp', ':call vlime#plugin#CloseWindow("preview")<cr>'],
                    \ ['n', '<LocalLeader>wr', ':call vlime#plugin#CloseWindow("arglist")<cr>'],
                    \ ['n', '<LocalLeader>wn', ':call vlime#plugin#CloseWindow("notes")<cr>'],
                    \ ['n', '<LocalLeader>wR', ':call vlime#plugin#CloseWindow("repl")<cr>'],
                    \ ['n', '<LocalLeader>wA', ':call vlime#plugin#CloseWindow("")<cr>'],
                    \ ['n', '<LocalLeader>wl', ':call vlime#plugin#CloseWindow()<cr>'],
                    \
                    \ ['n', '<LocalLeader>i', ':call vlime#plugin#InteractionMode()<cr>'],
                    \ ['n', '<LocalLeader>l', ':call vlime#plugin#LoadFile(expand("%:p"))<cr>'],
                    \ ['n', '<LocalLeader>a', ':call vlime#plugin#DisassembleForm(vlime#ui#CurExpr())<cr>'],
                    \ ['n', '<LocalLeader>p', ':call vlime#plugin#SetPackage()<cr>'],
                    \ ['n', '<LocalLeader>b', ':call vlime#plugin#SetBreakpoint()<cr>'],
                    \ ['n', '<LocalLeader>t', ':call vlime#plugin#ListThreads()<cr>'],
                \ ],
                \
                \ 'sldb': [
                    \ ['n', '<cr>', ':call vlime#ui#sldb#ChooseCurRestart()<cr>'],
                    \ ['n', 'd', ':call vlime#ui#sldb#ShowFrameDetails()<cr>'],
                    \ ['n', 'S', ':<c-u>call vlime#ui#sldb#OpenFrameSource()<cr>'],
                    \ ['n', 'T', ':<c-u>call vlime#ui#sldb#OpenFrameSource("tabedit")<cr>'],
                    \ ['n', 'O', ':<c-u>call vlime#ui#sldb#FindSource()<cr>'],
                    \ ['n', 'r', ':call vlime#ui#sldb#RestartCurFrame()<cr>'],
                    \ ['n', 's', ':call vlime#ui#sldb#StepCurOrLastFrame("step")<cr>'],
                    \ ['n', 'x', ':call vlime#ui#sldb#StepCurOrLastFrame("next")<cr>'],
                    \ ['n', 'o', ':call vlime#ui#sldb#StepCurOrLastFrame("out")<cr>'],
                    \ ['n', 'c', ':call b:vlime_conn.SLDBContinue()<cr>'],
                    \ ['n', 'a', ':call b:vlime_conn.SLDBAbort()<cr>'],
                    \ ['n', 'C', ':call vlime#ui#sldb#InspectCurCondition()<cr>'],
                    \ ['n', 'i', ':call vlime#ui#sldb#InspectInCurFrame()<cr>'],
                    \ ['n', 'e', ':call vlime#ui#sldb#EvalStringInCurFrame()<cr>'],
                    \ ['n', 'E', ':call vlime#ui#sldb#SendValueInCurFrameToREPL()<cr>'],
                    \ ['n', 'D', ':call vlime#ui#sldb#DisassembleCurFrame()<cr>'],
                    \ ['n', 'R', ':call vlime#ui#sldb#ReturnFromCurFrame()<cr>'],
                \ ],
                \
                \ 'repl': [
                    \ ['n', '<c-c>', ':call b:vlime_conn.Interrupt({"name": "REPL-THREAD", "package": "KEYWORD"})<cr>'],
                    \ ['n', '<LocalLeader>I', ':call vlime#ui#repl#InspectCurREPLPresentation()<cr>'],
                    \ ['n', '<LocalLeader>y', ':call vlime#ui#repl#YankCurREPLPresentation()<cr>'],
                    \ ['n', '<LocalLeader>C', ':call vlime#ui#repl#ClearREPLBuffer()<cr>'],
                \ ],
                \
                \ 'inspector': [
                    \ ['n', ['<cr>', '<space>'], ':call vlime#ui#inspector#InspectorSelect()<cr>'],
                    \ ['n', 's', ':call vlime#ui#inspector#SendCurValueToREPL()<cr>'],
                    \ ['n', 'S', ':call vlime#ui#inspector#SendCurInspecteeToREPL()<cr>'],
                    \ ['n', 'o', ':<c-u>call vlime#ui#inspector#FindSource("part")<cr>'],
                    \ ['n', 'O', ':<c-u>call vlime#ui#inspector#FindSource("inspectee")<cr>'],
                    \ ['n', ['<c-n>', '<tab>'], ':call vlime#ui#inspector#NextField(v:true)<cr>'],
                    \ ['n', '<c-p>', ':call vlime#ui#inspector#NextField(v:false)<cr>'],
                    \ ['n', 'p', ':call vlime#ui#inspector#InspectorPop()<cr>'],
                    \ ['n', 'P', ':call vlime#ui#inspector#InspectorNext()<cr>'],
                    \ ['n', 'R', ':call b:vlime_conn.InspectorReinspect({c, r -> c.ui.OnInspect(c, r, v:null, v:null)})<cr>'],
                \ ],
                \
                \ 'trace': [
                    \ ['n', ['<cr>', '<space>'], ':call vlime#ui#trace_dialog#Select()<cr>'],
                \ ],
                \
                \ 'xref': [
                    \ ['n', '<cr>', ':<c-u>call vlime#ui#xref#OpenCurXref()<cr>'],
                    \ ['n', 't', ':<c-u>call vlime#ui#xref#OpenCurXref(v:true, "tabedit")<cr>'],
                    \ ['n', 's', ':<c-u>call vlime#ui#xref#OpenCurXref(v:true, "split")<cr>'],
                    \ ['n', 'S', ':<c-u>call vlime#ui#xref#OpenCurXref(v:true, "vsplit")<cr>'],
                \ ],
                \
                \ 'notes': [
                    \ ['n', '<cr>', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote()<cr>'],
                    \ ['n', 't', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote("tabedit")<cr>'],
                    \ ['n', 's', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote("split")<cr>'],
                    \ ['n', 'S', ':<c-u>call vlime#ui#compiler_notes#OpenCurNote("vsplit")<cr>'],
                \ ],
                \
                \ 'threads': [
                    \ ['n', '<c-c>', ':call vlime#ui#threads#InterruptCurThread()<cr>'],
                    \ ['n', 'K', ':call vlime#ui#threads#KillCurThread()<cr>'],
                    \ ['n', 'D', ':call vlime#ui#threads#DebugCurThread()<cr>'],
                    \ ['n', 'r', ':call vlime#ui#threads#Refresh()<cr>'],
                \ ],
                \
                \ 'server': [
                    \ ['n', '<LocalLeader>c', ':call vlime#server#ConnectToCurServer()<cr>'],
                    \ ['n', '<LocalLeader>s', ':call vlime#server#StopCurServer()<cr>'],
                \ ],
                \
                \ 'input': [
                    \ ['n', '<c-p>', ':call vlime#ui#input#NextHistoryItem("backward")<cr>'],
                    \ ['n', '<c-n>', ':call vlime#ui#input#NextHistoryItem("forward")<cr>'],
                \ ],
            \ }
endif


function! vlime#ui#mapping#GetBufferMappings(buf_type)
    return g:vlime_default_mappings[a:buf_type]
endfunction
