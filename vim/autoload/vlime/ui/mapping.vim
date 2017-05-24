if !exists('g:vlime_default_mappings')
    " buf_type: [
    "   [mode, key, command],
    "   ...
    " ]
    let g:vlime_default_mappings = {
                \ 'lisp': [
                    \ ['i', '<space>', '<space><c-r>=VlimeKey("space")<cr>'],
                    \ ['i', '<cr>', '<cr><c-r>=VlimeKey("cr")<cr>'],
                    \ ['i', '<tab>', '<c-r>=VlimeKey("tab")<cr>'],
                    \
                    \ ['n', '<LocalLeader>cc', ':call VlimeConnectREPL()<cr>'],
                    \ ['n', '<LocalLeader>cs', ':call VlimeSelectCurConnection()<cr>'],
                    \ ['n', '<LocalLeader>cd', ':call VlimeCloseCurConnection()<cr>'],
                    \ ['n', '<LocalLeader>cR', ':call VlimeRenameCurConnection()<cr>'],
                    \
                    \ ['n', '<LocalLeader>rr', ':call VlimeNewServer()<cr>'],
                    \ ['n', '<LocalLeader>rv', ':call VlimeShowSelectedServer()<cr>'],
                    \ ['n', '<LocalLeader>rs', ':call VlimeStopSelectedServer()<cr>'],
                    \ ['n', '<LocalLeader>rR', ':call VlimeRenameSelectedServer()<cr>'],
                    \
                    \ ['n', '<LocalLeader>ss', ':call VlimeSendToREPL(vlime#ui#CurExprOrAtom())<cr>'],
                    \ ['n', '<LocalLeader>se', ':call VlimeSendToREPL(vlime#ui#CurExpr())<cr>'],
                    \ ['n', '<LocalLeader>st', ':call VlimeSendToREPL(vlime#ui#CurTopExpr())<cr>'],
                    \ ['n', '<LocalLeader>sa', ':call VlimeSendToREPL(vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>si', ':call VlimeSendToREPL()<cr>'],
                    \ ['v', '<LocalLeader>s', ':<c-u>call VlimeSendToREPL(vlime#ui#CurSelection())<cr>'],
                    \
                    \ ['n', '<LocalLeader>m1', ':call VlimeExpandMacro(vlime#ui#CurExpr(), v:false)<cr>'],
                    \ ['n', '<LocalLeader>ma', ':call VlimeExpandMacro(vlime#ui#CurExpr(), v:true)<cr>'],
                    \
                    \ ['n', '<LocalLeader>oe', ':call VlimeCompile(vlime#ui#CurExpr(v:true))<cr>'],
                    \ ['n', '<LocalLeader>ot', ':call VlimeCompile(vlime#ui#CurTopExpr(v:true))<cr>'],
                    \ ['n', '<LocalLeader>of', ':call VlimeCompileFile(expand("%:p"))<cr>'],
                    \ ['v', '<LocalLeader>o', ':<c-u>call VlimeCompile(vlime#ui#CurSelection(v:true))<cr>'],
                    \
                    \ ['n', '<LocalLeader>xc', ':call VlimeXRefSymbol("CALLS", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xC', ':call VlimeXRefSymbol("CALLS-WHO", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xr', ':call VlimeXRefSymbol("REFERENCES", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xb', ':call VlimeXRefSymbol("BINDS", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xs', ':call VlimeXRefSymbol("SETS", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xe', ':call VlimeXRefSymbol("MACROEXPANDS", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xm', ':call VlimeXRefSymbol("SPECIALIZES", vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xd', ':call VlimeFindDefinition(vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>xi', ':<c-u>call VlimeXRefSymbolWrapper()<cr>'],
                    \
                    \ ['n', '<LocalLeader>do', ':call VlimeDescribeSymbol(vlime#ui#CurOperator())<cr>'],
                    \ ['n', '<LocalLeader>da', ':call VlimeDescribeSymbol(vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>di', ':call VlimeDescribeSymbol()<cr>'],
                    \ ['n', '<LocalLeader>ds', ':call VlimeAproposList()<cr>'],
                    \ ['n', '<LocalLeader>ddo', ':call VlimeDocumentationSymbol(vlime#ui#CurOperator())<cr>'],
                    \ ['n', '<LocalLeader>dda', ':call VlimeDocumentationSymbol(vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>ddi', ':call VlimeDocumentationSymbol()<cr>'],
                    \ ['n', '<LocalLeader>dr', ':call VlimeShowOperatorArgList(vlime#ui#CurOperator())<cr>'],
                    \
                    \ ['n', ['<LocalLeader>II', '<LocalLeader>Ii'], ':call VlimeInspect(vlime#ui#CurExprOrAtom())<cr>'],
                    \ ['n', ['<LocalLeader>IE', '<LocalLeader>Ie'], ':call VlimeInspect(vlime#ui#CurExpr())<cr>'],
                    \ ['n', ['<LocalLeader>IT', '<LocalLeader>It'], ':call VlimeInspect(vlime#ui#CurTopExpr())<cr>'],
                    \ ['n', ['<LocalLeader>IA', '<LocalLeader>Ia'], ':call VlimeInspect(vlime#ui#CurAtom())<cr>'],
                    \ ['v', '<LocalLeader>I', ':<c-u>call VlimeInspect(vlime#ui#CurSelection())<cr>'],
                    \
                    \ ['n', '<LocalLeader>uf', ':call VlimeUndefineFunction(vlime#ui#CurAtom())<cr>'],
                    \ ['n', '<LocalLeader>us', ':call VlimeUninternSymbol(vlime#ui#CurAtom())<cr>'],
                    \
                    \ ['n', '<LocalLeader>wp', ':call VlimeCloseWindow("preview")<cr>'],
                    \ ['n', '<LocalLeader>wr', ':call VlimeCloseWindow("arglist")<cr>'],
                    \ ['n', '<LocalLeader>wn', ':call VlimeCloseWindow("notes")<cr>'],
                    \ ['n', '<LocalLeader>wR', ':call VlimeCloseWindow("repl")<cr>'],
                    \ ['n', '<LocalLeader>wA', ':call VlimeCloseWindow("")<cr>'],
                    \ ['n', '<LocalLeader>wl', ':call VlimeCloseWindow()<cr>'],
                    \
                    \ ['n', '<LocalLeader>i', ':call VlimeInteractionMode()<cr>'],
                    \ ['n', '<LocalLeader>l', ':call VlimeLoadFile(expand("%:p"))<cr>'],
                    \ ['n', '<LocalLeader>a', ':call VlimeDisassembleForm(vlime#ui#CurExpr())<cr>'],
                    \ ['n', '<LocalLeader>p', ':call VlimeSetPackage()<cr>'],
                    \ ['n', '<LocalLeader>b', ':call VlimeSetBreakpoint()<cr>'],
                    \ ['n', '<LocalLeader>t', ':call VlimeListThreads()<cr>'],
                \ ],
                \
                \ 'sldb': [
                    \ ['n', '<cr>', ':call vlime#ui#sldb#ChooseCurRestart()<cr>'],
                    \ ['n', 'd', ':call vlime#ui#sldb#ShowFrameDetails()<cr>'],
                    \ ['n', 'S', ':<c-u>call vlime#ui#sldb#OpenFrameSource()<cr>'],
                    \ ['n', 'T', ':call vlime#ui#sldb#OpenFrameSource("tabedit")<cr>'],
                    \ ['n', 'r', ':call vlime#ui#sldb#RestartCurFrame()<cr>'],
                    \ ['n', 's', ':call vlime#ui#sldb#StepCurOrLastFrame("step")<cr>'],
                    \ ['n', 'x', ':call vlime#ui#sldb#StepCurOrLastFrame("next")<cr>'],
                    \ ['n', 'o', ':call vlime#ui#sldb#StepCurOrLastFrame("out")<cr>'],
                    \ ['n', 'c', ':call b:vlime_conn.SLDBContinue()<cr>'],
                    \ ['n', 'a', ':call b:vlime_conn.SLDBAbort()<cr>'],
                    \ ['n', 'C', ':call vlime#ui#sldb#InspectCurCondition()<cr>'],
                    \ ['n', 'i', ':call vlime#ui#sldb#InspectInCurFrame()<cr>'],
                    \ ['n', 'e', ':call vlime#ui#sldb#EvalStringInCurFrame()<cr>'],
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
                    \ ['n', ['<c-n>', '<tab>'], ':call vlime#ui#inspector#NextField(v:true)<cr>'],
                    \ ['n', '<c-p>', ':call vlime#ui#inspector#NextField(v:false)<cr>'],
                    \ ['n', 'p', ':call vlime#ui#inspector#InspectorPop()<cr>'],
                    \ ['n', 'R', ':call b:vlime_conn.InspectorReinspect({c, r -> c.ui.OnInspect(c, r, v:null, v:null)})<cr>'],
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
                    \ ['n', '<LocalLeader>c', ':call VlimeConnectToCurServer()<cr>'],
                    \ ['n', '<LocalLeader>s', ':call VlimeStopCurServer()<cr>'],
                \ ],
            \ }
endif


function! vlime#ui#mapping#GetBufferMappings(buf_type)
    return g:vlime_default_mappings[a:buf_type]
endfunction
