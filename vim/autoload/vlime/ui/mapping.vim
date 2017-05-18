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
                    \ ['n', '<LocalLeader>ss', ':call VlimeSendCurThingToREPL("thing")<cr>'],
                    \ ['n', '<LocalLeader>se', ':call VlimeSendCurThingToREPL("expr")<cr>'],
                    \ ['n', '<LocalLeader>st', ':call VlimeSendCurThingToREPL("top_expr")<cr>'],
                    \ ['n', '<LocalLeader>sa', ':call VlimeSendCurThingToREPL("atom")<cr>'],
                    \ ['v', '<LocalLeader>s', ':<c-u>call VlimeSendCurThingToREPL("selection")<cr>'],
                    \
                    \ ['n', '<LocalLeader>m1', ':call VlimeExpandCurMacro(v:false)<cr>'],
                    \ ['n', '<LocalLeader>ma', ':call VlimeExpandCurMacro(v:true)<cr>'],
                    \
                    \ ['n', '<LocalLeader>oe', ':call VlimeCompileCurThing("expr")<cr>'],
                    \ ['n', '<LocalLeader>ot', ':call VlimeCompileCurThing("top_expr")<cr>'],
                    \ ['n', '<LocalLeader>of', ':call VlimeCompileCurFile()<cr>'],
                    \ ['v', '<LocalLeader>o', ':<c-u>call VlimeCompileCurThing("selection")<cr>'],
                    \
                    \ ['n', '<LocalLeader>xc', ':call VlimeXRefCurSymbol("atom", "CALLS")<cr>'],
                    \ ['n', '<LocalLeader>xC', ':call VlimeXRefCurSymbol("atom", "CALLS-WHO")<cr>'],
                    \ ['n', '<LocalLeader>xr', ':call VlimeXRefCurSymbol("atom", "REFERENCES")<cr>'],
                    \ ['n', '<LocalLeader>xb', ':call VlimeXRefCurSymbol("atom", "BINDS")<cr>'],
                    \ ['n', '<LocalLeader>xs', ':call VlimeXRefCurSymbol("atom", "SETS")<cr>'],
                    \ ['n', '<LocalLeader>xe', ':call VlimeXRefCurSymbol("atom", "MACROEXPANDS")<cr>'],
                    \ ['n', '<LocalLeader>xm', ':call VlimeXRefCurSymbol("atom", "SPECIALIZES")<cr>'],
                    \ ['n', '<LocalLeader>xd', ':call VlimeFindCurDefinition("atom")<cr>'],
                    \
                    \ ['n', '<LocalLeader>do', ':call VlimeDescribeCurSymbol("operator")<cr>'],
                    \ ['n', '<LocalLeader>da', ':call VlimeDescribeCurSymbol("atom")<cr>'],
                    \ ['n', '<LocalLeader>ds', ':call VlimeAproposList()<cr>'],
                    \ ['n', '<LocalLeader>ddo', ':call VlimeDocumentationSymbol("operator")<cr>'],
                    \ ['n', '<LocalLeader>dda', ':call VlimeDocumentationSymbol("atom")<cr>'],
                    \ ['n', '<LocalLeader>dr', ':call VlimeShowOperatorArgList(vlime#ui#CurOperator())<cr>'],
                    \
                    \ ['n', ['<LocalLeader>II', '<LocalLeader>Ii'], ':call VlimeInspectCurThing("thing")<cr>'],
                    \ ['n', ['<LocalLeader>IE', '<LocalLeader>Ie'], ':call VlimeInspectCurThing("expr")<cr>'],
                    \ ['n', ['<LocalLeader>IT', '<LocalLeader>It'], ':call VlimeInspectCurThing("top_expr")<cr>'],
                    \ ['n', ['<LocalLeader>IA', '<LocalLeader>Ia'], ':call VlimeInspectCurThing("atom")<cr>'],
                    \ ['v', '<LocalLeader>I', ':call VlimeInspectCurThing("selection")<cr>'],
                    \
                    \ ['n', '<LocalLeader>uf', ':call VlimeUndefineCurFunction()<cr>'],
                    \ ['n', '<LocalLeader>us', ':call VlimeUninternCurSymbol()<cr>'],
                    \
                    \ ['n', '<LocalLeader>wp', ':call VlimeCloseWindow("preview")<cr>'],
                    \ ['n', '<LocalLeader>wr', ':call VlimeCloseWindow("arglist")<cr>'],
                    \ ['n', '<LocalLeader>wn', ':call VlimeCloseWindow("notes")<cr>'],
                    \ ['n', '<LocalLeader>wR', ':call VlimeCloseWindow("repl")<cr>'],
                    \ ['n', '<LocalLeader>wA', ':call VlimeCloseWindow("")<cr>'],
                    \ ['n', '<LocalLeader>wl', ':call VlimeCloseWindow()<cr>'],
                    \
                    \ ['n', '<LocalLeader>i', ':call VlimeInteractionMode()<cr>'],
                    \ ['n', '<LocalLeader>l', ':call VlimeLoadCurFile()<cr>'],
                    \ ['n', '<LocalLeader>a', ':call VlimeDisassembleCurForm()<cr>'],
                    \ ['n', '<LocalLeader>p', ':call VlimeSetCurPackage()<cr>'],
                    \ ['n', '<LocalLeader>b', ':call VlimeSetBreakpoint()<cr>'],
                    \ ['n', '<LocalLeader>t', ':call VlimeListThreads()<cr>'],
                \ ],
                \
                \ 'sldb': [
                    \ ['n', '<cr>', ':call vlime#ui#sldb#ChooseCurRestart()<cr>'],
                    \ ['n', 'd', ':call vlime#ui#sldb#ShowFrameDetails()<cr>'],
                    \ ['n', 'S', ':call vlime#ui#sldb#OpenFrameSource()<cr>'],
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
                    \ ['n', '<cr>', ':call vlime#ui#xref#OpenCurXref()<cr>'],
                \ ],
                \
                \ 'notes': [
                    \ ['n', '<cr>', ':call vlime#ui#compiler_notes#OpenCurNote()<cr>'],
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
