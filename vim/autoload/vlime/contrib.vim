if !exists('g:vlime_contrib_initializers')
    let g:vlime_contrib_initializers = {
                \ 'SWANK-REPL': function('vlime#contrib#repl#Init'),
                \ 'SWANK-PRESENTATIONS': function('vlime#contrib#presentations#Init'),
                \ 'SWANK-FUZZY': function('vlime#contrib#fuzzy#Init'),
                \ 'SWANK-ARGLISTS': function('vlime#contrib#arglists#Init'),
                \ 'SWANK-TRACE-DIALOG': function('vlime#contrib#trace_dialog#Init'),
                \ }
endif

" vlime#contrib#CallInitializers(conn[, callback])
function! vlime#contrib#CallInitializers(conn, ...)
    let Callback = get(a:000, 0, v:null)

    let contribs = get(a:conn.cb_data, 'contribs', [])
    for c in contribs
        let InitFunc = get(g:vlime_contrib_initializers, c, v:null)
        if type(InitFunc) != v:t_func && exists('g:vlime_user_contrib_initializers')
            let InitFunc = get(g:vlime_user_contrib_initializers, c, v:null)
        endif
        if type(InitFunc) == v:t_func
            let ToCall = function(InitFunc, [a:conn])
            call ToCall()
        endif
    endfor

    if type(Callback) == v:t_func
        let ToCall = function(Callback, [a:conn])
        call ToCall()
    endif
endfunction
