function! TestCurrentPackage()
    let ui = vlime#ui#New()
    call assert_equal(['COMMON-LISP-USER', 'CL-USER'], ui.GetCurrentPackage())
    " XXX: Concat the in-package expression at runtime, to avoid being detected
    " by GetCurrentPackage()
    execute 'normal! Go(' . 'in-package :dummy-package-1)'
    try
        call assert_equal(['DUMMY-PACKAGE-1', 'DUMMY-PACKAGE-1'], ui.GetCurrentPackage())
        call ui.SetCurrentPackage(['DUMMY-PACKAGE-2', 'DUMMY-PACKAGE-2'])
        call assert_equal(['DUMMY-PACKAGE-2', 'DUMMY-PACKAGE-2'], ui.GetCurrentPackage())
    finally
        normal! Gdd
    endtry
endfunction

function! TestCurrentThread()
    let ui = vlime#ui#New()
    call assert_equal(v:true, ui.GetCurrentThread())
    call ui.SetCurrentThread(1)
    call assert_equal(1, ui.GetCurrentThread())
endfunction

function! TestWithBuffer()
    function! s:DummyWithBufferAction()
        let b:vlime_test_dummy_value = 2
    endfunction

    let b:vlime_test_dummy_value = 1
    let new_buf = bufnr('vlime_test_with_buffer', v:true)
    call vlime#ui#WithBuffer(new_buf, function('s:DummyWithBufferAction'))
    call assert_equal(1, b:vlime_test_dummy_value)
    call assert_equal(2, getbufvar(new_buf, 'vlime_test_dummy_value'))
endfunction

function! TestOpenBuffer()
    let buf = vlime#ui#OpenBuffer(
                \ 'vlime_test_open_buffer', v:true, 'botright split')
    call assert_equal('vlime_test_open_buffer', expand('%'))
    execute 'bunload! ' . buf

    let cur_buf_name = expand('%')
    let buf = vlime#ui#OpenBuffer(
                \ 'vlime_test_open_buffer_2', v:true, v:false)
    call assert_notequal('vlime_test_open_buffer_2', expand('%'))
endfunction

function! TestCurBufferContent()
    let buf = bufnr('vlime_test_cur_buffer_content', v:true)
    let old_buf = bufnr('%')
    try
        execute 'hide buffer ' . buf
        call append(line('$'), ['line 1', 'line 2'])
        call assert_equal("line 1\nline 2", vlime#ui#CurBufferContent())
    finally
        execute 'bunload! ' . buf
        execute 'buffer ' . old_buf
    endtry
endfunction

function! TestCurChar()
    normal! Goa
    try
        call assert_equal('a', vlime#ui#CurChar())
    finally
        normal! Gdd
    endtry

    normal! Go字
    try
        call assert_equal('字', vlime#ui#CurChar())
    finally
        normal! Gdd
    endtry
endfunction

function! TestCurAtom()
    normal! Godummy-atom-name
    try
        normal! ^
        call assert_equal('dummy-atom-name', vlime#ui#CurAtom())
    finally
        normal! Gdd
    endtry

    normal! Godummy/atom/name another-name
    try
        normal! ^
        call assert_equal('dummy/atom/name', vlime#ui#CurAtom())
    finally
        normal! Gdd
    endtry

    normal! Go*dummy-atom-name* another-name
    try
        normal! ^
        call assert_equal('*dummy-atom-name*', vlime#ui#CurAtom())
    finally
        normal! Gdd
    endtry

    normal! Go+dummy-atom-name+ another-name
    try
        normal! ^
        call assert_equal('+dummy-atom-name+', vlime#ui#CurAtom())
    finally
        normal! Gdd
    endtry

    normal! Goyet-another-name dummy-package:dummy-atom-name another-name
    try
        normal! ^17l
        call assert_equal('dummy-package:dummy-atom-name', vlime#ui#CurAtom())
    finally
        normal! Gdd
    endtry
endfunction

function! TestCurExpr()
    normal! Go(cons 1 2)
    try
        let cur_line = line('.')
        call assert_equal(['(cons 1 2)', [cur_line, 1], [cur_line, 10]],
                    \ vlime#ui#CurExpr(v:true))
    finally
        normal! Gdd
    endtry

    normal! Go(cons (cons 1 2) 3)
    try
        normal! ^
        let cur_line = line('.')
        call assert_equal(['(cons (cons 1 2) 3)', [cur_line, 1], [cur_line, 19]],
                    \ vlime#ui#CurExpr(v:true))
    finally
        normal! Gdd
    endtry

    normal! Go(cons (cons 1 2) 3)
    try
        normal! ^6l
        let cur_line = line('.')
        call assert_equal(['(cons 1 2)', [cur_line, 7], [cur_line, 16]],
                    \ vlime#ui#CurExpr(v:true))
    finally
        normal! Gdd
    endtry

    call append(line('$'), ['(cons', '1 2)'])
    try
        normal! G
        let cur_line = line('.')
        call assert_equal(["(cons\n1 2)", [cur_line - 1, 1], [cur_line, 4]],
                    \ vlime#ui#CurExpr(v:true))
    finally
        normal! Gdddd
    endtry
endfunction

function! TestAppendString()
    let buf = vlime#ui#OpenBuffer('vlime_test_append_string',
                \ v:true, 'botright split')
    try
        call vlime#ui#AppendString('line1')
        call vlime#ui#AppendString(" line1 line1\n")
        call vlime#ui#AppendString("line2\nline3")
        call vlime#ui#AppendString("\nline5\nline6\nline7")
        call assert_equal("line1 line1 line1\nline2\nline3\nline5\nline6\nline7",
                    \ vlime#ui#CurBufferContent())

        call vlime#ui#ReplaceContent("replaced\ncontent")
        call assert_equal("replaced\ncontent", vlime#ui#CurBufferContent())

        call vlime#ui#ReplaceContent("some other\ntext")
        call assert_equal("some other\ntext", vlime#ui#CurBufferContent())
    finally
        execute 'bunload! ' . buf
    endtry
endfunction

function! TestMatchCoord()
    let coord = {
                \ 'begin': [1, 2],
                \ 'end': [1, 2],
                \ 'type': 'DUMMY',
                \ 'id': 1,
                \ }
    call assert_true(vlime#ui#MatchCoord(coord, 1, 2))
    call assert_false(vlime#ui#MatchCoord(coord, 1, 1))
    call assert_false(vlime#ui#MatchCoord(coord, 1, 3))
    call assert_false(vlime#ui#MatchCoord(coord, 2, 2))

    let coord = {
                \ 'begin': [1, 2],
                \ 'end': [1, 12],
                \ 'type': 'DUMMY',
                \ 'id': 1,
                \ }
    call assert_true(vlime#ui#MatchCoord(coord, 1, 2))
    call assert_true(vlime#ui#MatchCoord(coord, 1, 7))
    call assert_true(vlime#ui#MatchCoord(coord, 1, 12))
    call assert_false(vlime#ui#MatchCoord(coord, 1, 1))
    call assert_false(vlime#ui#MatchCoord(coord, 1, 13))
    call assert_false(vlime#ui#MatchCoord(coord, 2, 7))

    let coord = {
                \ 'begin': [1, 10],
                \ 'end': [2, 5],
                \ 'type': 'DUMMY',
                \ 'id': 1,
                \ }
    call assert_true(vlime#ui#MatchCoord(coord, 1, 15))
    call assert_true(vlime#ui#MatchCoord(coord, 2, 3))
    call assert_false(vlime#ui#MatchCoord(coord, 1, 5))
    call assert_false(vlime#ui#MatchCoord(coord, 2, 7))
endfunction

let v:errors = []
call TestCurrentPackage()
call TestCurrentThread()
call TestWithBuffer()
call TestOpenBuffer()
call TestCurBufferContent()
call TestCurChar()
call TestCurAtom()
call TestCurExpr()
call TestAppendString()
call TestMatchCoord()
