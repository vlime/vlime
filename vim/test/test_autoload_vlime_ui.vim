function! NewDummyBuffer()
    noswapfile tabedit vlime_test_dummy_buffer
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal nobuflisted
endfunction

function! CleanupDummyBuffer()
    bunload!
endfunction


function! TestCurrentPackage()
    call NewDummyBuffer()
    try
        let ui = vlime#ui#New()
        call assert_equal(['COMMON-LISP-USER', 'CL-USER'], ui.GetCurrentPackage())

        call append(line('$'), '(in-package :dummy-package-1)')
        call setpos('.', [0, line('$'), 1, 0])
        call assert_equal(['DUMMY-PACKAGE-1', 'DUMMY-PACKAGE-1'], ui.GetCurrentPackage())

        call ui.SetCurrentPackage(['DUMMY-PACKAGE-2', 'DUMMY-PACKAGE-2'])
        call assert_equal(['DUMMY-PACKAGE-2', 'DUMMY-PACKAGE-2'], ui.GetCurrentPackage())
    finally
        call CleanupDummyBuffer()
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
                \ 'vlime_test_open_buffer', v:true, 'botright')
    call assert_equal('vlime_test_open_buffer', expand('%'))
    execute 'bunload! ' . buf

    let cur_buf_name = expand('%')
    let buf = vlime#ui#OpenBuffer(
                \ 'vlime_test_open_buffer_2', v:true, v:false)
    call assert_notequal('vlime_test_open_buffer_2', expand('%'))
endfunction

function! TestCurBufferContent()
    call NewDummyBuffer()
    try
        call append(0, ['line 1', 'line 2'])
        call assert_equal("line 1\nline 2\n", vlime#ui#CurBufferContent())
    finally
        call CleanupDummyBuffer()
    endtry
endfunction

function! TestCurChar()
    call NewDummyBuffer()
    try
        call append(line('$'), 'a')
        call setpos('.', [0, line('$'), 1, 0])
        call assert_equal('a', vlime#ui#CurChar())

        call append(line('$'), '字')
        call setpos('.', [0, line('$'), 1, 0])
        call assert_equal('字', vlime#ui#CurChar())
    finally
        call CleanupDummyBuffer()
    endtry
endfunction

function! TestCurAtom()
    call NewDummyBuffer()
    try
        call append(line('$'), 'dummy-atom-name')
        call setpos('.', [0, line('$'), 1, 0])
        call assert_equal('dummy-atom-name', vlime#ui#CurAtom())

        call append(line('$'), 'dummy/atom/name another-name')
        call setpos('.', [0, line('$'), 1, 0])
        call assert_equal('dummy/atom/name', vlime#ui#CurAtom())

        call append(line('$'), '*dummy-atom-name* another-name')
        call setpos('.', [0, line('$'), 1, 0])
        call assert_equal('*dummy-atom-name*', vlime#ui#CurAtom())

        call append(line('$'), '+dummy-atom-name+ another-name')
        call setpos('.', [0, line('$'), 1, 0])
        call assert_equal('+dummy-atom-name+', vlime#ui#CurAtom())

        call append(line('$'), 'yet-another-name dummy-package:dummy-atom-name another-name')
        call setpos('.', [0, line('$'), 18, 0])
        call assert_equal('dummy-package:dummy-atom-name', vlime#ui#CurAtom())
    finally
        call CleanupDummyBuffer()
    endtry
endfunction

function! TestCurExpr()
    call NewDummyBuffer()
    try
        call append(line('$'), '(cons 1 2)')
        call setpos('.', [0, line('$'), 1, 0])
        let cur_line = line('.')
        call assert_equal(['(cons 1 2)', [cur_line, 1], [cur_line, 10]],
                    \ vlime#ui#CurExpr(v:true))

        call append(line('$'), '(cons (cons 1 2) 3)')
        call setpos('.', [0, line('$'), 1, 0])
        let cur_line = line('.')
        call assert_equal(['(cons (cons 1 2) 3)', [cur_line, 1], [cur_line, 19]],
                    \ vlime#ui#CurExpr(v:true))

        call append(line('$'), '(cons (cons 1 2) 3)')
        call setpos('.', [0, line('$'), 7, 0])
        let cur_line = line('.')
        call assert_equal(['(cons 1 2)', [cur_line, 7], [cur_line, 16]],
                    \ vlime#ui#CurExpr(v:true))

        call append(line('$'), ['(cons', '1 2)'])
        call setpos('.', [0, line('$'), 1, 0])
        let cur_line = line('.')
        call assert_equal(["(cons\n1 2)", [cur_line - 1, 1], [cur_line, 4]],
                    \ vlime#ui#CurExpr(v:true))
    finally
        call CleanupDummyBuffer()
    endtry
endfunction

function! TestAppendString()
    call NewDummyBuffer()
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
        call CleanupDummyBuffer()
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
