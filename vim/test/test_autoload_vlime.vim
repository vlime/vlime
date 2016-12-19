function! TestConnectFailed()
    let conn = vlime#New()
    try
        call conn.Connect('127.0.0.1', 65535)
        call assert_false(v:true, 'Connect call did not fail')
    catch
        call assert_exception('vlime#Connect:')
    endtry
endfunction

function! TestIsConnected()
    let conn = vlime#New()
    call assert_false(conn.IsConnected())
endfunction

function! TestClose()
    let conn = vlime#New()
    try
        call conn.Close()
    catch
        call add(v:errors, v:exception)
    endtry
endfunction

function! TestGetCurrentPackage()
    function! s:DummyPackageGetter() dict
        return ['DUMMY-PACKAGE', 'DUMMY-PACKAGE']
    endfunction
    let ui = vlime#ui#New()
    let ui['GetCurrentPackage'] = function('s:DummyPackageGetter')
    let conn = vlime#New(v:null, ui)
    let pkg = conn.GetCurrentPackage()
    call assert_equal(['DUMMY-PACKAGE', 'DUMMY-PACKAGE'], pkg)
endfunction

function! TestSetCurrentPackage()
    function! s:DummyPackageSetter(pkg) dict
        let b:vlime_test_dummy_package = a:pkg
    endfunction
    let ui = vlime#ui#New()
    let ui['SetCurrentPackage'] = function('s:DummyPackageSetter')
    let conn = vlime#New(v:null, ui)
    call conn.SetCurrentPackage(['DUMMY-PACKAGE', 'DUMMY-PACKAGE'])
    call assert_true(exists('b:vlime_test_dummy_package'))
    call assert_equal(['DUMMY-PACKAGE', 'DUMMY-PACKAGE'], b:vlime_test_dummy_package)
    unlet b:vlime_test_dummy_package
endfunction

function! TestGetCurrentThread()
    function! s:DummyThreadGetter() dict
        return {'name': 'REPL-THREAD', 'package': 'KEYWORD'}
    endfunction
    let ui = vlime#ui#New()
    let ui['GetCurrentThread'] = function('s:DummyThreadGetter')
    let conn = vlime#New(v:null, ui)
    let thread = conn.GetCurrentThread()
    call assert_equal({'name': 'REPL-THREAD', 'package': 'KEYWORD'}, thread)
endfunction

function! TestSetCurrentThread()
    function! s:DummyThreadSetter(thread) dict
        let b:vlime_test_dummy_thread = a:thread
    endfunction
    let ui = vlime#ui#New()
    let ui['SetCurrentThread'] = function('s:DummyThreadSetter')
    let conn = vlime#New(v:null, ui)
    call conn.SetCurrentThread({'name': 'REPL-THREAD', 'package': 'KEYWORD'})
    call assert_true(exists('b:vlime_test_dummy_thread'))
    call assert_equal({'name': 'REPL-THREAD', 'package': 'KEYWORD'}, b:vlime_test_dummy_thread)
    unlet b:vlime_test_dummy_thread
endfunction

function! TestWithThread()
    function! s:DummyThreadGetter() dict
        return b:vlime_test_dummy_thread
    endfunction

    function! s:DummyThreadSetter(thread) dict
        let b:vlime_test_dummy_thread = a:thread
    endfunction

    function! s:DummyAction(conn)
        let b:vlime_test_dummy_action_result = a:conn.GetCurrentThread()
    endfunction

    let ui = vlime#ui#New()
    let ui['GetCurrentThread'] = function('s:DummyThreadGetter')
    let ui['SetCurrentThread'] = function('s:DummyThreadSetter')
    let conn = vlime#New(v:null, ui)
    let b:vlime_test_dummy_thread = {'name': 'OLD-THREAD', 'package': 'KEYWORD'}
    call conn.WithThread(1, function('s:DummyAction'), [conn])
    call assert_equal(1, b:vlime_test_dummy_action_result)
    call assert_equal({'name': 'OLD-THREAD', 'package': 'KEYWORD'}, b:vlime_test_dummy_thread)
endfunction

function! TestWithPackage()
    function! s:DummyPackageGetter() dict
        return b:vlime_test_dummy_package
    endfunction

    function! s:DummyPackageSetter(pkg) dict
        let b:vlime_test_dummy_package = a:pkg
    endfunction

    function! s:DummyAction(conn)
        let b:vlime_test_dummy_action_result = a:conn.GetCurrentPackage()
    endfunction

    let ui = vlime#ui#New()
    let ui['GetCurrentPackage'] = function('s:DummyPackageGetter')
    let ui['SetCurrentPackage'] = function('s:DummyPackageSetter')
    let conn = vlime#New(v:null, ui)
    let b:vlime_test_dummy_package = ['OLD-PKG', 'OLD-PKG']
    call conn.WithPackage('NEW-PKG', function('s:DummyAction'), [conn])
    call assert_equal(['NEW-PKG','NEW-PKG'], b:vlime_test_dummy_action_result)
    call assert_equal(['OLD-PKG', 'OLD-PKG'], b:vlime_test_dummy_package)
endfunction

function! TestEmacsRex()
    let conn = vlime#New()
    let rex = conn.EmacsRex([{'package': 'SWANK', 'name': 'CONNECTION-INFO'}])
    call assert_equal([
                    \ {'package': 'KEYWORD', 'name': 'EMACS-REX'},
                    \ [{'package': 'SWANK', 'name': 'CONNECTION-INFO'}],
                    \ v:null, v:true],
                \ rex)
endfunction

let v:errors = []
call TestConnectFailed()
call TestIsConnected()
call TestClose()
call TestGetCurrentPackage()
call TestSetCurrentPackage()
call TestGetCurrentThread()
call TestSetCurrentThread()
call TestWithThread()
call TestWithPackage()
call TestEmacsRex()
