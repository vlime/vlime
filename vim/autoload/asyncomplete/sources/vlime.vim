let s:save_cpo = &cpo
set cpo&vim

function! asyncomplete#sources#vlime#get_source_options(opts) abort
  return extend({
    \   'name': 'vlime',
    \   'whitelist': ['lisp'],
    \   'completor': function('asyncomplete#sources#vlime#completor')
    \ }, a:opts)
endfunction

function! asyncomplete#sources#vlime#completor(opts, ctx) abort
  let l:vlime_conn = s:get_vlime_connection()
  if type(l:vlime_conn) == type(v:null)
    return
  endif

  let l:line = getline('.')
  let l:col = a:ctx['col']
  let l:start_col = s:find_start_col(l:col)
  let l:cur_pos = [bufnr('%')] + getcurpos()[1:2]
  let l:base = strpart(l:line, l:start_col, l:col - l:start_col + 1)
  let l:cur_pos[2] += len(l:base)

  call asyncomplete#log('vlime', a:ctx, 'start_col', l:start_col, 'cur_pos', l:cur_pos, 'base', l:base)
  call l:vlime_conn.SimpleCompletions(l:base, function('s:on_simple_completions_completed', [a:opts, a:ctx, l:start_col, l:cur_pos]))
endfunction

function! s:on_simple_completions_completed(opts, ctx, start_col, cur_pos, conn, result)
  let l:words = a:result[0]
  if type(l:words) == type(v:null)
    let l:words = []
  endif
  let l:mapper = "{'word': v:val}"
  let l:matches = map(l:words, l:mapper)

  call asyncomplete#complete(a:opts['name'], a:ctx, a:start_col + 1, l:matches)
endfunction

function! s:find_start_col(col) abort
  let l:col = a:col
  let l:line = getline('.')
  while l:col > 0 && match(l:line[l:col-1], '\_s\|[()#;"'']') < 0
    let l:col -= 1
  endwhile
  return l:col
endfunction

function! s:get_vlime_connection() abort
  return vlime#connection#Get()
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
