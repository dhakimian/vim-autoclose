scriptencoding utf-8
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" AutoClose.vim - Automatically close pair of characters: ( with ), [ with ], { with }, etc.
" Forked and majorly rewritten to take advantage of Vim 8 features like ^gU
" Version: 3.0
" Original Author: Thiago Alves <talk@thiagoalves.com.br>
" Maintainer: Daniel Hakimian <dhakimian@westmont.edu>
" Licence: This script is released under the Vim License.
" Last modified: 2019-05-14
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" check if script is already loaded
if !exists("g:debug_AutoClose") && exists("g:loaded_AutoClose")
    finish "stop loading the script"
endif
let g:loaded_AutoClose = 1

let s:cpo_save = &cpo   " store compatible-mode in local variable
set cpo&vim             " go into nocompatible-mode

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:GetCharAhead(len)
    if col('$') == col('.')
        return "\0"
    endif
    return strpart(getline('.'), col('.')-2 + a:len, 1)
endfunction

function! s:GetCharBehind(len)
    if col('.') == 1
        return "\0"
    endif
    return strpart(getline('.'), col('.') - (1 + a:len), 1)
endfunction

function! s:GetNextChar()
    return s:GetCharAhead(1)
endfunction

function! s:GetPrevChar()
    return s:GetCharBehind(1)
endfunction

" used to implement automatic deletion of closing character when opening
" counterpart is deleted and by space expansion
function! s:IsEmptyPair()
    let l:prev = s:GetPrevChar()
    let l:next = s:GetNextChar()
    return (l:next != "\0") && (get(b:AutoClosePairs, l:prev, "\0") == l:next)
endfunction

" Detect if this a space-expanded empty pair.
" Used to implement automatic deletion of closing space when opening
" counterpart is deleted in a space-expanded pair
function! s:IsEmptySpacePair()
    let l:pprev = s:GetCharBehind(2)
    let l:prev = s:GetPrevChar()
    let l:next = s:GetNextChar()
    let l:nnext = s:GetCharAhead(2)
    return (l:next != "\0") && (l:nnext != "\0") && l:prev == "\<Space>" && l:next == "\<Space>" && (get(b:AutoClosePairs, l:pprev, "\0") == l:nnext)
endfunction

function! s:GetCurrentSyntaxRegion()
    return synIDattr(synIDtrans(synID(line('.'), col('.'), 1)), 'name')
endfunction

function! s:GetCurrentSyntaxRegionIf(char)
    let l:origin_line = getline('.')
    let l:changed_line = strpart(l:origin_line, 0, col('.')-1) . a:char . strpart(l:origin_line, col('.')-1)
    call setline('.', l:changed_line)
    let l:region = synIDattr(synIDtrans(synID(line('.'), col('.'), 1)), 'name')
    call setline('.', l:origin_line)
    return l:region
endfunction

function! s:IsForbidden(char)
    let l:result = index(b:AutoCloseProtectedRegions, s:GetCurrentSyntaxRegion()) >= 0
    if l:result
        return l:result
    endif
    let l:region = s:GetCurrentSyntaxRegionIf(a:char)
    let l:result = index(b:AutoCloseProtectedRegions, l:region) >= 0
    return l:result || l:region == 'Comment'
endfunction

function! s:AllowQuote(char, isBS)
    let l:result = 1
    if b:AutoCloseSmartQuote
        let l:initPos = 1 + (a:isBS ? 1 : 0)
        let l:charBehind = s:GetCharBehind(l:initPos)
        let l:prev = l:charBehind
        let l:backSlashCount = 0
        while l:charBehind == '\'
            let l:backSlashCount = l:backSlashCount + 1
            let l:charBehind = s:GetCharBehind(l:initPos + l:backSlashCount)
        endwhile

        if l:backSlashCount % 2
            let l:result = 0
        else
            if a:char == "'" && l:prev =~ '[a-zA-Z0-9]'
                let l:result = 0
            endif
        endif
    endif
    return l:result
endfunction

function! s:CountQuotes(char)
    let l:currPos = col('.')-1
    let l:line = strpart(getline('.'), 0, l:currPos)
    let l:result = 0

    if l:currPos >= 0
        for [q,closer] in items(b:AutoClosePairs)
            " only consider twin pairs
            if q != closer | continue | endif

            if b:AutoCloseSmartQuote != 0
                let l:regex = q . '[ˆ\\' . q . ']*(\\.[ˆ\\' . q . ']*)*' . q
            else
                let l:regex = q . '[ˆ' . q . ']*' . q
            endif

            let l:closedQuoteIdx = match(l:line, l:regex)
            while l:closedQuoteIdx >= 0
                let l:matchedStr = matchstr(l:line, l:regex, l:closedQuoteIdx)
                let l:line = strpart(l:line, 0, l:closedQuoteIdx) . strpart(l:line, l:closedQuoteIdx + strlen(l:matchedStr))
                let l:closedQuoteIdx = match(l:line, l:regex)
            endwhile
        endfor

        let l:result = count(split(l:line, '\zs'), a:char)
    endif
    return l:result
endfunction

" Keep a record of auto-inserted closing delimiters so that we can know if we
" should move over them when typing the closing delimiter ourselves. This allows
" us to move through closing delimiters added in the current insertion action
" while not moving through any delimiters that were already present.
function! s:PushStack(char)
    if !exists("b:AutoCloseStack")
        let b:AutoCloseStack = []
    endif
    call insert(b:AutoCloseStack, a:char)
endfunction

function! s:PopStack()
    if exists("b:AutoCloseStack") && len(b:AutoCloseStack) > 0
        call remove(b:AutoCloseStack, 0)
    endif
endfunction

function! s:EmptyStack()
    if exists("b:AutoCloseStack")
        let b:AutoCloseStack = []
    endif
endfunction

" returns the opener, after having inserted its closer if necessary
function! s:InsertPair(opener)
    if b:AutoCloseOn && has_key(b:AutoClosePairs, a:opener) && ! s:IsForbidden(a:opener)

        let l:next = s:GetNextChar()
        " only add closing pair before space or any of the closepair chars
        " TODO: make close_before configurable
        let close_before = '\s\|\V\[,.;' . escape(join(keys(b:AutoClosePairs) + values(b:AutoClosePairs), ''), ']').']'
        if (l:next == "\0" || l:next =~ close_before) && s:AllowQuote(a:opener, 0)
            call s:PushStack(b:AutoClosePairs[a:opener])
            return a:opener.b:AutoClosePairs[a:opener]."\<C-G>U\<Left>"
        endif

    endif
    return a:opener
endfunction

" returns the closer, after having eaten identical one if necessary
function! s:ClosePair(closer)
    if b:AutoCloseOn

        "Could probably actually get away with just checking if the stack length is > 0 and not
        "worry about making sure the char matches the top of the stack
        if s:GetNextChar() == a:closer && (! b:AutoCloseConsumeOnlyOnSameInsertion || (exists("b:AutoCloseStack") && len(b:AutoCloseStack) > 0 && get(b:AutoCloseStack, 0) == a:closer))
            "call s:EraseNCharsAtCursor(1)
            call s:PopStack()
            return "\<Del>".a:closer
        endif

    endif
    return a:closer
endfunction

" in case closer is identical with its opener - heuristically decide which one
" is being typed and act accordingly
function! s:OpenOrCloseTwinPair(char)
    if s:CountQuotes(a:char) % 2 == 0
        " act as opening char
        return s:InsertPair(a:char)
    else
        " act as closing char
        return s:ClosePair(a:char)
    endif
endfunction

" maintain auto-close stack when delete key is pressed
function! s:Delete()
    if b:AutoCloseOn

        if exists("b:AutoCloseStack") && len(b:AutoCloseStack) > 0 && b:AutoCloseStack[0] == s:GetNextChar()
            call s:PopStack()
        endif

    endif
    return "\<Del>"
endfunction

" when backspace is pressed:
" - erase an empty pair if backspacing from inside one
" - maintain auto-close stack
function! s:Backspace()
    if b:AutoCloseOn
        let l:prev = s:GetPrevChar()
        let l:next = s:GetNextChar()

        "TODO: Check &backspace for 'start'. If not present, then don't attempt to delete an empty pair
        "unless it's in the Stack (otherwise, unless done in the same insertion action, it will
        "delete the closer, but the opener will remain since it is before the start of insertion)
        if (s:IsEmptyPair() && (l:prev != l:next || s:AllowQuote(l:prev, 1))) || s:IsEmptySpacePair()
            "call s:EraseNCharsAtCursor(1)
            call s:PopStack()
            return "\<Del>\<BS>"
        endif

    endif
    return "\<BS>"
endfunction

function! s:Space()
    if b:AutoCloseOn

        if s:IsEmptyPair()
            "If we insert a Space inside an empty Pair, expand it (basically
            "treat Space as a temporary Twin Pair when inside another Pair)
            call s:PushStack("\<Space>")
            return "\<Space>\<Space>\<C-G>U\<Left>"
        elseif exists("b:AutoCloseStack") && len(b:AutoCloseStack) > 0 && get(b:AutoCloseStack, 0) == "\<Space>" && s:GetNextChar() == "\<Space>"
            "If there is a Space in the Stack, then it was almost certainly
            "because it was put there by a Space expansion (defined just above)
            "which can only happen inside another pair (It is unlikely that
            "Space would be set as a normal twin pair, since it is impossible to
            "set it as such through ParsePairs. While it could be set directly
            "in AutoClosePairs, this is not recommended while AutoCloseExpandSpace
            "is enabled due to the special handling Space gets here.)
            "This sub-pair will act as if AutoCloseConsumeOnlyOnSameInsertion is
            "enabled regardless of the value of that setting.
            return s:ClosePair("\<Space>")
        endif

    endif
    return "\<Space>"
endfunction
"TODO: generalize to sub-pair
function! s:Enter()
    if b:AutoCloseOn

        if s:IsEmptyPair()
            "If we insert a carriage return inside an empty Pair, expand it
            "call s:PushStack("\<CR>")
            return "\<CR>\<ESC>O"
        "elseif exists("b:AutoCloseStack") && len(b:AutoCloseStack) > 1 && s:GetNextChar() == "\<CR>"
        "    "If there are at least two characters in the Stack, and the most
        "    "recent is Space, then the next one has to be the closer of a Pair.
        "    "Unless Space is somehow set as a Pair (which is unlikely), then the
        "    "only time it should be in the stack is after a Space was expanded
        "    "as above.
        "    return s:ClosePair("\<CR>")
        endif

    endif
    return "\<CR>"
endfunction

function! s:ToggleAutoClose()
    let b:AutoCloseOn = !b:AutoCloseOn
    if b:AutoCloseOn
        echo "AutoClose ON"
    else
        echo "AutoClose OFF"
    endif
endfunction

" Parse a whitespace separated line of pairs
" single characters are assumed to be twin pairs (closer identical to
" opener)
function! AutoClose#ParsePairs(string)
    if type(a:string) == type({})
        return a:string
    elseif type(a:string) != type("")
        echoerr "AutoClose#ParsePairs(): Argument not a dictionary or a string"
        return {}
    endif

    let l:dict = {}
    for pair in split(a:string)
        " strlen is length in bytes, we want in (wide) characters
        let l:pairLen = strlen(substitute(pair,'.','x','g'))
        if l:pairLen == 1
            " assume a twin pair
            let l:dict[pair] = pair
        elseif l:pairLen == 2
            let l:dict[pair[0]] = pair[1]
        else
            echoerr "AutoClose: Bad pair string - a pair longer then two character"
            echoerr " `- String: " . a:sring
            echoerr " `- Pair: " . pair . " Pair len: " . l:pairLen
        endif
    endfor
    return l:dict
endfunction

" this function is made visible for the sake of users
function! AutoClose#DefaultPairs()
    return AutoClose#ParsePairs(g:AutoClosePairs)
endfunction

function! s:ModifyPairsList(list, pairsToAdd, openersToRemove)
    return filter(
                \ extend(AutoClose#ParsePairs(a:pairsToAdd), a:list, "keep"),
                \ "stridx(a:openersToRemove,v:key)<0")
endfunction

function! AutoClose#DefaultPairsModified(pairsToAdd,openersToRemove)
    return s:ModifyPairsList(AutoClose#DefaultPairs(), a:pairsToAdd, a:openersToRemove)
endfunction

" Define variables (in the buffer namespace).
function! s:DefineVariables()
    " All the following variables can be set per buffer or global.
    " The buffer namespace is used internally
    let defaults = {
                \ 'AutoClosePairs': AutoClose#DefaultPairs(),
                \ 'AutoCloseProtectedRegions': ["Comment", "String", "Character"],
                \ 'AutoCloseSmartQuote': 1,
                \ 'AutoCloseOn': 1,
                \ 'AutoCloseExpandEnter': 1,
                \ 'AutoCloseExpandSpace': 1,
                \ 'AutoCloseConsumeOnlyOnSameInsertion': 1,
                \ }


    if exists ('b:AutoClosePairs') && type('b:AutoClosePairs') == type("")
        let tmp = AutoClose#ParsePairs(b:AutoClosePairs)
        unlet b:AutoClosePairs
        let b:AutoClosePairs = tmp
    endif

    " Now handle/assign values
    for key in keys(defaults)
        if exists('b:'.key) && type(eval('b:'.key)) == type(defaults[key])
            continue
        elseif exists('g:'.key) && type(eval('g:'.key)) == type(defaults[key])
            exec 'let b:' . key . ' = g:' . key
        else
            exec 'let b:' . key . ' = ' . string(defaults[key])
        endif
    endfor
endfunction

function! s:CreatePairsMaps()
    " create appropriate maps to defined open/close characters
    for key in keys(b:AutoClosePairs)
        let opener = s:keyName(key)
        let closer = s:keyName(b:AutoClosePairs[key])
        let quoted_opener = s:quoteAndEscape(opener)
        let quoted_closer = s:quoteAndEscape(closer)

        if key == b:AutoClosePairs[key]
            exec "inoremap <buffer> <silent> " . opener
                        \ . " <C-R>=<SID>OpenOrCloseTwinPair(" . quoted_opener . ")<CR>"
        else
            exec "inoremap <buffer> <silent> " . opener
                        \ . " <C-R>=<SID>InsertPair(" . quoted_opener . ")<CR>"
            exec "inoremap <buffer> <silent> " . closer
                        \ . " <C-R>=<SID>ClosePair(" . quoted_closer . ")<CR>"
        endif
    endfor
endfunction

function! s:CreateExtraMaps()
    " Extra mapping
    inoremap <buffer> <silent> <expr>  <BS>   <SID>Backspace()
    inoremap <buffer> <silent> <expr>  <C-H>  <SID>Backspace()
    inoremap <buffer> <silent> <expr>  <Del>  <SID>Delete()
    if b:AutoCloseExpandSpace
        inoremap <buffer> <silent> <expr>  <Space>  <SID>Space()
    endif
    if b:AutoCloseExpandEnter
        inoremap <buffer> <silent> <expr>  <CR>  <SID>Enter()
    endif

endfunction

function! s:CreateMaps()
    silent doautocmd FileType
    call s:DefineVariables()
    call s:CreatePairsMaps()
    call s:CreateExtraMaps()

    let b:loaded_AutoClose = 1
endfunction

function! s:IsLoadedOnBuffer()
    return (exists("b:loaded_AutoClose") && b:loaded_AutoClose)
endfunction

" map some characters to their key names
function! s:keyName(char)
    let s:keyNames = {'|': '<Bar>', ' ': '<Space>'}
    return get(s:keyNames,a:char,a:char)
endfunction

" escape some characters for use in strings
function! s:quoteAndEscape(char)
    let s:escapedChars = {'"': '\"'}
    return '"' . get(s:escapedChars,a:char,a:char) . '"'
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let s:AutoClosePairs_FactoryDefaults = AutoClose#ParsePairs("() {} [] ` \" '")
if !exists("g:AutoClosePairs_add") | let g:AutoClosePairs_add = "" | endif
if !exists("g:AutoClosePairs_del") | let g:AutoClosePairs_del = "" | endif
if !exists("g:AutoClosePairs")
    let g:AutoClosePairs = s:ModifyPairsList(
                \ s:AutoClosePairs_FactoryDefaults,
                \ g:AutoClosePairs_add,
                \ g:AutoClosePairs_del )
endif


augroup <Plug>(autoclose)
    autocmd!
    autocmd BufNewFile,BufRead,BufEnter * if !<SID>IsLoadedOnBuffer() | call <SID>CreateMaps() | endif
    autocmd InsertEnter * call <SID>EmptyStack()
    autocmd InsertLeave * call <SID>EmptyStack()
    autocmd BufEnter * if mode() == 'i' | call <SID>EmptyStack() | endif
augroup END

" Define convenient commands
command! AutoCloseOn :let b:AutoCloseOn = 1
command! AutoCloseOff :let b:AutoCloseOn = 0
command! AutoCloseToggle :call s:ToggleAutoClose()

" Restore 'cpoptions'
let &cpo = s:cpo_save
unlet s:cpo_save
" vim:sw=4:sts=4:et
