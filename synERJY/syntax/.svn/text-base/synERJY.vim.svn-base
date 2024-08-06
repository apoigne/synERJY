" Vim syntax file
" Language:	sE
" Maintainer:	Reinhard Budde <reinhard.budde@ais.fhg.de>
" URL:	        http://ais.fhg.de/~budde/download/sE.vim
" Last Change:	Mon Jul 29 12:09:57 CEST 2002

" developed using java.vim as a starting point:
" Language:	Java
" Maintainer:	Claudio Fleiner <claudio@fleiner.com>
" URL:	        http://www.fleiner.com/vim/syntax/java.vim
" Last Change:	1999 Dec 27

" Remove any old syntax stuff hanging around
syn clear

" some characters that cannot be in a sE program (outside a string)
syn match sEError "[\\`]"
syn match sEError "<<<\|<>\|||=\|&&=\|\*\/"

" we define it here so that included files can test for it
if !exists("main_syntax")
  let main_syntax='sE'
endif

" keyword definitions
syn keyword sEExternal        import package
"sE"-added
syn keyword sEExternal        precedence

syn keyword sEError           goto const
syn keyword sEConditional     if else switch
syn keyword sERepeat          while for do
"sE"-added
syn keyword sERepeat          loop

syn keyword sEBoolean         true false now
syn keyword sEConstant        null
syn keyword sETypedef         this super
syn keyword sEOperator        new instanceof
"sE"-added
syn keyword sEOperator        await emit next sustain when cancel pre nothing
syn keyword sEOperator        automaton state init entry exit during

syn keyword sEType            boolean bool byte char short int long float double
syn keyword sEType            int8 uint8 int16 uint16 int32 uint32 int64 uint64
syn keyword sEType            void unsigned native
syn keyword sEType            bool Sensor Signal DelayedSignal Delayed
syn keyword sEType            SimInput SimOutput
syn keyword sEType            time String
syn keyword sEStatement       return
"sE"-added
syn keyword sEStatement       active reactive
syn keyword sEStorageClass    parameter
syn keyword sEStorageClass    static synchronized transient volatile 
syn keyword sEStorageClass    final strictfp
syn keyword sEExceptions      throw try catch finally
syn keyword sEMethodDecl      synchronized throws
syn keyword sEClassDecl       extends implements interface
" to differentiate the keyword class from MyClass.class we use a match here
syn match   sETypedef         "\.\s*\<class\>"ms=s+1
syn match   sEClassDecl       "^class\>"
syn match   sEClassDecl       "[^.]\s*\<class\>"ms=s+1
syn keyword sEBranch          break continue nextgroup=sEUserLabelRef skipwhite
syn match   sEUserLabelRef    "\k\+" contained
"sE"-added
syn keyword sEScopeDecl       public protected private abstract

syn region  sELabelRegion     transparent matchgroup=sELabel start="\<case\>" matchgroup=NONE end=":" contains=sENumber
syn match   sEUserLabel       "^\s*[_$a-zA-Z][_$a-zA-Z0-9_]*\s*:[^=]"he=e-1 contains=sELabel
syn keyword sELabel           default

" The following cluster contains all sE groups except the contained ones
syn cluster sETop contains=sEExternal,sEError,sEError,sEBranch,sELabelRegion,sELabel,sEConditional,sERepeat,sEBoolean,sEConstant,sETypedef,sEOperator,sEType,sEType,sEType,sEStatement,sEStorageClass,sEExceptions,sEMethodDecl,sEClassDecl,sEClassDecl,sEClassDecl,sEClassDecl,sEScopeDecl,sEError,sEUserLabel,sELangClass,sELangObject

" Comments
syn keyword sETodo             contained TODO FIXME XXX
syn region  sECommentString    contained start=+"+ end=+"+ end=+\*/+me=s-1,he=s-1 contains=sESpecial,sECommentStar,sESpecialChar,@Spell
syn region  sEComment2String   contained start=+"+  end=+$\|"+  contains=sESpecial,sESpecialChar,@Spell
syn match   sECommentCharacter contained "'\\[^']\{1,6\}'" contains=sESpecialChar
syn match   sECommentCharacter contained "'\\''" contains=sESpecialChar
syn match   sECommentCharacter contained "'[^\\]'"
syn region  sEComment          start="/\*"  end="\*/" contains=sECommentString,sECommentCharacter,sENumber,sETodo,@Spell
syn match   sECommentStar      contained "^\s*\*[^/]"me=e-1
syn match   sECommentStar      contained "^\s*\*$"
syn match   sELineComment      "//.*" contains=sEComment2String,sECommentCharacter,sENumber,sETodo,@Spell
hi link sECommentString sEString
hi link sEComment2String sEString
hi link sECommentCharacter sECharacter

syn cluster sETop add=sEComment,sELineComment

if !exists("java_ignore_javadoc")
  syntax case ignore
  " syntax coloring for javadoc comments (HTML)
  syntax include @javaHtml $VIMRUNTIME/syntax/html.vim
  syn region  javaDocComment    start="/\*\*"  end="\*/" keepend contains=javaCommentTitle,@javaHtml,javaDocTags,javaTodo,@Spell
  syn region  javaCommentTitle  contained matchgroup=javaDocComment start="/\*\*"   matchgroup=javaCommentTitle keepend end="\.$" end="\.[ \t\r<&]"me=e-1 end="@"me=s-1,he=s-1 end="\*/"me=s-1,he=s-1 contains=@javaHtml,javaCommentStar,javaTodo,@Spell

  syn region javaDocTags contained start="{@link" end="}"
  syn match javaDocTags contained "@\(see\|param\|exception\|throws\)\s\+\S\+" contains=javaDocParam
  syn match javaDocParam contained "\s\S\+"
  syn match javaDocTags contained "@\(version\|author\|return\|deprecated\|since\)\>"
  syntax case match
endif

" match the special comment /**/
syn match   sEComment          "/\*\*/"

" Strings and constants
syn match   sESpecialError     contained "\\."
syn match   sESpecialCharError contained "[^']"
syn match   sESpecialChar      contained "\\\([4-9]\d\|[0-3]\d\d\|[\"\\'ntbrf]\|u\x\{4\}\)"
syn match   sEString           +"[^"]*"+  contains=sESpecialChar,sESpecialError,@Spell
syn match   sEStringError      +"\([^"\\]\|\\.\)*$+
syn match   sECharacter        "'[^']*'" contains=sESpecialChar,sESpecialCharError
syn match   sECharacter        "'\\''" contains=sESpecialChar
syn match   sECharacter        "'[^\\]'"
syn match   sENumber           "\<\(0[0-7]*\|0[xX]\x\+\|\d\+\)[lL]\=\>"
syn match   sENumber           "0[xX]\d\+"
syn match   sENumber           "\d\+min"
syn match   sENumber           "\d\+sec"
syn match   sENumber           "\d\+msec"
syn match   sENumber           "\d\+usec"
syn match   sENumber           "\(\<\d\+\.\d*\|\.\d\+\)\([eE][-+]\=\d\+\)\=[fFdD]\="
syn match   sENumber           "\<\d\+[eE][-+]\=\d\+[fFdD]\=\>"
syn match   sENumber           "\<\d\+\([eE][-+]\=\d\+\)\=[fFdD]\>"

" unicode characters
syn match   sESpecial "\\u\d\{4\}"

syn cluster sETop add=sEString,sECharacter,sENumber,sESpecial,sEStringError

" catch errors caused by wrong parenthesis
syn region  sEParen            transparent start="(" end=")" contains=@sETop,sEParen
syn match   sEParenError       ")"
hi link     sEParenError       sEError

if !exists("did_sE_syntax_inits")
  let did_sE_syntax_inits = 1
  " The default methods for highlighting.  Can be overridden later
  hi link sEFuncDef                 Function
  hi link sEBraces                  Function
  hi link sEBranch                  Conditional
  hi link sEUserLabelRef            sEUserLabel
  hi link sELabel                   Label
  hi link sEUserLabel               Label
  hi link sEConditional             Conditional
  hi link sERepeat                  Repeat
  hi link sEExceptions              Exception
  hi link sEStorageClass            StorageClass
  hi link sEMethodDecl              sEStorageClass
  hi link sEClassDecl               sEStorageClass
  hi link sEClassDecl               sEStorageClass
  hi link sEScopeDecl               sEStorageClass
  hi link sEBoolean                 Boolean
  hi link sESpecial                 Special
  hi link sESpecialError            Error
  hi link sESpecialCharError        Error
  hi link sEString                  String
  hi link sECharacter               Character
  hi link sESpecialChar		    SpecialChar
  hi link sENumber                  Number
  hi link sEError                         Error
  hi link sEStringError                   Error
  hi link sEStatement                     Statement
  hi link sEOperator                      Operator
  hi link sEComment                       Comment
  hi link javaDocComment                  Comment
  hi link sELineComment                   Comment
  hi link sEConstant			  sEBoolean
  hi link sETypedef			  Typedef
  hi link sETodo                          Todo

  hi link sECommentTitle                  SpecialComment
  hi link javaDocTags			  Special
  hi link javaDocParam			  Function
  hi link sECommentStar                   sEComment

  hi link sEType                          Type
  hi link sEType                          Type
  hi link sEExternal                      Include

  hi link htmlComment                     Special
  hi link htmlCommentPart                 Special
  hi link sESpaceError                    Error

endif

let b:current_syntax = "sE"

if main_syntax == 'sE'
  unlet main_syntax
endif

let b:spell_options="contained"

" vim: ts=8
