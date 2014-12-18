
```
Body ::= <Body-Line> ( <Body> | nul )
Body-Line ::= <Content-Specifier> <Content-Length> : <Content-String> \n
Content-Specifier ::=
  Version |
  VersionResult |
  Control |
  SourceFileName |
  Source |
  CompilerOption |
  StdIn |
  CompilerMessageE |
  CompilerMessageS |
  StdOut |
  StdErr |
  ExitCode |
  Signal
Content-Length ::= length in octet of Content-String (not include last \n)
Content-String ::= basic-charset (utf-8 quoted-printable)
```
