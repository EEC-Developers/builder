OPT OSVERSION=37
-> Builder is a replacement for e-build

-> By Samuel D. Crow

MODULE 'Hash/hashBase','Hash/orderedHash','Hash/unorderedHash',
  'Buffer/bufferBase','Buffer/fileBuffer','Queue/queue',
  'List/singleList','List/listBase','Iterator/iterator',
  'Filter/filterBase','Filter/splitter',
  'dos/dos','dos/dostags','dos/dosextens'

OBJECT arguments
  start
  file
  force
  verbose
  nohead
  constants
ENDOBJECT

OBJECT target OF hash_link
  name:PTR TO CHAR
  dependencies:PTR TO ordered_hash
  actions:PTR TO queue_header
ENDOBJECT

-> Constructor
PROC init_target(n:PTR TO CHAR,parent) OF target
  SUPER self.init_link(n,parent)
  self.name:=n
  NEW self.dependancies.init()
  NEW actions.init()
ENDPROC

PROC get_target_key(t:PTR TO target) IS t.name

PROC compare_target(a:PTR TO target,b:PTR TO target)
ENDPROC StrCmp(a.name,b.name)

OBJECT action OF queue_node
  command
ENDOBJECT

OBJECT target_hash OF ordered_hash
ENDOBJECT

PROC init() OF target_hash
  SUPER self.init_base(HASH_NORMAL,
    {get_target_key},{compare_target},{string_hash})
ENDPROC

OBJECT dependency OF hash_link
  name
  bullseye:PTR TO target
ENDOBJECT

PROC get_dependency_name(d:PTR TO dependency) IS d.name

PROC compare_dependency(a:PTR TO dependency,b:PTR TO dependency)
ENDPROC StrCmp(a.name,b.name)

CONST UNINITIALIZED_PTR=-1

PROC init() OF dependency
  SUPER self.init_base(HASH_NORMAL,
    {get_dependency_name},{compare_dependency},{string_hash})
ENDPROC

OBJECT constant OF hash_link
  id:PTR TO CHAR
  substitution:PTR TO CHAR
ENDOBJECT

PROC get_constant_key(c:PTR TO constant) IS c.id

PROC compare_constant(a:PTR TO constant,b:ptr to constant)
ENDPROC StrCmp(a.id,b.id)

OBJECT constant_hash OF unordered_hash
ENDOBJECT

-> Constructor
PROC init() OF constant_hash
  SUPER self.init_base(HASH_NORMAL,
    {get_constant_key},{compare_constant},{string_hash})
ENDPROC

ENUM ERR_OK,ERR_ACTION,ERR_SUBSTITUTION,ERR_CONSTANT,ERR_PARSE,
  ERR_DEEP_NEST

-> Globals
DEF targets:PTR TO target_hash,
  constants:PTR TO constant_hash,
  current_target:PTR TO target,
  start:PTR TO target,
  args:PTR TO arguments,line_buffer,line_buf,
  file,line_number,parse_actions,include_level

PROC is_whitespace(c)
  IF c=' ' OR c='\t' THEN RETURN TRUE
ENDPROC FALSE

PROC trim(line)
  DEF pos:REG,c:REG

  -> seek past front space
  pos:=0
  LOOP
    c:=line[pos]
    IFN is_whitespace(c) THEN EXIT
    INC pos
    -> Return empty string if whole string was whitespace.
    IF pos=EstrLen(line) THEN RETURN String(0)
  ENDLOOP
  IF pos>0 THEN RETURN MidStr(line,pos)
  -> unchanged line is returned by this point
ENDPROC line

PROC include_file(filename)
  DEF filebuf:PTR TO file_buffer,old_line,old_file,
    filt:PTR TO filter,split:PTR TO splitter,
    buf:PTR TO buffer,iter:PTR TO iterator

  INC include_level
  IF include_level>8 THEN Raise(ERR_DEEP_NEST)
  -> load and process build file
  filebuf.load_buffer(filename,TRUE)
  old_file:=file
  old_line:=line_number
  file:=filename
  line_number:=0
  NEW filt.init()
  NEW split.create(filt,filebuf.get_size())
  filt.process(filebuf)
  buf:=filt.get_output()
  iter:=buf.get_iterator()
  WHILE iter.next()
    parse_line(iter.get_current_item())
    INC line_number
  ENDWHILE
  file:=old_file
  line_number:=old_line
  DEC include_level
  END iter,filt,filebuf
ENDPROC

PROC parse_line(cstr)
  -> populate E-string
  SetStr(line_buf,0)
  StrAdd(line_buf,cstr)
  -> blank line?
  IF EstrLen(line_buf)=0
    parse_actions:=FALSE
    RETURN
  ENDIF
  -> action parsing
  IF is_whitespace(line_buf[0])
    IF parse_actions=FALSE THEN Raise(ERR_ACTION)
    add_action()
  ELSEIF StrCmp(line_buf,'#i',2) AND is_whitespace(line_buf[2])
    -> include file
    include_file(trim(MidStr(line_buf,3)))
  ELSE
    parse_rule()
  ENDIF
ENDPROC

PROC add_action()
  DEF text,node:PTR TO action_node

  substitutions()
  NEW node.init()
  node.command:=trim(line_buffer)
  current_target.actions.enqueue(node)
ENDPROC

PROC substitutions()
  DEF left:REG,prev:REG

  -> remove end-of-line comments
  left:=InStr(line_buf,'#')
  IF left>0 THEN SetStr(line_buf,left)
  -> Substitute constants
  prev:=0
  SetStr(line_buffer,0)
  left:=InStr(line_buf,'$(')
  WHILE left>=0
    prev:=substitute(left,line_buf)
    left:=InStr(line_buf,'$(',prev)
  ENDWHILE
  StrAdd(line_buffer,MidStr(line_buf,prev))
ENDPROC

-> recursively substitutes constants within constants
PROC substitute(cursor,source)
  DEF left:REG,right:REG,sub,k,node:PTR TO constant,prev:REG,
    i:PTR TO ordered_hash_iterator

  left:=cursor
  IF left>0
    StrAdd(line_buffer,MidStr(source,prev,left))
  ENDIF
  right:=InStr(source,')',left+2)
  if right<0 THEN Raise(ERR_SUBSTITUTION)
  k:=String(right-left)
  k:=MidStr(source,left,right-left)
  -> check for 'dep' reserved id
  IF StrCmp(k,'dep')
    NEW i.init(current_target.dependencies)
    IF i.next()
      StrAdd(line_buffer,i.get_current_item())
    ELSE
      Raise(ERR_CONSTANT)
    ENDIF
    END i
  -> check for 'target' reserved id
  ELSEIF StrCmp(k,'target')
	sub:=get_target_key(current_target)
	IF sub=NIL THEN Raise(ERR_CONSTANT)
    StrAdd(line_buffer,sub)
  ELSE
    node:=constants.find(k)
    IF node=NIL THEN Raise(ERR_CONSTANT)
    sub:=node.substitution
    left:=InStr(sub,'$(')
    WHILE left>=0
      prev:=substitute(left,sub)
      left:=InStr(source,'$(',prev)
    ENDWHILE
    StrAdd(line_buffer,source,prev)
  ENDIF
ENDPROC right+1

PROC parse_rule()
  DEF target,strbuf:PTR TO estring_buffer,list,d:PTR TO dependency,name,
    out:PTR TO buffer,filt:PTR TO filter,split:PTR TO splitter,iter:PTR TO iterator

  substitutions()
  -> split line_buffer along spaces
  NEW strbuf.init(EstrLen(line_buffer))
  strbuf.append(line_buffer)
  NEW filt.init()
  NEW split.create(filt,EstrLen(line_buffer),32)
  filt.process(strbuf)
  out:=get_output(filt)
  iter:=out.get_iterator()
  -> check for colon on target name
  IFN iter.next() THEN Raise(ERR_PARSE)
  target:=iter.get_current_value()
  IF RightStr(target,1)=':'
    SetStr(target,EstrLen(target)-1)
    NEW current_target.init_target(target,targets)
    WHILE iter.next()
      name:=iter.get_current_value()
      -> suppress empty names from extra spaces in line_buffer 
      ->   being split into extra strings in the filter
      IF EstrLen(name)>0
        NEW d.init()
        d.name:=iter.get_current_value()
        d.bullseye:=UNINITIALIZED_PTR
        current_target.dependencies.add(d)
      ENDIF
    ENDWHILE
    parse_actions:=TRUE
  ELSE
    parse_constant()
  ENDIF
  END filt,strbuf
ENDPROC

PROC rtrim(estr)
  WHILE is_whitespace(RightStr(estr,1)) DO SetStr(estr,EstrLen(estr)-1)
ENDPROC estr

PROC parse_constant()
  DEF name,l:REG,value,k:PTR TO constant

  l:=InStr(line_buffer,'=')
  IF l<0 THEN Raise(ERR_FORMAT)
  name:=String(l)
  StrCopy(name,line_buffer,l)
  name:=rtrim(name)
  value:=String(EstrLen(line_buf)-l)
  value:=MidStr(line_buffer,l+1)
  -> Truncate comments
  l:=InStr(value,'#')
  IF l>=0 THEN SetStr(value,l)
  value:=rtrim(value)
  NEW k.init(name,constants)
  k.substitution:=value
  constants.add(k)
ENDPROC

PROC main() HANDLE
  DEF rdargs

  -> parse arguments
  NEW args
  rdargs:=ReadArgs('TARGET,FROM/K,FORCE/S,VERBOSE/S,NOHEAD/S,CONSTANTS/S',
    args,NIL)
  IF rdargs=NIL THEN Raise("ARGS")
  IF args.file=NIL THEN args.file:='.build'
  IF args.nohead=NIL
    PrintF('\s\n\tProcessing: \s\n',version_string,args.file)
  ENDIF
  
  -> Initialization
  NEW targets.init()
  NEW constants.init()
  parse_actions:=FALSE
  include_level:=0
  file:=args.file
  line_number:=0
  -> double buffer for constant substitutions
  line_buffer:=String(4096)
  line_buf:=String(1024)
  
  -> process build file
  include_file(args.file)

EXCEPT
  SELECT exception
    CASE ERR_OK
      IF args.verbose THEN PrintF('Exiting successfully.')
    CASE ERR_ACTION
      PrintF('Leading whitespace on non-action line or action before rule.\n'+
        'in file \s line number \d\n',file,line_number)
    CASE ERR_SUBSTITUTION
      PrintF('Missing right parenthesis on substitution.\n'+
        'in file \s line number \d\n',file,line_number)
    CASE ERR_CONSTANT
      PrintF('Unknown substitution.\n'+
        'in file \s line number \d\n',file,line_number)
    CASE ERR_PARSE
      PrintF('Rule failed to parse.\n'+
        'in file \s line number \d\n',file,line_number)
    CASE ERR_DEEP_NEST
      PrintF('Nesting level is \d in file \s.\n',include_level,file)
    CASE "MEM"
      PrintF('Out of memory.\n')
    CASE "OPEN"
      PrintF("Input file couldn\'t be read.\n"+
        'in file \s line number \d\n',file,line_number)
    CASE "IN"
      PrintF('Input processing error.\n')
    CASE "FILT"
      PrintF('Internal error in filter processing.')
    CASE "INIT"
      PrintF('Error in initialization.')
    DEFAULT
      PrintF('Unhandled exception encountered.\n')
  ENDSELECT
  IF rdargs THEN FreeArgs(rdargs)
ENDPROC

version_tag: CHAR 0,'$VER:'
version_string: CHAR 'Builder v0.1 by Samuel D. Crow\n'+
  ' based on EBuild by Rob, Wouter, Jason and Glauschwuffel\n',0
