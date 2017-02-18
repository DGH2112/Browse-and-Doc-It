(**

  This module contains a class that represents a generic function (a base class for properties
  and methods).

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

**)
Unit BADI.Generic.FunctionDecl;

Interface

Uses
  Classes,
  Contnrs,
  BADI.ElementContainer,
  BADI.Generic.TypeDecl,
  BADI.Generic.Parameter,
  BADI.Types,
  BADI.Comment;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class is an ancester for both methods and properties so that they
      can be handled generically (parameters and returntypes). **)
  TGenericFunction = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FParameters: TObjectList;
    FReturnType: TGenericTypeDecl;
    FStartLine: Integer;
    FEndLine: Integer;
    FHasProfiling: Boolean;
    FIndent: Integer;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetQualifiedName: String; Virtual; Abstract;
    Function GetParameterCount: Integer;
    Function GetParameters(iIndex: Integer): TGenericParameter;
    Function RequiresReturn: Boolean; Virtual; Abstract;
    Function FunctionType: String; Virtual; Abstract;
  Public
    Constructor Create(const strName: String; AScope: TScope; iLine, iColumn: Integer;
      AImageIndex: TBADIImageIndex; AComment: TComment); Override;
    Destructor Destroy; Override;
    Procedure AddParameter(AParameter: TGenericParameter);
    Function LineofCode: Integer;
    (**
      This property returns the number of parameter in the parameter collection.
      @precon  None.
      @postcon Returns the number of parameter in the parameter collection.
      @return  an Integer
    **)
    Property ParameterCount: Integer Read GetParameterCount;
    (**
      This property returns an instance of the indexed parameter.
      @precon  iIndex must be a valid index.
      @postcon Returns an instance of the indexed parameter.
      @param   iIndex as       an Integer
      @return  a TGenericParameter
    **)
    Property Parameters[iIndex: Integer]: TGenericParameter Read GetParameters;
    (**
      This property returns the type corresponding to the method return.
      @precon  None.
      @postcon Returns the type corresponding to the method return.
      @return  a TGenericTypeDecl
    **)
    Property ReturnType: TGenericTypeDecl Read FReturnType;
    (**
      Returns the Qualified name of the method.
      @precon  None.
      @postcon Returns the Qualified name of the method.
      @return  a String
    **)
    Property QualifiedName: String Read GetQualifiedName;
    (**
      This property gets and sets the start line number for the code within the
      function.
      @precon  None.
      @postcon Gets and sets the start line number for the code within the
               function.
      @return  an Integer
    **)
    Property StartLine: Integer Read FStartLine Write FStartLine;
    (**
      This property gets and sets the end line number for the code within the
      function.
      @precon  None.
      @postcon Gets and sets the end line number for the code within the
               function.
      @return  an Integer
    **)
    Property EndLine: Integer Read FEndLine Write FEndLine;
    (**
      This property gets and sets whether the function has code profiling
      instrumentation installed.
      @precon  None.
      @postcon Gets and sets whether the function has code profiling
               instrumentation installed.
      @return  a Boolean
    **)
    Property HasProfiling: Boolean Read FHasProfiling Write FHasProfiling;
    (**
      This property gets and sets the Indent of the method.
      @precon  None.
      @postcon Gets and sets the Indent of the method.
      @return  an Integer
    **)
    Property Indent: Integer Read FIndent Write FIndent;
  End;

  (** A type to define sub classes of TGenericFunction **)
  TGenericFunctionClass = Class Of TGenericFunction;

Implementation

(**

  This method adds the given parameter to the internal list.

  @precon  AParameter must be a valid instance.
  @postcon Adds the given parameter to the internal list.

  @param   AParameter as a TGenericParameter

**)
procedure TGenericFunction.AddParameter(AParameter: TGenericParameter);
begin
  FParameters.Add(AParameter);
end;

(**

  This is a constructor for the TGenericFunction class.

  @precon  None.
  @postcon Creates an object list for parameters and initialises the return type
           to nil.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
constructor TGenericFunction.Create(const strName: String; AScope: TScope; iLine,
  iColumn: Integer; AImageIndex: TBADIImageIndex; AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FParameters := TObjectList.Create(True);
  FReturnType := TGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  FStartLine  := -1;
  FEndLine    := -1;
end;

(**

  This is a destructor for the TGenericFunction class.

  @precon  None.
  @postcon Frees the memory for the parameters and return type.

**)
destructor TGenericFunction.Destroy;
begin
  FReturnType.Free;
  FParameters.Free;
  Inherited Destroy;
end;

(**

  This is a getter method for the ParameterCount property.

  @precon  None.
  @postcon Returns the number of parameters associated with the method.

  @return  an Integer

**)
function TGenericFunction.GetParameterCount: Integer;
begin
  Result := FParameters.Count;
end;

(**

  This is a getter method for the Parameters property.

  @precon  iIndex must be a valid index.
  @postcon Returns the index instance of the paramter.

  @param   iIndex as an Integer
  @return  a TGenericParameter

**)
function TGenericFunction.GetParameters(iIndex: Integer): TGenericParameter;
begin
  Result := FParameters[iIndex] As TGenericParameter;
end;

(**

  This method returns the number of lines of code in the function.

  @precon  None.
  @postcon Returns the number of lines of code in the function.

  @return  an Integer

**)
function TGenericFunction.LineofCode: Integer;
begin
  Result := FEndLine - FStartLine + 1;
end;

End.
